{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}

{-|
Module:      Servant.TypeScript.Types
Copyright:   (c) 2022 Tom McLaughlin
License:     BSD3
Stability:   experimental
Portability: portable

This library generates TypeScript client libraries for Servant.

First, make sure you have 'TypeScript' instances defined for all of the types used in the API.

@
data User = User {
  name :: String
  , age :: Int
  , email :: String
  } deriving (Eq, Show)
deriveJSONAndTypeScript A.defaultOptions ''User
@

If you need to generate lots of boilerplate instances, the functions in @aeson-typescript@'s 'Data.Aeson.TypeScript.Recursive' module can be your friend.
I've used 'Data.Aeson.TypeScript.Recursive.recursivelyDeriveMissingTypeScriptInstancesFor' to derive instances for the Kubernetes API.

Next, you'll need some Servant API:

@
type UserAPI = "users" :> Get '[JSON] [User]
          :\<|\> "albert" :> Get '[JSON] User
          :\<|\> "isaac" :> Get '[JSON] User
@

Generating the library is as simple as this:

@
main = writeTypeScriptLibrary (Proxy :: Proxy UserAPI) "\/my\/destination\/folder\/"
@

-}


module Servant.TypeScript (
  writeTypeScriptLibrary
  , writeTypeScriptLibrary'

  -- * Options
  , ServantTypeScriptOptions
  , defaultServantTypeScriptOptions
  , extraTypes
  , getFileKey
  , getFunctionName
  , getFunctions

  -- * Misc
  , MainConstraints
  ) where

import Control.Lens
import Control.Monad.Reader
import Data.Aeson.TypeScript.TH
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import qualified Data.Set as S
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Servant.Foreign
import Servant.TypeScript.Types
import Servant.TypeScript.Util
import System.Directory
import System.FilePath


type MainConstraints api = (
  HasForeign LangTSDecls [TSDeclaration] api
  , GenerateList [TSDeclaration] (Foreign [TSDeclaration] api)
  , HasForeign LangTS T.Text api
  , GenerateList T.Text (Foreign T.Text api)
  )

-- | Write the TypeScript client library for the given API to the given folder using default options.
writeTypeScriptLibrary :: MainConstraints api => Proxy api -> FilePath -> IO ()
writeTypeScriptLibrary = writeTypeScriptLibrary' defaultServantTypeScriptOptions

-- | Write the TypeScript client library for the given API to the given folder.
writeTypeScriptLibrary' :: forall api. MainConstraints api => ServantTypeScriptOptions -> Proxy api -> FilePath -> IO ()
writeTypeScriptLibrary' opts _ rootDir = flip runReaderT opts $ do
  writeClientTypes (Proxy @api) rootDir
  writeClientLibraries (Proxy @api) rootDir

writeClientTypes :: forall api. (
  HasForeign LangTSDecls [TSDeclaration] api
  , GenerateList [TSDeclaration] (Foreign [TSDeclaration] api)
  ) => Proxy api -> FilePath -> ReaderT ServantTypeScriptOptions IO ()
writeClientTypes _ folder = do
  -- Types from API
  let decls = S.toList $ S.fromList $ getAllTypesFromReqs (getReqsWithDecls (Proxy :: Proxy api))

  -- Extra types not mentioned in the API (used in websocket protocols)
  extra <- asks extraTypes
  let decls' = mconcat [getTypeScriptDeclarations x | TSType x <- extra]

  liftIO $ writeFile (folder </> "client.d.ts") (formatTSDeclarations (L.nub (decls <> decls')))

writeClientLibraries :: forall api. (
  HasForeign LangTS T.Text api
  , GenerateList T.Text (Foreign T.Text api)
  ) => Proxy api -> FilePath -> ReaderT ServantTypeScriptOptions IO ()
writeClientLibraries _ folder = do
  -- Write the functions
  let allEndpoints = getEndpoints (Proxy :: Proxy api)
  ServantTypeScriptOptions {..} <- ask
  let groupedMap = groupBy getFileKey allEndpoints
  forM_ (M.toList groupedMap) $ \(fileKey, reqs) -> do
    let (dir, _) = splitFileName fileKey
    liftIO $ createDirectoryIfMissing True (folder </> dir)

    let path' = folder </> fileKey
    let functionNames = fmap getFunctionName reqs
    when (L.length functionNames /= S.size (S.fromList functionNames)) $ do
      let duplicates = L.foldl' (flip (M.alter (\case Nothing -> Just (1 :: Integer); Just x -> Just (x + 1)))) mempty functionNames
      error [i|Duplicate function names found when trying to generate '#{path'}': #{M.filter (>= 2) duplicates}|]

    liftIO $ T.writeFile path' (getFunctions getFunctionName reqs)
  where
    groupBy :: Ord k => (v -> k) -> [v] -> M.Map k [v]
    groupBy key as = M.fromListWith (++) as'
      where as' = map ((,) <$> key <*> (:[])) as

getReqsWithDecls :: (HasForeign LangTSDecls [TSDeclaration] api, GenerateList [TSDeclaration] (Foreign [TSDeclaration] api))
  => Proxy api -> [Req [TSDeclaration]]
getReqsWithDecls = listFromAPI (Proxy :: Proxy LangTSDecls) (Proxy :: Proxy [TSDeclaration])

getAllTypesFromReqs :: forall a. (Eq a, Ord a) => [Req [a]] -> [a]
getAllTypesFromReqs reqs = S.toList $ S.fromList vals
  where vals :: [a] = mconcat $ mconcat [catMaybes [req ^. reqReturnType, req ^. reqBody]
                                          <> getTypesFromUrl (req ^. reqUrl)
                                          <> concatMap getTypesFromHeaderArg (req ^. reqHeaders)
                                        | req <- reqs]
        getTypesFromUrl (Url path' queryArgs _) = concatMap getTypesFromSegment path' <> concatMap getTypesFromQueryArg queryArgs
        getTypesFromSegment (Segment (Static {})) = []
        getTypesFromSegment (Segment (Cap arg)) = [arg ^. argType]

        getTypesFromQueryArg queryArg = [queryArg ^. (queryArgName . argType)]

        getTypesFromHeaderArg ha = [ha ^. (headerArg . argType)]

getEndpoints :: (HasForeign LangTS T.Text api, GenerateList T.Text (Foreign T.Text api)) => Proxy api -> [Req T.Text]
getEndpoints = listFromAPI (Proxy :: Proxy LangTS) (Proxy :: Proxy T.Text)
