{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}

module Servant.TypeScript (
  writeTypeScriptLibrary
  , writeTypeScriptLibrary'

  , ServantTypeScriptOptions
  , defaultServantTypeScriptOptions
  , extraTypes
  , getFileKey
  , getFunctionName
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Aeson as A hiding (Options)
import Data.Aeson.TH as A hiding (Options)
import Data.Aeson.TypeScript.TH
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import qualified Data.Set as S
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Servant.API
import Servant.Foreign.Internal as FI
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

writeTypeScriptLibrary :: MainConstraints api => Proxy api -> FilePath -> IO ()
writeTypeScriptLibrary = writeTypeScriptLibrary' defaultServantTypeScriptOptions

writeTypeScriptLibrary' :: forall api. MainConstraints api => ServantTypeScriptOptions -> Proxy api -> FilePath -> IO ()
writeTypeScriptLibrary' opts _ rootDir = flip runReaderT opts $ do
  writeClientTypes (Proxy @api) "/tmp/test"
  writeClientLibraries (Proxy @api) "/tmp/test"

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

    let path' = folder </> fileKey <> ".ts"
    let functionNames = fmap getFunctionName reqs
    when (L.length functionNames /= S.size (S.fromList functionNames)) $ do
      let duplicates = L.foldl' (flip (M.alter (\case Nothing -> Just (1 :: Integer); Just x -> Just (x + 1)))) mempty functionNames
      error [i|Duplicate function names found when trying to generate '#{path'}': #{M.filter (>= 2) duplicates}|]

    liftIO $ T.writeFile path' (getFunctions reqs getFunctionName)
  where
    groupBy :: Ord k => (v -> k) -> [v] -> M.Map k [v]
    groupBy key as = M.fromListWith (++) as'
      where as' = map ((,) <$> key <*> (:[])) as

getReqsWithDecls :: (HasForeign LangTSDecls [TSDeclaration] api, GenerateList [TSDeclaration] (Foreign [TSDeclaration] api))
  => Proxy api -> [Req [TSDeclaration]]
getReqsWithDecls = listFromAPI (Proxy :: Proxy LangTSDecls) (Proxy :: Proxy [TSDeclaration])

getAllTypesFromReqs :: forall a. (Eq a, Ord a) => [Req [a]] -> [a]
getAllTypesFromReqs reqs = S.toList $ S.fromList vals
  where vals :: [a] = mconcat $ mconcat [(catMaybes [req ^. reqReturnType, req ^. reqBody])
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
