
module Servant.TypeScript.Types where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Aeson as A hiding (Options)
import Data.Aeson.TH as A hiding (Options)
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import qualified Data.Set as S
import Data.String.Interpolate
import qualified Data.Text as T
import Servant.API
import Servant.Foreign.Internal as FI
import Servant.TypeScript.Util
import System.Directory
import System.FilePath


-- | Foreign type for getting TS types
data LangTS
instance (TypeScript a) => HasForeignType LangTS T.Text a where
  typeFor _proxyLang _proxyFtype proxyA = T.pack $ getTypeScriptType proxyA

-- | Foreign type for getting TS declarations
data LangTSDecls
instance (TypeScript a) => HasForeignType LangTSDecls [TSDeclaration] a where
  typeFor _proxyLang _proxyFtype proxyA = getTypeScriptDeclarationsRecursively proxyA

data ServantTypeScriptOptions = ServantTypeScriptOptions {
  -- | Extra TypeScript types to include in the `d.ts` file.
  -- Useful if you want to expose types that don't appear in your API, for whatever reason.
  extraTypes :: [TSType]

  -- | Determine to which output file the client function for the given request is mapped.
  -- Useful for larger APIs.
  -- A good approach is to split on "case req ^. (reqFuncName . _FunctionName) of ..."
  -- By default, puts everything in "all.ts"
  , getFileKey :: Req T.Text -> FilePath

  -- | Mangle a given request into a corresponding client function name.
  -- By default, just prepends the HTTP method to the camel-cased route.
  , getFunctionName :: Req T.Text -> T.Text
  }

defaultServantTypeScriptOptions = ServantTypeScriptOptions {
  extraTypes = []

  , getFileKey = const "all"

  , getFunctionName = \req -> case req ^. (reqFuncName . _FunctionName) of
      (method:xs) -> toCamelList $ fmap snakeToCamel (method:xs)
      _ -> error [i|Case not handled in getFunctionName: '#{req}'|]
  }