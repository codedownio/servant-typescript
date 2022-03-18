
module Servant.TypeScript.Types where

import Control.Lens
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import Data.String.Interpolate
import qualified Data.Text as T
import Servant.Foreign.Internal as FI
import qualified Servant.TypeScript.GetFunctions as GetFunctions
import Servant.TypeScript.Util


-- | Foreign type for getting TS types
data LangTS
instance (TypeScript a) => HasForeignType LangTS T.Text a where
  typeFor _proxyLang _proxyFtype proxyA = T.pack $ getTypeScriptType proxyA

-- | Foreign type for getting TS declarations
data LangTSDecls
instance (TypeScript a) => HasForeignType LangTSDecls [TSDeclaration] a where
  typeFor _proxyLang _proxyFtype proxyA = getTypeScriptDeclarationsRecursively proxyA

data ServantTypeScriptOptions = ServantTypeScriptOptions {
  -- | Extra TypeScript types to include in the @d.ts@ file.
  --
  -- Useful if you want to expose types that don't appear in your API, for whatever reason.
  extraTypes :: [TSType]

  -- | Determine to which output file the client function for the given request is mapped.
  --
  -- Useful to break up larger APIs into separate files based on criteria like route prefixes.
  --
  -- It's fine if the file key contains sub-directories; they will be created as needed.
  --
  -- A good approach is to split on @case req ^. (reqFuncName . _FunctionName) of ...@.
  --
  -- Default implementation is @const "client.ts"@.
  , getFileKey :: Req T.Text -> FilePath

  -- | Mangle a given request into a corresponding client function name.
  -- By default, just prepends the HTTP method to the camel-cased route.
  , getFunctionName :: Req T.Text -> T.Text

  -- | Given a list of requests, output a complete TypeScript module with the (exported) client functions,
  -- ready to be consumed by your TypeScript app.
  --
  -- For example, you can import dependencies at the top
  -- to use in your functions. The default version relies on the NPM "query-string" package to construct
  -- URLs. It uses the built-in @window.fetch@ by default, but allows you to pass your own @fetch@ function
  -- instead (useful for server-side rendering etc.). The default client functions return @Promise@s with
  -- the given return value, and on failure they reject the promise with a value of interface
  -- @{ status: number; text: string;  }@.
  --
  -- If you want to write your own 'getFunctions', check out the 'Servant.TypeScript.GetFunctions' module for
  -- inspiration.
  --
  -- The first argument passed to 'getFunctions' is the 'getFunctionName' function.
  , getFunctions :: (Req T.Text -> T.Text) -> [Req T.Text] -> T.Text
  }

-- | Reasonable default options.
defaultServantTypeScriptOptions :: ServantTypeScriptOptions
defaultServantTypeScriptOptions = ServantTypeScriptOptions {
  extraTypes = []

  , getFileKey = const "client.ts"

  , getFunctionName = \req -> case req ^. (reqFuncName . _FunctionName) of
      (method:xs) -> toCamelList $ fmap snakeToCamel (method:xs)
      _ -> error [i|Case not handled in getFunctionName: '#{req}'|]

  , getFunctions = GetFunctions.getFunctions
  }
