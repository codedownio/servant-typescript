{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson as A
import Data.Aeson.TH as A hiding (Options)
import Data.Aeson.TypeScript.TH
import Data.Proxy
import Data.String.Interpolate
import Servant.API
import Servant.TypeScript
import System.IO.Temp


data User = User {
  name :: String
  , age :: Int
  , email :: String
  } deriving (Eq, Show)
deriveJSONAndTypeScript A.defaultOptions ''User

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "albert" :> Get '[JSON] User
          :<|> "isaac" :> Get '[JSON] User

main = do
  tmpDir <- getCanonicalTemporaryDirectory
  dir <- createTempDirectory tmpDir "servant-typescript-example"
  writeTypeScriptLibrary (Proxy @UserAPI) dir
  putStrLn [i|Wrote TypeScript library to: #{dir}|]
