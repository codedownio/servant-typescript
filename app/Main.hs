{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson as A
import Data.Aeson.TH as A hiding (Options)
import Data.Aeson.TypeScript.TH
import Data.Proxy
import Servant.API
import Servant.TypeScript


data User = User {
  name :: String
  , age :: Int
  , email :: String
  } deriving (Eq, Show)
deriveJSONAndTypeScript A.defaultOptions ''User

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "albert" :> Get '[JSON] User
          :<|> "isaac" :> Get '[JSON] User

main = writeTypeScriptLibrary (Proxy @UserAPI) "/tmp/test"
