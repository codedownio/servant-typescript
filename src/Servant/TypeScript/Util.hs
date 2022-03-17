
module Servant.TypeScript.Util where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T


snakeToCamel :: Text -> Text
snakeToCamel t = toCamelList $ T.splitOn "_" t

toCamelList :: [Text] -> Text
toCamelList [] = ""
toCamelList [x] = T.toLower x
toCamelList (x:xs) = mconcat (T.toLower x : fmap capitalize xs)

capitalize :: Text -> Text
capitalize t | T.length t == 1 = T.toUpper t
capitalize t = toUpper (T.head t) `T.cons` T.tail t
