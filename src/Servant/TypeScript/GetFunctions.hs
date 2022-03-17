
module Servant.TypeScript.GetFunctions (
  getFunctions
  ) where

import Control.Lens
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Servant.Foreign.Internal as FI


getFunctions :: [Req Text] -> (Req Text -> Text) -> Text
getFunctions reqs getFunctionName =
  [i|import queryString from "query-string";\n\n|]
  <> (T.intercalate "\n" $ fmap (reqToFunction getFunctionName) reqs)

reqToFunction :: (Req Text -> Text) -> Req Text -> Text
reqToFunction getFunctionName req = [i|
export function #{getFunctionName req}#{getGenericBrackets req}(#{getFunctionArgs req}): Promise<#{getReturnType req}> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "#{req ^. reqMethod}",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };

  #{case (req ^. reqBody) of Nothing -> ("" :: Text); Just _ -> "\n  options.body = JSON.stringify(body);\n" }
  let params = {#{T.intercalate ", " (getQueryParamNames req)}};
  return (fetchFn || window.fetch)(`#{getPath req}` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        #{if hasReturn req
          then ("return response.json().then((json) => resolve(json));" :: Text)
          else "resolve();"}
      }
    });
  });
}|]

hasReturn :: Req Text -> Bool
hasReturn req = case req ^. reqReturnType of
  Nothing -> False
  Just "void" -> False
  Just _ -> True

getQueryParamNames :: Req Text -> [Text]
getQueryParamNames req = [x ^. (queryArgName . argName . _PathSegment)
                         | x <- req ^. (reqUrl . queryStr)]

getFunctionArgs :: Req Text -> Text
getFunctionArgs req = T.intercalate ", " $ catMaybes $
  maybeBodyArg
  : (fmap formatCaptureArg $ req ^. (reqUrl . path))
  <> (fmap (Just . formatQueryArg) $ req ^. (reqUrl . queryStr))
  <> [Just [i|fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>|]]

  where
    maybeBodyArg = case req ^. reqBody of
      Nothing -> Nothing
      Just x -> Just [i|body: #{x}|]

formatCaptureArg :: Segment Text -> Maybe Text
formatCaptureArg (Segment (Static {})) = Nothing
formatCaptureArg (Segment (Cap arg)) = Just [i|#{arg ^. (argName . _PathSegment)}: #{arg ^. argType}|]

formatQueryArg :: QueryArg Text -> Text
formatQueryArg arg = case arg ^. queryArgType of
  Normal -> [i|#{name}?: #{typ}|]
  Flag -> [i|#{name}?: boolean|]
  FI.List -> [i|#{name}?: [#{typ}]|]
  where
    qaName = arg ^. queryArgName
    name = qaName ^. (argName . _PathSegment)
    typ = qaName ^. argType

getReturnType :: Req Text -> Text
getReturnType req = fromMaybe "void" (req ^. reqReturnType)

getGenericBrackets :: Req Text -> Text
getGenericBrackets _req = ""

getPath :: Req Text -> Text
getPath req = "/" <> (T.intercalate "/" $ fmap formatPathSegment (req ^. (reqUrl . path)))
  where
    formatPathSegment :: Segment Text -> Text
    formatPathSegment (Segment (Static (PathSegment t))) = t
    formatPathSegment (Segment (Cap ((^. argName) -> (PathSegment t)))) = [i|${#{t}}|]
