
module Servant.TypeScript.Util where

import Control.Lens
import Data.Char
import Data.Maybe
import Data.Proxy
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Servant.API
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
getGenericBrackets req
  | "PackageStoreConfig<T>" `inside` req = "<T extends keyof StoreConfig>"
  | "PackageStoreT<T>" `inside` req = "<T extends keyof PackageStoreDeploymentInfo>"
  | "NodeInfo<T>" `inside` req = "<T extends keyof NodeDetails>"
  | "FullRunnerConfigV<T>" `inside` req = "<T extends keyof RunnerConfig>"
  | "RunnerDeployableInfoT<T>" `inside` req = "<T extends keyof RunnerDeploymentInfo>"
  | "RunnerInstance<T>" `inside` req = "<T extends keyof RunnerInstance>"
  | "RunnerInstanceWrapper<T>" `inside` req = "<T extends keyof RunnerInstance>"
  | "StoreInstance<T>" `inside` req = "<T extends keyof StoreInstance>"
  | "StoreInstanceWrapper<T>" `inside` req = "<T extends keyof StoreInstance>"
  | "SandboxDeployableInfoT<T>" `inside` req = "<T extends keyof SandboxDeploymentInfo>"
  | "PackageStoreDeployableInfoT<T>" `inside` req = "<T extends keyof PackageStoreDeploymentInfo>"
  | "SandboxT<T>" `inside` req = "<T extends keyof SandboxDeploymentInfo>"
  | "RunnerT<T>" `inside` req = "<T extends keyof RunnerDeploymentInfo>"
  | "<T>" `inside` req = "<T>"
  | otherwise = if "<T>" `T.isInfixOf` (getFunctionArgs req) then "<T>" else ""
  where
    t `inside` req = (((t `T.isInfixOf`) <$> (req ^. reqReturnType)) == Just True) || t `T.isInfixOf` (getFunctionArgs req)

getPath :: Req Text -> Text
getPath req = "/" <> (T.intercalate "/" $ fmap formatPathSegment (req ^. (reqUrl . path)))
  where
    formatPathSegment :: Segment Text -> Text
    formatPathSegment (Segment (Static (PathSegment t))) = t
    formatPathSegment (Segment (Cap ((^. argName) -> (PathSegment t)))) = [i|${#{t}}|]

snakeToCamel :: Text -> Text
snakeToCamel t = toCamelList $ T.splitOn "_" t

toCamelList :: [Text] -> Text
toCamelList [] = ""
toCamelList [x] = T.toLower x
toCamelList (x:xs) = mconcat ((T.toLower x) : fmap capitalize xs)

capitalize :: Text -> Text
capitalize t | T.length t == 1 = T.toUpper t
capitalize t = (toUpper $ T.head t) `T.cons` (T.tail t)
