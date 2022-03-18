# Welcome to `servant-typescript` [![Hackage](https://img.shields.io/hackage/v/servant-typescript.svg)](https://hackage.haskell.org/package/servant-typescript) ![servant-typescript](https://github.com/codedownio/servant-typescript/workflows/servant-typescript/badge.svg)


This library generates TypeScript client libraries for Servant.

First, make sure you have [TypeScript](https://hackage.haskell.org/package/aeson-typescript) instances defined for all of the types used in the API.

```haskell
data User = User {
  name :: String
  , age :: Int
  , email :: String
  } deriving (Eq, Show)
deriveJSONAndTypeScript A.defaultOptions ''User
```

If you need to generate lots of boilerplate instances, the functions in `aeson-typescript`'s [Recursive](https://hackage.haskell.org/package/aeson-typescript-0.4.0.0/docs/Data-Aeson-TypeScript-Recursive.html) module can be your friend.
I've used [recursivelyDeriveMissingTypeScriptInstancesFor](https://hackage.haskell.org/package/aeson-typescript-0.4.0.0/docs/Data-Aeson-TypeScript-Recursive.html#v:recursivelyDeriveMissingTypeScriptInstancesFor) to derive instances for the Kubernetes API.

Next, you'll need some Servant API:

```haskell
type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "albert" :> Get '[JSON] User
          :<|> "isaac" :> Get '[JSON] User
```

Generating the library is as simple as this:

```haskell
main = writeTypeScriptLibrary (Proxy :: Proxy UserAPI) "/my/destination/folder/"
```
## Caveats

* This library doesn't yet support generating generic TypeScript functions to match generic TypeScript instances. You can hack around this by writing your own `getFunctions` and hardcoding them manually for the necessary types.

## Supporting additional combinators

If you use unusual Servant combinators in your API, you may need to define additional `HasForeign` instances to explain how to convert them to TypeScript. For example, when I work with the [servant-websockets](https://hackage.haskell.org/package/servant-websockets) package, I add instances like the following.

The same applies to custom `AuthProtect` combinators from [Servant.API.Experimental.Auth](https://hackage.haskell.org/package/servant-0.19/docs/Servant-API-Experimental-Auth.html), etc.

```haskell
instance HasForeign LangTS Text WebSocket where
    type Foreign Text WebSocket = Text
    foreignFor _ _ _ _ = "void"

instance HasForeign LangTSDecls [TSDeclaration] WebSocket where
    type Foreign [TSDeclaration] WebSocket = [TSDeclaration]
    foreignFor _ _ _ _ = []
```
