# Solga: simple typesafe routing [![Build Status](https://travis-ci.org/chpatrick/solga.svg?branch=master)](https://travis-ci.org/chpatrick/solga)

[Haddock](http://chpatrick.github.io/solga/doc/solga-0.1.0.0/Solga.html)

A library for easily specifying web APIs and implementing them in a type-safe way.

## Implementing services

At the center of Solga is a typeclass called `Router`. You can serve any `Router` as a WAI application:

```haskell
serve :: Router r => r -> Wai.Application
```

`Router`s are generally simple `newtypes`. For example, to serve a fixed JSON response, just use:

```haskell
-- From Solga:
newtype JSON a = JSON {jsonResponse :: a}

instance ToJSON a => Router (JSON a)
```
This router will respond to every request with the given `jsonResponse`, ie. `serve (JSON "It works!")` produces an `Application` that always responds with "It works!".

Routers can also be composed. Let's say you only want to respond to GET requests under `/does-it-work`. We'll encode the path and the method in the type itself with `DataKinds`.

```haskell
type MyAPI = Seg "does-it-work" (Method "GET" (JSON Text))

myAPI :: MyAPI
myAPI = Seg (Method (JSON "It works"))
```

There's some syntactic sugar we can apply here. First, let's use the `:>` operator to compose our routers. This is the same as type application.

```haskell
-- From Solga:
-- type f :> g = f g

type MyAPI = Seg "does-it-work" :> Method "GET" :> JSON Text
```

Second, we can replace `Seg` with `/>`:

```haskell
-- From Solga:
-- type (/>) (seg :: Symbol) g = Seg seg :> g

type MyAPI = "does-it-work" /> Method "GET" :> JSON Text
```

And third, we can get rid of the constructor boilerplate using `brief`:

```haskell
myAPI :: MyAPI
myAPI = brief "It works!"
```

What if we want to serve multiple different routes? It's easy - any product of Routers is automatically a Router, and Solga will try each field in order:

```haskell
data MyAPI = MyAPI
  { doesItWork :: "does-it-work" /> Method "GET" :> JSON Text
  , whatAboutThis :: "what-about-this" /> Method "GET" :> JSON Text
  } deriving (Generic)
instance Router MyAPI
instance Abbreviated MyAPI

myAPI :: MyAPI
myAPI = MyAPI
  { doesItWork = brief "It works!"
  , whatAboutThis = brief "It also works!"
  }
```

We can nest these record routers as expected:

```haskell
data UserAPI = UserAPI {..}
data WidgetAPI = WidgetAPI {..}

data MyAPI = MyAPI
  { userAPI :: "user" /> UserAPI 
  , widgetAPI :: "widget" /> WidgetAPI
  } deriving (Generic)
```

What if we want to capture a path segment? Let's see:

```haskell
-- newtype Capture a next = Capture {captureNext :: a -> next}

data MyAPI = MyAPI
  { echo :: "echo" /> Method "GET" :> Capture Text :> JSON Text
  } deriving (Generic)
instance Router MyAPI
instance Abbreviated MyAPI

myAPI :: MyAPI
myAPI = MyAPI
  { echo = brief id -- short for: Seg $ Method $ Capture $ \captured -> JSON captured
  }
```

How about doing IO?

```haskell
data MyAPI = MyAPI
  { rng :: "rng" /> Method "GET" :> WithIO :> JSON Int
  } deriving (Generic)
instance Router MyAPI
instance Abbreviated MyAPI

myAPI :: MyAPI
myAPI = MyAPI
  { rng = brief (getStdRandom random)
  }
```

Solga comes with a large set of useful Routers for parsing request bodies and producing responses. See the [documentation](http://chpatrick.github.io/solga/doc/solga-0.1.0.0/Solga.html) for more details.

## Creating Routers
To create a router yourself, just implement the Router typeclass:
```haskell
-- | The right hand side of `Application`. `Request` is already known.
type Responder = (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived

class Router r where
  -- | Given a request, if the router supports the given request
  -- return a function that constructs a response with a concrete router.
  tryRoute :: Wai.Request -> Maybe (r -> Responder)
```

In Solga, all routing decisions are performed purely on the type of the Router - it's not possible to use its _value_ to decide whether to accept a request or not. This is because this way an outer router can predict whether an inner router will match, even if the value of its implementation is non-deterministic.

For example, let's consider the router `CustomAuthRouter :> "foo" /> WithIO :> JSON Text`. We don't know exactly how `"foo" /> WithIO :> JSON Text` will be executed, as it contains `WithIO`. However, because of the restriction above, we can predict that it will only work for a path `/foo`. and so if we get a request with `/bar`, there's no need to do any authentication.

This is why the type of `tryRoute` is `Wai.Request -> Maybe (r -> Responder)`. The router instance essentially says "I can't handle this request, try something else" or "I can handle this, please give me the implementation".

For example, here is the implementation of the `JSON` router:

```haskell
instance Aeson.ToJSON a => Router (JSON a) where
  tryRoute _ = Just $ \json cont ->
    cont $ Wai.responseBuilder HTTP.status200 headers $ Aeson.encodeToBuilder $ Aeson.toJSON $ jsonResponse json
      where headers = [ ( HTTP.hContentType, "application/json" ) ]
```

`tryRouteNext` is a very useful function for implementing routers:
```haskell
tryRouteNext :: Router r' => (r -> r') -> Wai.Request -> Maybe (r -> Responder)
tryRouteNextIO :: Router r' => (r -> IO r') -> Wai.Request -> Maybe (r -> Responder)
```

Essentially, if you can convert from your type `r` to another Router type `r'`, you get the implementation for `tryRoute` for free. With this, it's easy to implement the Servant "fish operator":
```haskell
data left :<|> right = (:<|>) { altLeft :: left, altRight :: right }
  deriving (Eq, Ord, Show)

infixr 1 :<|>

instance (Router left, Router right) => Router (left :<|> right) where
  tryRoute req = tryRouteNext altLeft req <|> tryRouteNext altRight req
```

Or `Seg`:

```haskell
newtype Seg (seg :: Symbol) next = Seg { segNext :: next }
  deriving (Eq, Ord, Show)

instance (KnownSymbol seg, Router next) => Router (Seg seg next) where
  tryRoute req = case Wai.pathInfo req of
    s : segs | Text.unpack s == symbolVal (Proxy :: Proxy seg) ->
      tryRouteNext segNext req { Wai.pathInfo = segs }
    _ -> Nothing
```

## Swagger and client generation
It is also possible to generate Swagger specifications and TypeScript clients for Solga types. This has been implemented and will be published soon. Stay tuned!
