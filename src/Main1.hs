module Main1 where

import Control.Monad.Trans.State.Strict
import Control.Monad

type Request = String
type Response = String
type Application = Request -> Response

type Middleware = Application -> Application

newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = State AppState

routeAction1 :: Request -> Response
routeAction1 request = mkResponse request "Hello from Route 1"

routeAction2 :: Request -> Response
routeAction2 request = mkResponse request "Hello from Route 2"

notFound :: Request -> Response
notFound request = mkResponse request "Hello from the DEFAULT route"

mkResponse :: String -> String -> String
mkResponse req msg = unwords ["Request:", req, "\nResponse:", msg]

myApp :: AppStateT ()
myApp = do
  addRoute "one" routeAction1
  addRoute "two" routeAction2

main :: IO ()
main = myServer myApp

addRoute :: String -> (Request -> Response) -> AppStateT ()
addRoute pat rA = modify $ \s -> addRoute' (route pat rA) s

addRoute' :: Middleware -> AppState -> AppState
addRoute' m s@AppState {routes = ms} = s {routes = m:ms}

route :: String -> (Request -> Response) -> Middleware
route pat routeAction nextApp req =
  let tryNext = nextApp req in
  if pat == req
  then
    routeAction req
  else
    tryNext

runMyApp :: (Request -> Response) -> AppState -> Request -> Response
runMyApp defHandler appState =
  foldl (flip ($)) defHandler (routes appState)

userInputLoop :: AppState -> IO ()
userInputLoop appState = do
  putStrLn "Awaiting requests..."
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp notFound appState request
    putStrLn response
    userInputLoop appState

myServer :: AppStateT () -> IO ()
myServer myApp = do
  let appState = execState myApp AppState{routes=[]}
  userInputLoop appState
