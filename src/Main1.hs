module Main1 where

import Control.Monad.Trans.State.Strict
import Control.Monad

type Request = String
type Response = String
type Application = Request -> Response

type Route = Application -> Application

newtype AppState = AppState { routes :: [Route] }
type AppStateT = State AppState

-- client methods
constructResponse :: String -> String -> String
constructResponse req msg = unwords ["Request:", req, "\nResponse:", msg]

routeHandler1 :: Request -> Response
routeHandler1 request = constructResponse request "Hello from Route 1"

routeHandler2 :: Request -> Response
routeHandler2 request = constructResponse request "Hello from Route 2"

defaultRoute :: Request -> Response
defaultRoute request = constructResponse request "Hello from the DEFAULT route"

myApp :: AppStateT ()
myApp = do
  addRoute "one" routeHandler1
  addRoute "two" routeHandler2

main :: IO ()
main = myServer myApp

-- framework methods

addRoute :: String -> (Request -> Response) -> AppStateT ()
addRoute pat mf = modify $ \s -> addRoute' (route pat mf) s

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route :: String -> (Request -> Response)
      -> ((Request -> Response) -> (Request -> Response))
route pat routehandler mw1 request =
  let tryNext = mw1 request in
  if pat == request
  then
    routehandler request
  else
    tryNext

runMyApp :: (Request -> Response) -> AppState -> Request -> Response
runMyApp def app_state =
  foldl (flip ($)) def (routes app_state)

userInputLoop :: AppState -> IO ()
userInputLoop app_state = do
  putStrLn "Awaiting requests..."
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp defaultRoute app_state request
    putStrLn response
    userInputLoop app_state

myServer :: AppStateT () -> IO ()
myServer myApp = do
  let appState = execState myApp AppState{routes=[]}
  userInputLoop appState
