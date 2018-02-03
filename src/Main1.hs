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
routeHandler1 :: Request -> Response
routeHandler1 request = "hello from route: " ++ request

routeHandler2 :: Request -> Response
routeHandler2 request = "hello from route: " ++ request

defaultRoute :: Request -> Response
defaultRoute request = "hello from the default route"

myApp :: AppStateT ()
myApp = do
  addRoute "one" routeHandler1
  addRoute "two" routeHandler1

main :: IO ()
main = myServer myApp

-- framework methods

addRoute :: Monad m =>
  String -> (String -> String) -> StateT AppState m ()
addRoute pat mf = modify $ \s -> addRoute' (route pat mf) s

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route :: String -> (String -> String) -> Route
route pat mw mw1 input_string =
  let tryNext = mw1 input_string in
  if pat == input_string
  then
    mw input_string
  else
    tryNext

--runMyApp :: Application -> AppState -> Request ->
runMyApp def app_state request = do
  let output = foldl (flip ($)) def (routes app_state) request
  return output

userInputLoop app_state = do
  putStrLn "Please type in the request"
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp defaultRoute app_state request
    case response of
      Just x -> putStrLn x
      Nothing -> putStrLn "Error"
    userInputLoop app_state

myServer :: AppStateT () -> IO ()
myServer myApp = do
  let appState = execState myApp AppState{routes=[]}
  userInputLoop appState
