module Main4 where

import Control.Monad.Trans.State.Strict
import Control.Monad
import Control.Monad.Error.Class

type Request = String
type Response = String

type Application = Request -> ActionT
type ActionT = Either ActionError Response
type ActionError = String

type Route = Application -> Application

newtype AppState = AppState { routes :: [Route] }
type AppStateT = State AppState

-- client methods
constructResponse :: String -> String -> String
constructResponse req msg = unwords ["Request:", req, "\nResponse:", msg]

routeHandler1 :: Request -> ActionT
routeHandler1 request = return $ constructResponse request "Hello from Route 1"

routeHandler2 :: Request -> ActionT
routeHandler2 request = throwError "Error in Route 2"

defaultRoute :: Request -> ActionT
defaultRoute request = return $ constructResponse request "Hello from the DEFAULT route"

myApp :: AppStateT ()
myApp = do
  addRoute "one" routeHandler1
  addRoute "two" routeHandler2

main :: IO ()
main = myServer myApp

-- framework methods
addRoute :: String -> Application -> AppStateT ()
addRoute pat mf = modify $ \s -> addRoute' (route pat mf) s

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route :: String -> (Request -> ActionT) -> Route
route pat mw mw1 input_string =
  let tryNext = mw1 input_string in
  if pat == input_string
  then
    mw input_string
  else
    tryNext

runMyApp :: (Request -> ActionT) -> AppState -> Request -> ActionT
runMyApp def app_state =
  foldl (flip ($)) def (routes app_state)

userInputLoop app_state = do
  putStrLn "Awaiting requests..."
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp defaultRoute app_state request
    case response of
      Right x -> putStrLn x
      Left e -> putStrLn $ "Error: " ++ e
    userInputLoop app_state

myServer :: AppStateT () -> IO ()
myServer myApp = do
  let appState = execState myApp AppState{routes=[]}
  userInputLoop appState
