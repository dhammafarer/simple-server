module Main4 where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.Error.Class

type Request = String
type Response = String

type Application = Request -> Response
type ActionT = ExceptT ActionError (Reader Request) Response
type ActionError = String

type Route = Application -> Application

newtype AppState = AppState { routes :: [Route] }
type AppStateT = State AppState

-- client methods
constructResponse :: String -> String -> String
constructResponse req msg = unwords ["Request:", req, "\nResponse:", msg]

routeHandler1 :: ActionT
routeHandler1 = do
  request <- lift ask
  return $ constructResponse request "Hello from Route 1"

routeHandler2 :: ActionT
routeHandler2 = throwError "Error in Route 2"

defaultRoute :: Application
defaultRoute request = "Hello from the DEFAULT route"

myApp :: AppStateT ()
myApp = do
  addRoute "one" routeHandler1
  addRoute "two" routeHandler2

main :: IO ()
main = myServer myApp

-- framework methods
addRoute :: String -> ActionT -> AppStateT ()
addRoute pat mf = modify $ \s -> addRoute' (route pat mf) s

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route :: String -> ActionT -> Route
route pat mw mw1 input_string =
  let tryNext = mw1 input_string in
    if pat == input_string
      then
        runAction mw input_string
      else
        tryNext

runMyApp :: Application -> AppState -> Application
runMyApp def app_state =
  foldl (flip ($)) def (routes app_state)

runAction :: ActionT -> Request -> Response
runAction action request = either id id
                           $ flip runReader request
                           $ runExceptT action

userInputLoop :: AppState -> IO ()
userInputLoop app_state = do
  putStrLn "Awaiting requests..."
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp defaultRoute app_state request
    putStrLn response
    putStrLn "---"
    userInputLoop app_state

myServer :: AppStateT () -> IO ()
myServer myApp = do
  let appState = execState myApp AppState{routes=[]}
  userInputLoop appState
