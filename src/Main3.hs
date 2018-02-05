module Main3 where

import Control.Monad.Trans.State.Strict
import Control.Monad
import Control.Monad.Error.Class

type Request = String
type Response = String

type Application = Request -> Response
type ActionT = Either ActionError Response
type ActionError = String

type Route = Application -> Application

newtype AppState = AppState { routes :: [Route] }
type AppStateT = State AppState

-- Route Handlers -------------------------------
constructResponse :: String -> String -> String
constructResponse req msg = unwords ["Request:", req, "\nResponse:", msg]

routeHandler1 :: Request -> ActionT
routeHandler1 request = return $
  constructResponse request "Hello from Route 1"

routeHandler2 :: Request -> ActionT
routeHandler2 request = throwError "Error in Route 2"

notFound :: Request -> Response
notFound request = constructResponse request "Hello from the DEFAULT route"
-------------------------------------------------

-- App State ------------------------------------
myApp :: AppStateT ()
myApp = do
  addRoute "one" routeHandler1
  addRoute "two" routeHandler2

myServer :: AppStateT () -> IO ()
myServer myApp = do
  let appState = execState myApp AppState{routes=[]}
  userInputLoop appState

main :: IO ()
main = myServer myApp
-------------------------------------------------

-- Adding Routes --------------------------------
addRoute :: String -> (Request -> ActionT) -> AppStateT ()
addRoute pat mf = modify $ \s -> addRoute' (route pat mf) s

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route :: String -> (Request -> ActionT) -> Route
route pat rhandler mw req =
  let tryNext = mw req in
  if pat == req
  then
    either ("Error: " ++ ) id (rhandler req)
  else
    tryNext
-------------------------------------------------

-- Running the App ------------------------------
runMyApp :: Application -> AppState -> Application
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
-------------------------------------------------
