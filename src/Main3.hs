module Main3 where

import Control.Monad.Trans.State.Strict
import Control.Monad
import Control.Monad.Error.Class

type Request = String
type Response = String

type Application = Request -> Response
type Middleware = Application -> Application

type ActionT = Either ActionError Response
type ActionError = String


newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = State AppState

-- Route Handlers -------------------------------
routeAction1 :: Request -> ActionT
routeAction1 request = return $
  textResponse request "Hello from Route 1"

routeAction2 :: Request -> ActionT
routeAction2 request = throwError "Error in Route 2"

notFound :: Request -> Response
notFound request = textResponse request "Hello from the DEFAULT route"

textResponse :: String -> String -> String
textResponse req msg = unwords ["Request:", req, "\nResponse:", msg]
-------------------------------------------------

-- App State ------------------------------------
myApp :: AppStateT ()
myApp = do
  addRoute "one" routeAction1
  addRoute "two" routeAction2

myServer :: AppStateT () -> IO ()
myServer myApp = do
  let appState = execState myApp AppState{routes=[]}
  userInputLoop appState

main :: IO ()
main = myServer myApp
-------------------------------------------------

-- Adding Routes --------------------------------
addRoute :: String -> (Request -> ActionT) -> AppStateT ()
addRoute pat action = modify $ \s -> addRoute' (route pat action) s

addRoute' :: Middleware -> AppState -> AppState
addRoute' m s@AppState {routes = ms} = s {routes = m:ms}

route :: String -> (Request -> ActionT) -> Middleware
route pat action nextApp req =
  let tryNext = nextApp req in
  if pat == req
  then
    either ("Error: " ++ ) id (action req)
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
