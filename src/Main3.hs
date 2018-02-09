module Main3 where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.Error.Class

type Request = String
type Response = String

type Application = Request -> Response
type Middleware = Application -> Application

type ActionT = ExceptT ActionError (Reader Request) Response
type ActionError = String

newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = State AppState

-- Route Handlers ----------------------------------------------------
routeAction1 :: ActionT
routeAction1 = do
  request <- lift ask
  return $ textResponse request "Hello from Route 1"

routeAction2 :: ActionT
routeAction2 = throwError "Error in Route 2"

notFound :: Application
notFound request = "Hello from the DEFAULT route"

textResponse :: String -> String -> String
textResponse req msg = unwords ["Request:", req, "\nResponse:", msg]
----------------------------------------------------------------------

-- App State ---------------------------------------------------------
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
----------------------------------------------------------------------

-- Adding Routes -----------------------------------------------------
addRoute :: String -> ActionT -> AppStateT ()
addRoute pat action = modify $ \s -> addRoute' (route pat action) s

addRoute' :: Middleware -> AppState -> AppState
addRoute' m s@AppState {routes = ms} = s {routes = m:ms}

route :: String -> ActionT -> Middleware
route pat action nextApp req =
  let tryNext = nextApp req in
    if pat == req
      then
        runAction action req
      else
        tryNext
----------------------------------------------------------------------

-- Running Actions ---------------------------------------------------
runAction :: ActionT -> Request -> Response
runAction action request = either (const "Error") id
                           $ flip runReader request
                           $ runExceptT
                           $ action `catchError` errorHandler

errorHandler :: ActionError -> ActionT
errorHandler err = return $ "Oops: " ++ err
----------------------------------------------------------------------

-- Running the App ---------------------------------------------------
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
    putStrLn "---"
    userInputLoop appState
----------------------------------------------------------------------
