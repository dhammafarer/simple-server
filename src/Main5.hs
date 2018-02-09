module Main5 where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Error.Class

type Request = String
type Response = String

type Application = Request -> IO Response
type Middleware = Application -> Application

type ActionT a = ExceptT ActionError (ReaderT Request (StateT Response IO)) a
type ActionError = String

newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = State AppState

-- Route Handlers ----------------------------------------------------
routeAction1 :: ActionT ()
routeAction1 = do
  request <- lift ask
  liftIO $ putStrLn "We're doing IO"
  lift . lift $ modify (const $ textResponse request "Hello from Route 1")

routeAction2 :: ActionT ()
routeAction2 = throwError "Error in Route 2"

notFound :: Application
notFound request = return "Hello from the DEFAULT route"

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
addRoute :: String -> ActionT () -> AppStateT ()
addRoute pat action = modify $ \s -> addRoute' (route pat action) s

addRoute' :: Middleware -> AppState -> AppState
addRoute' m s@AppState {routes = ms} = s {routes = m:ms}

route :: String -> ActionT () -> Middleware
route pat action nextApp req =
  let tryNext = nextApp req in
    if pat == req
      then
        runAction action req
      else
        tryNext
----------------------------------------------------------------------

-- Running Actions ---------------------------------------------------
runAction :: ActionT () -> Request -> IO Response
runAction action request = do
  (a,s) <- flip runStateT ""
           $ flip runReaderT request
           $ runExceptT
           $ action `catchError` errorHandler
  return $ either (const "Error") (const s) a

errorHandler :: ActionError -> ActionT ()
errorHandler err = lift . lift $ modify (const $ "Oops: " ++ err)
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
    response >>= putStrLn
    putStrLn "---"
    userInputLoop appState
----------------------------------------------------------------------
