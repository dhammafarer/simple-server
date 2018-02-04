{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main7 where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Reader as RT
import qualified Control.Monad.Trans.State.Strict as ST
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class

type Request = String
type Response = String

type Application = Request -> IO Response
newtype ActionT a = ActionT { runAT :: ExceptT ActionError
                                      (RT.ReaderT Request
                                      (ST.StateT Response IO)) a }
                            deriving (Functor, Applicative, Monad,
                                      MonadIO, MonadReader Request,MonadState Response, MonadError ActionError)
type ActionError = String

type Route = Application -> Application

newtype AppState = AppState { routes :: [Route] }
type AppStateT = ST.State AppState

-- client methods
constructResponse :: String -> String -> String
constructResponse req msg = unwords ["Request:", req, "\nResponse:", msg]

routeHandler1 :: ActionT ()
routeHandler1 = do
  request <- ask
  liftIO $ putStrLn "We're doing IO"
  modify (const $ constructResponse request "Hello from Route 1")

routeHandler2 :: ActionT ()
routeHandler2 = throwError "Error in Route 2"

defaultRoute :: Application
defaultRoute request = return "Hello from the DEFAULT route"

myApp :: AppStateT ()
myApp = do
  addRoute "one" routeHandler1
  addRoute "two" routeHandler2

main :: IO ()
main = myServer myApp

-- framework methods
errorHandler :: ActionError -> ActionT ()
errorHandler err = modify (const $ "Oops: " ++ err)

addRoute :: String -> ActionT () -> AppStateT ()
addRoute pat mf = modify $ \s -> addRoute' (route pat mf) s

addRoute' :: Route -> AppState -> AppState
addRoute' mf s@AppState {routes = mw} = s {routes = mf:mw}

route :: String -> ActionT () -> Route
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

runAction :: ActionT () -> Request -> IO Response
runAction action request = flip ST.execStateT ""
                           $ flip RT.runReaderT request
                           $ runExceptT
                           $ runAT
                           $ action `catchError` errorHandler

userInputLoop app_state = do
  putStrLn "Awaiting requests..."
  request <- getLine

  unless (request == "q") $ do
    let response = runMyApp defaultRoute app_state request
    response >>= putStrLn
    putStrLn "---"
    userInputLoop app_state

myServer :: AppStateT () -> IO ()
myServer myApp = do
  let appState = ST.execState myApp AppState{routes=[]}
  userInputLoop appState
