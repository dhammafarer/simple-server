module Main2 where

import Control.Monad.Trans.State.Strict
import Control.Monad

type Request = String
type Response = Maybe String
type Application = Request -> Response

type Middleware = Application -> Application

newtype AppState = AppState { routes :: [Middleware] }
type AppStateT = State AppState

-- client methods
routeAction1 :: Request -> Response
routeAction1 request = return $ mkResponse request "Hello from Route 1"

routeAction2 :: Request -> Response
routeAction2 request = Nothing

defaultRoute :: Request -> Response
defaultRoute request = return $ mkResponse request "Hello from the DEFAULT route"

mkResponse :: String -> String -> String
mkResponse req msg = unwords ["Request:", req, "\nResponse:", msg]

myApp :: AppStateT ()
myApp = do
  addRoute "one" routeAction1
  addRoute "two" routeAction2

main :: IO ()
main = myServer myApp

-- framework methods

addRoute :: String -> (Request -> Response) -> AppStateT ()
addRoute pat rA = modify $ \s -> addRoute' (route pat rA) s

addRoute' :: Middleware -> AppState -> AppState
addRoute' mw s@AppState {routes = rs} = s {routes = mw:rs}

route :: String -> (Request -> Response) -> Middleware
route pat routeAction nextApp request =
  let tryNext = nextApp request in
  if pat == request
  then
    routeAction request
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
    let response = runMyApp defaultRoute appState request
    case response of
      Just x -> putStrLn x
      Nothing -> putStrLn "Error"
    userInputLoop appState

myServer :: AppStateT () -> IO ()
myServer myApp = do
  let appState = execState myApp AppState{routes=[]}
  userInputLoop appState
