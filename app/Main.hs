{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Char8 as BS
import Data.Monoid (mconcat)
import Data.Text.Lazy as T
import qualified Data.Text.Lazy as TL
import qualified Data.Yaml as Y
import Database.SQLite.Simple
import GHC.Base (IO (IO))
import GHC.Generics (Generic)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import TasksActions
import Web.Scotty
import TasksDomain
import Prelude

data AppConfig = AppConfig
  {
    database :: String
  }
  deriving (Show, Generic)

instance FromJSON AppConfig

readConfig :: String -> IO AppConfig
readConfig path = do
  confBytes <- BS.readFile path
  case Y.decodeThrow confBytes of
    Nothing -> error "application config is not provided"
    Just config -> return config

main :: IO ()
main = do
  config <- readConfig "config.yaml"
  conn <- open (database config)
  scotty 3000 $ do
    middleware logStdoutDev
    get "/v1/tasks" $ readTasks conn
    get "/v1/tasks/:id" $ do
      id <- param "id"
      readTask conn id
    post "/v1/tasks" $ do
      b <- body
      let newTask = decode b :: Maybe (NewTask)
      liftIO $ print "newTask"
      createTask conn newTask
    put "/v1/tasks/" $ do
      b <- body
      let taskForUpdate = decode b :: Maybe (Task)
      updateTask conn taskForUpdate
    delete "/v1/tasks/:id" $ do
      id <- param "id"
      deleteTask conn id