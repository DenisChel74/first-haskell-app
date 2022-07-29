module TasksActions where

import Control.Monad.IO.Class
import Database.SQLite.Simple
import TasksRepository as TR
import TasksDomain
import qualified Data.Text.Lazy as TL
import Web.Scotty

readTasks :: Connection -> ActionM ()
readTasks conn = do
  tasks <- liftIO $ TR.getTasks conn
  json tasks

readTask :: Connection -> TL.Text -> ActionM ()
readTask conn id = do
  task <- liftIO $ TR.getTask conn id
  json task

deleteTask :: Connection -> TL.Text -> ActionM ()
deleteTask conn id = do
  task <- liftIO $ TR.deleteTask conn id
  json task

createTask :: Connection -> Maybe (NewTask) -> ActionM ()
createTask conn (Just (NewTask title description)) = do 
  tasks <- liftIO $ TR.createTask conn title description
  json tasks
createTask conn (Nothing) = return ()

updateTask :: Connection -> Maybe (Task) -> ActionM ()
updateTask conn (Just (Task id title description)) = do 
  tasks <- liftIO $ TR.updateTask conn id title description
  json tasks
updateTask conn (Nothing) = return ()