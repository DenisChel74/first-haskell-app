{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TasksRepository where
import Database.SQLite.Simple
import qualified Data.Text.Lazy as TL
import TasksDomain

getTasks :: Connection -> IO [Task]
getTasks conn = query_ conn sql
    where sql = "select * from tasks"

getTask :: Connection -> TL.Text -> IO (Maybe Task)
getTask conn id = do 
    tasks <- queryNamed conn sql args
    return $ one tasks
    where sql = "select * from tasks where id=:id"
          args = [":id" := id]
          one (x:_) = Just x
          one [] = Nothing

deleteTask :: Connection -> TL.Text -> IO (Maybe Task)
deleteTask conn id = do 
    tasks <- queryNamed conn sql args
    return $ one tasks
    where sql = "delete from tasks where id=:id returning *"
          args = [":id" := id]
          one (x:_) = Just x
          one [] = Nothing

createTask :: Connection -> String -> String -> IO (Maybe Task)
createTask conn title description = do
    tasks <- queryNamed conn sql args
    return $ one tasks
    where sql = "insert into tasks (title, description) values (:title, :description) returning *"
          args = [":title" := title, ":description" := description]
          one (x:_) = Just x
          one [] = Nothing              

updateTask :: Connection -> Int -> String -> String -> IO (Maybe Task)
updateTask conn id title description = do
    tasks <- queryNamed conn sql args
    return $ one tasks
    where sql = "update tasks set title = :title, description = :description where id= :id returning *"
          args = [":title" := title, ":description" := description, ":id" := id]
          one (x:_) = Just x
          one [] = Nothing          