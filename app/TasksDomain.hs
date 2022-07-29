{-# LANGUAGE OverloadedStrings #-}

module TasksDomain where

import Data.Aeson
import Database.SQLite.Simple

data Task = Task Int String String deriving (Show)
data NewTask = NewTask String String deriving (Show)

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field

instance ToJSON Task where
     toJSON (Task id title description) =
         object ["id" .= id,
                 "title" .= title,
                 "description" .= description]

instance FromJSON Task where
     parseJSON (Object v) = Task <$>
                            v .: "id" <*>
                            v .:  "title" <*>
                            v .:  "description"

instance FromRow NewTask where
  fromRow = NewTask <$> field <*> field

instance ToJSON NewTask where
     toJSON (NewTask title description) =
         object ["title" .= title,
                 "description" .= description]

instance FromJSON NewTask where
     parseJSON (Object v) = NewTask <$>
                            v .:  "title" <*>
                            v .:  "description"