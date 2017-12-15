{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

-- PERSISTENT
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

-- JSON
import qualified Data.Aeson.Parser
import Data.Aeson.Compat 
import Data.Aeson.Types

-- TIME
import Data.Time.LocalTime
import Data.Time


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Repository json
    url String
    nodes Int
    start_time UTCTime
    end_time UTCTime Maybe
    deriving Show
CommitInfo json
    repositoryId RepositoryId
    commit_str String
    start_time UTCTime 
    end_time UTCTime Maybe
    deriving Show
CommitResults json
    repositoryId RepositoryId
    commit_str String
    file_name String
    complexity String Maybe
    deriving Show
|]


