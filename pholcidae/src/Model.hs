{-# LANGUAGE FlexibleInstances #-}

module Model
  ( module Export
  , module Model
  ) where

import ClassyPrelude

import Control.Monad.Logger hiding (LoggingT, runLoggingT)
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.Persist.TH
import Network.URI

import Model.Types as Export

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      ] [persistLowerCase|
Bill sql=bills
  url URI
  number Text
  title Text
  description Text
  lastAction Text Maybe
  nextAction Text Maybe
  sponsors Text
  created UTCTime
  modified UTCTime

Sponsor sql=sponsors
  bill BillId
  title Text -- Senator, Representative
  name Text -- Tony Exum
  created UTCTime
  modified UTCTime

BillAction sql=bill_actions
  bill BillId
  date Day
  location Text
  action Text
  created UTCTime
  modified UTCTime
|]

-- data Bill = Bill { billURL :: Text
--                , billNumber :: Text 
--                , billTitle :: Text
--                , billDescription :: Text
--                , lastAction :: Maybe Text 
--                , nextAction :: Maybe Text 
--                , billSponsors :: Text 
--                } deriving (Show, Eq)

-- "100 Years Of 4th Infantry Division","HJR17-1007","Concerning recognition of one hundred years of service by the 4th Infantry Division.","PRIME SPONSORS
-- Representative
-- Tony Exum
-- Representative
-- Patrick Neville
-- Senator
-- Larry Crowder
-- Senator
-- Daniel Kagan","02/02/2017 Signed Act PDF","","http://leg.colorado.gov/bills/hjr17-1007"

dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

devConn :: ConnectionString
devConn =
  "dbname=coleg host=localhost user=coleg password=coleg port=5432"

runDevDB :: DB a -> IO a
runDevDB a =
  runNoLoggingT $
    withPostgresqlPool devConn 3
      $ \pool -> liftIO $ runSqlPersistMPool a pool

runDevDBV :: DB a -> IO a
runDevDBV a =
  runStdoutLoggingT $
    withPostgresqlPool devConn 3
      $ \pool -> liftIO $ runSqlPersistMPool a pool
