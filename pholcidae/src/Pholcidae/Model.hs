{-# LANGUAGE FlexibleInstances #-}

module Pholcidae.Model
  ( module Export
  , module Pholcidae.Model
  ) where

import ClassyPrelude

import Control.Monad.Logger hiding (LoggingT, runLoggingT)
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.Persist.TH
-- import Network.URI

import Pholcidae.Model.Types as Export

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      ] [persistLowerCase|
Bill sql=bills
  url Text
  number CodeNumber
  title CodeTitle
  committee Committee
  description Description
  created UTCTime
  modified UTCTime
  UniqueBillNumber number
  UniqueBillUrl url
  deriving Show

Legislator sql=legislators
  name Text -- Tony Exum
  url Text
  created UTCTime
  UniqueLegislatorName name
  deriving Show

LegislatorTenure sql=legislator_tenures
  title Text -- Senator, Representative
  started Day
  stopped Day Maybe
  legislator LegislatorId
  created UTCTime

BillSponsor sql=bill_sponsors
  sponsor LegislatorId
  bill BillId
  created UTCTime
  UniqueBillSponsor sponsor bill
  deriving Show

BillAction sql=bill_actions
  date Day
  location Text
  action Text
  bill BillId
  created UTCTime
  UniqueBillAction date location action bill
  deriving Show
|]

type BillF = UTCTime -> UTCTime -> Bill
type BillActionF = BillId -> UTCTime -> BillAction
type LegislatorF = UTCTime -> Legislator

-- data BillOmnibus =
--   BillOmnibus {
--     _omnibusBill :: !(Entity Bill)
--   , _omnibusBillActions :: ![Entity BillAction]
--   , _omnibusLegislators :: ![( Entity Legislator
--                              , Entity BillSponsor )]
--   } deriving Show

-- instance ToNamedData ...

-- This needs upsert/dedup semantics for all of the models
insertBill :: [LegislatorF]
           -> [BillActionF]
           -> BillF
           -> DB ( Entity Bill
                 , [ Entity BillAction ]
                 , [ ( Entity Legislator
                     , Entity BillSponsor
                     )
                   ]
                 )
insertBill sponsors billActions bill = do
  t <- liftIO getCurrentTime
  billEntity@(Entity billKey _) <- insertEntity (bill t t)
  billActions <- traverse (insertBillAction billKey t) billActions
  legislators <- traverse (insertLegislator t billKey) sponsors
  return (billEntity, billActions, legislators)
  where
    insertBillAction :: BillId
                     -> UTCTime
                     -> BillActionF
                     -> DB (Entity BillAction)
    insertBillAction billKey t billActionF =
      insertEntity (billActionF billKey t)

    insertLegislator :: UTCTime
                     -> BillId
                     -> LegislatorF
                     -> DB (Entity Legislator, Entity BillSponsor)
    insertLegislator t billKey legislatorF = do
      legEnt <- insertEntity (legislatorF t)          
      bsEnt <- insertBillSponsor t billKey (entityKey legEnt)
      return (legEnt, bsEnt)

    insertBillSponsor :: UTCTime
                      -> BillId
                      -> LegislatorId
                      -> DB (Entity BillSponsor)
    insertBillSponsor t billKey legKey = do
      insertEntity (BillSponsor legKey billKey t)

  -- startedService Day
  -- endedService Day

  -- lastAction Text Maybe
  -- nextAction Text Maybe
  -- sponsors Text

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
