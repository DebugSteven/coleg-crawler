module Model.Fixtures where

import ClassyPrelude
import Data.Fixed
import Database.Persist.Sql
import Prelude ((!!))
import Text.Shakespeare.Text (st)

import Model

data BillFixtures =
  BillFixtures { allBillsF :: ![Entity Bill] }
  deriving (Show)

data LegislatorFixtures =
  LegislatorFixtures { allLegislatorsF :: ![Entity Legislator] }
  deriving (Show)

data BillSponsorFixtures =
  BillSponsorFixtures { allBillSponsorsF :: ![Entity BillSponsor] }
  deriving (Show)

data BillActionFixtures =
  BillActionFixtures { allBillActionsF :: ![Entity BillAction] }
  deriving (Show)

data Fixtures =
  Fixtures { billF :: !BillFixtures
           , legislatorF :: !LegislatorFixtures
           , billSponsorF :: !BillSponsorFixtures
           , billActionF :: !BillActionFixtures
           }
  deriving Show

insertFixtures :: DB Fixtures
insertFixtures = do
  let allBillsF = undefined
  let billF = BillFixtures {..}
      legislatorF = undefined
      billSponsorF = undefined
      billActionF = undefined
  return Fixtures {..}

getTables :: DB [Text]
getTables = do
  tables <- rawSql [st|
                      SELECT table_name
                      FROM information_schema.tables
                      WHERE table_schema = 'public'
                      AND table_type='BASE TABLE';
                   |] []
  return $ map unSingle tables

truncateAllTables :: DB ()
truncateAllTables = do
  tables <- getTables
  sqlBackend <- ask
  let escapedTables :: [Text]
      escapedTables =
        map (connEscapeName sqlBackend . DBName) tables
      query =
        [st|TRUNCATE TABLE #{intercalate ", " escapedTables} RESTART IDENTITY CASCADE|]
  case escapedTables of
    [] ->
      error "List of tables is empty, something has gone wrong!"
    _ -> rawExecute query []

wipeAndReinstallFixtures :: DB ()
wipeAndReinstallFixtures = do
  truncateAllTables
  void $ insertFixtures

wipeAndMigrateDatabase :: DB ()
wipeAndMigrateDatabase = do
  truncateAllTables
  runMigrations
  void $ insertFixtures

freshDatabase :: DB ()
freshDatabase = do
  runMigrations
  void $ insertFixtures
