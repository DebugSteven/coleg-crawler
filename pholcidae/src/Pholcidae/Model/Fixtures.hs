module Pholcidae.Model.Fixtures where

import ClassyPrelude
import Data.Fixed
import Data.List (unfoldr)
import Database.Persist.Sql
import Prelude ((!!))
import Text.Shakespeare.Text (st)

import Pholcidae.Model

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

makeBill :: Text -> Text -> DB (Entity Bill)
makeBill billUrl number = do
  t <- liftIO getCurrentTime
  let billCommittee = Committee "Committee of doing nothing"
      billDescription = Description "This is a description"
      billTitle = CodeTitle "This is a bill title"
      billNumber = CodeNumber number
      billCreated = t
      billModified = t
      bill = Bill{..}
  insertEntity bill

genBillRef :: [(Text, Text)]
genBillRef = do
  unfoldr f'in 1000
  where
    urlPrefix = "https://leg.colorado.gov/bills/HB18-"
    numberPrefix = "HB18-"
    f'in n =
      Just (( urlPrefix <> tshow n
            , numberPrefix <> tshow n)
           , n + 1)

-- https://leg.colorado.gov/legislators/faith-winter
makeLegislator :: Text
               -> Text
               -> DB (Entity Legislator)
makeLegislator firstName lastName = do
  t <- liftIO getCurrentTime
  let urlPrefix = "https://leg.colorado.gov/legislators/"
      legislatorName = firstName <> " " <> lastName
      legislatorUrl =
           urlPrefix
        <> toLower firstName
        <> "-"
        <> toLower lastName
      legislatorCreated = t
      leggoMyEggo = Legislator{..}
  insertEntity leggoMyEggo

makeBillSponsor :: LegislatorId -> BillId -> DB (Entity BillSponsor)
makeBillSponsor billSponsorSponsor billSponsorBill = do
  t <- liftIO getCurrentTime
  let billSponsorCreated = t
  insertEntity BillSponsor{..}

makeBillAction :: Day -> BillId -> DB (Entity BillAction)
makeBillAction billActionDate billActionBill = do
  t <- liftIO getCurrentTime
  let billActionCreated = t
      billActionLocation = "Hell's Mouth"
      billActionAction = "Summoning Hell's Minions to serve the State"
  insertEntity BillAction{..}

insertFixtures :: DB Fixtures
insertFixtures = do
  allBillsF <-
    traverse (uncurry makeBill) $
      take 3 genBillRef
  allLegislatorsF <-
    traverse (uncurry makeLegislator) $
      [("Faith", "Winter"), ("Matt", "Gray"), ("Kerry", "Donovan")]
  allBillSponsorsF <-
    traverse (uncurry makeBillSponsor) $
      fmap (bimap entityKey entityKey)
      [ (allLegislatorsF !! 0, allBillsF !! 0)
      , (allLegislatorsF !! 1, allBillsF !! 0)
      , (allLegislatorsF !! 1, allBillsF !! 1)
      ]
  allBillActionsF <-
    traverse (uncurry makeBillAction) $
      fmap (second entityKey)
      [ (fromGregorian 2011 1 1, allBillsF !! 0)
      , (fromGregorian 2011 1 2, allBillsF !! 1)
      , (fromGregorian 2011 1 3, allBillsF !! 1)
      ]
  let billF = BillFixtures {..}
      legislatorF = LegislatorFixtures{..}
      billSponsorF = BillSponsorFixtures{..}
      billActionF = BillActionFixtures{..}
  return Fixtures{..}

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
