module Scraper where

import ClassyPrelude
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as WC
import qualified Test.WebDriver.Session as WS
import Data.Text (Text)
-- import Data.Text.Encoding (encodeUtf8)
import Text.Shakespeare.Text (st)
import Text.Trifecta

import Pholcidae.Model

newtype IndexPageURL =
  IndexPageURL String deriving (Show, Eq)
newtype BillPageURL =
  BillPageURL Text deriving (Show, Eq)


-- data BillOmnibus =
--   BillOmnibus {
--     _omnibusBill :: !(Entity Bill)
--   , _omnibusBillActions :: ![Entity BillAction]
--   , _omnibusLegislators :: ![( Entity Legislator
--                              , Entity BillSponsor )]
--   } deriving Show

-- data BillF f =
--   BillF {
--     _omnibusBill :: !(f Bill)
--   , _omnibusBillActions :: ![f BillAction]
--   , _omnibusLegislators :: ![( f Legislator
--                              , f BillSponsor )]
--   } deriving Show

-- type BillOmnibus = BillF Entity
-- type BillScraped = BillF Identity

data BillScraped =
  BillScraped {
    _scrapedBill :: !BillF
  , _scrapedBillActions :: ![BillActionF]
  , _scrapedLegislators :: ![LegislatorF]
  }

testSponsorText :: Text
testSponsorText = [st|PRIME SPONSORS
Representative
Tony Exum
Representative
Patrick Neville
Senator
Larry Crowder
Senator
Daniel Kagan
|]

parseSponsors :: Parser [LegislatorF]
parseSponsors = do
  _ <- text "PRIME SPONSORS\n"
  some parseSponsor
  where parseSponsor :: Parser LegislatorF
        parseSponsor = do
          legTitle <- manyTill anyChar (char '\n')
          legName <- manyTill anyChar ((void $ char '\n') <|> eof)
          return $ Legislator (pack legName) (pack legTitle)

parseSponsorText :: Text -> Result [LegislatorF]
parseSponsorText text =
  parseByteString parseSponsors mempty (encodeUtf8 text)

testParse = do
  let res = parseSponsorText testSponsorText
  case res of
    Failure err -> print err
    Success a -> do
      t <- getCurrentTime
      print $ fmap ($ t) a

runScrape :: IO [BillScraped]
runScrape = do
  ws <- W.runSession
        (W.useBrowser
         (W.Chrome
          Nothing
          Nothing
          [] []
          mempty)
          WC.defaultConfig)
        WS.getSession
  let run = W.runWD ws
  run $ do 
    scrapeLoop  

    where
      -- we start with initial page with bill listings
      firstIndex :: IndexPageURL
      firstIndex = IndexPageURL "http://leg.colorado.gov/bill-search?field_sessions=10171&amp;sort_bef_combine=field_bill_number%20ASC&amp;page=31&page=0"

      scrapeLoop :: W.WD [BillScraped]
      scrapeLoop = do
        billList <- scrapeIndexPage firstIndex
        -- traverse scrapeBillPage (take 1 billList)
        traverse scrapeBillPage billList

      scrapeIndexPage :: IndexPageURL -> W.WD [BillPageURL]
      scrapeIndexPage (IndexPageURL indexPage) = do
        W.openPage indexPage
        currLink <- W.getCurrentURL
        lastLink <- getNextLink "pager-last"
        case lastLink of
          Nothing -> getBillLinks
          Just l -> do
            nextLink <- getNextLink "pager-next"
            let n = fromMaybe "" nextLink
            links <- getBillLinks 
            rest <- scrapeIndexPage (IndexPageURL n)
            return (mappend links rest)
            -- return links

      scrapeBillPage :: BillPageURL -> W.WD BillScraped
      scrapeBillPage (BillPageURL billPage) = do
        W.openPage $ unpack billPage 
        getBillInfo

      getNextLink :: Text -> W.WD (Maybe String)
      getNextLink elemClass = do                    -- works for "pager-next" and "pager-last"
        getLis <- W.findElems $ W.ByClass elemClass   -- gets the li element that should contain an a tag child
        for (listToMaybe getLis) $ \getLi -> do
          getA <- W.findElemFrom getLi $ W.ByTag "a"  -- gets the a child element from li
          link <- W.attr getA "href"                  -- gets the link!
          return $ unpack (fromMaybe "" link)

      getBillLinks :: W.WD [BillPageURL] 
      getBillLinks = do
        getH4 <- W.findElems $ W.ByClass "node-title"                         -- gets list of bill node pointers
        getAs <- traverse (\elem -> W.findElemFrom elem $ W.ByTag "a") getH4  -- get a tags from nodes 
        links <- traverse (\elem -> W.attr elem "href") getAs                 -- get links for bills!
        return $ fmap BillPageURL (catMaybes links)

      getBillInfo :: W.WD BillScraped        -- this function is for real
      getBillInfo = do 
        url <- W.getCurrentURL
        number <- W.getText =<< W.findElem (W.ByCSS ".field-name-field-bill-number") 
        title <- W.getText =<< W.findElem (W.ByClass "node-title")
        desc <- W.getText =<< W.findElem (W.ByCSS ".field-name-field-bill-long-title")
        sponsors <- W.getText =<< W.findElem (W.ByCSS "#block-cga-bills-bill-sponsors")
        upcomingActions <- W.findElems (W.ByCSS ".node-schedule-item:first-child")
        upcomingAction <- traverse W.getText (listToMaybe upcomingActions)
        previousActions <- W.findElems (W.ByCSS "[data-long-bill-sort]:first-child")
        previousAction <- traverse W.getText (listToMaybe previousActions)

        let _scrapedBill = Bill (pack url)
                                (CodeNumber number)
                                (CodeTitle title)
                                (Description desc)
            noScrapeBillActionDate =
              ModifiedJulianDay 0
            noScrapeBillActionLocation =
              "We don't scrape action location yet"
            noScrapedAction =
              "We couldn't scrape action text here"
            _scrapedBillActions =
              [ BillAction
                noScrapeBillActionDate
                noScrapeBillActionLocation
                (fromMaybe noScrapedAction previousAction)
              , BillAction
                noScrapeBillActionDate
                noScrapeBillActionLocation
                (fromMaybe noScrapedAction upcomingAction)
              ]
            _scrapedLegislators = []
        return BillScraped{..}
        -- return Bill { billURL = url
        --             , billNumber = number
        --             , billTitle = title
        --             , billDescription = desc
        --             , lastAction = previousAction
        --             , nextAction = upcomingAction
        --             , billSponsors = sponsors}
  --   _scrapedBill :: !Bill
  -- , _scrapedBillActions :: ![BillAction]
  -- , _scrapedLegislators :: ![(Legislator
  --                            ,BillSponsor)]
