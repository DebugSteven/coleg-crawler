{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import Bill
import ClassyPrelude
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as WC
import qualified Test.WebDriver.Session as WS
import Data.Text (Text)

newtype IndexPageURL = IndexPageURL String deriving (Show, Eq)
newtype BillPageURL = BillPageURL Text deriving (Show, Eq)

runScrape :: IO [Bill]
runScrape = do
  ws <- W.runSession (W.useBrowser (W.Chrome Nothing (Nothing) [] [] mempty) WC.defaultConfig) WS.getSession
  let run = W.runWD ws
  run $ do 
    scrapeLoop  

    where
      -- we start with initial page with bill listings
      firstIndex :: IndexPageURL
      firstIndex = IndexPageURL "http://leg.colorado.gov/bill-search?field_sessions=10171&amp;sort_bef_combine=field_bill_number%20ASC&amp;page=31&page=0"

      scrapeLoop :: W.WD [Bill]
      scrapeLoop = do
        billList <- scrapeIndexPage firstIndex
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
            rest <- (scrapeIndexPage (IndexPageURL n))
            return (mappend links rest)

      scrapeBillPage :: BillPageURL -> W.WD Bill
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

      getBillInfo :: W.WD Bill        -- this function is for real
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
  
        return Bill { billURL = url
                    , billNumber = number
                    , billTitle = title
                    , billDescription = desc
                    , lastAction = previousAction
                    , nextAction = upcomingAction
                    , billSponsors = sponsors}
