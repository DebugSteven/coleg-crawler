{-# LANGUAGE OverloadedStrings #-}

module NewScraper where

import ClassyPrelude
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as WC
import qualified Test.WebDriver.Session as WS
import Data.Text (Text)

data Bill = Bill { billURL :: String
                 , billNumber :: Text 
                 , billTitle :: Text
                 , billDescription :: Text
                 , lastAction :: Maybe Text 
                 , nextAction :: Maybe Text 
                 , billSponsors :: Text 
                 } deriving (Show, Eq)

newtype IndexPageURL = IndexPageURL String deriving (Show, Eq)
newtype BillPageURL = BillPageURL String deriving (Show, Eq)

newScrape = do
  -- we start with initial page with bill listings
  ws <- W.runSession (W.useBrowser (W.Chrome Nothing (Nothing) [] [] mempty) WC.defaultConfig) WS.getSession
  let run = W.runWD ws
  run $ do 
    W.openPage "http://leg.colorado.gov/bill-search?field_sessions=10171&amp;sort_bef_combine=field_bill_number%20ASC&amp;page=31&page=0"
    getNextLink "pager-next"
    W.openPage "http://leg.colorado.gov/bills/hb17-1304"

    where
      scrapeLoop :: W.WD [Bill]
      scrapeLoop = undefined

      scrapeIndexPage :: IndexPageURL -> W.WD [BillPageURL]
      scrapeIndexPage (IndexPageURL indexPage) = do
        W.openPage indexPage
        if getNextLink "pager-next" == getNextLink "pager-last"
        then undefined
        else undefined 

      scrapeBillPage :: BillPageURL -> W.WD Bill
      scrapeBillPage (BillPageURL billPage) = do
        W.openPage billPage 
        getBillInfo

      getNextLink elemClass = do                    -- works for "pager-next" and "pager-last"
        getLi <- W.findElem $ W.ByClass elemClass   -- gets the li element that should contain an a tag child
        getA <- W.findElemFrom getLi $ W.ByTag "a"  -- gets the a child element from li
        link <- W.attr getA "href"                  -- gets the link!
        return link

      getBillLinks :: W.WD [Maybe Text] -- !! idk I don't want it to be a [Maybe Text] I want a [Text]
      getBillLinks = do
        getH4 <- W.findElems $ W.ByClass "node-title"                         -- gets list of bill node pointers
        getAs <- traverse (\elem -> W.findElemFrom elem $ W.ByTag "a") getH4  -- get a tags from nodes 
        links <- traverse (\elem -> W.attr elem "href") getAs                 -- get links for bills!
        return links

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
