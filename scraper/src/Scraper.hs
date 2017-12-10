{-# LANGUAGE OverloadedStrings #-}

module Scraper where

import ClassyPrelude
import Control.Monad.IO.Class
import qualified Data.Conduit.List as CL
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as BS8
import System.IO (stdout, hSetBuffering, BufferMode( NoBuffering ), getLine )
import qualified Text.HTML.Scalpel as S
import Control.Applicative 
import Data.Maybe

data Bill = Bill { billNumber :: String
                 , billURL :: URL
                 , billTitle :: String
                 , billDescription :: String
                 , lastAction :: String --date? MM/DD/YYYY
                 , nextAction :: String --date? MM/DD/YYYY
                 , billSponsors :: [Legislature] 
                 } deriving (Show, Eq)

data Legislature = Legislature Name URL deriving (Show, Eq)
newtype Name = Name String deriving (Show, Eq)
newtype URL = URL String deriving (Show, Eq)

runScrape :: IO ()
runScrape = do
  userOption <- recentBills
  case userOption of
    "1" -> putStrLn "Do the default flow"
    "2" -> putStrLn "Do the everything in database flow"
    _   -> putStrLn "Oh god what happened" --do the default flow anyway...?

  putStrLn "Save the results."

defaultScrape website = do
  result <- nextPage website
  if nextPage website /= lastPage website then (nextPage $ (++) "http://leg.colorado.gov" $ result) else return result

-- if nextPage website == lastPage website then return parse done else
-- nextPage bs 

-- this will be the base page for any scrape we do
scrape :: IO L8.ByteString 
scrape = do 
  website <- httpLBS "http://leg.colorado.gov/bill-search?field_sessions=10171&sort_bef_combine=field_bill_number%20ASC" 
  return $ getResponseBody website

scrapePage :: Request -> IO L8.ByteString
scrapePage website = do
  body <- httpLBS website
  return $ getResponseBody body

-- this will be a test bill to use for now
scrapeBill :: IO L8.ByteString
scrapeBill = do
  billSite <- httpLBS "http://leg.colorado.gov/bills/hb17-1001"
  return $ getResponseBody billSite

nextPage :: ByteString -> (Maybe ByteString) 
nextPage website = do 
  S.scrapeStringLike website next >>= listToMaybe
    where 
      next :: S.Scraper ByteString [ByteString]
      next = S.chroots ("li" S.@: [S.hasClass "pager-next"]) link

      link :: S.Scraper ByteString ByteString
      link = do 
        S.attr "href" "a" 

lastPage :: ByteString -> (Maybe ByteString)
lastPage website = do 
  S.scrapeStringLike website next >>= listToMaybe
    where 
      next :: S.Scraper ByteString [ByteString]
      next = S.chroots ("li" S.@: [S.hasClass "pager-last"]) link

      link :: S.Scraper ByteString ByteString
      link = do 
        S.attr "href" "a" 

parseBillLinks :: ByteString -> Maybe [ByteString]
parseBillLinks website = do
  S.scrapeStringLike website bill
    where
      bill :: S.Scraper ByteString [ByteString]
      bill = S.chroots ("article" S.@: [S.hasClass "node-bill"]) about

      about :: S.Scraper ByteString ByteString
      about = S.attr "about" "article" 

parseBills :: ByteString -> Maybe [[ByteString]]
parseBills website = do
  S.scrapeStringLike website bill
    where
      bill :: S.Scraper ByteString [[ByteString]]
      bill = S.chroots "article" info

      info :: S.Scraper ByteString [ByteString]
      info = do
        billURL <- S.attr "about" "article"
        billNum <- S.text $ "div" S.@: [S.hasClass "field-item", S.hasClass "even"]
        billTitle <- S.text $ "h1" S.@: [S.hasClass "node-title"]
        billDesc <- S.text $ "div" S.@: [S.hasClass "field-name-field-bill-long-title"]
        return [billURL, billNum, billTitle, billDesc]

recentBills :: IO String
recentBills = do
  hSetBuffering stdout NoBuffering
  putStrLn "Please enter a number to select the option"
  putStrLn "1) Get the recent bills from the Colorado General Assembly"
  putStrLn "2) Get all the bills stored in the database since {oldest bill date}"
  option <- getLine
  case option of
    "1" -> return option
    "2" -> return option
    _   -> return "1" -- I am defaulting the parameter to 1 right now if there's any mistake
