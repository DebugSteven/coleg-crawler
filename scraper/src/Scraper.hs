module Scraper where

import ClassyPrelude
import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit.List as CL
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
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
                 } deriving Show

data Legislature = Legislature Name URL 
newtype Name = Name String
newtype URL = URL String

runScrape :: IO ()
runScrape = do
  userOption <- recentBills
  case userOption of
    "1" -> putStrLn "Do the default flow"
    "2" -> putStrLn "Do the everything in database flow"
    _   -> putStrLn "Oh god what happened" --do the default flow anyway...?

  putStrLn "Save the results."

scrape :: IO L8.ByteString 
scrape = do 
  website <- httpLBS "http://leg.colorado.gov/bill-search?field_sessions=10171&sort_bef_combine=field_bill_number%20ASC" 
  return $ getResponseBody website

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
