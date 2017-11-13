module Scraper 
  ( runScrape ) 
    where

import ClassyPrelude
import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit.List as CL
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO (stdout, hSetBuffering, BufferMode( NoBuffering ), getLine )

runScrape :: IO ()
runScrape = do
  userOption <- recentBills
  case userOption of
    "1" -> putStrLn "Do the default flow"
    "2" -> putStrLn "Do the everything in database flow"
    _   -> putStrLn "Oh god what happened" --do the default flow anyway...?

  putStrLn "Save the results."

scrape :: IO ()
scrape = do 
  website <- httpLBS "http://leg.colorado.gov/bill-search?field_sessions=10171&sort_bef_combine=field_bill_number%20ASC" 
  L8.writeFile "test.html" $ getResponseBody website

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
