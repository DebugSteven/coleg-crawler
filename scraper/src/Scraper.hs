module Scraper where

import ClassyPrelude
import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit.List as CL
import Network.HTTP.Simple
import System.IO (stdout, hSetBuffering, BufferMode( NoBuffering ), getLine )

runScrape :: IO ()
runScrape = do
  userOption <- recentBills
  case userOption of
    "1" -> putStrLn "Do the default flow"
    "2" -> putStrLn "Do the everything in database flow"
    _   -> putStrLn "Oh god what happened"

  putStrLn "Do a scrape job!"
  putStrLn "Save the results."

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
