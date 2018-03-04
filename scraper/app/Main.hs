module Main where

import Prelude
import Scraper
import Bill

main :: IO ()
main = do
  print "This may take awhile"
  ourBills <- runScrape
  writeCSV ourBills
  return ()
