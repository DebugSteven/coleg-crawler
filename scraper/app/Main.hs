module Main where

import Prelude
import Scraper

main :: IO ()
main = do
  print "This may take awhile"
  ourBills <- runScrape
  print ourBills
  return ()
