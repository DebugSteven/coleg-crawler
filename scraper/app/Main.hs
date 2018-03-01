module Main where

import Prelude
import NewScraper

main :: IO ()
main = do
  print "This may take awhile"
  ourBills <- newScrape
  print ourBills
  return ()
