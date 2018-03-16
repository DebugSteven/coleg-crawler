{-# LANGUAGE OverloadedStrings #-}

module Bill where

import ClassyPrelude
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List as CL
import Data.CSV.Conduit
import Data.CSV.Conduit.Conversion
import Data.Text
import System.IO

data Bill = Bill { billURL :: String
                 , billNumber :: Text 
                 , billTitle :: Text
                 , billDescription :: Text
                 , lastAction :: Maybe Text 
                 , nextAction :: Maybe Text 
                 , billSponsors :: Text 
                 } deriving (Show, Eq)

instance FromNamedRecord Bill where
  parseNamedRecord m = Bill <$>
                       m .: "gbillURL" <*>
                       m .: "bbillNumber" <*>
                       m .: "abillTitle" <*>
                       m .: "cbillDescription" <*>
                       m .: "elastAction" <*>
                       m .: "fnextAction" <*>
                       m .: "dbillSponsors"

instance ToNamedRecord Bill where
  toNamedRecord (Bill billURL billNumber billTitle billDescription lastAction nextAction billSponsors) = namedRecord 
                     [ "gbillURL" .= billURL
                     , "bbillNumber" .= billNumber
                     , "abillTitle" .= billTitle
                     , "cbillDescription" .= billDescription
                     , "elastAction" .= lastAction
                     , "fnextAction" .= nextAction
                     , "dbillSponsors" .= billSponsors
                     ]

writeCSV :: [Bill] -> IO ()
writeCSV = writeCSVFile defCSVSettings "bills.csv" WriteMode . fmap Named
