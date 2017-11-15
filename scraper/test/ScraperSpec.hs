{-# LANGUAGE OverloadedStrings #-}

module ScraperSpec where

import Test.Hspec
import Scraper
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.IO.Class
import Data.String.Conversions

spec :: Spec
spec = do
 describe "parsing correct links from base page" $ do 
   it "nextPage matches the link" $ do
     nextPage `shouldBe` Just "/bill-search?field_sessions=10171&sort_bef_combine=field_bill_number%20ASC&page=1"
   it "lastPage matches the link" $ do
     lastPage `shouldBe` Just "/bill-search?field_sessions=10171&sort_bef_combine=field_bill_number%20ASC&page=31"
