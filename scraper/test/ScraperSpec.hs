{-# LANGUAGE OverloadedStrings #-}

module ScraperSpec where

import Test.Hspec
import Scraper
import qualified Data.ByteString.Char8 as L8
import Control.Monad.IO.Class
import Data.String.Conversions

spec :: Spec
spec = do
 describe "parsing correct links from base page" $ do 
   website <- runIO $ L8.readFile "test/example/bill_page_1.html" 
   billSite <- runIO $ L8.readFile "test/example/test_bill_1.html"
   it "nextPage matches the link" $ do
     nextPage website `shouldBe` (Just "/bill-search?field_sessions=10171&sort_bef_combine=field_bill_number%20ASC&page=1")
   it "lastPage matches the link" $ do
     lastPage website `shouldBe` (Just "/bill-search?field_sessions=10171&sort_bef_combine=field_bill_number%20ASC&page=31")
   it "bill links" $ do 
     parseBillLinks website `shouldBe` (Just ["/bills/hb17-1001","/bills/hb17-1002","/bills/hb17-1003","/bills/hb17-1004","/bills/hb17-1005","/bills/hb17-1006","/bills/hb17-1007","/bills/hb17-1008","/bills/hb17-1009","/bills/hb17-1010","/bills/hb17-1011","/bills/hb17-1012","/bills/hb17-1013","/bills/hb17-1014","/bills/hb17-1015","/bills/hb17-1016","/bills/hb17-1017","/bills/hb17-1018","/bills/hb17-1019","/bills/hb17-1020","/bills/hb17-1021","/bills/hb17-1022","/bills/hb17-1023","/bills/hb17-1024","/bills/hb17-1025"])
