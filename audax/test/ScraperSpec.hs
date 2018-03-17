{-# LANGUAGE OverloadedStrings #-}

module ScraperSpec where

import Test.Hspec
import Scraper
import qualified Data.ByteString.Char8 as L8
import Control.Monad.IO.Class
import Data.String.Conversions

spec :: Spec
spec = do
 describe "parsing correct information out of a bill" $ do 
   billSite <- runIO $ L8.readFile "test/example/test_bill_scheduled.html"
   it "parse bill results" $ do
     parseBills billSite `shouldBe` (Just [["/bills/hb17-1001", "HB17-1001", "Employee Leave Attend Child's Academic Activities", "Concerning the ability of an employee of an employer who employs at least fifty employees to take up to eighteen hours of leave from work for purposes of attending his or her child's academic activities."]])
