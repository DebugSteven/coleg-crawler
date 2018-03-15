{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Home.View where

-- import Data.Fixed
-- import Data.Time.Format
import Import
-- import Text.Julius (rawJS)

homeView :: Handler Html
homeView =
  defaultLayout $ do
    setTitle "Welcome to coleg-crawler's website!"
    [whamlet|
<div .masthead>
    <div .container>
        <div .row>
            <h1 .header>
                coleg-crawler — an unofficial and community supported source of raw CSV data for the Colorado legislature
            <a href="/static/csv/2018-03-14-bills.csv" .btn.btn-info.btn-lg>
                Download the legislative CSV dump
    |]
