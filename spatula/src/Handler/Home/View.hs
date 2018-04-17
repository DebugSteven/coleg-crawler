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
<div .container>
  <div .row>
    <div .jumbotron>
      <h1 .display-4>
        coleg-crawler
      <p .lead>
        An unofficial and community supported source of raw CSV data for the Colorado legislature
      <hr .my-4>
      <p>
        It uses utility classes for typography and spacing to space content out within the larger container.
      <a.btn.btn-primary.btn-lg
        href="/static/csv/2018-03-14-bills.csv"
        role="button">
        Download the legislative CSV dump                
|]

williamsView :: [Entity Bill] -> Handler Html
williamsView williamses =
  defaultLayout $ do
    setTitle "Welcome to coleg-crawler's website!"
    [whamlet|
<div .container>
  <div .row>
    <h1 .header>Bills
    <table .human-table>
      <tr>
        <th>Url
        <th>Number
        <th>Title
        <th>Committee
        <th>Description
        <th>Created
      $forall (Entity _ Bill{..}) <- williamses
        <tr>
          <td>
            <a href="#{billUrl}">#{billUrl}</a>
          <td>
            #{billNumber}
          <td>
            #{billTitle}
          <td>
            #{billCommittee}
          <td>
            #{billDescription}
          <td>
            #{tshow billCreated}
|]
