module Handler.Home where

import Import

import Handler.Home.View

getHomeR :: Handler Html
getHomeR = homeView

getWilliamsR :: Handler Html
getWilliamsR = do
  daBills <- runDB $ selectList [] []
  williamsView daBills
