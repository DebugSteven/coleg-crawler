module Handler.Home where

import Import

import Handler.Home.View

getHomeR :: Handler Html
getHomeR = homeView
