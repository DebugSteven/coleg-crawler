{-# OPTIONS_GHC -fno-warn-orphans #-}

module Routes where

import           Import.NoFoundation

import           AppType

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ HomeR GET
|]
