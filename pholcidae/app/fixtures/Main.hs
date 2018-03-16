module Main where

import Prelude (IO)
import Pholcidae.Model (runDevDB)
import Pholcidae.Model.Fixtures (wipeAndReinstallFixtures)

main :: IO ()
main =
  runDevDB wipeAndReinstallFixtures
