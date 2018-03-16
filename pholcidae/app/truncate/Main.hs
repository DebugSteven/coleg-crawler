module Main where

import Prelude (IO)
import Pholcidae.Model (runDevDB)
import Pholcidae.Model.Fixtures (truncateAllTables)

main :: IO ()
main =
  runDevDB truncateAllTables
