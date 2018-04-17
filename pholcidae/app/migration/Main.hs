module Main where

import Prelude

import Pholcidae.Model

main :: IO ()
main = do
  runDevDB runMigrations
