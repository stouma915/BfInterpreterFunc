module Main (main) where

import Lib

main :: IO ()
main = do
  let sourceCode = ">+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++<++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.>."

  case evaluate sourceCode of
    Just output ->
      putStrLn output
    Nothing ->
      putStrLn "Error."
