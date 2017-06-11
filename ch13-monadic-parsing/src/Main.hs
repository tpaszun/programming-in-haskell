module Main where

import Parser
import Data.Tree.Pretty

main :: IO ()
main = do
  line <- getLine
  putStrLn $ drawVerticalTree $ exprToTreeFull $ eval line
  putStrLn $ drawVerticalTree $ exprToTree $ eval line
