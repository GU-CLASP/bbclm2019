module Main (main) where

import ParseLib
-- import Data.List.Split
import Data.Foldable
import System.Environment


main :: IO ()
main = do
  [fname] <- getArgs
  rawInput <-  readFile fname
  let input = filter (not . null) $ preParse rawInput
  forM_ input $ \x -> do
    putStr "parse "
    print x

