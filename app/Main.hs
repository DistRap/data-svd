{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.SVD
import qualified System.Environment

main :: IO ()
main = System.Environment.getArgs >>= \case
  [filename] -> do
    Data.SVD.parseSVD filename >>= \case
      Left e -> error $ show e
      Right p ->
        putStrLn $ Data.SVD.displayDevice p
  _ -> error "No input svd file"
