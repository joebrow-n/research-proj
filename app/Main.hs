module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml

main :: IO ()
main = do
    yamlData <- decodeFileThrow "C:/Users/brown/OneDrive/Desktop/Research Project/haskellCode/app/example.yaml" :: IO Value
    print yamlData
