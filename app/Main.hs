module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml as Y
import System.Environment
import qualified Data.ByteString.Char8 as BS
import qualified MyValue as MV

main :: IO ()
main = do
    -- "C:\Users\brown\OneDrive\Desktop\Research Project\rtems-central\spec\rtems\event\req\send-receive.yml""
    args <- getArgs
    let fileName = Prelude.head args
    content <- BS.readFile fileName
    let yamlData = decodeThrow content :: Maybe Value
    case yamlData of 
        Just a -> do
            print $ (MV.toMyValue' a)
            putStrLn $ MV.prettyPrint (MV.toMyValue a)
        Nothing -> putStrLn "Error"
    putStrLn " "
