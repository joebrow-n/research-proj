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
            putStrLn $ "--------------------------------------------------------------"
            putStrLn $ "File being processed: " ++ show fileName
            putStr $ "--------------------------------------------------------------"
            putStr $ "\nReadable file format:\n"
            putStrLn $ MV.prettyPrint (MV.toMyValue a)
            putStrLn $ "--------------------------------------------------------------"
            putStr $ "Percentage coverage of pre conditions: " ++ show (MV.getPreCondPercent (MV.toMyValue a)) ++ "%\n"
            putStr $ MV.prettyPrintPreConds (MV.removeMatchingSublists (MV.getPreConditions (MV.toMyValue a)) (MV.getMaxPreConditions (MV.toMyValue a)))
            putStrLn $ "--------------------------------------------------------------"
            putStr $ "Percentage coverage of post conditions: " ++ show (MV.getPostCondPercent (MV.toMyValue a)) ++ "%\n"
            putStr $ MV.prettyPrintPostConds (MV.removeMatchingSublists (MV.getPostConditions (MV.toMyValue a)) (MV.getMaxPostConditions (MV.toMyValue a)))
            putStrLn $ "--------------------------------------------------------------"
        Nothing -> putStrLn "Error"
    putStrLn " "
