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
            MV.checkType (MV.toMyValue a)
            putStrLn " "
            print $ (MV.toMyValue a)
            putStrLn " "
            putStrLn $ MV.prettyPrint (MV.moveKey "type" (MV.toMyValue a))
            putStrLn " "
            putStrLn "Specific fields:"
            putStrLn $ MV.prettyPrint (MV.myObjectSpecificFields (Prelude.tail args) (MV.toMyValue a)) -- passing in commandline arguments
        Nothing -> putStrLn "Error"
    putStrLn " "
