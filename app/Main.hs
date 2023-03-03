module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Data.Yaml as Y
import Data.Typeable
import Data.Aeson.KeyMap

main :: IO ()
main = do
    content <- BS.readFile "C:/Users/brown/OneDrive/Desktop/Research Project/haskellCode/app/example.yaml"
    let yamlData = decodeThrow content :: Maybe Object
    print $ typeOf yamlData
    case yamlData of 
        Just a -> print $ toList a -- [(Key, Value)]
        Nothing -> putStrLn "Error"
    putStrLn " "
    print yamlData

checkType :: (String a) => a -> String
checkType "build" = "Build Item type"
checkType "constraint" = "Constraint Item type"
checkType "glossary" = "Glossary Item type"
checkType "interface" = "Interface Item type"
checkType "requirement" = "Requirement Item type"
checkType "requirement-validation" = "Requirement Validation Item type"
checkType "runtime-measurement-test" = "Runtime Measurement Test Item type"
checkType "specification" = "Specification Item type"
checkType "test-case" = "Test Case Item type"
checkType "test-platform" = "Test Platform Item type"
checkType "test-procedure" = "Test Procedure Item type"
checkType "test-suite" = "Test Suite Item type"
    

    
