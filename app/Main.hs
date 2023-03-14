module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import Data.Text
-- import Data.Typeable
import Data.Yaml as Y
import System.Environment

import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Map.Strict as MS
import qualified Data.Scientific as S
import qualified Data.Vector as V

main :: IO ()
main = do
    -- try with event-recieve yaml
    -- "C:\Users\brown\OneDrive\Desktop\Research Project\rtems-central\spec\rtems\event\req\send.yml"
    -- "C:\Users\brown\OneDrive\Desktop\Research Project\rtems-central\spec\rtems\event\if\event-07.yml"
    args <- getArgs
    let fileName = Prelude.head args
    content <- BS.readFile fileName
    let yamlData = decodeThrow content :: Maybe Value
    case yamlData of 
        Just a -> do
            checkType (toMyValue a)
            putStrLn " "
            putStrLn $ prettyPrint (moveKey "type" (toMyValue a))
        Nothing -> putStrLn "Error"
    putStrLn " "

-- Custom Data Type which is used to reprsent the Data.Aeson "Value"
-- Data Type, but instead of using custom constructors, uses the std.
-- Haskell constructors
data MyValue
  = MyObject [(String, MyValue)]
  | MyArray [MyValue]
  | MyString String
  | MyNumber Float 
  | MyBool Bool
  | MyNull
  deriving (Show)

-- Converts Value Data Type to custom MyValue Data Type
toMyValue :: Value -> MyValue
toMyValue (Object obj) = MyObject $ L.map (\(k, v) -> (sanitiseString $ show k, toMyValue v)) $ MS.toList $ AKM.toMap obj
toMyValue (Array arr) = MyArray $ L.map (\k -> toMyValue k) $ V.toList arr
toMyValue (String str) = MyString $ unpack str
toMyValue (Number num) = MyNumber (S.toRealFloat num)
toMyValue (Bool bool) = MyBool bool
toMyValue Null = MyNull

-- checks the item type and prints message based on that
checkType :: MyValue -> IO ()
checkType (MyObject obj) = case lookup "type" obj of
    Just (MyString "requirement") -> putStrLn "Item Type is Requirement"
    Just (MyString "build") -> putStrLn "Item Type is Build"
    Just (MyString "constraint") -> putStrLn "Item Type is Constraint"
    Just (MyString "glossary") -> putStrLn "Item Type is Glossary"
    Just (MyString "interface") -> putStrLn "Item Type is Interface"
    Just (MyString "requirment-validation") -> putStrLn "Item Type is Requirment Validation"
    Just (MyString "runtime-measurement-test") -> putStrLn "Item Type is Runtime Measurement Test"
    Just (MyString "specification") -> putStrLn "Item Type is Specification"
    Just (MyString "test-case") -> putStrLn "Item Type is Test Case"
    Just (MyString "test-platform") -> putStrLn "Item Type is Test Platform"
    Just (MyString "test-procedure") -> putStrLn "Item Type is Test Procedure"
    Just (MyString "test-suite") -> putStrLn "Item Type is Test Suite"
    Nothing -> putStrLn "Error"
    _ -> putStrLn "Error: not an object"

-- removes the two quotation marks from either side of a string that has
-- been processed with the "show" function
sanitiseString :: String -> String
sanitiseString = Prelude.drop 1 . Prelude.init

prettyPrint :: MyValue -> String
prettyPrint (MyObject obj) = "{\n" ++ Prelude.concatMap (\(k, v) -> "  " ++ "- " ++ k ++ ": " ++ prettyPrint' v) obj ++ "  }"
  where prettyPrint' v = Prelude.unlines $ Prelude.map ("" ++) $ Prelude.lines $ prettyPrint v
prettyPrint (MyArray arr) = "[\n" ++ Prelude.concatMap (\v -> "  " ++ prettyPrint' v) arr ++ "  ]"
  where prettyPrint' v = Prelude.unlines $ Prelude.map (Prelude.replicate 2 ' ' ++) $ Prelude.lines $ prettyPrint v
prettyPrint (MyString s) = show s
prettyPrint (MyNumber n) = show n
prettyPrint (MyBool b) = if b then "true" else "false"
prettyPrint MyNull = "null"

moveKey :: String -> MyValue -> MyValue
moveKey _ MyNull = MyNull
moveKey _ (MyString s) = MyString s
moveKey _ (MyNumber n) = MyNumber n
moveKey _ (MyBool b) = MyBool b
moveKey key (MyObject obj) = 
    case lookup key obj of
        Just value ->
            let objectWithoutKey = L.filter (\(k, v) -> k /= key) obj
            in MyObject $ (key, value) : objectWithoutKey
        Nothing -> MyObject obj
moveKey key (MyArray arr) = MyArray $ L.map (moveKey key) arr