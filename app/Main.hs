module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Typeable
import Data.Yaml as Y
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Map.Strict as MS
import qualified Data.Scientific as S
import qualified Data.Vector as V

main :: IO ()
main = do
    content <- BS.readFile "C:/Users/brown/OneDrive/Desktop/Research Project/haskellCode/app/example.yaml"
    let yamlData = decodeThrow content :: Maybe Value
    case yamlData of 
        Just a -> do
            putStrLn "Original Data Type:"
            print $ a -- [(Key, Value)]'
            putStrLn " "
            putStrLn "Custom Data Type:"
            print $ toMyValue a
            putStrLn " "
            checkType (toMyValue a)
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
    Just x -> putStrLn "unrecognised item type"
    Nothing -> putStrLn "Error"
checkType _ = putStrLn "Error: not an object"

-- removes the two quotation marks from either side of a string that has
-- been processed with the "show" function
sanitiseString :: String -> String
sanitiseString = Prelude.drop 1 . Prelude.init