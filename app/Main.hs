module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Data.Yaml as Y
import Data.Typeable
import Data.Aeson.KeyMap
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Text
import qualified Data.Aeson.Types as AT
import Data.Map.Strict

main :: IO ()
main = do
    content <- BS.readFile "C:/Users/brown/OneDrive/Desktop/Research Project/haskellCode/app/example.yaml"
    let yamlData = decodeThrow content :: Maybe Value
    case yamlData of 
        Just a -> do
            print $ a -- [(Key, Value)]
            putStrLn " "
            case a of
                Object obj -> do
                    print $ Data.List.map (\(k, v) -> (show k, v)) $ Data.Map.Strict.toList $ Data.Aeson.KeyMap.toMap obj -- Converts Object data type to List
                    print $ typeOf (Data.Map.Strict.toList $ Data.Aeson.KeyMap.toMap obj)
        Nothing -> putStrLn "Error"
    putStrLn " "

-- data MyValue
--   = MyObject [(String, Value)]
--   | MyArray [MyValue]
--   | MyString String
--   | MyNumber Scientific
--   | MyBool Bool
--   | MyNull

-- toMyValue :: Value -> MyValue
-- toMyValue (Object obj) = MyObject $ Data.Aeson.KeyMap.map (\(k, v) -> (unpack k, v)) $ Data.Aeson.KeyMap.toList obj
-- toMyValue (Array arr) = MyArray $ map toMyValue $ V.toList arr
-- toMyValue (String str) = MyString $ unpack str
-- toMyValue (Number num) = MyNumber num
-- toMyValue (Bool bool) = MyBool bool
-- toMyValue Null = MyNull
