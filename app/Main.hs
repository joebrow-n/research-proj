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

main :: IO ()
main = do
    content <- BS.readFile "C:/Users/brown/OneDrive/Desktop/Research Project/haskellCode/app/example.yaml"
    let yamlData = decodeThrow content :: Maybe Value
    case yamlData of 
        Just a -> do
            print $ a -- [(Key, Value)]
            putStrLn " "
            case a of
                Object obj -> print $ typeOf (Data.Aeson.KeyMap.toList obj) -- This right here, this is the key to it all
        Nothing -> putStrLn "Error"
    putStrLn " "

-- data MyValue
--   = MyObject (HM.HashMap String MyValue)
--   | MyArray (V.Vector MyValue)
--   | MyString String
--   | MyNumber Double
--   | MyBool Bool
--   | MyNull
--   deriving (Show)

-- toMyValue :: Value -> MyValue
-- toMyValue (Object obj) = MyObject $ HM.fromList $ Data.List.map (\(k, v) -> (k, toMyValue v)) $ HM.toList obj
-- toMyValue (Array arr) = MyArray $ V.fromList $ Data.List.map toMyValue $ V.toList arr
-- toMyValue (String str) = MyString (Data.Text.unpack str)
-- toMyValue (Number num) = MyNumber $ realToFrac num
-- toMyValue (Bool bool) = MyBool bool
-- toMyValue Null = MyNull

-- listToPrint :: Value -> toList
-- listToPrint (Object) = toList Object

-- myObjectList :: [(Text, Value)]
-- myObjectList = case myObject of
--   Object obj -> print $ HM.toList obj
--   _ -> []