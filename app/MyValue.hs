module MyValue where

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

-- Pretty Prints a the MyValue data type
prettyPrint :: MyValue -> String
prettyPrint (MyObject obj) = "{\n" ++ Prelude.concatMap (\(k, v) -> "  " ++ "- " ++ k ++ ": " ++ prettyPrint' v) obj ++ "  }"
  where prettyPrint' v = Prelude.unlines $ Prelude.map ("" ++) $ Prelude.lines $ prettyPrint v
prettyPrint (MyArray arr) = "[\n" ++ Prelude.concatMap (\v -> "  " ++ prettyPrint' v) arr ++ "  ]"
  where prettyPrint' v = Prelude.unlines $ Prelude.map (Prelude.replicate 2 ' ' ++) $ Prelude.lines $ prettyPrint v
prettyPrint (MyString s) =
  let ls = Prelude.lines s
      indent = Prelude.replicate 2 ' '
  in Prelude.unlines $ Prelude.map (\l -> "  " ++ indent ++ l) ls
prettyPrint (MyNumber n) = show n
prettyPrint (MyBool b) = if b then "true" else "false"
prettyPrint MyNull = "null"

-- moves a given key to the top of an object
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

-- Prints only the fields corresponding to the keys given in a list
myObjectSpecificFields :: [String] -> MyValue -> MyValue
myObjectSpecificFields keys (MyObject obj) = MyObject $ L.filter (\(k, _) -> k `Prelude.elem` keys) obj