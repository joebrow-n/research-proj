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
  deriving (Show, Eq)

{-
Description: Extends the MyValue' function so that the MyValue is automatically
sorted with the "type" fields at the top.
Parameters: Value obtained from a yaml file
Returns: MyValue
-}
toMyValue :: Value -> MyValue
toMyValue obj = prioritiseType (toMyValue' obj) 

{-
Description: Converts Value Data Type to custom MyValue Data Type
Parameters: Value obtained from a yaml file
Returns: MyValue
-}
toMyValue' :: Value -> MyValue
toMyValue' (Object obj) = MyObject $ L.map (\(k, v) -> (sanitiseString $ show k, toMyValue' v)) $ MS.toList $ AKM.toMap obj
toMyValue' (Array arr) = MyArray $ L.map (\k -> toMyValue' k) $ V.toList arr
toMyValue' (String str) = MyString $ unpack str
toMyValue' (Number num) = MyNumber (S.toRealFloat num)
toMyValue' (Bool bool) = MyBool bool
toMyValue' Null = MyNull

{-
Description: checks the item type and prints message based on that
Parameters: MyValue 
Returns: IO operation which prints a string to the console
-}
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

{-
Description: removes the two quotation marks from either side of a string that has
been processed with the "show" function
Parameters: String
Returns: String
-}
sanitiseString :: String -> String
sanitiseString = Prelude.drop 1 . Prelude.init

{-
Description: Pretty Prints a the MyValue data type
Parameters: MyValue
Returns: String
-}
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

{-
Description: moves a given key to the top of an object
Parameters: String (key to be moved), MyValue (MyValue to be searched)
Returns: MyValue
-}
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

{-
Description: Moves any fields with "type" contained in the field to the top of the MyValue
Parameters: MyValue
Returns: MyValue
-}
prioritiseType :: MyValue -> MyValue
prioritiseType (MyObject obj) = MyObject $ ((Prelude.reverse typeEntries) ++ otherEntries) where (typeEntries, otherEntries) = L.partition (\k -> "type" `L.isInfixOf` fst k) obj

{-
Description: Prints only the fields corresponding to the keys given in a list
Parameters: [String] (Might be obtained from commandline arguments), MyValue (MyValue to be searched)
Returns: MyValue
-}
myObjectSpecificFields :: [String] -> MyValue -> MyValue
myObjectSpecificFields keys (MyObject obj) = MyObject $ L.filter (\(k, _) -> k `L.elem` keys) obj

{-
Description: Takes a MyValue and retrieves a list of lists of post-conditions
Parameters: MyValue
Returns: [[MyValue]] 
-}
getPostConditions :: MyValue -> [[MyValue]]
getPostConditions myVal = makeUniqueCondList (conditionsToList (findConditions (findKey "transition-map" myVal) "post-conditions"))

{-
Description: Takes a MyValue and retrieves a list of lists of pre-conditions
Parameters: MyValue
Returns: [[MyValue]]
-}
getPreConditions :: MyValue -> [[MyValue]]
getPreConditions myVal = makeUniqueCondList (conditionsToList (findConditions (findKey "transition-map" myVal) "pre-conditions"))

{-
Description:
Parameters:
Returns:
-}
getMaxPostConditions :: MyValue -> [[MyValue]]
getMaxPostConditions myVal = reorderPostConditions (combinations (extractStates (convertToList (findConditions (findKey "post-conditions" myVal) "states"))))

{-
Description:
Parameters:
Returns:
-}
getMaxPreConditions :: MyValue -> [[MyValue]]
getMaxPreConditions myVal = reorderPreConditions (combinations (extractStates (convertToList (findConditions (findKey "pre-conditions" myVal) "states"))))

getPostCondPercent :: MyValue -> Int
getPostCondPercent myVal = getPercentageCoverage (removeMatchingSublists (getPostConditions myVal) (getMaxPostConditions myVal)) (getMaxPostConditions myVal)

getPreCondPercent :: MyValue -> Int
getPreCondPercent myVal = getPercentageCoverage (removeMatchingSublists (getPreConditions myVal) (getMaxPreConditions myVal)) (getMaxPreConditions myVal)

{-
Description: Recursively applies splitArrays to a list of list of MyValues
Parameters: [[MyValue]]
Returns: [[MyValue]]
-}
makeUniqueCondList :: [[MyValue]] -> [[MyValue]]
makeUniqueCondList [] = []
makeUniqueCondList (x:xs) = (splitArrays x) ++ (makeUniqueCondList xs)

{-
Description:Takes a list of MyValues - these will be either MyString or MyArrays containing MyStrings. If
there is a MyArray present, it duplicates the list but each time changes the value of the MyString
that was in the MyArray. For example:
Input:
[MyString "Jeff", MyString "Anna", MyString "Amy", MyArray [MyString "Joe", MyString "Greg"], MyString "Fred"] 
Output:
[MyString "Jeff", MyString "Anna", MyString "Amy", MyString "Greg", MyString "Fred"] 
[MyString "Jeff", MyString "Anna", MyString "Amy", MyString "Joe", MyString "Fred"] 
Parameters: [MyValue]
Returns: [[MyValue]]
-}
splitArrays :: [MyValue] -> [[MyValue]]
splitArrays [] = [[]]
splitArrays (MyArray arr : rest) =
  [x : xs | x <- arr, xs <- splitArrays rest]
splitArrays (x:rest) =
  [x : xs | xs <- splitArrays rest]

{-
Description: Following function can extract a given keys value from a MyValue, i.e., extract the
transition map
Parameters: String, MyValue
Returns: MyValue
-}
findKey :: String -> MyValue -> MyValue
findKey key (MyObject obj) = snd $ Prelude.head $ L.filter (\(k, v) -> k == key) obj

{-
Description: Given MyArray of MyObjects, filters out any nested MyObjects with a given key
and adds them to a list. i.e., can add all post- or pre-conditions to a list
of MyObjects
Parameters: MyValue, String
Returns: [MyValue]
-}
findConditions :: MyValue -> String -> [MyValue]
findConditions (MyArray arr) key = L.map (findKey key) arr

{-
Description: Takes a list of MyValues (i.e., the list of post- or pre-condition MyObjects
extracted from the transition map) and turns them into a list of MyStrings
Parameters: [MyValue]
Returns: [[MyValue]]
-}
conditionsToList :: [MyValue] -> [[MyValue]]
conditionsToList [] = []
conditionsToList (MyObject x : xs) = [extractValues x] ++ conditionsToList xs
conditionsToList (MyString x : xs) = conditionsToList xs
conditionsToList _ = []

{-
Description: Helper functino which helps conditionsToList to extract values from each MyObject
Parameters: [(String, MyValue)]
Returns: [MyValue]
-}
extractValues :: [(String, MyValue)] -> [MyValue]
extractValues [] = []
extractValues (x : xs) = [snd x] ++ extractValues xs

{-
Description: Converts a list of MyArrays to a list of lists of MyValues
Parameters: [MyValue]
Returns: [[MyValue]]
-}
convertToList :: [MyValue] -> [[MyValue]]
convertToList [] = []
convertToList ([MyArray arr]) = [arr]
convertToList (MyArray arr : xs) = [arr] ++ convertToList xs

{-
Description: Extracts ther various states that each pre/post condition can have, and puts
them into separate lists. i.e., there is a list for SendStatus, ReceieveStatus
and SenderPreemption
Parameters: [[MyValue]]
Returns: [[MyValue]]
-}
extractStates :: [[MyValue]] -> [[MyValue]]
extractStates [] = []
extractStates [x] = [extractStates' x]
extractStates (x : xs) = [extractStates' x] ++ extractStates xs

{-
Description: Helper function for extractStates which goes through each sublist 
and extracts the "name" field
Parameters: [MyValue]
Returns: [MyValue]
-}
extractStates' :: [MyValue] -> [MyValue]
extractStates' [] = []
extractStates' [x] = [findKey "name" x]
extractStates' (x : xs) = [findKey "name" x] ++ extractStates' xs

{-
Description: Gets all possible combinations of the different values that the pre/post condition
fields can take on - presents the maximal list.
Parameters: [[MyValue]]
Returns: [[MyValue]]
-}
combinations :: [[MyValue]] -> [[MyValue]]
combinations [] = [[]]
combinations (xs:xss) = [y:ys | y <- xs, ys <- combinations xss]

{-
The post conditions are defined in a different order to which they 
appear in the transition map, Thus, the maximal list needs to be
reordered before being compared with the actual list
-}
reorderPostConditions :: [[MyValue]] -> [[MyValue]]
reorderPostConditions xs = L.map (\(a:b:cs) -> b:a:cs) xs

{-
The post conditions are defined in a different order to which they 
appear in the transition map, Thus, the maximal list needs to be
reordered before being compared with the actual list
-}
reorderPreConditions :: [[MyValue]] -> [[MyValue]]
reorderPreConditions xs = L.map (\(a:b:c:d:rest) -> a:d:b:c:rest) xs

{-
Description: Takes two lists of lists of MyValues and checks to see if any of the sublists
in the first list appear in the second list. If they do, then that sublist is 
removed from the second list. This is done for all the lists and the final list
is returned
Parameters: [[MyValue]], [[MyValue]]
Returns: [[MyValue]]
-}
removeMatchingSublists :: [[MyValue]] -> [[MyValue]] -> [[MyValue]]
removeMatchingSublists listsToRemove fromLists =
  L.filter (\lst -> notElem lst listsToRemove) fromLists'
  where
    fromLists' = L.foldl (\acc lst -> L.filter (not . match lst) acc) fromLists listsToRemove
    match [] [] = True
    match (MyString "N/A":xs) (_:ys) = match xs ys
    match (x:xs) (y:ys) = x == y && match xs ys
    match _ _ = False

{-
Description: Takes two lists of lists of MyValues, and checks the percentage coverage
Parameters: [[MyValue]], [[MyValue]]
Returns: Int
-}
getPercentageCoverage :: [[MyValue]] -> [[MyValue]] -> Int
getPercentageCoverage condsNotCovered allConds = round (((fromIntegral (Prelude.length allConds - Prelude.length condsNotCovered)) / (fromIntegral (Prelude.length allConds))) * 100)

{-
Description:
Parameters:
Returns:
-}
prettyPrintPreConds :: [[MyValue]] -> String
prettyPrintPreConds [] = []
prettyPrintPreConds (a:xs) = prettyPrintPreConds' a ++ "\n\n" ++ prettyPrintPreConds xs

prettyPrintPreConds' :: [MyValue] -> String
prettyPrintPreConds' [] = []
prettyPrintPreConds' (MyString a : MyString b : MyString c : MyString d : xs) = "ID: " ++ show a ++ "\nReceiver State: " ++ show b ++ "\nSatisfy: " ++ show c ++ "\nSend: " ++ show d 

{-
Description:
Parameters:
Returns:
-}
prettyPrintPostConds :: [[MyValue]] -> String
prettyPrintPostConds [] = []
prettyPrintPostConds (a:xs) = prettyPrintPostConds' a ++ "\n\n" ++ prettyPrintPostConds xs

prettyPrintPostConds' :: [MyValue] -> String
prettyPrintPostConds' [] = []
prettyPrintPostConds' (MyString a : MyString b: MyString c : xs) = "Receive Status: " ++ show a ++ "\nSend Status: " ++ show b ++ "\nSender Pre-emption: " ++ show c
prettyPrintPostConds' _ = []

-- Check that all post/pre condition variations (combinations) are covered
-- Estimate how many different combinations there are (how many entries will be in the maximal list)
-- Being able to follow links within around the directory
-- Given type, follow relevant links to other 