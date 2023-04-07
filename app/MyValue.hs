module MyValue where

import Data.Text
-- import Data.Typeable
import Data.Yaml as Y

import qualified Data.Aeson.KeyMap as AKM
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
toMyValue' (Array arr) = MyArray $ L.map toMyValue' $ V.toList arr
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
checkType _ = putStrLn "Error: Not an object"

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
            let objectWithoutKey = L.filter (\(k, _) -> k /= key) obj
            in MyObject $ (key, value) : objectWithoutKey
        Nothing -> MyObject obj
moveKey key (MyArray arr) = MyArray $ L.map (moveKey key) arr

{-
Description: Moves any fields with "type" contained in the field to the top of the MyValue
Parameters: MyValue
Returns: MyValue
-}
prioritiseType :: MyValue -> MyValue
prioritiseType (MyObject obj) = MyObject (Prelude.reverse typeEntries ++ otherEntries) where (typeEntries, otherEntries) = L.partition (\k -> "type" `L.isInfixOf` fst k) obj
prioritiseType _ = MyString "type to be priortised is not an object"
{-
Description: returns only the fields corresponding to the keys given in a list
Parameters: [String] (Might be obtained from commandline arguments), MyValue (MyValue to be searched)
Returns: MyValue
-}
myObjectSpecificFields :: [String] -> MyValue -> MyValue
myObjectSpecificFields keys (MyObject obj) = MyObject $ L.filter (\(k, _) -> k `L.elem` keys) obj
myObjectSpecificFields _ _ = MyString "Error finding Specific Key from Object"

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

{-
Description:
Parameters:
Returns:
-}
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
makeUniqueCondList xs = Prelude.concatMap splitArrays xs

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
findKey key (MyObject obj) = snd $ Prelude.head $ L.filter (\(k, _) -> k == key) obj
findKey _ _ = MyString "Error Finding Key"

{-
Description: Given MyArray of MyObjects, filters out any nested MyObjects with a given key
and adds them to a list. i.e., can add all post- or pre-conditions to a list
of MyObjects
Parameters: MyValue, String
Returns: [MyValue]
-}
findConditions :: MyValue -> String -> [MyValue]
findConditions (MyArray arr) key = L.map (findKey key) arr
findConditions _ _ = [MyString "Error finding conditions"]

{-
Description: Takes a list of MyValues (i.e., the list of post- or pre-condition MyObjects
extracted from the transition map) and turns them into a list of MyStrings
Parameters: [MyValue]
Returns: [[MyValue]]
-}
conditionsToList :: [MyValue] -> [[MyValue]]
conditionsToList [] = []
conditionsToList (MyObject x : xs) = extractValues x : conditionsToList xs
conditionsToList (MyString _ : xs) = conditionsToList xs
conditionsToList _ = []

{-
Description: Helper functino which helps conditionsToList to extract values from each MyObject
Parameters: [(String, MyValue)]
Returns: [MyValue]
-}
extractValues :: [(String, MyValue)] -> [MyValue]
extractValues [] = []
extractValues xs = Prelude.foldr (\ x -> (++) [snd x]) [] xs

{-
Description: Converts a list of MyArrays to a list of lists of MyValues
Parameters: [MyValue]
Returns: [[MyValue]]
-}
convertToList :: [MyValue] -> [[MyValue]]
convertToList [] = []
convertToList [MyArray arr] = [arr]
convertToList (MyArray arr : xs) = arr : convertToList xs
convertToList _ = []

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
extractStates (x : xs) = extractStates' x : extractStates xs

{-
Description: Helper function for extractStates which goes through each sublist 
and extracts the "name" field
Parameters: [MyValue]
Returns: [MyValue]
-}
extractStates' :: [MyValue] -> [MyValue]
extractStates' [] = []
extractStates' [x] = [findKey "name" x]
extractStates' (x : xs) = findKey "name" x : extractStates' xs

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
reorderPostConditions = L.map (\(a:b:cs) -> b:a:cs)

{-
The post conditions are defined in a different order to which they 
appear in the transition map, Thus, the maximal list needs to be
reordered before being compared with the actual list
-}
reorderPreConditions :: [[MyValue]] -> [[MyValue]]
reorderPreConditions = L.map (\(a:b:c:d:rest) -> a:c:d:b:rest)

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
  Prelude.filter (`notElem` listsToRemove) fromLists'
  where
    fromLists' = Prelude.foldl (\acc lst -> L.filter (not . match lst) acc) fromLists listsToRemove
    match [] [] = True
    match (MyString "N/A":xs) (_:ys) = match xs ys
    match (MyString "all":xs) (_:ys) = match xs ys
    match (x:xs) (y:ys) = x == y && match xs ys
    match _ _ = False

{-
Description: Takes two lists of lists of MyValues, and checks the percentage coverage
Parameters: [[MyValue]], [[MyValue]]
Returns: Int
-}
getPercentageCoverage :: [[MyValue]] -> [[MyValue]] -> Int
getPercentageCoverage condsNotCovered allConds = round ((fromIntegral (Prelude.length allConds - Prelude.length condsNotCovered) / fromIntegral (Prelude.length allConds)) * 100)

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
prettyPrintPreConds' (MyString a : MyString b : MyString c : MyString d : _) = "ID: " ++ show a ++ "\nReceiver State: " ++ show b ++ "\nSatisfy: " ++ show c ++ "\nSend: " ++ show d 
prettyPrintPreConds' _ = []

{-
Description:
Parameters:
Returns:
-}
prettyPrintPreCondsWithCode :: [[MyValue]] -> MyValue-> String
prettyPrintPreCondsWithCode [] _= []
prettyPrintPreCondsWithCode (a:xs) myVal = prettyPrintPreCondsWithCode' a myVal ++ "\n\n" ++ prettyPrintPreCondsWithCode xs myVal

prettyPrintPreCondsWithCode' :: [MyValue] -> MyValue -> String
prettyPrintPreCondsWithCode' [] _ = "Empty list of Pre Conditions" 
prettyPrintPreCondsWithCode' (MyString idCond : MyString receiverCond : MyString  satisfyCond : MyString sendCond : _) myVal = "ID: " ++ show idCond ++ "\nReceiver State: " ++ show receiverCond ++ "\nSatisfy: " ++ show satisfyCond ++ "\nSend: " ++ show sendCond ++ "\n" ++  (getCondCode "Id" idCond "pre-conditions" myVal ++ getCondCode "ReceiverState" receiverCond "pre-conditions" myVal ++ getCondCode "Satisfy" satisfyCond "pre-conditions" myVal ++ getCondCode "Send" sendCond "pre-conditions" myVal)
prettyPrintPreCondsWithCode' _ _ = "Error Printing Pre Condition code"

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
prettyPrintPostConds' (MyString a : MyString b: MyString c : _) = "Receive Status: " ++ show a ++ "\nSend Status: " ++ show b ++ "\nSender Pre-emption: " ++ show c
prettyPrintPostConds' _ = []

{-
Description:
Parameters:
Returns:
-}
prettyPrintPostCondsWithCode :: [[MyValue]] -> MyValue-> String
prettyPrintPostCondsWithCode [] _ = []
prettyPrintPostCondsWithCode (a:xs) myVal = prettyPrintPostCondsWithCode' a myVal ++ "\n\n" ++ prettyPrintPostCondsWithCode xs myVal

prettyPrintPostCondsWithCode' :: [MyValue] -> MyValue -> String
prettyPrintPostCondsWithCode' [] _ = "Empty list of Pre Conditions" 
prettyPrintPostCondsWithCode' (MyString receiveCond : MyString sendCond : MyString  senderPreCond : _) myVal = "Receive Status: " ++ show receiveCond ++ "\nSend Status: " ++ show sendCond ++ "\nSender Pre-emption: " ++ show senderPreCond ++ "\n" ++ (getCondCode "ReceiveStatus" receiveCond "post-conditions" myVal ++ getCondCode "SendStatus" sendCond "post-conditions" myVal ++ getCondCode "SenderPreemption" senderPreCond "post-conditions" myVal)
prettyPrintPostCondsWithCode' _ _ = "Error Printing Pre Condition code"

{-
Description:
Parameters:
Returns:
-}
getCondCode :: String -> String -> String -> MyValue -> String
getCondCode condName condState preOrPost myVal = fromMyStringToString (snd (Prelude.head (L.filter (\x -> Prelude.head x == ("name", MyString condState)) listToSearch)!!1)) where 
    listToSearch = fromMyObjectToList (fromMyArrayToList (snd (getSpecifiedCond condName preOrPost myVal!!1)))

{-
Description:
Parameters:
Returns:
-}
getSpecifiedCond :: String -> String -> MyValue -> [(String, MyValue)]
getSpecifiedCond condName preOrPost myVal = Prelude.head (L.filter (\x -> Prelude.head x == ("name", MyString condName)) listToSearch) where 
    listToSearch = fromMyObjectMyArrayToList preOrPost myVal

getSpecifiedCond' :: String -> [[(String, MyValue)]] -> [(String, MyValue)]
getSpecifiedCond' condName myValList = Prelude.head (L.filter (\x -> Prelude.head x == ("name", MyString condName)) myValList)

{-
Description:
Parameters:
Returns:
-}
fromMyObjectMyArrayToList :: String -> MyValue -> [[(String, MyValue)]]
fromMyObjectMyArrayToList str myVal = fromMyObjectToList (fromMyArrayToList (findKey str myVal))

fromMyArrayToList :: MyValue -> [MyValue]
fromMyArrayToList (MyArray arr) = arr 
fromMyArrayToList _ = [MyString "Conversion from MyArray to List failed"]

fromMyObjectToList :: [MyValue] -> [[(String, MyValue)]]
fromMyObjectToList [] = []
fromMyObjectToList xs = Prelude.foldr (\x -> (++) [fromMyObjectToList' x]) [] xs

fromMyObjectToList' :: MyValue -> [(String, MyValue)]
fromMyObjectToList' (MyObject obj) = obj
fromMyObjectToList' _ = [("Error", MyString "Conversion from MyObject to List failed.")]

{-
Description:
Parameters:
Returns:
-}
fromMyStringToString :: MyValue -> String
fromMyStringToString (MyString str) = str
fromMyStringToString _ = "Error - not a MyString"

{-
Description:
Parameters:
Returns:
-}
parseArgs :: [String] -> [(String, [String])]
parseArgs [] = []
parseArgs (flag:args) =
  case Prelude.break isFlag args of
    ([], rest) -> (flag, []) : parseArgs rest
    (params, rest) -> (flag, params) : parseArgs rest
  where isFlag str = Prelude.take 2 str == "--"

{-
Description:
Parameters:
Returns:
-}
processArgs :: [(String, [String])] -> MyValue -> String
processArgs xs myVal = Prelude.foldl (\acc x -> acc ++ cmdFunc x myVal) "" xs

{-
Description:
Parameters:
Returns:
-}
cmdFunc :: (String, [String]) -> MyValue -> String
cmdFunc x myVal = case fst x of "--prettyprint" -> handlePrettyPrint (snd x) myVal
                                "--preconds" -> handlePreConds (snd x) myVal
                                "--postconds" -> handlePostConds (snd x) myVal
                                _ -> "Error - input not recognised"

{-
Description:
Parameters:
Returns:
-}
handlePrettyPrint :: [String] -> MyValue -> String
handlePrettyPrint xs myVal = Prelude.foldl (\acc x -> acc ++ handlePrettyPrint' x myVal) "" xs

handlePrettyPrint' :: String -> MyValue -> String
handlePrettyPrint' str obj = prettyPrint $ myObjectSpecificFields [str] obj

{-
Description:
Parameters:
Returns:
-}
handlePreConds :: [String] -> MyValue -> String
handlePreConds xs myVal = Prelude.foldl (\acc x -> acc ++ handlePreConds' x myVal) "" xs

handlePreConds' :: String -> MyValue -> String
handlePreConds' "coverage" myVal = "Percentage coverage of pre conditions: " ++ show (getPreCondPercent myVal) ++ "%\n" ++ prettyPrintPreConds (removeMatchingSublists (getPreConditions myVal) (getMaxPreConditions myVal))
handlePreConds' "test-code" myVal = "Percentage coverage of pre conditions: " ++ show (getPreCondPercent myVal) ++ "%\n" ++ prettyPrintPreCondsWithCode (removeMatchingSublists (getPreConditions myVal) (getMaxPreConditions myVal)) myVal
handlePreConds' _ _ = "Error"

handlePostConds :: [String] -> MyValue -> String
handlePostConds xs myVal = Prelude.foldl (\acc x -> acc ++ handlePostConds' x myVal) "" xs

handlePostConds' :: String -> MyValue -> String
handlePostConds' "coverage" myVal = "Percentage coverage of pre conditions: " ++ show (getPostCondPercent myVal) ++ "%\n" ++ prettyPrintPostConds (removeMatchingSublists (getPostConditions myVal) (getMaxPostConditions myVal))
handlePostConds' "test-code" myVal = "Percentage coverage of pre conditions: " ++ show (getPostCondPercent myVal) ++ "%\n" ++ prettyPrintPostCondsWithCode (removeMatchingSublists (getPostConditions myVal) (getMaxPostConditions myVal)) myVal
handlePostConds' _ _ = "Error"   


-- Check that all post/pre condition variations (combinations) are covered
-- Estimate how many different combinations there are (how many entries will be in the maximal list)
-- Being able to follow links within around the directory
-- Given type, follow relevant links to other 