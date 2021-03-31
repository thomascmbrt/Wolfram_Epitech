module ArgsManagment where
import MyData
import Data.Maybe

atoi :: String -> Int
atoi s = read s::Int 

myElem :: Eq a => a ->[a] -> Bool 
myElem _ [] = False 
myElem a (x:xs)
    | a == x = True 
    | otherwise = myElem a xs

isNum :: [Char] -> Bool 
isNum [] = True 
isNum (x:xs)
    | myElem x ['0'..'9'] = isNum xs
    | otherwise = False 

getOpts :: Conf -> [String] -> Maybe Conf
getOpts c [] =  Just c 
getOpts conf (x:y:xs) 
    | x == "--rule" && (y== "30" || y == "90" || y == "110") = getOpts conf {ruleValue = Just (atoi y)} xs 
    | x == "--start" && isNum y = getOpts conf {startValue = atoi y} xs
    | x == "--lines" && isNum y = getOpts conf {linesValue = atoi y} xs
    | x == "--window" && isNum y = getOpts conf {windwsValue = atoi y} xs
    | x == "--move" && isNum y = getOpts conf {moveValue = atoi y} xs
    | otherwise = Nothing 