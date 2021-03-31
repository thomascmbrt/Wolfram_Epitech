module Handler where

import System.Environment
import Control.Monad
import MyData
import Data.Maybe

import Rules
import ErrorHandling

import Data.Char (isDigit)
import System.Exit
    ( exitWith, ExitCode(ExitFailure), ExitCode(ExitSuccess) )

checkMyRule :: Conf -> IO()
checkMyRule conf | isNothing (ruleValue  conf) = usage >> exitWith (ExitFailure 84)
                 | otherwise = return ()

removeMaybe :: Maybe Conf -> Conf
removeMaybe (Just dataconf) = dataconf

doMyWindow :: Int -> String
doMyWindow window = replicate (div window 2) ' '

createLine :: Int -> String -> String
createLine _ [x, y, z] = [x, y]
createLine 30 (x:y:z:xs) = rule30 x y z : createLine 30 (y:z:xs)
createLine 90 (x:y:z:xs) = rule90 x y z : createLine 90 (y:z:xs)
createLine 110 (x:y:z:xs) = rule110 x y z : createLine 110 (y:z:xs)

doMyLine :: Int -> Conf -> [Char] -> [Char]
doMyLine index conf line
    | windwsValue conf < length line - 1 = doMyLine index conf (init (tail line))
    | windwsValue conf < length line = doMyLine index conf (init line)
    | otherwise = line


handle30 :: Conf  -> String -> Int -> Int -> IO()
handle30 conf str tick ctrlStart =
    if ctrlStart < startValue conf
        then handler conf prevStr tick (ctrlStart + 1)
        else putStrLn toPrint >> handler conf prevStr (tick - 1) ctrlStart
    where
        myLine = createLine 30 (" " ++ str ++ " ")
        toPrint = doMyLine 0 conf myLine
        prevStr = " " ++ myLine

handle90 :: Conf  -> String -> Int -> Int -> IO()
handle90 conf str tick ctrlStart =
    if ctrlStart < startValue conf
        then handler conf prevStr tick (ctrlStart + 1)
        else putStrLn toPrint >> handler conf prevStr (tick - 1) ctrlStart
    where
        myLine = createLine 90 (" " ++ str ++ " ")
        toPrint = doMyLine 0 conf myLine
        prevStr = " " ++ myLine

handle110 :: Conf  -> String -> Int -> Int -> IO()
handle110 conf str tick ctrlStart =
    if ctrlStart < startValue conf
        then handler conf prevStr tick (ctrlStart + 1)
        else putStrLn toPrint >> handler conf prevStr (tick - 1) ctrlStart
    where
        myLine = createLine 110 (" " ++ str ++ " ")
        toPrint = doMyLine 0 conf myLine
        prevStr = " " ++ myLine


handler :: Conf -> String -> Int -> Int -> IO ()
handler _ [] _ _= return ()
handler _ _ 1 _ = return ()
handler conf str tick ctrlStart
    | ruleValue conf == Just 30 = handle30 conf str tick ctrlStart
    | ruleValue conf == Just 90 = handle90 conf str tick ctrlStart
    | ruleValue conf == Just 110 = handle110 conf str tick ctrlStart