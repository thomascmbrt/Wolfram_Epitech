module ErrorHandling where

import MyData
import Data.Maybe
import Data.Char (isDigit)
import System.Exit (exitWith)
import System.Exit (ExitCode(ExitFailure)) 
import System.Exit (ExitCode(ExitSuccess)) 

isNum :: String -> Bool  
isNum [] = False  
isNum s = all isDigit s

errorHandling :: [String] -> IO ()
errorHandling [] = usage >> exitWith (ExitFailure 84)
errorHandling [x] = usage >> exitWith (ExitFailure 84)
errorHandling (x:"-":xs) = usage >> exitWith (ExitFailure 84)
errorHandling (x:"--":xs) = usage >> exitWith (ExitFailure 84)
errorHandling (x:y:"-":xs) = usage >> exitWith (ExitFailure 84)
errorHandling (x:y:"--":xs) = usage >> exitWith (ExitFailure 84)
errorHandling (x:y:xs)
    | isNum y = return ()
    | otherwise = usage >> exitWith (ExitFailure 84)

usage :: IO ()
usage = putStr "Usage: ./wolfram [params] [value] \n\n" >>
        putStrLn "Options:" >>
        putStr "\t--rule\t\t[30] [90] [110] \n" >>
        putStr "\t--start\t\t[value]          (default 0) \n" >>
        putStr "\t--lines\t\t[value]          (if no provided the program never stop)\n" >>
        putStr "\t--window\t[value]          (default 80)\n" >>
        putStr "\t--move\t\t[value]          (apply translation on the window)\n"