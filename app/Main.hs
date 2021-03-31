import System.Environment
import Data.List
import Data.String
import MyData
import ArgsManagment
import ErrorHandling
import Handler ( handler, doMyWindow, checkMyRule, removeMaybe )
import System.Exit (exitWith)
import System.Exit (ExitCode(ExitFailure)) 
import System.Exit (ExitCode(ExitSuccess)) 

setMyLoop :: Maybe Int -> Conf -> Int 
setMyLoop x mdb
    | x == Just 90 = 1
    | otherwise = 0

setMyLine :: Maybe Int -> Conf -> Int 
setMyLine x mdb
    | x == Just 90 && startValue mdb == 100 = linesValue mdb + 1
    | otherwise = linesValue mdb

main :: IO ()
main = do
    l_a <- getArgs
    errorHandling l_a
    let mdb = removeMaybe (getOpts defaultConf l_a)
    checkMyRule mdb
    let prevString = (doMyWindow ((windwsValue mdb) + (moveValue mdb))) ++ "*" ++ doMyWindow ((windwsValue mdb) - 1)
    let tick = setMyLine (ruleValue mdb) mdb
    let ctrlStart = setMyLoop (ruleValue mdb) mdb
    if startValue mdb > 0 
        then handler mdb prevString tick ctrlStart  
        else putStrLn prevString >> handler mdb prevString tick (startValue mdb + 1)
    exitWith (ExitSuccess)