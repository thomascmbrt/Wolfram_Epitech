module MyData where

data Conf = Conf
    { ruleValue         :: Maybe Int
    , startValue        :: Int
    , linesValue        :: Int
    , windwsValue       :: Int
    , moveValue         :: Int
    } deriving (Show)

defaultConf :: Conf
defaultConf = Conf
    { ruleValue     = Nothing
    , startValue    = 0
    , linesValue    = -1
    , windwsValue   = 80
    , moveValue     = 0
    }