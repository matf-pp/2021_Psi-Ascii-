module Asciic
    ( StatePsic(..)
    , Psic(..)
    , defaultPsic 
    , asciic
    , feed
    , moodLevel
    , hungerLevel
    , dirtinessLevel
    ) where

import Data.Range
import Food


data StatePsic = Alive
               | Dead
               deriving (Show, Read, Eq)

data Psic = Psic 
    { name      :: String
    , owner     :: String 
    , age       :: Int
    , hunger    :: Int
    , mood      :: Int
    , dirtiness :: Int
    , state     :: StatePsic
    } deriving (Show, Read)


asciic :: String
asciic  = " /^ ^\\\n"
       ++ "/ 0 0 \\\n"
       ++ "V\\ Y /V\n"
       ++ " / - \\\n"
       ++ "/    |\n"
       ++ "V__) ||\n"

defaultPsic :: Psic
defaultPsic = Psic 
    { name      = "Asciic"
    , owner     = "Igor"
    , age       = 0
    , hunger    = 50
    , mood      = 53 
    , dirtiness = 50
    , state     = Alive
    }

fLevel :: (Psic -> Int) -> Psic -> Integer
fLevel f psic
    | inRange ( 0 +=+  5) (f psic) = 0
    | inRange ( 6 +=+ 10) (f psic) = 1
    | inRange (11 +=+ 15) (f psic) = 2
    | inRange (16 +=+ 20) (f psic) = 3
    | inRange (21 +=+ 25) (f psic) = 4
    | otherwise                    = 5

hungerLevel :: Psic -> Integer
hungerLevel = fLevel hunger

moodLevel :: Psic -> Integer
moodLevel = fLevel mood

dirtinessLevel :: Psic -> Integer
dirtinessLevel = fLevel dirtiness

feed :: Food -> Psic -> Psic
feed food psic = psic 
    { mood      = m + moodValue food
    , hunger    = h + hungerValue food
    , dirtiness = d + dirtinessValue food 
    } where m = mood psic
            h = hunger psic
            d = dirtiness psic

