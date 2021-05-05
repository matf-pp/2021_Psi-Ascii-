module Asciic
    ( StatePsic(..)
    , Psic(..)
    , defaultPsic 
    , feed
    , moodLevel
    , hungerLevel
    , dirtinessLevel
    , updatePsicMood
    , updatePsicHunger
    , updatePsicDirtiness
    , setToSleep
    ) where

import Data.Range
import Food


data StatePsic = Alive
               | Sleepy
               | Bored
            --   | Eating
               | Playing
               | Pooping
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
    , psicSays  :: String
    , psicLook  :: String
    } deriving (Show, Read)

setToSleep :: Psic -> Psic
setToSleep psic = psic {state = Sleepy
                       , psicSays = "Good night! Zzzz..."
                       , psicLook = asciicSleepy }

updatePsicMood :: Int -> Psic -> Psic
updatePsicMood newMood psic
    | newMood < 0   = psic { mood     = 0
                           , state    = Bored
                           , psicSays = "Amuse me or I will die. I am dead serious. :|"
                           , psicLook = asciicBored }
    | newMood > 100 = psic { mood = 100 }
    | state psic == Dead = psic {mood = 0}
    | state psic == Sleepy = psic {mood = newMood
                                  , psicLook = asciicSleepy}
    | otherwise     = psic { mood = newMood
                           , psicLook = asciic}

updatePsicHunger :: Int -> Psic -> Psic
updatePsicHunger newHunger psic
    | newHunger < 0   = psic { hunger   = 0 }
    | newHunger > 100 = psic { hunger    = 0
                             , mood      = 0
                             , dirtiness = 0
                             , state     = Dead
                             , psicSays = "I died from starvation. You are terrible owner :("
                             , psicLook  = asciicDead }
    | otherwise       = psic { hunger = newHunger }

updatePsicDirtiness :: Int -> Psic -> Psic
updatePsicDirtiness newDirtiness psic
    | newDirtiness < 0   = psic { dirtiness = 0 }
    | newDirtiness > 100 = psic { dirtiness = 100 }
    | otherwise          = psic { dirtiness = newDirtiness }

updatePsicState :: StatePsic -> Psic -> Psic
updatePsicState currentState psic = psic {state = currentState}

asciic :: String
asciic  = " /^ ^\\\n"
       ++ "/ 0 0 \\\n"
       ++ "V\\ Y /V\n"
       ++ " / - \\\n"
       ++ "/    |\n"
       ++ "V__) ||\n"

asciicDead :: String
asciicDead  = " /^ ^\\\n"
       ++ "/ x x \\\n"
       ++ "V\\ Y /V\n"
       ++ " / - \\\n"
       ++ "/    |\n"
       ++ "V__) ||\n"

asciicSleepy :: String
asciicSleepy  = " /^ ^\\\n"
       ++ "/ _ _ \\\n"
       ++ "V\\ Y /V\n"
       ++ " / . \\\n"
       ++ "/    |\n"
       ++ "V__) ||\n"

asciicBored :: String
asciicBored  = " /^ ^\\\n"
       ++ "/ - - \\\n"
       ++ "V\\ Y /V\n"
       ++ " / - \\\n"
       ++ "/    |\n"
       ++ "V__) ||\n"

defaultPsic :: Psic
defaultPsic = Psic 
    { name      = "Asciic"
    , owner     = "Igor"
    , age       = 0
    , hunger    = 25
    , mood      = 25 
    , dirtiness = 25
    , state     = Alive
    , psicSays  = "Hello there, " ++ owner defaultPsic ++ "!"
    , psicLook  = asciic
    }

fLevel :: (Psic -> Int) -> Psic -> Integer
fLevel f psic
    | inRange ( 0 +=+ 20) (f psic)  = 0
    | inRange (21 +=+ 40) (f psic)  = 1
    | inRange (41 +=+ 60) (f psic)  = 2
    | inRange (61 +=+ 80) (f psic)  = 3
    | inRange (81 +=+ 100) (f psic) = 4
    | otherwise                     = 5

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

