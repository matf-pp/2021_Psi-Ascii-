module Asciic
    ( StatePsic(..)
    , Psic(..)
    , defaultPsic 
    , moodLevel
    , hungerLevel
    , dirtinessLevel
    , updatePsicMood
    , updatePsicHunger
    , updatePsicDirtiness
    , inValidState
    , psicSays
    , asciic2Draw
    ) where

-------------------------------------------------------------------------------

import Data.Range
import Food

-------------------------------------------------------------------------------

data StatePsic = Existing
               | Sleeping
               | Hungry
               | Eating
               | Playing
               | Pooping
               | Cleaning
               | Depression
               | Mania
               | Overfed
               | Starvation
               | OCD
               | Diogenes  
               deriving (Show, Read, Eq)

data Psic = Psic 
    { owner     :: String 
    , age       :: Int
    , hunger    :: Int
    , mood      :: Int
    , dirtiness :: Int
    , state     :: StatePsic
    } deriving (Show, Read)

-------------------------------------------------------------------------------

updatePsicMood :: Int -> Psic -> Psic
updatePsicMood newMood psic
    | newMood < 0   = psic { mood = 0,   state = Depression }
    | newMood > 100 = psic { mood = 100, state = Mania }
    | otherwise     = psic { mood = newMood }

updatePsicHunger :: Int -> Psic -> Psic
updatePsicHunger newHunger psic
    | newHunger < 0   = psic { hunger = 0,   state = Overfed }
    | newHunger > 100 = psic { hunger = 100, state = Starvation }
    | otherwise       = psic { hunger = newHunger }

updatePsicDirtiness :: Int -> Psic -> Psic
updatePsicDirtiness newDirtiness psic
    | newDirtiness < 0   = psic { dirtiness = 0,   state = OCD}
    | newDirtiness > 100 = psic { dirtiness = 100, state = Diogenes }
    | otherwise          = psic { dirtiness = newDirtiness }

asciicAlive :: String
asciicAlive  = " /^ ^\\\n"
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

defaultPsic :: Psic
defaultPsic = Psic 
    { owner     = "Igor"
    , age       = 0
    , hunger    = 50
    , mood      = 50 
    , dirtiness = 50
    , state     = Existing
    }

statLevel :: (Psic -> Int) -> Psic -> Integer
statLevel f psic
    | inRange ( 0 +=+ 19) (f psic) = 0
    | inRange (20 +=+ 39) (f psic) = 1
    | inRange (40 +=+ 59) (f psic) = 2
    | inRange (60 +=+ 79) (f psic) = 3
    | inRange (80 +=+ 99) (f psic) = 4
    | otherwise                    = 5

hungerLevel :: Psic -> Integer
hungerLevel = statLevel hunger

moodLevel :: Psic -> Integer
moodLevel = statLevel mood

dirtinessLevel :: Psic -> Integer
dirtinessLevel = statLevel dirtiness

validStates :: [StatePsic]
validStates = [Existing, Sleeping, Hungry, Eating, Playing, Pooping, Cleaning]

inValidState :: Psic -> Bool
inValidState psic = elem (state psic) validStates

psicSays :: Psic -> String
psicSays psic 
    | (state psic) == Existing   = "Hello there, " ++ owner psic ++ "!"
    | (state psic) == Sleeping   = "Good night! Zzzzz..."
    | (state psic) == Hungry     = "Hey, I'm hungty!"
    | (state psic) == Eating     = "Mmmm, noice!"
    | (state psic) == Playing    = "Love you, " ++ owner psic ++ "!"
    | (state psic) == Pooping    = "Aghe, something stinks!"
    | (state psic) == Cleaning   = "Wash washy wash washy wash wash!"
    | (state psic) == Depression = "Asciic hanged himself! Game Over!"
    | (state psic) == Mania      = "Asciic become maniac! Game Over!"
    | (state psic) == Overfed    = "Asciic died from obesity! Game Over!"
    | (state psic) == Starvation = "Asciic died from starvation! Game Over!"
    | (state psic) == OCD        = "Your OCD killed Asciic! Game Over!"
    | (state psic) == Diogenes   = "Asciic bacome Diogenes! Game Over!"
    | otherwise                  = ""

asciic2Draw :: Psic -> String
asciic2Draw psic
    | inValidState psic = asciicAlive
    | otherwise         = asciicDead 

