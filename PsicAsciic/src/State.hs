module State where

data StatePsic = Alive
               | Sleepy
               | Bored
               | Showering
               | Eating
               | Playing
               | Pooping
               | Dead
               deriving (Show, Read, Eq)