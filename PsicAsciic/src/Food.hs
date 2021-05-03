module Food
    ( Food(..)
    , moodValue
    , hungerValue
    , dirtinessValue
    ) where


data Food = Water
          | Bone
          | Meat
          deriving (Show, Read, Eq, Ord)

moodValue :: Food -> Int
moodValue Water = 1
moodValue Bone  = 10
moodValue Meat  = 5

hungerValue :: Food -> Int
hungerValue Water = -1
hungerValue Bone  = -5
hungerValue Meat  = -10

dirtinessValue :: Food -> Int
dirtinessValue Water = 2
dirtinessValue Bone  = 5
dirtinessValue Meat  = 5

