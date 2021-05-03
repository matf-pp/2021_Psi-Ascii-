module Asciic
    ( StatePsic(..)
    , Psic(..)
    , defaultPsic 
    , asciic
    , moodLevel
    , hungerLevel
    , dirtinessLevel
    ) where

import Data.Range
import UI.NCurses
import Linear
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

--asciic2Glyphs :: String -> [(V2 Int Int, Glyph)]
--asciic2Glyphs str = 

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
feed food oldPsic = oldPsic -- TODO: Implement for all types of food!

--imenujPsica :: Psic -> String -> Psic
--imenujPsica psic novoIme = psic { ime = novoIme }

--promeniVlasnika :: Psic -> String -> Psic
--promeniVlasnika psic noviVlasnik = psic { vlasnik = noviVlasnik }

--nahrani :: Psic -> Hrana -> Psic
--nahrani psic Hrana.Hleb
    -- | gladnost psic == Umire    = psic { gladnost = Gladan }
    -- | gladnost psic == Gladan   = psic { gladnost = Sit }
    -- | gladnost psic == Sit      = psic { gladnost = Prejeden }
    -- | gladnost psic == Prejeden = psic { stanje = Uginuo } 

--nahrani psic Hrana.Kost
    -- | gladnost psic == Umire    = psic { gladnost = Gladan }
    -- | gladnost psic == Gladan   = psic { gladnost = Sit }
    -- | gladnost psic == Sit      = psic { gladnost = Prejeden }
    -- | gladnost psic == Prejeden = psic { stanje = Uginuo }

--nahrani psic Hrana.Meso
    -- | gladnost psic == Umire    = psic { gladnost = Sit }
    -- | gladnost psic == Gladan   = psic { gladnost = Prejeden }
    -- | gladnost psic == Sit      = psic { gladnost = Prejeden }
    -- | gladnost psic == Prejeden = psic { stanje = Uginuo }

--igrajSe :: Psic -> Psic
--igrajSe psic 
    -- | gladnost psic == Umire    = psic { stanje = Uginuo }
    -- | gladnost psic == Gladan   = psic { stanje = IgraSe, gladnost = Umire}
    -- | gladnost psic == Sit      = psic { stanje = IgraSe, gladnost = Gladan}
    -- | gladnost psic == Prejeden = psic { stanje = IgraSe, gladnost = Sit}

