module Asciic
    ( sayHi
    , rodiPsica
    , imenujPsica
    ) where

data Starost = Beba
             | Dete
             | Tinejdzer
             | Dzukac
             deriving (Show, Read, Eq, Ord)

data Zanimanje = Slikar
               | Muzicar
               | NemaZanimanje
               deriving (Show, Read, Eq)

data Gladnost = Umire
              | Gladan
              | Sit
              | Prejeden
              deriving (Show, Read, Eq, Ord)

data Raspolozenje = Ljut
                  | Smoren
                  | Srecan
                  | Uzbudjen
                  deriving (Show, Read, Eq, Ord)

data Stanje = Spava
            | IgraSe
            | Radi
            | Postoji
            deriving (Show, Read, Eq)

data Psic = Psic 
    { ime :: String
    , vlasnik :: String 
    , starost :: Starost 
    , zanimanje :: Zanimanje
    , gladnost :: Gladnost
    , raspolozenje :: Raspolozenje
    , stanje :: Stanje 
    } deriving (Show, Read)

rodiPsica :: Psic
rodiPsica = Psic 
    { ime = "Asciic"
    , vlasnik = "Igor"
    , starost = Beba
    , zanimanje = NemaZanimanje
    , gladnost = Gladan
    , raspolozenje = Uzbudjen
    , stanje = Postoji
    }

imenujPsica :: Psic -> String -> Psic
imenujPsica psic novoIme = psic { ime = novoIme }

sayHi :: IO ()
sayHi = putStrLn "Hi I'm Psic Asciic!"
