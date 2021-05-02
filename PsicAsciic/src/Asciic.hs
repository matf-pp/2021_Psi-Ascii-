module Asciic
    ( sayHi
    , rodiPsica
    , imenujPsica
    , igrajSe
    , nahrani
    ) where

import Hrana

data Uzrast = Beba
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
            | Uginuo
            deriving (Show, Read, Eq)

data Psic = Psic 
    { ime :: String
    , vlasnik :: String 
    , uzrast :: Uzrast 
    , zanimanje :: Zanimanje
    , gladnost :: Gladnost
    , raspolozenje :: Raspolozenje
    , stanje :: Stanje 
    } deriving Read

instance Show Psic where 
    show psic = "Ime: " ++ ime psic ++ "   " 
        ++ "Vlasnik: " ++ show (vlasnik psic) ++ "   "
        ++ "Uzrast: " ++ show (uzrast psic) ++ "   " 
        ++ "Zanimanje: " ++ show (zanimanje psic) ++ "\n\n"
        ++ "\t\t\t /^ ^\\\n"
        ++ "\t\t\t/ 0 0 \\\n"
        ++ "\t\t\tV\\ Y /V\n"
        ++ "\t\t\t / - \\\n"
        ++ "\t\t\t/    |\n"
        ++ "\t\t\tV__) ||\n\n"
        ++ "Stanje: " ++ show (stanje psic) ++ "   "
        ++ "Raspolozenje: " ++ show (raspolozenje psic) ++ "   " 
        ++ "Gladnost: " ++ show (gladnost psic) ++ "\n\n"

rodiPsica :: Psic
rodiPsica = Psic 
    { ime = "Asciic"
    , vlasnik = "Igor"
    , uzrast = Beba
    , zanimanje = NemaZanimanje
    , gladnost = Gladan
    , raspolozenje = Uzbudjen
    , stanje = Postoji
    }

imenujPsica :: Psic -> String -> Psic
imenujPsica psic novoIme = psic { ime = novoIme }

promeniVlasnika :: Psic -> String -> Psic
promeniVlasnika psic noviVlasnik = psic { vlasnik = noviVlasnik }

nahrani :: Psic -> Hrana -> Psic
nahrani psic Hrana.Hleb
    | gladnost psic == Umire    = psic { gladnost = Gladan }
    | gladnost psic == Gladan   = psic { gladnost = Sit }
    | gladnost psic == Sit      = psic { gladnost = Prejeden }
    | gladnost psic == Prejeden = psic { stanje = Uginuo } 

nahrani psic Hrana.Kost
    | gladnost psic == Umire    = psic { gladnost = Gladan }
    | gladnost psic == Gladan   = psic { gladnost = Sit }
    | gladnost psic == Sit      = psic { gladnost = Prejeden }
    | gladnost psic == Prejeden = psic { stanje = Uginuo }

nahrani psic Hrana.Meso
    | gladnost psic == Umire    = psic { gladnost = Sit }
    | gladnost psic == Gladan   = psic { gladnost = Prejeden }
    | gladnost psic == Sit      = psic { gladnost = Prejeden }
    | gladnost psic == Prejeden = psic { stanje = Uginuo }

igrajSe :: Psic -> Psic
igrajSe psic 
    | gladnost psic == Umire    = psic { stanje = Uginuo }
    | gladnost psic == Gladan   = psic { stanje = IgraSe, gladnost = Umire}
    | gladnost psic == Sit      = psic { stanje = IgraSe, gladnost = Gladan}
    | gladnost psic == Prejeden = psic { stanje = IgraSe, gladnost = Sit}

sayHi :: IO ()
sayHi = putStrLn "Hi I'm Psic Asciic!"
