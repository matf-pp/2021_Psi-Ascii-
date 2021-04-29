module Hrana
    (
      Hrana (Hleb, Meso, Kost)
    ) where

data Hrana = Hleb
           | Meso
           | Kost
           deriving (Show, Eq, Ord, Read)