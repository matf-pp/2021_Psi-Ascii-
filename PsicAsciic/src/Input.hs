{-# LANGUAGE LambdaCase #-}

module Input 
    ( EventGame(..)
    , nextEvent 
    , randomEvent
    ) where

-------------------------------------------------------------------------------

import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import UI.NCurses
import System.Random
import Food

-------------------------------------------------------------------------------

data EventGame = Feed Food
               | Play
               | Clean
               | Poop
               | Hunger
               | Sleep
               | Idle
               | Quit
               deriving (Eq ,Show)

-------------------------------------------------------------------------------

nextEvent :: Curses EventGame
nextEvent = let maybeEvent = fmap (event2EventGame . fromJust) . (`getEvent` Nothing)
            in defaultWindow >>= maybeEvent >>= \case
                                                    Just ev -> pure ev
                                                    Nothing -> nextEvent
            where event2EventGame (EventCharacter 'w') = Just $ Feed Water
                  event2EventGame (EventCharacter 'b') = Just $ Feed Bone
                  event2EventGame (EventCharacter 'm') = Just $ Feed Meat
                  event2EventGame (EventCharacter 'p') = Just $ Play
                  event2EventGame (EventCharacter 'c') = Just $ Clean
                  event2EventGame (EventCharacter '.') = Just $ Idle
                  event2EventGame (EventCharacter 'q') = Just $ Quit
                  event2EventGame _                    = Nothing

randomEvent :: StdGen -> Curses EventGame
randomEvent gen = do
    let (randNum, _) = randomR (1,4) gen :: (Int, StdGen)
    pure $ rand2EventGame randNum
    where rand2EventGame 1 = Poop
          rand2EventGame 2 = Hunger
          rand2EventGame 3 = Sleep
          rand2EventGame 4 = Idle

