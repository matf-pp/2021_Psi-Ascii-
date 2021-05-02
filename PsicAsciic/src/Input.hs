{-# LANGUAGE LambdaCase #-}

module Input 
    ( EventGame(..)
    , nextEvent 
    ) where

import Data.Maybe (fromJust)
import UI.NCurses

data EventGame = Feed
               | Play
               | Clean
               | Idle
               | Quit
               deriving Show


nextEvent :: Curses EventGame
nextEvent = let maybeEvent = fmap (event2EventGame . fromJust) . (`getEvent` Nothing)
            in defaultWindow >>= maybeEvent >>= \case
                                                    Just ev -> pure ev
                                                    Nothing -> nextEvent
            where event2EventGame (EventCharacter 'f') = Just Feed
                  event2EventGame (EventCharacter 'p') = Just Play
                  event2EventGame (EventCharacter 'c') = Just Clean
                  event2EventGame (EventCharacter '.') = Just Idle
                  event2EventGame (EventCharacter 'q') = Just Quit
                  event2EventGame _                    = Nothing
