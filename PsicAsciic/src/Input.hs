{-# LANGUAGE LambdaCase #-}
module Input 
    ( GameEvents(..)
    , nextEvent 
    ) where

import Data.Maybe (fromJust)
import UI.NCurses

data GameEvents = Feed
                | Play
                | Clean
                | Idle
                | Quit
                deriving Show


nextEvent :: Curses GameEvents
nextEvent = let maybeEvent = fmap (event2GameEvent . fromJust) . (`getEvent` Nothing)
            in defaultWindow >>= maybeEvent >>= \case
                                                    Just ev -> pure ev
                                                    Nothing -> nextEvent
            where event2GameEvent (EventCharacter 'f') = Just Feed
                  event2GameEvent (EventCharacter 'p') = Just Play
                  event2GameEvent (EventCharacter 'c') = Just Clean
                  event2GameEvent (EventCharacter '.') = Just Idle
                  event2GameEvent _                    = Nothing
