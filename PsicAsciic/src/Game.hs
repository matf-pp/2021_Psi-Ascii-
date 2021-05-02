module Game (runGame) where

import Control.Monad ( when )
import Input
import Asciic
import UI.NCurses

data Game = Game 
    { running :: Bool 
    } deriving Show

initialGame :: Game
initialGame = Game True

update :: EventGame -> Game -> Game
update Quit game = game { running = False }
update _    game = game -- Implement other actions

renderGame :: Game -> Curses ()
renderGame game = do
    w <- defaultWindow
    updateWindow w $ do
        clear
        moveCursor 2 10
        drawString "Psic Asciic"
        moveCursor 4 10
        drawString "(To quit press q)"
        moveCursor 0 0
    render

loop :: Game -> Curses ()
loop oldGame = do
    renderGame oldGame
    event <- nextEvent
    let newGame = update event oldGame
    when (running newGame) $
        loop newGame

runGame :: IO ()
runGame = runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    loop initialGame
