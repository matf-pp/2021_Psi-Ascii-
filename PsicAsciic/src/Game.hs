module Game (runGame) where

import Control.Monad ( when )
import Input
import Asciic
import UI.NCurses

data Game = Game 
    { running :: Bool 
    , psic    :: Psic
    } deriving Show

initialGame :: Game
initialGame = Game True defaultPsic

update :: EventGame -> Game -> Game
update Quit game = game { running = False }
update _    game = game -- Implement other actions

drawGame :: Game -> Update ()
drawGame (Game _ psicState) = do
    hBar
    moveCursor 1 19
    drawString "Psic Asciic"
    moveCursor 2 0
    hBar
    moveCursor 3 2
    drawString "Mood:"
    drawLineH (Just glyphBlock) (moodLevel psicState)
    moveCursor 3 15
    drawString "Hunger:"
    drawLineH (Just glyphBlock) (hungerLevel psicState)
    moveCursor 3 30
    drawString "Dirtiness:"
    drawLineH (Just glyphBlock) (dirtinessLevel psicState)
    moveCursor 5 0
    drawString asciic
    moveCursor 12 0
    hBar
    moveCursor 13 30
    drawString "(Press q to quit)"
    moveCursor 0 0
    where hBar = drawLineH Nothing 50

renderGame :: Game -> Curses ()
renderGame game = do
    w <- defaultWindow
    updateWindow w $ do
        clear
        drawGame game
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
