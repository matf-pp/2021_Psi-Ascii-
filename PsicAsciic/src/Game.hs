module Game (runGame, drawAsciic) where

import qualified Data.Map as Map
import Data.String
import Control.Monad ( when )
import Control.Monad.IO.Class ( liftIO )
import System.Random
import Input
import Linear
import Asciic
import Food
import UI.NCurses

type IndexedGlyph = (V2 Integer, Glyph)
type IndexedGlyphs = [IndexedGlyph]

type Inventory = Map.Map Food Int

data Game = Game 
    { running   :: Bool 
    , stdGen    :: StdGen
    , psic      :: Psic
    , inventory :: Inventory
    } deriving Show

initialInventory :: Inventory
initialInventory = Map.fromList [(Water, 10), (Bone, 5), (Meat, 3)]

initialGame :: StdGen -> Game
initialGame gen = Game True gen defaultPsic initialInventory 

update :: EventGame -> Game -> Game
update Quit game = game { running = False }
update Idle game@(Game _ _ psic inventory) = 
    let newMood     = mood psic - 2
        newHunger   = hunger psic - 5
    in game { psic      = psic { mood = newMood
                               , hunger = newHunger } 
            , inventory = Map.adjust (+2) Water 
                        $ Map.adjust (+1) Bone 
                        $ Map.adjust (+1) Meat
                        $ inventory
            }
update _    game = game -- Implement other actions

inventoryLookup :: Food -> Inventory -> Integer
inventoryLookup food inventory =
    let maybeCount = Map.lookup food inventory
    in case maybeCount of
        Nothing      -> 0
        (Just count) -> toInteger count

asciic2IndexedGlyphs :: String -> IndexedGlyphs
asciic2IndexedGlyphs str = 
    let indexedRows = zip [0..] (lines str)
        indexedCols = foldr (\(row, line) acc -> (row, zip [0..] line):acc) 
                            [] indexedRows
        indexedRowsCols = concat 
                        $ map (\(row, lst) -> map (\(col, chr) 
                                           -> (row, col, chr)) lst) indexedCols
    in map (\(row, col, chr) -> (V2 row col, Glyph chr [])) indexedRowsCols

drawAsciic :: String -> Integer -> Integer -> Update ()
drawAsciic asciic xOffset yOffset =
    mapM_ (\(V2 row col, glyph) -> do
                moveCursor (xOffset + row) (yOffset + col) 
                drawGlyph glyph) $ indexedAsciic
    where indexedAsciic = asciic2IndexedGlyphs asciic

header :: Update ()
header = do
    moveCursor 1 19
    drawString "Psic Asciic"
    moveCursor 2 0

body :: Psic -> Inventory -> Update ()
body psicState inventoryState = do
    moveCursor 3 2
    drawString "Mood:"
    drawLineH (Just glyphBlock) (moodLevel psicState)
    moveCursor 3 15
    drawString "Hunger:"
    drawLineH (Just glyphBlock) (hungerLevel psicState)
    moveCursor 3 30
    drawString "Dirtiness:"
    drawLineH (Just glyphBlock) (dirtinessLevel psicState)
    drawAsciic asciic 5 20
    moveCursor 13 3
    drawString "Water:"
    drawString $ "x" ++ (show $ inventoryLookup Water inventoryState)
    moveCursor 13 20
    drawString "Bone:"
    drawString $ "x" ++ (show $ inventoryLookup Bone inventoryState)
    moveCursor 13 35
    drawString "Meat:"
    drawString $ "x" ++ (show $ inventoryLookup Meat inventoryState)
 
footer :: Update ()
footer = do
    moveCursor 15 30
    drawString "(Press q to quit)"

drawGame :: Game -> Update ()
drawGame (Game _ _ psicState inventoryState) = do
    moveCursor 0 0
    hBar
    header
    hBar
    moveCursor 3 0
    body psicState inventoryState
    moveCursor 14 0
    hBar
    footer
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
    gen       <- liftIO $ newStdGen
    event     <- nextEvent
    randEvent <- randomEvent gen
    let newGame = update randEvent 
                $ update event oldGame 
                    { stdGen = gen }
    when (running newGame) $ do
        loop newGame

runGame :: IO ()
runGame = runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    gen <- liftIO $ getStdGen
    loop $ initialGame gen
