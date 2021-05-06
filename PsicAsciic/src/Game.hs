module Game (runGame) where

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

type IndexedGlyph = (V2 Integer, Glyph)
type IndexedGlyphs = [IndexedGlyph]

type Inventory = Map.Map Food Int

data Game = Game 
    { running   :: Bool 
    , stdGen    :: StdGen
    , psic      :: Psic
    , inventory :: Inventory
    } deriving Show

-------------------------------------------------------------------------------

initialInventory :: Inventory
initialInventory = Map.fromList [(Water, 10), (Bone, 5), (Meat, 3)]

initialGame :: StdGen -> Game
initialGame gen = Game True gen defaultPsic initialInventory 

update :: EventGame -> Game -> Game
update Quit game = game { running = False }
update (Feed food) game@(Game _ _ psic inventory) = 
    let newMood         = mood psic + (moodValue food)
        newHunger       = hunger psic + (hungerValue food)
        newDirtiness    = dirtiness psic + (dirtinessValue food)
        foodInInventory = inventoryLookup food inventory
        newPsic      
            | foodInInventory > 0 = updatePsicMood newMood
                                  $ updatePsicHunger newHunger
                                  $ updatePsicDirtiness newDirtiness
                                  $ psic
            | otherwise = psic
        inventoryNew
                    | foodInInventory > 0 = Map.adjust pred food $ inventory
                    | otherwise           = Map.adjust (*0) food $ inventory
    in game { psic = newPsic { age = 1 + (age psic) }
            , inventory = inventoryNew
            }
update Idle game@(Game _ oldGen psic inventory) = 
    let (randMood, newGen)          = randomR (-5, 5) oldGen
        (randHunger, newGen')       = randomR ( 0, 5) newGen
        (randDirtiness, newGen'')   = randomR ( 0, 5) newGen'
        (randFood, newGen''')       = randomR ( 0, 2) newGen'' :: (Int, StdGen)
        newMood                     = mood psic + randMood
        newHunger                   = hunger psic + randHunger
        newDirtiness                = dirtiness psic + randDirtiness
    in game { stdGen    = newGen''
            , psic      = updatePsicMood newMood
                        $ updatePsicHunger newHunger
                        $ updatePsicDirtiness newDirtiness
                        $ psic { age = 1 + (age psic) }
            , inventory = Map.adjust (+1) (num2Food randFood) inventory
            }
    where num2Food 0 = Water
          num2Food 1 = Bone
          num2Food 2 = Meat
update Play game@(Game _ oldGen psic _) =
    let (randMood, newGen)          = randomR (0, 20) oldGen 
        (randHunger, newGen')       = randomR (0,  5) newGen
        (randDirtiness, newGen'')   = randomR (0,  5) newGen'
        newMood                     = mood psic + randMood
        newHunger                   = hunger psic + randHunger
        newDirtiness                = dirtiness psic + randDirtiness
    in game { stdGen    = newGen''
            , psic      = updatePsicMood newMood
                        $ updatePsicHunger newHunger
                        $ updatePsicDirtiness newDirtiness
                        $ psic { age = 1 + (age psic), state = Playing }
            }
update Clean game@(Game _ oldGen psic _) =
    let (randMood, newGen)          = randomR (-10,  0) oldGen 
        (randDirtiness, newGen')    = randomR ( 30, 80) newGen
        newMood                     = mood psic + randMood
        newDirtiness                = dirtiness psic - randDirtiness
    in game { stdGen    = newGen'
            , psic      = updatePsicMood newMood
                        $ updatePsicDirtiness newDirtiness
                        $ psic { age = 1 + (age psic), state = Cleaning }
            }
update Poop game@(Game _ oldGen psic _) =
    let (randMood, newGen)          = randomR ( 0, 30) oldGen 
        (randHunger, newGen')       = randomR ( 0,  5) newGen
        (randDirtiness, newGen'')   = randomR (10, 70) newGen'
        newMood                     = mood psic - randMood
        newHunger                   = hunger psic + randHunger
        newDirtiness                = dirtiness psic + randDirtiness
    in game { stdGen    = newGen''
            , psic      = updatePsicMood newMood
                        $ updatePsicHunger newHunger
                        $ updatePsicDirtiness newDirtiness
                        $ psic { age = 1 + (age psic), state = Pooping }
            }
update Hunger game@(Game _ oldGen psic _) =
    let (randMood, newGen)          = randomR ( 0, 10) oldGen 
        (randHunger, newGen')       = randomR (10, 30) newGen
        (randDirtiness, newGen'')   = randomR ( 0,  5) newGen'
        newMood                     = mood psic - randMood
        newHunger                   = hunger psic + randHunger
        newDirtiness                = dirtiness psic + randDirtiness
    in game { stdGen    = newGen''
            , psic      = updatePsicMood newMood
                        $ updatePsicHunger newHunger
                        $ updatePsicDirtiness newDirtiness
                        $ psic { age = 1 + (age psic), state = Hungry }
            }
update Sleep game@(Game _ oldGen psic _) =
    let (randMood, newGen)          = randomR (5, 10) oldGen 
        (randHunger, newGen')       = randomR (0,  5) newGen
        (randDirtiness, newGen'')   = randomR (0,  5) newGen'
        newMood                     = mood psic + randMood
        newHunger                   = hunger psic + randHunger
        newDirtiness                = dirtiness psic + randDirtiness
    in game { stdGen    = newGen''
            , psic      = updatePsicMood newMood
                        $ updatePsicHunger newHunger
                        $ updatePsicDirtiness newDirtiness
                        $ psic { age = 1 + (age psic), state = Sleeping }
            }

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
    let name = "Psic Asciic"
        len  = fromIntegral $ length name
    moveCursor 1 (25 - (floor $ len / 2))
    drawString name
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
    drawAsciic (asciic2Draw psicState) 5 22
    drawSaying 11 $ psicSays psicState
    when (not (inValidState psicState)) $ do
        drawSaying 12 $ "Asciic was " ++ show (age psicState) ++ " years old!"
    moveCursor 13 3
    drawString "Water:"
    drawString $ "x" ++ (show $ inventoryLookup Water inventoryState)
    moveCursor 13 20
    drawString "Bone:"
    drawString $ "x" ++ (show $ inventoryLookup Bone inventoryState)
    moveCursor 13 35
    drawString "Meat:"
    drawString $ "x" ++ (show $ inventoryLookup Meat inventoryState)
    where drawSaying row saying = do
            let len = fromIntegral $ length saying
            moveCursor row (25 - (floor $ len / 2))
            drawString saying
 
footer :: Update ()
footer = do
    moveCursor 15 2
    drawString "play(p) feed(w/b/m) idle(.)"
    moveCursor 15 35
    drawString "quit(q)"

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
    let newGame = if inValidState (psic oldGame) || event == Quit
                    then update randEvent 
                       $ update event oldGame { stdGen = gen }
                    else oldGame
    when (running newGame) $ do
        loop newGame

runGame :: IO ()
runGame = runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    gen <- liftIO $ getStdGen
    loop $ initialGame gen

