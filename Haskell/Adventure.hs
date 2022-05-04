-- Doges&Cateons, by Jakub Robaczewski, Paweł Muller, Marianna Gromadzka

import Prelude
import State
import Room
import Utilites

introductionText = [
    "Placeholder introduction"
    ]

instructionsText = [
    "Available commands are:",
    "n.  s.  e.  w.    -- to go in that direction.",
    "flee Direction    -- to flee from combat.",
    "take Object.      -- to pick up an object.",
    "drop Object       -- to put down an object.",
    "look              -- to look around you again.",
    "search            -- to search the room.",
    "inventory, i      -- to check inventory.",
    "attack Enemy      -- to attack the enemy.",
    "instructions      -- to see this message again.",
    "quit              -- to end the game and quit."
    ]

beginningState = State
-- Comment
    [

    ]
-- My location
    "entrance"
-- Items
    [
        ("key", "attendant_room"),
        ("torch", "acolyte_chamber_1")
    ]
-- Enemies
    [
        ("skele_cat_1", "attendant_room"),
        ("catmint_guardian", "guardian"),
        ("fallen_cat", "sarcophagus")
    ]
-- Health
    [
        ("player", 6),
        ("skele_cat_1", 3),
        ("catmint_guardian", 13),
        ("fallen_cat", 6)
    ]
-- Doors
    [
        ("acolyte_chamber_2", "serket_chamber", "normal_door", False),
        ("acolyte_chamber_2", "acolyte_chamber_1", "moonlight_door", False)
    ]
-- Holding
    [

    ]

printState :: State -> IO ()
printState state = do
    putStr (unlines (comment state))

help state = state { comment = instructionsText }

gameLoop :: State -> IO State
gameLoop state = do
    printState state
    let modifiedState = state {comment = []}
    printLines ["\nWaiting for command:"]
    cmd <- readCommand
    if not (cmd == "quit") then
        gameLoop (case cmd of
            -- "flee Direction"
            -- "take Object"
            -- "drop Object"
            -- "attack Enemy"
            -- "inventory" -> help modifiedState
            -- "search" -> help modifiedState
            -- "look" -> help modifiedState
            "instructions" -> help modifiedState
            "n" -> go N modifiedState
            "s" -> go S modifiedState
            "e" -> go E modifiedState
            "w" -> go W modifiedState
        )
    else do return(modifiedState)

main :: IO State
main = do
    printLines introductionText
    printLines instructionsText
    gameLoop beginningState
