-- Doges&Cateons, by Jakub Robaczewski, Paweł Muller, Marianna Gromadzka
import Prelude hiding (take, drop)
import Data.List (isPrefixOf)
import Items ( take, drop, inventory )
import State ( State(State, comment, holding), printState )
import Room ( Direction(W, E, S, N), go, look, search)
import Utilites ( printLines, readCommand, split )

introductionText :: [[Char]]
introductionText = [
    "Placeholder introduction"
    ]

instructionsText :: [[Char]]
instructionsText = [
    "Available commands are:",
    "n.  s.  e.  w.     -- to go in that direction.",
    "flee Direction     -- to flee from combat.",
    "take Object.       -- to pick up an object.",
    "drop Object        -- to put down an object.",
    "look               -- to look around you again.",
    "search             -- to search the room.",
    "inventory, i       -- to check inventory.",
    "attack Enemy       -- to attack the enemy.",
    "instructions, help -- to see this message again.",
    "quit               -- to end the game and quit.",
    ""
    ]

beginningState :: State
beginningState = State
-- Comment
    [
        "First"
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

help :: State -> State
help state = state { comment = instructionsText }

gameLoop :: State -> IO State
gameLoop state = do
    printState state
    let modifiedState = state
    cmd <- readCommand
    if cmd /= "quit" then
        gameLoop (case cmd of
            -- "flee Direction"
            -- "attack Enemy"
            "inventory" -> inventory modifiedState
            "i" -> inventory modifiedState

            "instructions" -> help modifiedState
            "help" -> help modifiedState

            "look" -> look modifiedState
            "search" -> search modifiedState

            "n" -> go N modifiedState
            "s" -> go S modifiedState
            "e" -> go E modifiedState
            "w" -> go W modifiedState
            _ -> if "take" `isPrefixOf` cmd then take (split cmd!!1) modifiedState
                else if "drop" `isPrefixOf` cmd then drop (split cmd!!1) modifiedState
                else modifiedState { comment = ["Wait, that illegal. You used wrong command."]}
        )
    else do return modifiedState

main :: IO State
main = do
    -- printLines introductionText
    printLines instructionsText
    gameLoop(look beginningState)
