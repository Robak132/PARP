-- Doges&Cateons, by Jakub Robaczewski, Paweł Muller, Marianna Gromadzka
import Prelude hiding (take, drop)
import Data.List (isPrefixOf)
import Items ( take, drop, inventory )
import State ( State(State, comment, holding), printState, initialState)
import Room (go, look, search)
import Utilites ( printLines, readCommand, split, splitCommand )
import System.Process ( system )
import Combat ( attack )

introductionText :: [String]
introductionText = [
    "Placeholder introduction"
    ]

instructionsText :: [String]
instructionsText = [
    "Available commands are:",
    "n,  s,  e,  w,                 -- to go in that direction.",
    "flee n, flee s, flee e, flee w -- to flee from combat.",
    "take Object.                   -- to pick up an object.",
    "drop Object                    -- to put down an object.",
    "look                           -- to look around you again.",
    "search                         -- to search the room.",
    "inventory, i                   -- to check inventory.",
    "attack Enemy                   -- to attack the enemy.",
    "instructions, help             -- to see this message again.",
    "quit                           -- to end the game and quit.",
    ""
    ]

help :: State -> State
help state = state { comment = instructionsText }

gameLoop :: State -> IO State
gameLoop state = do
    printState state
    cmd <- readCommand
    system "clear"
    if cmd /= "quit" then
        gameLoop (case cmd of
            -- "flee Direction"
            -- "attack Enemy"
            "inventory" -> inventory state
            "i" -> inventory state

            "instructions" -> help state
            "help" -> help state

            "look" -> look state
            "search" -> search state

            "n" -> go "N" state
            "s" -> go "S" state
            "e" -> go "E" state
            "w" -> go "W" state
            _ -> if "take" `isPrefixOf` cmd then take (splitCommand cmd) state
                else if "drop" `isPrefixOf` cmd then drop (splitCommand cmd) state
                else if "attack" `isPrefixOf` cmd then attack (splitCommand cmd) state
                else state { comment = ["Wait, that illegal. You used wrong command."]}
        )
    else do return state

main :: IO State
main = do
    -- printLines introductionText
    printLines instructionsText
    gameLoop(look initialState)
