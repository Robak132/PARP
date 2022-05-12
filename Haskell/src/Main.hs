-- Doges&Cateons, by Jakub Robaczewski, Paweł Muller, Marianna Gromadzka
import qualified System.Random as Random
import Prelude hiding (take, drop)
import Data.List (isPrefixOf)
import ItemActions ( take, drop, inventory, search)
import State ( State(comment, holding, randomGen, you), printState, initialState)
import Room (go, look, flee)
import Utilites ( printLines, split, splitCommand )
import Combat ( attack )
import Character (alive)

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
    if alive (you state) then do
        cmd <- getLine
        if cmd /= "quit" then
            gameLoop (case cmd of
                "inventory" -> inventory state
                "i" -> inventory state

                "instructions" -> help state
                "help" -> help state

                "look" -> look state
                "search" -> search state

                "flee n" -> flee "N" state
                "flee s" -> flee "S" state
                "flee e" -> flee "E" state
                "flee w" -> flee "W" state

                "n" -> go "N" state
                "s" -> go "S" state
                "e" -> go "E" state
                "w" -> go "W" state
                _ | "take" `isPrefixOf` cmd -> take (splitCommand cmd) state
                  | "drop" `isPrefixOf` cmd -> drop (splitCommand cmd) state
                  | "attack" `isPrefixOf` cmd -> attack (splitCommand cmd) state
                  | otherwise -> state{comment = ["Wait, that illegal. You used wrong command."]}
            )
        else return state
    else do 
        printLines ["The game ends here. Please restart the game."]
        return state

main :: IO State
main = do
    randomGen <- Random.initStdGen
    printLines instructionsText
    gameLoop(look initialState {randomGen=randomGen})
