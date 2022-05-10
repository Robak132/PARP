module Combat where
    import qualified Data.List as List
    import qualified System.Random.Stateful as Random
    import State (State(comment, enemies, you, items_at, randomGen))
    import Character (Character(location, name, defense, strength, damage, health), alive)
    
    attack :: String -> State -> State
    attack enemyName state = case List.find (\x -> enemyName == name x && location (you state) == location x) (enemies state) of
        Nothing -> state { comment = ["There is no " ++ enemyName ++ " in this room"] }
        Just enemy -> do
            let (modifiedEnemy, modfifiedState) = hit (you state) enemy state {comment = []}
            let (_, modfifiedState2) = hit modifiedEnemy (you state) modfifiedState
            modfifiedState2

    hit :: Character -> Character -> State -> (Character, State)
    hit attacker defender state = if alive attacker then do
            let (attackRoll, modifiedState) = randInt (1, 20) state
            if attackRoll + strength attacker >= defense defender then do
                let (damageRoll, modifiedState2) = randInt (1, damage attacker) modifiedState
                harm defender damageRoll modifiedState2 {comment = comment state ++ [name attacker ++ " attacks " ++ name defender ++ " (" ++ show attackRoll ++ ">=" ++ show (defense defender) ++ ") [" ++ show damageRoll ++ " dmg]."]}
            else
                (defender, modifiedState {comment = comment state ++ [name attacker ++ " failed to attack " ++ name defender ++ " (" ++ show attackRoll ++ ">=" ++ show (defense defender) ++ ")."]})
        else
            (defender, state)
        
    harm :: Character -> Int -> State -> (Character, State)
    harm character dmg state = do
        let newHealth = health character - dmg
        let harmMsg = if newHealth > 0 then ["Remaining HP: " ++ show newHealth] else [name character ++ " is dead"]
        let modifiedCharacter = (character {health = newHealth})

        if name character == "Doge" then
            (modifiedCharacter, state {comment = comment state ++ harmMsg, you = modifiedCharacter})
        else do
            let modifiedState = state {comment = comment state ++ harmMsg, enemies = List.delete character (enemies state)}
            (modifiedCharacter, modifiedState {enemies = modifiedCharacter : enemies state})

    randInt :: (Int, Int) -> State -> (Int, State)
    randInt a state = do 
        let (rand, gen) = Random.randomR a (randomGen state)
        (rand, state {randomGen=gen})
