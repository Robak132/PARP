module Combat where
    import qualified Data.List as List
    import qualified System.Random.Stateful as Random
    import State (State(comment, enemies, you, items_at, randomGen))
    import Character (Character(location, name))
    
    attack :: String -> State -> State
    attack enemyName state = case List.find (\x -> enemyName == name x && location (you state) == location x) (enemies state) of
        Nothing -> state { comment = ["There is no " ++ enemyName ++ " in this room"] }
        Just enemy -> state { comment = ["You are fighting with " ++ enemyName ++ ". Luckly combat isn't implemented so you killed him without problems."], enemies = List.delete enemy (enemies state)}

    rollDice :: State -> State
    rollDice state = do
        let (x, newRandomGen) = randInt (1, 20) state
        state {randomGen = newRandomGen}

    randInt :: (Int, Int) -> State -> (Int, Random.StdGen)
    randInt a state = Random.randomR a (randomGen state)