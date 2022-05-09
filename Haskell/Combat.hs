module Combat where
    import qualified Data.List as List
    import State (State(comment, enemies, you, items_at))
    import Character (Character(location, name))
    
    attack :: String -> State -> State
    attack enemyName state = do
        case List.find (\x -> enemyName == name x && location (you state) == location x) (enemies state) of
            Nothing -> state { comment = ["There is no " ++ enemyName ++ " in this room"] }
            Just enemy -> state { comment = ["You are fighting with " ++ enemyName ++ ". Luckly combat isn't implemented so you killed him without problems."], enemies = List.delete enemy (enemies state)}
