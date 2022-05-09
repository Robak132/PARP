module Combat where
    import qualified Data.List as List
    import State (State(comment, enemies, i_am_at, items_at))
    import Room (look)
    import Character (Character(location, name))
    
    attack :: String -> State -> State
    attack enemyName state = do
        case List.find (\x -> enemyName == Character.name x && i_am_at state == Character.location x) (enemies state) of
            Nothing -> state { comment = ["There is no " ++ enemyName ++ " in this room"] }
            Just enemy -> state { comment = ["You are fighting with " ++ enemyName ++ ". Luckly combat isn't implemented so you killed him without problems."], enemies = List.delete enemy (enemies state)}
