module Combat where
    import qualified Data.List as List
    import State ( State (comment, enemy_at, i_am_at, items_at) )
    import Room (look)
    
    attack :: String -> State -> State
    attack enemy state = do
        case List.find (\x -> enemy == fst x && i_am_at state == snd x) (enemy_at state) of
            Nothing -> state { comment = ["There is no " ++ enemy ++ " in this room"] }
            Just enemyTuple -> state { comment = ["You are fighting with " ++ enemy ++ ". Luckly combat isn't implemented so you killed him without problems."], enemy_at = List.delete enemyTuple (enemy_at state)}