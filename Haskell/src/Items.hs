module Items where
    import State ( State(comment, holding, items_at, you) )
    import Character (Character(location))
    import qualified Data.List as List
    import Combat (die)
    
    take :: String -> State -> State
    take itemName state = case List.find (\x -> itemName == fst x && location (you state) == snd x) (items_at state) of
        Nothing -> state { comment = ["There is no " ++ itemName ++ " in this room"] }
        Just ("Floating Crystal", _) -> die (you state) state { comment = ["You tried to grab the crystal, but floor collapsed under you. You fall into spikes."] }
        Just itemTuple -> state { holding = itemName:holding state, items_at = List.delete itemTuple (items_at state), comment = ["You took " ++ itemName ++ " from the ground"]  }

    drop :: String -> State -> State
    drop itemName state = if itemName `elem` holding state then
            state { holding = List.delete itemName (holding state), items_at = (itemName, location (you state)):items_at state, comment = ["You drop " ++ itemName ++ " to ground"]  }
        else
            state { comment = ["You don't have " ++ itemName ++ " in your inventory"] }

    inventory :: State -> State
    inventory state = state {comment = "Your inventory:" : holding state}