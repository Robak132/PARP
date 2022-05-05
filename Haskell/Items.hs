module Items where
    import State ( State(comment, holding, items_at, i_am_at) )
    import qualified Data.List as List

    take :: String -> State -> State
    take itemName state = do
        let item = List.find (\x -> itemName == fst x) (items_at state)
        case item of
            Nothing -> state { comment = ["There is no " ++ itemName ++ " in this room"] }
            Just realThing -> if snd realThing == i_am_at state then
                  state { holding = itemName:holding state, items_at = List.delete realThing (items_at state), comment = ["You took " ++ itemName ++ " from the ground"]  }
              else
                  state { comment = ["There is no " ++ itemName ++ " in this room"] }

    drop :: String -> State -> State
    drop itemName state = do
        if itemName `elem` holding state then
            state { holding = List.delete itemName (holding state), items_at = (itemName, i_am_at state):items_at state, comment = ["You drop " ++ itemName ++ " to ground"]  }
        else
            state { comment = ["You don't have " ++ itemName ++ " in your inventory"] }

    inventory :: State -> State
    inventory state = state {comment = "Your inventory:" : holding state}