module ItemActions where
    import qualified Data.List as List
    import State ( State(comment, holding, items, you) )
    import Character (Character(location))
    import Combat (die)
    import Items (Item(name, itemLocation), floatingCrystal)
    
    take :: String -> State -> State
    take itemName state = case List.find (\x -> itemName == name x && location (you state) == itemLocation x) (items state) of 
        Nothing -> state { comment = ["There is no " ++ itemName ++ " in this room"] }
        Just item | item == floatingCrystal -> die (you state) state { comment = ["You tried to grab the crystal, but floor collapsed under you. You fall into spikes."] }
                  | otherwise -> state { holding = item {itemLocation=""}:holding state, items = List.delete item (items state), comment = ["You took " ++ itemName ++ " from the ground"]  }

    drop :: String -> State -> State
    drop itemName state = case List.find (\x -> itemName == name x) (holding state) of
        Nothing -> state { comment = ["You don't have " ++ itemName ++ " in your inventory"] }
        Just item -> state { holding = List.delete item (holding state), items = item {itemLocation=location (you state)}:items state, comment = ["You drop " ++ itemName ++ " to ground"]  }

    inventory :: State -> State
    inventory state = state {comment = "Your inventory:" : map name (holding state)}

    search :: State -> State
    search state = case map name (filter (\x -> itemLocation x == location (you state)) (items state)) of
        [] -> state { comment = ["There is nothing here"]}
        items -> state { comment = "You found these items:" : items }