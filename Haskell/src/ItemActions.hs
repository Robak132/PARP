module ItemActions where
    import qualified Data.List as List
    import State ( State(comment, enemies, holding, items, you) )
    import Character (Character(location, name), alive)
    import Combat (die)
    import Items (Item(name, itemLocation), floatingCrystal)

    take :: String -> State -> State
    take itemName state = case List.find (\x -> itemName == Items.name x && location (you state) == itemLocation x) (items state) of
        Nothing -> state { comment = ["There is no " ++ itemName ++ " in this room"] }
        Just item -> case List.find (\x -> location (you state) == location x) (enemies state) of
            Just enemy | alive enemy -> state { comment = ["You cannot take items in room when there is " ++ Character.name enemy ++ " in it."]}
                       | otherwise -> if item == floatingCrystal then
                                die (you state) state { comment = ["You tried to grab the crystal, but floor collapsed under you. You fall into spikes."] }
                           else
                                state { holding = item {itemLocation=""}:holding state, items = List.delete item (items state), comment = ["You took " ++ itemName ++ " from the ground"]}
            Nothing -> if item == floatingCrystal then
                    die (you state) state { comment = ["You tried to grab the crystal, but floor collapsed under you. You fall into spikes."] }
                else
                    state { holding = item {itemLocation=""}:holding state, items = List.delete item (items state), comment = ["You took " ++ itemName ++ " from the ground"]  }


    drop :: String -> State -> State
    drop itemName state = case List.find (\x -> itemName == Items.name x) (holding state) of
        Nothing -> state { comment = ["You don't have " ++ itemName ++ " in your inventory"] }
        Just item -> case List.find (\x -> location (you state) == location x) (enemies state) of
            Just enemy | alive enemy -> state { comment = ["You cannot drop items in room when there is " ++ Character.name enemy ++ " in it."]}
                       | otherwise -> state { holding = List.delete item (holding state), items = item {itemLocation=location (you state)}:items state, comment = ["You drop " ++ itemName ++ " to ground"]}
            Nothing -> state { holding = List.delete item (holding state), items = item {itemLocation=location (you state)}:items state, comment = ["You drop " ++ itemName ++ " to ground"]  }

    inventory :: State -> State
    inventory state = state {comment = "Your inventory:" : map Items.name (holding state)}

    search :: State -> State
    search state = case map Items.name (filter (\x -> itemLocation x == location (you state)) (items state)) of
            [] -> state { comment = ["There is nothing here"]}
            items -> case List.find (\x -> location (you state) == location x) (enemies state) of
                Nothing -> state { comment = "You found these items:" : items }
                Just enemy | alive enemy -> state { comment = ["You cannot search this room when there is " ++ Character.name enemy ++ " in it."]}
                           | otherwise -> state { comment = "You found these items:" : items }