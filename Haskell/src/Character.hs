module Character where
    data Character = Character {
            name :: String,
            location :: String,
            defense :: Integer,
            health :: Integer,
            damage :: Integer,
            attack :: Integer
        } deriving (Show, Eq)

    character :: Character
    character = Character "" "" 6 12 0 4
    
    basicSkeleton :: Character
    basicSkeleton = Character "" "" 3 12 2 2
    
    basicGuardian :: Character
    basicGuardian = Character "" "" 9 12 (-4) 6
    
    fallenCat :: Character
    fallenCat = Character "" "" 6 12 0 4