module Character where
    data Character = Character {
            name :: String,
            location :: String,
            health :: Int,
            defense :: Int,
            strength :: Int,
            damage :: Int
        } deriving (Show, Eq)

    character :: Character
    character = Character "Doge" "" 6 12 0 4
    
    basicSkeleton :: Character
    basicSkeleton = Character "" "" 3 12 2 2
    
    basicGuardian :: Character
    basicGuardian = Character "" "" 9 9 (-4) 6
    
    fallenCat :: Character
    fallenCat = Character "" "" 6 12 0 4

    alive :: Character -> Bool
    alive character = health character > 0