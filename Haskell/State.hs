module State where
    data State = State {
            comment :: [String],
            i_am_at :: String,
            items_at :: [(String, String)],
            enemy_at :: [(String, String)],
            health :: [(String, Integer)],
            door :: [(String, String, String, Bool)],
            holding :: [String]
        } deriving (Show)