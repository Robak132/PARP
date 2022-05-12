module Doors where
    data Door = Door {
        name :: String,
        location_from :: String,
        location_to :: String,
        status :: Bool
    } deriving (Show, Eq)
