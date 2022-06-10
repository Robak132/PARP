module Items where
    data Item = Item {
        name :: String,
        itemLocation :: String
    } deriving (Show, Eq)

    key :: Item
    key = Item "Key" "attendant_room"
    
    torch :: Item
    torch = Item "Torch" "acolyte_chamber_1"

    floatingCrystal :: Item
    floatingCrystal = Item "Floating Crystal" "false_floor_room"