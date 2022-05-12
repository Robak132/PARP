module Traps where
    data Trap = Trap {
        location_from :: String,
        location_to :: String,
        strengthModifier :: Int,
        damage :: Int,
        dodge :: Int
    } deriving (Show)

    bladeTrap :: Trap
    bladeTrap = Trap "false_floor_room" "trap_corridor_a" 0 6 10

    tripWire :: Trap
    tripWire = Trap "trap_corridor_a" "trap_corridor_b" 0 2 6

    dustTrap :: Trap
    dustTrap = Trap "trap_corridor_b" "trap_corridor_c" (-2) 0 14
