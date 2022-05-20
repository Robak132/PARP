#!/usr/bin/env gst
"Doges&Cateons, by Jakub Robaczewski, Paweł Muller, Marianna Gromadzka."
Object subclass: IOModule [
    IOModule class >> new [
        | module |
        module := super new.
        ^module.
    ]

    instructionsText := #(
        'Available commands are:'
        'n,  s,  e,  w,                 -- to go in that direction.'   "Working"
        'flee n, flee s, flee e, flee w -- to flee from combat.'
        'take Object.                   -- to pick up an object.'      "Working"
        'drop Object                    -- to put down an object.'     "Working"
        'look                           -- to look around you again.'  "Working"
        'search                         -- to search the room.'        "Working"
        'inventory, i                   -- to check inventory.'        "Working"
        'attack Enemy                   -- to attack the enemy.'
        'instructions, help             -- to see this message again.' "Working"
        'quit                           -- to end the game and quit.'  "Working"
        ''
    ).
 
    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ]
    ]

    printInstructions [
        self printLines: instructionsText.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]
]

Object subclass: Enemy [
    | name location health defense strength damage|
    Enemy class >> new [
        | enemy |
        enemy := super new.
        ^enemy.
    ]
    init: name_ health: health_ defense: defense_ strength: strength_ damage: damage_ [
        name := name_.
        health := health_.
        defense := defense_.
        strength := strength_.
        damage := damage_.
    ]
    getName [
        ^name
    ]
    getHealth [
        ^health
    ]
    attack: enemy [
        | attackRoll damageRoll |
        attackRoll:= Random new between: 1 and: 20.
        damageRoll:= Random new between: 1 and: damage.
        (attackRoll >= enemy getDefense) ifTrue: [
            IOModule new printLines: {name, ' attacks ', (enemy getName), ' (', (attackRoll displayString), '>=', (enemy getDefense displayString), ') [', (damageRoll displayString), ' dmg].'}.
            enemy harm: damageRoll.
        ] ifFalse: [
            IOModule new printLines: {name, ' failed to attack ', (enemy getName), ' (', (attackRoll displayString), '<', (enemy getDefense displayString), ').'}.
        ].
    ]
    harm: damage [
        health := health - damage. 
        (self notAlive) ifTrue: [
            IOModule new printLines: {name, ' is dead.'}.
        ]
    ]
    getDefense [
        ^defense
    ]
    getStrength [
        ^strength
    ]
    getDamage [
        ^damage
    ]
    alive [
        ^(health > 0)
    ]
    notAlive [
        ^(health = 0)
    ]
]

Enemy subclass: Player [
    | currentlyAt inventory |
    Player class >> new [
        | player |
        player := super new.
        ^ player
    ]
    attack: enemy [
        super attack: enemy.
        (enemy alive) ifTrue: [
            enemy attack: self.
        ]
    ]
    init: health_ defense: defense_ strength: strength_ damage: damage_ [
        currentlyAt := 'entrance'.
        inventory := OrderedCollection new.
        ^super init: 'Player' health: health_ defense: defense_ strength: strength_ damage: damage_.
    ]
    harm: damage [
        super harm: damage.
        (health <= 0) ifTrue: [
            IOModule new printLines: {name, ' is dead.'}.
        ] ifFalse: [
            IOModule new printLines: {'Remaining HP: ', health displayString}.
        ]
    ]
    getCurrentRoom [
        ^currentlyAt
    ]
    setCurrentRoom: currentRoom [
        currentlyAt := currentRoom
    ]
    getInventory [
        ^inventory
    ]
    addItem: item [
        inventory add: item
    ]
    removeItem: item [
        inventory remove: item
    ]
]

OrderedCollection extend [
    init: list [
        | collection |
        collection := OrderedCollection new.
        collection addAll: list.
        ^list
    ]
]

Object subclass: Game [
    | player paths enemies items room_descriptions isGameOver |
 
    Game class >> new [
        | g |
        g := super new.
        g init.
        ^g
    ]

    init [
        player := (Player new init: 6 defense: 12 strength: 0  damage: 4).
        isGameOver := false.

        paths := Dictionary new.
        paths at: ('entrance', 'N') put: 'antechamber'.
        paths at: ('antechamber', 'N') put: 'altar_room'.
        paths at: ('antechamber', 'S') put: 'entrance'.
        paths at: ('antechamber', 'W') put: 'jar_room'.
        paths at: ('antechamber', 'E') put: 'attendant_room'.
        paths at: ('jar_room', 'E') put: 'antechamber'.
        paths at: ('jar_room', 'W') put: 'acolyte_chamber_1'.
        paths at: ('attendant_room', 'W') put: 'antechamber'.
        paths at: ('attendant_room', 'N') put: 'corridor'.
        paths at: ('corridor', 'N') put: 'false_floor_room'.
        paths at: ('corridor', 'W') put: 'altar_room'.
        paths at: ('acolyte_chamber_1', 'N') put: 'acolyte_chamber_2'.
        paths at: ('acolyte_chamber_1', 'E') put: 'jar_room'.
        paths at: ('acolyte_chamber_2', 'S') put: 'acolyte_chamber_1'.
        paths at: ('acolyte_chamber_2', 'E') put: 'serket_chamber'.
        paths at: ('altar_room', 'S') put: 'antechamber'.
        paths at: ('altar_room', 'E') put: 'corridor'.
        paths at: ('false_floor_room', 'S') put: 'corridor'.
        paths at: ('false_floor_room', 'N') put: 'trap_corridor_a'.
        paths at: ('trap_corridor_a', 'S') put: 'false_floor_room'.
        paths at: ('trap_corridor_a', 'N') put: 'trap_corridor_b'.
        paths at: ('trap_corridor_b', 'S') put: 'trap_corridor_a'.
        paths at: ('trap_corridor_b', 'N') put: 'treasure_room'.
        paths at: ('serket_chamber', 'W') put: 'acolyte_chamber_2'.
        paths at: ('serket_chamber', 'N') put: 'guardian'.
        paths at: ('guardian', 'S') put: 'serket_chamber'.
        paths at: ('guardian', 'N') put: 'sarcophagus'.
        paths at: ('treasure_room', 'S') put: 'trap_corridor_b'.
        paths at: ('treasure_room', 'N') put: 'hidden_exit'.
        paths at: ('sarcophagus', 'S') put: 'guardian'.
        paths at: ('sarcophagus', 'E') put: 'hidden_exit'.
        paths at: ('hidden_exit', 'N') put: 'sarcophagus'.
        paths at: ('hidden_exit', 'S') put: 'treasure_room'.

        room_descriptions := Dictionary new.
        room_descriptions at: 'entrance' put: #('You are at the entrance to tomb. There is an gate before you, with small cat door').
        room_descriptions at: 'attendant_room' put: #('You are in a room filled with skeletons.').
        room_descriptions at: 'antechamber' put: #('You are in the first room. The walls are covered in hieroglyphic description of the antient curse that forbids any cat that walks in there to go to heaven. They will be forever doomed to live in the tomb, turned into skeletons.').
        room_descriptions at: 'jar_room' put: #('You have entered the romm filled with jars. There are some tasty bones and shiny jewels in them').
        room_descriptions at: 'corridor' put: #('You are in the dark corridor.').
        room_descriptions at: 'acolyte_chamber_1' put: #('You are in yet another room. You see door with a symbol of the moon and long shadows.').
        room_descriptions at: 'acolyte_chamber_2' put: #('You see tombs of important cats. Unfortunately cats cant read, so you dont know their names.').
        room_descriptions at: 'altar_room' put: #('You walked to the room with big altar in the middle.').
        room_descriptions at: 'false_floor_room' put: #('The centre of the room has a marble table with a floating purple crystal. The floor in the middle looks cracked and hastily built. You see massive blades falling from the roof and reseting after that on the north.').
        room_descriptions at: 'trap_corridor_a' put: #('You have entered yet another dark corridor. You see massive blades falling from the roof and reseting after that on your south.').
        room_descriptions at: 'trap_corridor_a' put: #('You are going further through corridor, one of the slabs on the north looks suspiciously.').
        room_descriptions at: 'serket_chamber' put: #('The hieroglyphs in this room describe how every cat devotes their life to lasagna, and therefore is cursed dou to its greed').
        room_descriptions at: 'guardian' put: #('You are in the room lit with hundreds of candles. In the middle there is a guardian, chained to a metal pole').
        room_descriptions at: 'treasure_room' put: #('There is a variety of treasure, such as bones and tennis balls. There is also some ancient stuff, one of the slabs on the south of you looks suspiciously.').
        room_descriptions at: 'sarcophagus' put: #('There is a big sarcophagus in the middle of the room').
        room_descriptions at: 'hidden_exit' put: #('There are two statues of cats in this room. Under one of them a small breeze can be felt.' 'You made it to the end, please enter the quit command.').

        items := Dictionary new.
        items at: 'attendant_room'    put: (OrderedCollection new init: {'Key'}).
        items at: 'acolyte_chamber_1' put: (OrderedCollection new init: {'Torch'}).
        items at: 'false_floor_room'  put: (OrderedCollection new init: {'Floating Crystal'}).

        enemies := Dictionary new.
        enemies at: 'attendant_room' put: (Enemy new init: 'Skele-cat'        health: 3 defense: 12 strength: 2  damage: 2).
        enemies at: 'guardian'       put: (Enemy new init: 'Catmint Guardian' health: 9 defense: 9  strength: -4 damage: 6).
        enemies at: 'sarcophagus'    put: (Enemy new init: 'Fallen Cat'       health: 6 defense: 12 strength: 0  damage: 4).
    ]

    initOrderedCollection: element [
        |list|
        list := OrderedCollection new.
        list add: element.
        ^list.
    ]

    look [
        |enemy|
        enemy := (enemies at: (player getCurrentRoom) ifAbsent: ['']).
        IOModule new printLines: (room_descriptions at: (player getCurrentRoom) ifAbsent: [#('Nothing here, probably an error')]).
        (enemy = '' or: [enemy notAlive]) ifFalse: [
            IOModule new printLines: {'There is ', (enemy getName), ' here. Time to fight!'}.
        ].
    ]

    search [
        |locatedItems enemy|
        enemy := (enemies at: (player getCurrentRoom) ifAbsent: ['']).
        (enemy = '' or: [enemy notAlive]) ifFalse: [
            IOModule new printLines: {'You cannot search this room when there is ', (enemy getName), ' in it.' }, #('').
        ] ifTrue: [
            locatedItems := items at: (player getCurrentRoom) ifAbsent: [OrderedCollection new].
            (locatedItems size = 0) ifTrue: [
                IOModule new printLines: #('You found nothing' ), #('').
            ] ifFalse: [
                IOModule new printLines: #('You found these items:' ), locatedItems, #('').
            ]
        ].
    ]

    move: direction [
        | new_room |
        new_room := (paths at: ((player getCurrentRoom), direction) ifAbsent: [player getCurrentRoom]).
        new_room == (player getCurrentRoom) ifFalse: [
            player setCurrentRoom: new_room.
            self look.
            ^true.
        ] ifTrue: [
            IOModule new printLines: #('There is no way there.').
            ^false.
        ]
    ]

    go: direction [
        | new_room enemy |
        enemy := (enemies at: (player getCurrentRoom) ifAbsent: ['']).
        (enemy = '' or: [enemy notAlive]) ifFalse: [
            IOModule new printLines: {'You cannot exit room, when is ', (enemy getName), ' in it.' }, #('').
        ] ifTrue: [
            self move: (direction asUppercase).
            IOModule new  printLines: #('').
        ]
    ]

    attack: temp [
        | enemy |
        enemy := (enemies at: (player getCurrentRoom) ifAbsent: ['']).
        (enemy = '' or: [enemy notAlive]) ifFalse: [
            player attack: enemy.
        ] ifTrue: [
            IOModule new printLines: {'There is no ', temp, ' in this room.'}, #('').
        ]
    ]

    flee: direction [
        | enemy |
        enemy := (enemies at: (player getCurrentRoom) ifAbsent: ['']).
        (self move: (direction asUppercase)) ifTrue: [
            (enemy = '' or: [enemy notAlive]) ifFalse: [
                IOModule new printLines: {(enemy getName), ' attacks you when you leave.'}.
                player harm: 1.
            ]
        ].
        IOModule new  printLines: #('').
    ]

    take: item [
        |locatedItems collection enemy|
        enemy := (enemies at: (player getCurrentRoom) ifAbsent: ['']).
        (enemy = '' or: [enemy notAlive]) ifFalse: [
            IOModule new printLines: {'You cannot take items in room when there is ', (enemy getName), ' in it.' }, #('').
        ] ifTrue: [
            locatedItems := items at: (player getCurrentRoom) ifAbsent: [OrderedCollection new].
            IOModule new printLines: locatedItems.
            (locatedItems includes: item) ifTrue: [
                player addItem: item.
                collection := items at: (player getCurrentRoom).
                collection remove: item.
                items at: (player getCurrentRoom) put: collection.
                IOModule new printLines: {'You took ', item, ' from the ground.'}.
            ] ifFalse: [
                IOModule new printLines: {'There is no ', item, ' in this room.'}.
            ]
        ]
    ]

    drop: item [
        |collection enemy|
        enemy := (enemies at: (player getCurrentRoom) ifAbsent: ['']).
        (enemy = '' or: [enemy notAlive]) ifFalse: [
            IOModule new printLines: {'You cannot drop items in room when there is ', (enemy getName), ' in it.' }, #('').
        ] ifTrue: [
            (((player getInventory) includes: item)) ifTrue: [
                player removeItem: item.
                collection := items at: (player getCurrentRoom) ifAbsent: [OrderedCollection new].
                collection add: item.
                items at: (player getCurrentRoom) put: collection.
                IOModule new printLines: {'You drop ', item, ' to the ground.'}.
            ] ifFalse: [
                IOModule new printLines: {'You dont have ', item, ' in your inventory'}.
            ]
        ]
    ]

    join: list [
        | str |
        str := ''.
        list do: [:each | str := ((str = '') ifTrue: [each] ifFalse: [str, ' ', each])].
        ^str.
    ]

    run [
        IOModule new printInstructions.
        self look.
        IOModule new printLines: #('').

        "Game loop."
        [isGameOver] whileFalse: [
            | cmd args argument isUnknown |
            isUnknown := true.
            args := (IOModule new readCommand) substrings.
            cmd := args first.
            args removeFirst.
            argument := self join: args.

            (cmd = 'attack') ifTrue: [
                self attack: argument.
                isUnknown := false
            ].

            ((cmd = 'inventory') | (cmd = 'i')) ifTrue: [
                IOModule new printLines: #('Your inventory:' ), (player getInventory), #('').
                isUnknown := false
            ].

            (cmd = 'flee') ifTrue: [
                self flee: argument.
                isUnknown := false
            ].
            
            ((cmd = 'n') | (cmd = 'N')) ifTrue: [
                self go: 'N'.
                isUnknown := false
            ].

            ((cmd = 's') | (cmd = 'S')) ifTrue: [
                self go: 'S'.
                isUnknown := false
            ].

            ((cmd = 'e') | (cmd = 'E')) ifTrue: [
                self go: 'E'.
                isUnknown := false
            ].

            ((cmd = 'w') | (cmd = 'W')) ifTrue: [
                self go: 'W'.
                isUnknown := false
            ].

            (cmd = 'look') ifTrue: [
                self look.
                IOModule new printLines: #('').
                isUnknown := false
            ].

            (cmd = 'search') ifTrue: [
                self search.
                isUnknown := false
            ].

            ((cmd = 'instructions') | (cmd = 'help')) ifTrue: [
                IOModule new printInstructions.
                isUnknown := false
            ].

            cmd = 'quit' ifTrue: [
                isGameOver := true.
                isUnknown := false
            ].

            cmd = 'take' ifTrue: [
                self take: argument.
                isUnknown := false
            ].

            cmd = 'drop' ifTrue: [
                self drop: argument.
                isUnknown := false
            ].

            isUnknown ifTrue: [
                IOModule new printLines: #('Unknown command.' '')
            ].
            
            (player alive) ifFalse: [
                isGameOver := true.
            ].
        ].
    ]
]

Game new run.
