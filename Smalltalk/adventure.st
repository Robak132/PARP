#!/usr/bin/env gst
"Doges&Cateons, by Jakub Robaczewski, Paweł Muller, Marianna Gromadzka."
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
    getDefense [
        ^defense
    ]
    getStrength [
        ^strength
    ]
    getDamage [
        ^damage
    ]
]
Enemy subclass: Player [
    | currentlyAt inventory |
    Player class >> new [
        | player |
        player := super new.
        ^ player
    ]

    init: health_ defense: defense_ strength: strength_ damage: damage_ [
        super init: '' health: health_ defense: defense_ strength: strength_ damage: damage_.
        currentlyAt := 'entrance'.
        inventory := OrderedCollection new.
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
    alive [
        ^(health > 0)
    ]
    notAlive [
        ^(health = 0)
    ]
]
Object subclass: Game [
    | player paths enemies items room_descriptions isGameOver |

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
        paths at: ('trap_corridor_b', 'N') put: 'trap_corridor_c'.
        paths at: ('trap_corridor_c', 'S') put: 'trap_corridor_b'.
        paths at: ('trap_corridor_c', 'N') put: 'treasure_room'.
        paths at: ('serket_chamber', 'W') put: 'acolyte_chamber_2'.
        paths at: ('serket_chamber', 'N') put: 'guardian'.
        paths at: ('guardian', 'S') put: 'serket_chamber'.
        paths at: ('guardian', 'N') put: 'sarcophagus'.
        paths at: ('treasure_room', 'S') put: 'trap_corridor_c'.
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
        room_descriptions at: 'false_floor_room' put: #('The centre of the room has a marble table with a floating purple crystal. The floor in the middle looks cracked and hastily built.').
        room_descriptions at: 'trap_corridor_a' put: #('You have entered yet another dark corridor. You see massive blades falling from the roof and reseting after that.').
        room_descriptions at: 'serket_chamber' put: #('The hieroglyphs in this room describe how every cat devotes their life to lasagna, and therefore is cursed dou to its greed').
        room_descriptions at: 'guardian' put: #('You are in the room lit with hundreds of candles. In the middle there is a guardian, chained to a metal pole').
        room_descriptions at: 'treasure_room' put: #('There is a variety of treasure, such as bones and tennis balls. There is also some ancient stuff').
        room_descriptions at: 'sarcophagus' put: #('There is a big sarcophagus in the middle of the room').
        room_descriptions at: 'hidden_exit' put: #('There are two statues of cats in this room. Under one of them a small breeze can be felt.' 'You made it to the end, please enter the quit command.').

        items := Dictionary new.
        items at: 'attendant_room' put: (self initOrderedCollection: 'Key').
        items at: 'acolyte_chamber_1' put: (self initOrderedCollection: 'Torch').
        items at: 'false_floor_room' put: (self initOrderedCollection: 'Floating Crystal').

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

    "Print strings from array in separate lines."
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

    look [
        |enemy|
        enemy := (enemies at: (player getCurrentRoom) ifAbsent: ['']).
        self printLines: (room_descriptions at: (player getCurrentRoom) ifAbsent: [#('Nothing here, probably an error')]).
        (enemy = '') ifFalse: [
            self printLines: {'There is ', (enemy getName), ' here. Time to fight!'}.
        ].
        self printLines: #('').
    ]

    search [
        |locatedItems enemy|
        enemy := (enemies at: (player getCurrentRoom) ifAbsent: ['']).
        (enemy = '') ifFalse: [
            self printLines: {'You cannot search this room when there is ', (enemy getName), ' in it.' }, #('').
        ] ifTrue: [
            locatedItems := items at: (player getCurrentRoom) ifAbsent: [OrderedCollection new].
            (locatedItems size = 0) ifTrue: [
                self printLines: #('You found nothing' ), #('').
            ] ifFalse: [
                self printLines: #('You found these items:' ), locatedItems, #('').
            ]
        ].
    ]
    go: direction [
        | new_room |
        new_room := (paths at: ((player getCurrentRoom), direction) ifAbsent: [player getCurrentRoom]).
        new_room == (player getCurrentRoom) ifFalse: [
            player setCurrentRoom: new_room.
            self look.
        ] ifTrue: [
            self printLines: #('There is no way there.')
        ]
    ]

    take: item [
        |locatedItems collection enemy|
        enemy := (enemies at: (player getCurrentRoom) ifAbsent: ['']).
        (enemy = '') ifFalse: [
            self printLines: {'You cannot take items in room when there is ', (enemy getName), ' in it.' }, #('').
        ] ifTrue: [
            locatedItems := items at: (player getCurrentRoom) ifAbsent: [OrderedCollection new].
            self printLines: locatedItems.
            (locatedItems includes: item) ifTrue: [
                player addItem: item.
                collection := items at: (player getCurrentRoom).
                collection remove: item.
                items at: (player getCurrentRoom) put: collection.
                self printLines: {'You took ', item, ' from the ground.'}.
            ] ifFalse: [
                self printLines: {'There is no ', item, ' in this room.'}.
            ]
        ]
    ]

    drop: item [
        |collection enemy|
        enemy := (enemies at: (player getCurrentRoom) ifAbsent: ['']).
        (enemy = '') ifFalse: [
            self printLines: {'You cannot drop items in room when there is ', (enemy getName), ' in it.' }, #('').
        ] ifTrue: [
            (((player getInventory) includes: item)) ifTrue: [
                player removeItem: item.
                collection := items at: (player getCurrentRoom) ifAbsent: [OrderedCollection new].
                collection add: item.
                items at: (player getCurrentRoom) put: collection.
                self printLines: {'You drop ', item, ' to the ground.'}.
            ] ifFalse: [
                self printLines: {'You dont have ', item, ' in your inventory'}.
            ]
        ]
    ]

    join: list [
        | str |
        str := ''.
        list do: [:each | str := ((str = '') ifTrue: [each] ifFalse: [str, ' ', each])].
        ^str.
    ]

    "Run the game."
    run [
        | cmd argument isUnknown |

        self look.
        self printInstructions.

        "Game loop."
        [player notAlive | isGameOver] whileFalse: [
            | args str |
            isUnknown := true.
            args := (self readCommand) substrings.
            cmd := args first.
            args removeFirst.
            argument := self join: args.

            ((cmd = 'inventory') | (cmd = 'i')) ifTrue: [
                self printLines: #('Your inventory:' ), (player getInventory), #('').
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
                isUnknown := false
            ].

            (cmd = 'search') ifTrue: [
                self search.
                isUnknown := false
            ].

            ((cmd = 'instructions') | (cmd = 'help')) ifTrue: [
                self printInstructions.
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
                self printLines: #('Unknown command.' '')
            ]
        ]
    ]
]

Game new run.
