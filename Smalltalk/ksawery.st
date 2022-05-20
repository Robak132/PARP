"TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek."

Object subclass: Locations [
    | paths people items currentlyAt |
    Locations class >> new [
        | locations |
        locations := super new.
        locations init.
        ^ locations
    ]

    init [
        currentlyAt := 'room_of_thomas_and_giulia'.

        paths := Dictionary new.
        paths at: ('room_of_thomas_and_giulia', 'west') put: 'corridor'.
        paths at: ('corridor', 'east') put: 'room_of_thomas_and_giulia'.
        paths at: ('corridor', 'west') put: 'room_of_zoe'.
        paths at: ('corridor', 'south') put: 'reception'.
        paths at: ('room_of_zoe', 'east') put: 'corridor'.
        paths at: ('reception', 'north') put: 'corridor'.
        paths at: ('reception', 'west') put: 'bar'.
        paths at: ('reception', 'south') put: 'hotel_entrance'.
        paths at: ('bar', 'east') put: 'reception'.
        paths at: ('bar', 'south') put: 'kitchen'.
        paths at: ('hotel_entrance', 'north') put: 'reception'.
        paths at: ('hotel_entrance', 'south') put: 'hunters_shaque'.
        paths at: ('kitchen', 'north') put: 'bar'.
        paths at: ('kitchen', 'east') put: 'hunters_shaque'.
        paths at: ('hunters_shaque', 'north') put: 'hotel_entrance'.
        paths at: ('hunters_shaque', 'west') put: 'kitchen'.

        people := Dictionary new.
        people at: 'room_of_thomas_and_giulia' put: #('thomas' 'giulia' 'andreas').
        people at: 'room_of_zoe' put: #('zoe').
        people at: 'bar' put: #('karl' 'amy' 'stephan').
        people at: 'corridor' put: #('jurgen' 'hilda').
        people at: 'kitchen' put: #('theodor').
        people at: 'reception' put: #('hans' 'hermann').
        people at: 'hotel_entrance' put: #('jonas' 'urlich').

        items := Dictionary new.
        items at: 'room_of_thomas_and_giulia' put: #('watch' 'thomas_journal' 'cigarette_light').
        items at: 'room_of_zoe' put: #('sleep_mask' 'sleeping_pills' 'cup').
        items at: 'karl' put: #('clubs_symbol').
        items at: 'bar' put: #('glass').
        items at: 'hilda' put: #('brooch').
        items at: 'corridor' put: #('cleaning_stuff' 'cutlery_tray').
        items at: 'reception' put: #('guest_book' 'telephone' 'ball').
        items at: 'hermann' put: #('hunting_weapon').
        items at: 'urlich' put: #('gilded_epaulettes').
        items at: 'hotel_entrance' put: #('bush').
        items at: 'hunters_shaque' put: #('bullets' 'knife_scabbard' 'blooded_knife').
        items at: 'kitchen' put: #('deer' 'broth').
        items at: 'me' put: #('money').
    ]

    go: direction [
        currentlyAt := (paths at: (currentlyAt, direction) ifAbsent: [currentlyAt]).
        ^ { 'You are currently at ', currentlyAt }, self look.
    ]

    list: collection at: location [
        ^ (collection at: location ifAbsent: [#()])
    ]

    look [
        ^ #('There are following people here:'), (self list: people at: currentlyAt), #('')
    ]

    notice [
        ^ #('There are following items here:'), (self list: items at: currentlyAt), #('')
    ]

    trinkets: person [
        ^ { person, ' bears following items:'}, (self list: items at: person), #('')
    ]

    inventory [
        ^ #('You have following items:'), (self list: items at: 'me'), #('')
    ]

    move: object in: collection from: from to: to [
        ((collection at: from) select: [ :it | it = object ]) size > 0
        ifTrue: [
            collection at: from put: ((collection at: from) reject: [ :it | it = object ]).
            collection at: to put: ({ object }, (collection at: to)).
            ^ true.
        ]
        ifFalse: [
            ^ false.
        ].
    ]

    take: item [
        (self move: item in: items from: currentlyAt to: 'me')
        ifTrue: [
            ^ { 'Took ', item, ' into your inventory'. '' }.
        ]
        ifFalse: [
            ^ { 'Failed to take ', item. '' }.
        ]
    ]

    isNear: person [
        ^ (((people at: currentlyAt) select: [ :it | it = person ]) size = 1)
    ]

    isItem: item on: person [
        ^ (((items at: person) select: [ :it | it = item ]) size = 1) | (((items at: 'me') select: [ :it | it = item ]) size = 1)
    ]
]

Object subclass: Dialogues [
    | talkingTo knownFacts prerequisites factsMap dialogues |
    Dialogues class >> new [
        | dialogues |
        dialogues := super new.
        dialogues init.
        ^ dialogues
    ]

    init [
        talkingTo := 'nobody'.
        knownFacts := #('thomas_had_been_murdered') asBag.

        prerequisites := Dictionary new.
        prerequisites at: ('hilda', 'thomas_had_been_murdered') put: [ ((knownFacts occurrencesOf: 'asked_about_brooch') > 0) | ((knownFacts occurrencesOf: 'theodor_trusts_me') > 0) ].
        prerequisites at: ('urlich', 'watch') put: [ ((knownFacts occurrencesOf: 'poker_is_played_here') > 0) ].
        prerequisites at: ('amy', 'watch') put: [ ((knownFacts occurrencesOf: 'poker_is_played_here') > 0) & ((knownFacts occurrencesOf: 'watch_has_changed_hands_during_last_game') > 0) ].
        prerequisites at: ('amy', 'watch_has_changed_hands_during_last_game') put: [ ((knownFacts occurrencesOf: 'zoe_befriended_hilda') > 0) ].
        " TODO prerequisites at: ('urlich', 'karl') put: [ (i_know(poker_is_played_here), person_at(urlich, Place), \+ ((person_at(Person, Place), Person \= urlich))) ]. "
        prerequisites at: ('karl', 'andreas') put: [ ((knownFacts occurrencesOf: 'asked_andreas_about_why_is_he_here') > 0) ].
        prerequisites at: ('andreas', 'andreas_was_here_yesterday') put: [ ((knownFacts occurrencesOf: 'watch_was_originally_andreases') > 0) & ((knownFacts occurrencesOf: 'thomas_was_here_to_buy_a_watch') > 0) & ((knownFacts occurrencesOf: 'asked_andreas_about_why_is_he_here') > 0) & ((knownFacts occurrencesOf: 'watch_has_changed_hands_during_last_game') > 0) ].
        " TODO prerequisites at: ('jonas', 'karl') put: [ person_at(jonas, bar) ]. "
        prerequisites at: ('hilda', 'zoe_was_thomas_lovers') put: [ ((knownFacts occurrencesOf: 'zoe_befriended_hilda') > 0) ].

        factsMap := Dictionary new.
        factsMap at: ('hilda', 'brooch') put: 'asked_about_brooch'.
        factsMap at: ('urlich', 'gilded_epaulettes') put: 'poker_is_played_here'.
        factsMap at: ('urlich', 'watch') put: 'watch_has_changed_hands_during_last_game'.
        factsMap at: ('amy', 'watch') put: 'amy_won_the_watch'.
        factsMap at: ('hilda', 'cigarette_light') put: 'hilda_smokes_light_cigarettes'.
        factsMap at: ('amy', 'cigarette_light') put: 'amy_smokes_standard_cigarettes'.
        factsMap at: ('karl', 'clubs_symbol') put: 'karl_likes_playing_some_card_game'.
        factsMap at: ('zoe', 'sleeping_pills') put: 'zoe_has_trouble_sleeping'.
        factsMap at: ('giulia', 'thomas_journal') put: 'thomas_kept_his_journal_really_secret'.
        factsMap at: ('hilda', 'thomas_journal') put: 'thomas_was_here_to_buy_a_watch'.
        factsMap at: ('zoe', 'sleep_mask') put: 'zoe_has_trouble_sleeping'.
        factsMap at: ('zoe', 'glass') put: 'zoe_parting_yesterday'.
        factsMap at: ('karl', 'glass') put: 'karl_is_barman'.
        factsMap at: ('hilda', 'cleaning_stuff') put: 'hilda_is_cleaning_lady'.
        factsMap at: ('jurgen', 'cutlery_tray') put: 'jurgen_is_butler'.
        factsMap at: ('hans', 'guest_book') put: 'andreas_was_here_yesterday'.
        factsMap at: ('hans', 'telephone') put: 'hans_is_hotel_owner'.
        factsMap at: ('hermann', 'ball') put: 'hermann_dog_is_promyczek'.
        factsMap at: ('hermann', 'hunting_weapon') put: 'hermann_is_hunter'.
        factsMap at: ('jonas', 'bush') put: 'jonas_parting_yesterday'.
        factsMap at: ('urlich', 'bush') put: 'ulrich_is_doorkeeper'.
        factsMap at: ('hermann', 'bullets') put: 'hermann_is_hunter'.
        factsMap at: ('hermann', 'knife_scabbard') put: 'hermann_is_hunter'.
        factsMap at: ('hermann', 'blooded_knife') put: 'hermann_lost_knife'.
        factsMap at: ('theodor', 'deer') put: 'theodor_is_chef'.
        factsMap at: ('theodor', 'broth') put: 'theodor_trusts_me'.

        factsMap at: ('amy', 'watch_has_changed_hands_during_last_game') put: 'amy_passed_out'.
        factsMap at: ('amy', 'karl_cheats_at_poker') put: 'amy_knows_about_karl_cheating_at_poker'.
        factsMap at: ('hilda', 'thomas_had_been_murdered') put: 'poker_is_played_here'.
        factsMap at: ('hilda', 'watch_has_changed_hands_during_last_game') put: 'zoe_knew_about_watch_changing_hands'.
        factsMap at: ('giulia', 'thomas_had_been_murdered') put: 'giulia_is_heart_broken'.
        factsMap at: ('karl', 'poker_is_played_here') put: 'ulrich_has_an_open_mouth'.
        factsMap at: ('karl', 'karl_cheats_at_poker') put: 'karl_trusts_me'.
        factsMap at: ('andreas', 'thomas_was_here_to_buy_a_watch') put: 'watch_was_originally_andreases'.
        factsMap at: ('andreas', 'andreas_was_here_yesterday') put: 'andreas_needs_money'.
        factsMap at: ('jonas', 'jonas_likes_drinking_in_company') put: 'jonas_went_to_bar'.
        factsMap at: ('hilda', 'zoe_was_thomas_lovers') put: 'zoe_did_not_know_about_giulia'.

        factsMap at: ('amy', 'thomas') put: 'zoe_was_thomas_lovers'.
        factsMap at: ('amy', 'zoe') put: 'zoe_was_thomas_lovers'.
        factsMap at: ('amy', 'karl') put: 'karl_likes_betting'.
        factsMap at: ('jurgen', 'karl') put: 'karl_likes_betting'.
        factsMap at: ('giulia', 'thomas') put: 'thomas_had_been_murdered'.
        factsMap at: ('hans', 'karl') put: 'karl_has_a_keen_eye'.
        factsMap at: ('hilda', 'karl') put: 'karl_has_a_keen_eye'.
        factsMap at: ('hilda', 'thomas') put: 'thomas_had_been_murdered'.
        factsMap at: ('hilda', 'zoe') put: 'zoe_befriended_hilda'.
        factsMap at: ('jonas', 'karl') put: 'karl_admitted_to_cheating'.
        factsMap at: ('karl', 'andreas') put: 'andreas_was_here_yesterday'.
        factsMap at: ('karl', 'jonas') put: 'jonas_likes_drinking_in_company'.
        factsMap at: ('stephan', 'jonas') put: 'jonas_likes_drinking_in_company'.
        factsMap at: ('stephan', 'karl') put: 'karl_hides_cards_in_his_sleeve'.
        factsMap at: ('urlich', 'karl') put: 'karl_cheats_at_poker'.

        dialogues := Dictionary new.
        dialogues at: ('hilda', 'brooch') put: 'Oh, this! I am so glad you asked! This is a present from my dad for my 19th birthday. Beautiful, is it not?'.
        dialogues at: ('urlich', 'gilded_epaulettes') put: 'Very fine epaulets, would not you say, dear Sir? Very fine, if I say so myself. I have won these beauties the last time I won anything in our little poker game downstairs. Oh, shoot! I should not have said that!'.
        dialogues at: ('urlich', 'watch') put: 'I saw that watch somewhere before! Is not this the watch that was on our table last game? Where did you find it?'.
        dialogues at: ('amy', 'watch') put: 'Hey, where did you get that thing?! That is mine. I have won it fair and square last night.'.
        dialogues at: ('hilda', 'cigarette_light') put: 'Oh, that looks like the cigarettes I smoke. Where did you get it? Thomases room? How did it get there? I was not there since yesterdays cleaning, and I sure as hell did not leave no cigarette there!.'.
        dialogues at: ('amy', 'cigarette_light') put: 'Did not know you were such a softie, detective. Wanna know what a real cigarette looks like? *shows a standard cigarette*'.
        dialogues at: ('karl', 'clubs_symbol') put: 'I do like playing clubs, they are quite unobvious... I could rant about why it is so, but you probably do not even know about what game I am talking about, so I will not bother.'.
        dialogues at: ('zoe', 'sleeping_pills') put: 'I have had trouble sleeping lately, so this helps.'.
        dialogues at: ('giulia', 'thomas_journal') put: 'What is this? That belonged to Thomas? I did not know he had it.'.
        dialogues at: ('hilda', 'thomas_journal') put: 'Is that... Norwegian? Show it to me, I learned it a bit. ...well, there is something written here about a watch, and that he came here because he wanted to buy it. Whose is this journal anyway?'.
        dialogues at: ('zoe', 'sleep_mask') put: 'I still have jet lag. How can anyone sleep here when it is so early and it is so bright?'.
        dialogues at: ('zoe', 'glass') put: 'We had a great party yesterday! I still feel that.'.
        dialogues at: ('karl', 'glass') put: 'Thanks for that. I need to clean all of them before night. Do not get me wrong. I like my work at the bar, but people are so messy.'.
        dialogues at: ('hilda', 'cleaning_stuff') put: 'Maybe I can do something for you? Only tell me where, and I do my best to clean that place.'.
        dialogues at: ('jurgen', 'cutlery_tray') put: 'Anything for you, sir? At 4th o clock I will bring dinner to your room.'.
        dialogues at: ('hans', 'guest_book') put: 'I do my best to keep the hotel papers in order. Here I save all information about visitors.'.
        dialogues at: ('hans', 'telephone') put: 'The phone stopped ringing since the media heard about the murder. I hope you find the murderer soon.'.
        dialogues at: ('hermann', 'ball') put: 'Who likes to play with the ball? My little boy. *huggling the dog*'.
        dialogues at: ('hermann', 'hunting_weapon') put: 'I hunted such an enormous deer yesterday. I love these forests.'.
        dialogues at: ('jonas', 'bush') put: 'This is a great bush. Yesterday after the party, he helped me stay straight. The most comfortable bush I have ever slept under.'.
        dialogues at: ('urlich', 'bush') put: 'See how nicely trimmed? I take care of them myself. I spend a large part of my life in this bush waiting for visitors.'.
        dialogues at: ('hermann', 'bullets') put: '30-caliber. Where did you get them? Are they not mine?'.
        dialogues at: ('hermann', 'knife_scabbard') put: 'My familys coat of arms is on the scabbard. This knife was given to me by my father. He was a hunter too.'.
        dialogues at: ('hermann', 'blooded_knife') put: 'Oh, someone finally found it! After taking the deer to the kitchen, the knife was lost. I thought it had fallen on the way back.'.
        dialogues at: ('theodor', 'deer') put: 'Hermann brought it yesterday. Poor animal, but it will not be wasted. There will be a delicious stew tomorrow. I hope you will stay.'.
        dialogues at: ('theodor', 'broth') put: 'Do you really like my soup? I know that you are a great men.'.
        
        dialogues at: ('amy', 'watch_has_changed_hands_during_last_game') put: 'Yeah, I won the game last night, and the watch too. I think I passed out and lost it when I was returning to my room last night. I mean, I drank a bit, but not more than usual, and normally I do not even feel drunk, let alone pass out. The weirdest feeling. But I swear, when I woke up the next day the thing was gone!'.
        dialogues at: ('amy', 'karl_cheats_at_poker') put: 'That scoundrel! Thanks for letting me know, mate. I will keep an eye on him next time.'.
        dialogues at: ('hilda', 'thomas_had_been_murdered') put: 'I do not really know anything about this, but... I do know that he has been playing poker with some other people here. Maybe something went wrong there?'.
        dialogues at: ('hilda', 'watch_has_changed_hands_during_last_game') put: 'Oh yeah, Amy won the watch yesterday. That watch surely must have cost a lot. I was so shocked when it appear on the table. When I told this to Zoe, she also could not believe this.'.
        dialogues at: ('giulia', 'thomas_had_been_murdered') put: 'What I am suppose to do? Is he trully dead? He cannot be. He promised. I want him back...'.
        dialogues at: ('karl', 'poker_is_played_here') put: 'Who told you - it was Urlich, was it not? He never could keep his mouth shut. Yes, we do like to play some poker around here, at different stakes. Since you already know about it, maybe you would like to give it a try?'.
        dialogues at: ('karl', 'karl_cheats_at_poker') put: 'So, you are a pretty good detective, are you not? Well, you got me. I will tell you what you want.'.
        dialogues at: ('andreas', 'thomas_was_here_to_buy_a_watch') put: 'Looks like nothing is a secret to you, huh? Yes, this watch was mine and yes, I wanted to sell it, but I found out that Thomas was the buyer and I just could not let him know that I am penniless just like that. And I certainly did not kill him!'.
        dialogues at: ('andreas', 'andreas_was_here_yesterday') put: 'Yes, I was here. I am sorry that I lied to you earlier. I really do not like anyone noticing that my life is not as great as I want people to see it. I was here, because I wanted to sell my watch to get some money. I really need them right now. I found out that Thomas was the buyer just yesterday, and I could not bear the fact that he would know. So I tried my luck in cards, and obviously, I lost it.'.
        dialogues at: ('jonas', 'jonas_likes_drinking_in_company') put: 'Of course I love drinking in good company, who does not? By the way, it is about the best time to go drinking together! I will go save a table for us in the bar and you go get the drinks!'.
        dialogues at: ('hilda', 'zoe_was_thomas_lovers') put: 'WHAT?! Her? No way! You are serious?! She told me she did not even knew who they were! That is... interesting to say the least.'.

        factsMap at: ('amy', 'thomas') put: 'Well, I once was his girlfriend. Probably will not be again, will I? Hahah. Oh, do not look at me like that, he always wanted to have everything, that is how people like him end. But damn, he looked good. If you ask me, I bet it has something to do with that Zoe girl and Giulia. Him and Zoe looked like they have done something that Giulia might not have liked...'.
        factsMap at: ('amy', 'zoe') put: 'Oh, yeah, her. Not much I can say about her except that she most probably slept with Thomas. They were all drooly towards each other.'.
        factsMap at: ('amy', 'karl') put: 'Karl is a nice guy, but he will not tell you any important information, unless... Unless you bet him. He is a gambler, he can not refuse a good bet.'.
        factsMap at: ('jurgen', 'karl') put: 'Karl does not trust almost anyone, but I will give you an advice - if you want to get some important information from him, try betting him instead of asking straight forward.'.
        factsMap at: ('giulia', 'thomas') put: 'As soon as you mention Thomases name, Giulia starts crying again...'.
        factsMap at: ('hans', 'karl') put: 'Karl has a keen eye, nothing slips past him. He probably can help you.'.
        factsMap at: ('hilda', 'karl') put: 'Karl has a keen eye, nothing slips past him. He probably can help you.'.
        factsMap at: ('hilda', 'thomas') put: 'Poor Thomas... I knew him a bit - he used to work here with his good friend back when he was younger. I wonder if their friendship survived through all this time...'.
        factsMap at: ('hilda', 'zoe') put: 'Well, it was at hard at the beginning, but once you get to know her, she is a really sweet and nice person. We talked quite a lot lately.'.
        factsMap at: ('jonas', 'karl') put: 'This guy is a crook, I will tell you that! *hic!* We were drinking together once and he told me *hic!* that he is cheating all the time! He was so proud that no one can catch him red-handed. *hic!* But I promised that I would not tell anyone, so you have to promise me, too! *hic!*'.
        factsMap at: ('karl', 'andreas') put: 'About that one I can say something, I heard he told you that he came here today, while in fact, he was playing with us yesterday.'.
        factsMap at: ('karl', 'jonas') put: 'This guy, I do not remember how many times I have seen him drinking here, but he rarely pays for his drinks - usually he sits with some random people, tells silly jokes, chit chats a bit and gets his drinks from these people completely for free. I have seen him perfecting this technique for months now...'.
        factsMap at: ('stephan', 'jonas') put: 'Ah yes, this funny boy. I drank with him a couple of times. He iss the funniest person in the whole hotel and the best drinking buddy - that is why I buy him a drink from time to time.'.
        factsMap at: ('stephan', 'karl') put: 'If I were you, I would not play any card game with this guy. When I ordered a drink one day, I saw a playing card sliding out of his sleeve!'.
        factsMap at: ('urlich', 'karl') put: 'Is no one around? Fine. If Sir really wants to know, I believe karl is cheating during our little poker games! I saw him once playing an ace of clubs - the exact same one I had in my hand!'.
    ]

    talkAbout: object [
        | condition discoveredFact message |
        talkingTo = 'nobody' ifTrue: [
            ^ #('You try talking to your new imaginary friend, but she/he is not responding.' '')
        ].
        (prerequisites keys occurrencesOf: (talkingTo, object)) > 0
        ifTrue: [
            condition := (prerequisites at: (talkingTo, object)) value.
        ]
        ifFalse: [
            condition := true.
        ].
        condition
        ifTrue: [
            discoveredFact := factsMap at: (talkingTo, object).
            message := { dialogues at: (talkingTo, object) }.
            (knownFacts occurrencesOf: discoveredFact) = 0
            ifTrue: [
                knownFacts := knownFacts, { discoveredFact }.
                message := message, #('NEW FACT ADDED').
            ].
            ^ message, #('')
        ]
        ifFalse: [
            ^ #('Maybe I know something about it, or maybe I do not...' '')
        ]
    ]

    tellAbout: fact [
        ((knownFacts occurrencesOf: fact) > 0)
        ifTrue: [
            ^ (self talkAbout: fact).
        ]
        ifFalse: [
            ^ #('So... what I am suppose to do with that information?' '').
        ].
    ]

    askAbout: item [
        ^ (self talkAbout: item)
    ]

    gossipAbout: person [
        ^ (self talkAbout: person)
    ]

    journal [
        ^ (knownFacts asOrderedCollection), #('')
    ]

    go [
        talkingTo := 'nobody'
    ]

    isTalkingNow [
        ^ talkingTo ~= 'nobody'
    ]

    talkingTo [
        ^ talkingTo
    ]

    talk: person [
        talkingTo := person.
        ^ { 'You are talking to ', person. '' }
    ]
]

Object subclass: Game [
    | isGameOver locations dialogues |

    introductionText := #(
        'Daily life in the alpine hotel was disrupted by the barking of the Promyczek. The wife, which came after that to the room, saw her husband on the floor. She started screaming and woke up everyone who stayed in the hotel. The hotel owner immediately calls you to solve that riddle. Who is the murderer? You do not know. Yet...'
    ).

    helpText := #(
        'Available commands are:'
        ''
        'w a s d       -- to go in that direction.'
        'take Item     -- to pick up an Item.'
        'talk Person   -- to approach a Person.'
        'look / l      -- to look at people around you.'
        'notice / n    -- to notice things around you.'
        'journal / j   -- to list all known facts.'
        'inventory / i -- to list all taken things.'
        'help / h      -- to see this message again.'
        'quit          -- to end the game and quit.'
        ''
    ).

    dialogueHelpText := #(
        'Available commands are:'
        ''
        'w a s d       -- to go in that direction.'
        'askAbout Item -- to ask about an item.'
        'tellAbout Fact -- to tell about a fact.'
        'gossipAbout Person -- to gossip about a person.'
        'look / l      -- to look at people around you.'
        'notice / n    -- to notice things around you.'
        'journal / j   -- to list all known facts.'
        'inventory / i -- to list all taken things.'
        'help / h      -- to see this message again.'
        'quit          -- to end the game and quit.'
        ''
    ).
 
    Game class >> new [
        | game |
        game := super new.
        game init.
        ^ game
    ]

    init [
        isGameOver := false.
        locations := Locations new.
        dialogues := Dialogues new.
    ]

    write: array [
        array do: [ :string | Transcript show: string; cr ]
    ]

    help [
        dialogues isTalkingNow
        ifTrue: [
            self write: dialogueHelpText.
        ]
        ifFalse: [
            self write: helpText.
        ]
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    run [
        | cmd args isUnknown |

        self write: introductionText.
        self help.

        [isGameOver] whileFalse: [

            isUnknown := true.
            args := (self readCommand) substrings.
            cmd := args first.

            ((cmd = 'help') | (cmd = 'h')) ifTrue: [
                self help.
                isUnknown := false
            ].

            cmd = 'w' ifTrue: [
                self write: (locations go: 'north').
                dialogues go.
                isUnknown := false
            ].

            cmd = 'd' ifTrue: [
                self write: (locations go: 'east').
                dialogues go.
                isUnknown := false
            ].

            cmd = 'a' ifTrue: [
                self write: (locations go: 'west').
                dialogues go.
                isUnknown := false
            ].

            cmd = 's' ifTrue: [
                self write: (locations go: 'south').
                dialogues go.
                isUnknown := false
            ].

            ((cmd = 'look') | (cmd = 'l')) ifTrue: [
                self write: (locations look).
                isUnknown := false
            ].

            ((cmd = 'notice') | (cmd = 'n')) ifTrue: [
                self write: (locations notice).
                isUnknown := false
            ].

            ((cmd = 'journal') | (cmd = 'j')) ifTrue: [
                self write: (dialogues journal).
                isUnknown := false
            ].

            ((cmd = 'inventory') | (cmd = 'i')) ifTrue: [
                self write: (locations inventory).
                isUnknown := false
            ].

            cmd = 'take' ifTrue: [
                self write: (locations take: (args last)).
                isUnknown := false
            ].

            cmd = 'talk' ifTrue: [
                (locations isNear: (args last))
                ifTrue: [
                    self write: (dialogues talk: (args last)), (locations trinkets: (args last))
                ]
                ifFalse: [
                    self write: {'You start to formulate your sentence towards ', (args last), ', when suddenly you realise, that she/he cannot hear you, for she/he is not here.'. ''}
                ].
                isUnknown := false
            ].

            cmd = 'askAbout' ifTrue: [
                (locations isItem: (args last) on: (dialogues talkingTo))
                ifTrue: [
                    self write: (dialogues askAbout: (args last))
                ]
                ifFalse: [
                    self write: {'A/An ', (args last), '? Never heard of it. Maybe if you would show me I could tell you something about it.'. ''}
                ].
                isUnknown := false
            ].

            cmd = 'tellAbout' ifTrue: [
                self write: (dialogues tellAbout: (args last)).
                isUnknown := false
            ].

            cmd = 'gossipAbout' ifTrue: [
                self write: (dialogues gossipAbout: (args last)).
                isUnknown := false
            ].

            cmd = 'quit' ifTrue: [
                isGameOver := true.
                isUnknown := false
            ].
                
            isUnknown ifTrue: [
                self write: #('Unknown command.' '')
            ]
        ]
    ]
]

Game new run.