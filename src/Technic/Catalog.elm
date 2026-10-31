module Technic.Catalog exposing
    ( Category(..)
    , PartItem
    , categoryLabel
    , curatedParts
    , partsForCategory
    )

{-| Curated catalog of Technic parts: gears, beams, axles and pins, in
categories. Free building offers them in its Technic tab (`Free.Catalog`), and
the Technic placement rules use their categories (`Technic.Placement`).
-}


{-| Curated part item definition.
-}
type alias PartItem =
    { name : String
    , file : String
    , color : Int
    , category : Category
    , description : String
    }


{-| Technic part categories.
-}
type Category
    = GearsCategory
    | BeamsCategory
    | AxlesCategory
    | PinsCategory


{-| Human-readable label for a category.
-}
categoryLabel : Category -> String
categoryLabel category =
    case category of
        GearsCategory ->
            "Gears"

        BeamsCategory ->
            "Beams"

        AxlesCategory ->
            "Axles"

        PinsCategory ->
            "Pins & Bushings"


{-| The curated list of Technic parts.
-}
curatedParts : List PartItem
curatedParts =
    [ -- Gears
      { name = "8T Gear"
      , file = "3647.dat"
      , color = 7
      , category = GearsCategory
      , description = "8-tooth spur gear"
      }
    , { name = "16T Gear"
      , file = "4019.dat"
      , color = 7
      , category = GearsCategory
      , description = "16-tooth spur gear"
      }
    , { name = "24T Gear"
      , file = "3648.dat"
      , color = 7
      , category = GearsCategory
      , description = "24-tooth spur gear"
      }
    , { name = "40T Gear"
      , file = "3649.dat"
      , color = 7
      , category = GearsCategory
      , description = "40-tooth spur gear"
      }

    -- Beams
    , { name = "Beam 1x5"
      , file = "32316.dat"
      , color = 1
      , category = BeamsCategory
      , description = "1x5 Technic liftarm"
      }
    , { name = "Beam 1x7"
      , file = "32524.dat"
      , color = 1
      , category = BeamsCategory
      , description = "1x7 Technic liftarm"
      }
    , { name = "Beam 1x9"
      , file = "40490.dat"
      , color = 1
      , category = BeamsCategory
      , description = "1x9 Technic liftarm"
      }

    -- Axles
    , { name = "Axle 2"
      , file = "3704.dat"
      , color = 4
      , category = AxlesCategory
      , description = "Length 2 axle (Red)"
      }
    , { name = "Axle 4"
      , file = "3705.dat"
      , color = 0
      , category = AxlesCategory
      , description = "Length 4 axle (Black)"
      }
    , { name = "Axle 6"
      , file = "3706.dat"
      , color = 0
      , category = AxlesCategory
      , description = "Length 6 axle (Black)"
      }
    , { name = "Axle 8"
      , file = "3707.dat"
      , color = 0
      , category = AxlesCategory
      , description = "Length 8 axle (Black)"
      }

    -- Pins & Bushings
    , { name = "Bushing"
      , file = "6590.dat"
      , color = 7
      , category = PinsCategory
      , description = "Technic half/full bush"
      }
    , { name = "Pin with Friction"
      , file = "2780.dat"
      , color = 0
      , category = PinsCategory
      , description = "Technic connector pin"
      }
    , { name = "Pin without Friction"
      , file = "3673.dat"
      , color = 7
      , category = PinsCategory
      , description = "Smooth 2L pin for spinning gears"
      }
    , { name = "Pin 3L with Friction"
      , file = "6558.dat"
      , color = 1
      , category = PinsCategory
      , description = "Long 3L friction pin"
      }
    , { name = "Axle 1L with Pin"
      , file = "43093.dat"
      , color = 1
      , category = PinsCategory
      , description = "Axle-pin connector with friction"
      }
    , { name = "Pin 1/2 with Stud"
      , file = "4274.dat"
      , color = 7
      , category = PinsCategory
      , description = "Half pin with stud"
      }
    ]


{-| Filter curated parts by category.
-}
partsForCategory : Category -> List PartItem
partsForCategory category =
    List.filter (\p -> p.category == category) curatedParts
