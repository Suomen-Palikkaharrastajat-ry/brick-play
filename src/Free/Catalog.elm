module Free.Catalog exposing
    ( Entry
    , Tab(..)
    , entriesFor
    , tabLabel
    , tabs
    )

{-| The curated dock of free building: common bricks, plates, slopes and tiles,
and the Technic parts of `Technic.Catalog`. Every entry is unlimited. The dock
shows one tab at a time, and the colour row (in `Main`) sets the colour of every
part added from it.

Each file was checked against `viewer-app/public/ldraw/parts-index.json`, so its
name and description match the studio's part search. Suffixed variants (3040b,
3069b, 3070b, 3068b, 3023b) are the ones the index lists under those numbers.

-}

import Technic.Catalog as Technic


{-| A dock entry: the part file, its readable name, and a short description
for the tooltip.
-}
type alias Entry =
    { file : String
    , name : String
    , description : String
    }


{-| A dock tab.
-}
type Tab
    = BricksTab
    | PlatesTab
    | SlopesTab
    | BracketsTab
    | TechnicTab


{-| The tabs in dock order.
-}
tabs : List Tab
tabs =
    [ BricksTab, PlatesTab, SlopesTab, BracketsTab, TechnicTab ]


{-| The label of a dock tab.
-}
tabLabel : Tab -> String
tabLabel tab =
    case tab of
        BricksTab ->
            "Bricks"

        PlatesTab ->
            "Plates"

        SlopesTab ->
            "Slopes & tiles"

        BracketsTab ->
            "Brackets & SNOT"

        TechnicTab ->
            "Technic"


{-| The entries a tab lists, in order.
-}
entriesFor : Tab -> List Entry
entriesFor tab =
    case tab of
        BricksTab ->
            [ entry "3005.dat" "Brick 1 x 1" "1 x 1 brick"
            , entry "3004.dat" "Brick 1 x 2" "1 x 2 brick"
            , entry "3622.dat" "Brick 1 x 3" "1 x 3 brick"
            , entry "3010.dat" "Brick 1 x 4" "1 x 4 brick"
            , entry "3009.dat" "Brick 1 x 6" "1 x 6 brick"
            , entry "3003.dat" "Brick 2 x 2" "2 x 2 brick"
            , entry "3002.dat" "Brick 2 x 3" "2 x 3 brick"
            , entry "3001.dat" "Brick 2 x 4" "2 x 4 brick"
            , entry "3007.dat" "Brick 2 x 8" "2 x 8 brick"
            ]

        PlatesTab ->
            [ entry "3024.dat" "Plate 1 x 1" "1 x 1 plate"
            , entry "3023b.dat" "Plate 1 x 2" "1 x 2 plate"
            , entry "3710.dat" "Plate 1 x 4" "1 x 4 plate"
            , entry "3666.dat" "Plate 1 x 6" "1 x 6 plate"
            , entry "3460.dat" "Plate 1 x 8" "1 x 8 plate"
            , entry "3022.dat" "Plate 2 x 2" "2 x 2 plate"
            , entry "3021.dat" "Plate 2 x 3" "2 x 3 plate"
            , entry "3020.dat" "Plate 2 x 4" "2 x 4 plate"
            , entry "3795.dat" "Plate 2 x 6" "2 x 6 plate"
            , entry "3034.dat" "Plate 2 x 8" "2 x 8 plate"
            , entry "3031.dat" "Plate 4 x 4" "4 x 4 plate"
            ]

        SlopesTab ->
            [ entry "3040b.dat" "Slope 45° 2 x 1" "45° slope, 2 x 1"
            , entry "3039.dat" "Slope 45° 2 x 2" "45° slope, 2 x 2"
            , entry "3038.dat" "Slope 45° 2 x 3" "45° slope, 2 x 3"
            , entry "3037.dat" "Slope 45° 2 x 4" "45° slope, 2 x 4"
            , entry "3298.dat" "Slope 33° 3 x 2" "33° slope, 3 x 2"
            , entry "3070b.dat" "Tile 1 x 1" "1 x 1 tile with groove"
            , entry "3069b.dat" "Tile 1 x 2" "1 x 2 tile with groove"
            , entry "3068b.dat" "Tile 2 x 2" "2 x 2 tile with groove"
            , entry "2431.dat" "Tile 1 x 4" "1 x 4 tile with groove"
            , entry "6636.dat" "Tile 1 x 6" "1 x 6 tile"
            ]

        BracketsTab ->
            [ entry "87087.dat" "Brick 1 x 1 with Stud on 1 Side" "1 x 1 brick with side stud"
            , entry "4070.dat" "Brick 1 x 1 with Headlight" "1 x 1 headlight brick"
            , entry "26604.dat" "Brick 1 x 1 with Studs on 2 Sides" "1 x 1 brick with two side studs"
            , entry "4733.dat" "Brick 1 x 1 with Studs on 4 Sides" "1 x 1 brick with four side studs"
            , entry "11211.dat" "Brick 1 x 2 with Studs on Side" "1 x 2 brick with two side studs"
            , entry "30414.dat" "Brick 1 x 4 with Studs on Side" "1 x 4 brick with four side studs"
            , entry "32952.dat" "Brick 1 x 1 x 1 2/3 with Studs on 1 Side" "1 x 1 x 1 2/3 brick with two side studs"
            , entry "22885.dat" "Brick 1 x 2 x 1 2/3 with Studs on 1 Side" "1 x 2 x 1 2/3 brick with four side studs"
            , entry "36840.dat" "Bracket 1 x 1 - 1 x 1 Up" "1 x 1 up bracket"
            , entry "36841.dat" "Bracket 1 x 1 - 1 x 1 Down" "1 x 1 down bracket"
            , entry "99781.dat" "Bracket 1 x 2 - 1 x 2 Down" "1 x 2 down bracket"
            , entry "99780.dat" "Bracket 1 x 2 - 1 x 2 Up" "1 x 2 up bracket"
            , entry "99207.dat" "Bracket 1 x 2 - 2 x 2 Up" "1 x 2 - 2 x 2 up bracket"
            , entry "44728.dat" "Bracket 1 x 2 - 2 x 2 Down" "1 x 2 down bracket"
            , entry "99206.dat" "Plate 2 x 2 x 2/3 with Two Studs On Side" "2 x 2 plate with two side studs"
            ]

        TechnicTab ->
            List.map
                (\part -> entry part.file part.name part.description)
                Technic.curatedParts


entry : String -> String -> String -> Entry
entry file name description =
    { file = file, name = name, description = description }
