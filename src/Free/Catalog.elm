module Free.Catalog exposing
    ( Entry
    , Tab(..)
    , entriesFor
    , tabLabel
    , tabs
    )

{-| The curated dock of free building: common bricks, plates, slopes and tiles,
SNOT and microscale parts, the Technic parts of `Technic.Catalog` and vehicle
parts. Every entry is unlimited. The dock shows one tab at a time, and the
colour row (in `Main`) sets the colour of every part added from it.

Each file is a visible part of brick-catalog's `data/curated-parts.json`, and
each has a hand-made `LDraw.Connectivity.knownCatalogPart` entry in brick-studio.
New entries carry the catalog name; older entries keep their short dock names
(tests pin them). Suffixed variants (3040b, 3069b, 3070b, 3068b, 3023b) are the
ones the catalog lists under those numbers.

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
    | TilesTab
    | BracketsTab
    | MicroscaleTab
    | TechnicTab
    | VehiclesTab


{-| The tabs in dock order.
-}
tabs : List Tab
tabs =
    [ BricksTab, PlatesTab, SlopesTab, TilesTab, BracketsTab, MicroscaleTab, TechnicTab, VehiclesTab ]


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
            "Slopes"

        TilesTab ->
            "Tiles"

        BracketsTab ->
            "SNOT"

        MicroscaleTab ->
            "Microscale"

        TechnicTab ->
            "Technic"

        VehiclesTab ->
            "Vehicles"


{-| The entries a tab lists, in order.
-}
entriesFor : Tab -> List Entry
entriesFor tab =
    case tab of
        BricksTab ->
            [ entry "3005.dat" "Brick 1 x 1" "1 x 1 brick"
            , entry "3004.dat" "Brick 1 x 2" "1 x 2 brick"
            , entry "3062b.dat" "Brick 1 x 1 Round with Hollow Stud" "1 x 1 round brick"
            , entry "3622.dat" "Brick 1 x 3" "1 x 3 brick"
            , entry "3010.dat" "Brick 1 x 4" "1 x 4 brick"
            , entry "3009.dat" "Brick 1 x 6" "1 x 6 brick"
            , entry "3003.dat" "Brick 2 x 2" "2 x 2 brick"
            , entry "3941.dat" "Brick 2 x 2 Round without Reinforcement" "2 x 2 round brick"
            , entry "3002.dat" "Brick 2 x 3" "2 x 3 brick"
            , entry "3001.dat" "Brick 2 x 4" "2 x 4 brick"
            , entry "3007.dat" "Brick 2 x 8" "2 x 8 brick"
            , entry "6541.dat" "Technic Brick 1 x 1 with Hole" "1 x 1 brick with hole"
            , entry "3700.dat" "Technic Brick 1 x 2 with Hole" "1 x 2 technic brick with hole"
            , entry "3701.dat" "Technic Brick 1 x 4 with Holes" "1 x 4 technic brick with holes"
            , entry "3894.dat" "Technic Brick 1 x 6 with Holes" "1 x 6 technic brick with holes"
            , entry "3702.dat" "Technic Brick 1 x 8 with Holes" "1 x 8 technic brick with holes"
            , entry "3703.dat" "Technic Brick 1 x 16 with Holes" "1 x 16 technic brick with holes"
            ]

        PlatesTab ->
            [ entry "3024.dat" "Plate 1 x 1" "1 x 1 plate"
            , entry "6141.dat" "Plate 1 x 1 Round" "1 x 1 round plate"
            , entry "85861.dat" "Plate 1 x 1 Round with Open Stud" "1 x 1 round plate with open stud"
            , entry "3023b.dat" "Plate 1 x 2" "1 x 2 plate"
            , entry "15573.dat" "Plate 1 x 2 with 1 Stud" "1 x 2 jumper plate"
            , entry "3623.dat" "Plate 1 x 3" "1 x 3 plate"
            , entry "3710.dat" "Plate 1 x 4" "1 x 4 plate"
            , entry "3666.dat" "Plate 1 x 6" "1 x 6 plate"
            , entry "3460.dat" "Plate 1 x 8" "1 x 8 plate"
            , entry "3022.dat" "Plate 2 x 2" "2 x 2 plate"
            , entry "4032b.dat" "Plate 2 x 2 Round with Reduced Axlehole" "2 x 2 round plate"
            , entry "87580.dat" "Plate 2 x 2 with 1 Center Stud" "2 x 2 jumper plate"
            , entry "3021.dat" "Plate 2 x 3" "2 x 3 plate"
            , entry "3020.dat" "Plate 2 x 4" "2 x 4 plate"
            , entry "3795.dat" "Plate 2 x 6" "2 x 6 plate"
            , entry "3034.dat" "Plate 2 x 8" "2 x 8 plate"
            , entry "3031.dat" "Plate 4 x 4" "4 x 4 plate"
            , entry "3958.dat" "Plate 6 x 6" "6 x 6 plate"
            , entry "41539.dat" "Plate 8 x 8" "8 x 8 plate"
            , entry "3709b.dat" "Technic Plate 2 x 4 with Holes" "2 x 4 technic plate with holes"
            , entry "3867.dat" "Baseplate 16 x 16" "16 x 16 baseplate"
            ]

        SlopesTab ->
            [ entry "3040b.dat" "Slope 45° 2 x 1" "45° slope, 2 x 1"
            , entry "3039.dat" "Slope 45° 2 x 2" "45° slope, 2 x 2"
            , entry "3038.dat" "Slope 45° 2 x 3" "45° slope, 2 x 3"
            , entry "3037.dat" "Slope 45° 2 x 4" "45° slope, 2 x 4"
            , entry "3298.dat" "Slope 33° 3 x 2" "33° slope, 3 x 2"
            , entry "60481a.dat" "Slope Brick 65 2 x 1 x 2 with Symmetric Stud Holder" "65° slope, 2 x 1 x 2"
            , entry "35464.dat" "Slope Brick 45 1 x 1 Double" "45° double slope, 1 x 1"
            , entry "54200.dat" "Slope 31° 1 x 1 x 2/3" "1 x 1 cheese slope"
            , entry "85984.dat" "Slope Brick 31 1 x 2 x 0.667" "1 x 2 cheese slope"
            , entry "11477.dat" "Slope Brick Curved 2 x 1" "2 x 1 curved slope"
            , entry "15068.dat" "Slope Brick Curved 2 x 2 x 0.667" "2 x 2 curved slope"
            , entry "50950.dat" "Slope Brick Curved 3 x 1" "3 x 1 curved slope"
            , entry "61678.dat" "Slope Brick Curved 4 x 1" "4 x 1 curved slope"
            , entry "93273.dat" "Slope Brick Curved 4 x 1 Double" "4 x 1 double curved slope"
            , entry "3665b.dat" "Slope Brick 45 2 x 1 Inverted with Inner Stopper Ring" "45° inverted slope, 2 x 1"
            , entry "3660b.dat" "Slope Brick 45 2 x 2 Inverted with Flat Bottom Pin with Inner Stopper Ring" "45° inverted slope, 2 x 2"
            , entry "3747b.dat" "Slope Brick 33 3 x 2 Inverted with Ribs between Studs" "33° inverted slope, 3 x 2"
            , entry "24201.dat" "Slope Brick Curved 2 x 1 Inverted" "2 x 1 inverted curved slope"
            ]

        TilesTab ->
            [ entry "3070b.dat" "Tile 1 x 1" "1 x 1 tile with groove"
            , entry "98138.dat" "Tile 1 x 1 Round" "1 x 1 round tile"
            , entry "25269.dat" "Tile 1 x 1 Round Quarter" "1 x 1 quarter round tile"
            , entry "24246.dat" "Tile 1 x 1 with Rounded End" "1 x 1 half round tile"
            , entry "39739.dat" "Tile 1 x 1 Heart" "1 x 1 heart tile"
            , entry "3069b.dat" "Tile 1 x 2" "1 x 2 tile with groove"
            , entry "2412b.dat" "Tile 1 x 2 Grille" "1 x 2 grille tile"
            , entry "63864.dat" "Tile 1 x 3" "1 x 3 tile"
            , entry "3068b.dat" "Tile 2 x 2" "2 x 2 tile with groove"
            , entry "87079.dat" "Tile 2 x 4" "2 x 4 tile"
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
            , entry "4085c.dat" "Plate 1 x 1 with Clip Vertical (Thick U-Clip)" "1 x 1 plate with vertical clip"
            ]

        MicroscaleTab ->
            [ entry "60592.dat" "Window 1 x 2 x 2 without Sill" "1 x 2 x 2 window frame"
            , entry "98283.dat" "Brick 1 x 2 with Embossed Bricks" "1 x 2 masonry brick"
            , entry "30136.dat" "Brick 1 x 2 Log" "1 x 2 log brick"
            , entry "4490.dat" "Arch 1 x 3" "1 x 3 arch"
            , entry "3659.dat" "Arch 1 x 4" "1 x 4 arch"
            , entry "87552.dat" "Panel 1 x 2 x 2 Reinforced" "1 x 2 x 2 panel"
            , entry "4865b.dat" "Panel 1 x 2 x 1 with Rounded Corners" "1 x 2 x 1 panel"
            , entry "3633.dat" "Fence Lattice 1 x 4 x 1" "1 x 4 lattice fence"
            , entry "4589.dat" "Cone 1 x 1" "1 x 1 cone"
            , entry "3942c.dat" "Cone 2 x 2 x 2 with Hollow Stud Open" "2 x 2 x 2 cone"
            , entry "4740.dat" "Dish 2 x 2 Inverted" "2 x 2 inverted dish"
            , entry "43888.dat" "Brick 1 x 1 x 6 Round with Square Base" "1 x 1 x 6 round pillar"
            , entry "87994.dat" "Bar 3L" "3L bar"
            , entry "30374.dat" "Bar 4L Lightsaber Blade" "4L bar"
            , entry "32828.dat" "Bar 1L with 1 x 1 Round Plate with Hollow Stud" "1L bar with round plate / cannon"
            , entry "3957a.dat" "Antenna 4H with Rounded Top" "4H antenna"
            , entry "61252.dat" "Plate 1 x 1 with Clip Horizontal (Thick C-Clip)" "1 x 1 plate with horizontal clip"
            , entry "15070.dat" "Plate 1 x 1 with Tooth Perpendicular" "1 x 1 plate with tooth"
            , entry "33291.dat" "Plate 1 x 1 Round with Tabs" "1 x 1 flower plate"
            , entry "32607.dat" "Plate 1 x 1 Round with 3 Leaves" "1 x 1 plant leaves"
            , entry "24866.dat" "Plate 1 x 1 with 5 Petals" "1 x 1 petal plate"
            , entry "2423.dat" "Plant Leaves 4 x 3" "4 x 3 plant leaves"
            , entry "2877.dat" "Brick 1 x 2 with Grille" "1 x 2 grille brick"
            ]

        TechnicTab ->
            List.map
                (\part -> entry part.file part.name part.description)
                Technic.curatedParts

        VehiclesTab ->
            [ entry "4600.dat" "Plate 2 x 2 with 2 Wheel Pins" "2 x 2 wheel holder"
            , entry "4488.dat" "Plate 2 x 2 with 1 Wheel Pin" "2 x 2 wheel holder with pin"
            , entry "24326.dat" "Car Base 4 x 4 with 4 Wheel Pins" "4 x 4 car base with wheel pins"
            , entry "4624.dat" "Wheel Rim 8mm D. x 6mm" "Small wheel rim"
            , entry "3641.dat" "Tyre 15mm D. x 6mm" "Small tire"
            , entry "6014b.dat" "Wheel Rim 11mm D. x 12mm" "11 x 12 wheel rim"
            , entry "6015.dat" "Tyre 21mm D. x 12mm Wide" "21 x 12 wide tire"
            , entry "56890.dat" "Tyre 24mm D. x 14mm Balloon" "24 x 14 balloon tire"
            , entry "42610.dat" "Technic Wheel Rim 11mm D. x 20mm" "11 x 20 technic wheel rim"
            , entry "50951.dat" "Tyre 16mm D. x 6mm" "16 x 6 tire"
            , entry "55982.dat" "Wheel Rim 18mm D. x 14mm with Axle Hole" "18 x 14 wheel rim with axle hole"
            , entry "55981.dat" "Wheel Rim 18mm D. x 14mm with Pin Hole" "18 x 14 wheel rim with pin hole"
            , entry "56891.dat" "Tyre 37mm D. x 18mm Off-Road" "37 x 18 off-road tire"
            , entry "56145.dat" "Wheel Rim 30mm D. x 20mm with 6 Spokes" "30 x 20 wheel rim"
            , entry "44309.dat" "Tyre 43mm D. x 22mm" "43 x 22 tire"
            , entry "50745.dat" "Car Mudguard 4 x 2.5" "4 x 2.5 mudguard"
            , entry "1846.dat" "Car Mudguard 4 x 3 x 1.667" "4 x 3 mudguard"
            , entry "2352.dat" "Windscreen 2 x 4 x 3" "2 x 4 x 3 windscreen"
            , entry "3823.dat" "Windscreen 2 x 4 x 2" "2 x 4 x 2 windscreen"
            , entry "3829c01.dat" "Car Steering Stand and Wheel" "Car steering stand and wheel"
            , entry "4079.dat" "Minifig Seat 2 x 2" "2 x 2 minifig seat"
            , entry "3679.dat" "Turntable 2 x 2 Plate Base" "2 x 2 turntable base"
            , entry "3680.dat" "Turntable 2 x 2 Plate Top" "2 x 2 turntable top"
            ]


entry : String -> String -> String -> Entry
entry file name description =
    { file = file, name = name, description = description }
