module PlayTest exposing (suite)

{-| Unit tests for the Brick Play application.
-}

import Dict
import Expect
import Html exposing (Html)
import Html.Attributes as Attr
import I18n
import LDraw.Document as Document
import Main
    exposing
        ( Flags
        , Model
        , Msg(..)
        , Participant
        , dockOverlapWidth
        , dockPlacement
        , helpRows
        , init
        , kitItemName
        , parseReverseHostRoom
        , parseReverseParticipantRoom
        , remainingCount
        , shareHash
        , thumbnailRequests
        , update
        , view
        , viewToast
        )
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Set
import ShareModal exposing (ShareKind(..))
import Studio.Core as Core
import Studio.HashPayload as HashPayload
import Studio.Shortcuts exposing (TurnDirection(..))
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import UI.CameraGlobe as CameraGlobe
import UI.SnapshotModal as SnapshotModal
import Url
import Viewport


suite : Test
suite =
    describe "Brick Play"
        [ menuTests
        , snapshotTests
        , shareModalTests
        , sharingAndToastTests
        , liveSharingTests
        , reverseParticipantTests
        , reverseHostTests
        , touchTests
        , dockPlacementTests
        , test "initial inventory has 10 bricks available" <|
            \_ ->
                let
                    ( model, _ ) =
                        init testFlags
                in
                Expect.equal (remainingCount model) 10
        , test "adding a brick decrements inventory count" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        addFirst model0
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 9
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 1
                    ]
                    model1
        , test "Adding bricks stops at inventory limit of 10" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    model10 =
                        List.foldl (\_ m -> Tuple.first (addFirst m)) model0 (List.range 1 10)

                    model11 =
                        Tuple.first (addFirst model10)
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 0
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 10
                    ]
                    model11
        , test "Undo increments inventory count" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        addFirst model0

                    ( modelUndone, _ ) =
                        update Undo model1
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 10
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 0
                    ]
                    modelUndone
        , test "Delete restores inventory count" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        addFirst model0

                    -- model1 has the newly added brick selected
                    ( modelDeleted, _ ) =
                        update Delete model1
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 10
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 0
                    ]
                    modelDeleted
        , test "Clear restores inventory count to 10" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    model3 =
                        List.foldl (\_ m -> Tuple.first (addFirst m)) model0 (List.range 1 3)

                    ( modelCleared, _ ) =
                        update Clear model3
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 10
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 0
                    ]
                    modelCleared
        , test "no axis gizmo: arrows and rings are off from the start" <|
            \_ ->
                let
                    ( model, _ ) =
                        init testFlags
                in
                Expect.equal Core.NoGizmo model.studio.gizmoStyle
        , test "the action bar edits the selected brick, one undo step each" <|
            \_ ->
                let
                    ( added, _ ) =
                        init testFlags |> Tuple.first |> addFirst

                    matrixOf m =
                        Core.getPlacements m.studio |> List.map .matrix

                    run msg =
                        update msg added |> Tuple.first
                in
                Expect.all
                    [ \_ -> Expect.notEqual (matrixOf added) (matrixOf (run (TurnSelection TurnLeft)))
                    , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run (TurnSelection TurnRight)))
                    , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run (TurnSelection TipForward)))
                    , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run RaiseSelection))
                    , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run LowerSelection))
                    , \_ -> Expect.equal 0 (List.length (Core.getPlacements (run Delete).studio))
                    , \_ -> Expect.equal True (Core.canUndo (run RaiseSelection).studio)
                    , \_ -> Expect.equal (matrixOf added) (matrixOf (run RaiseSelection |> update Undo |> Tuple.first))
                    ]
                    ()
        , test "ToggleHelp opens and closes the help and sets the studio help flag" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        update ToggleHelp model0

                    ( model2, _ ) =
                        update ToggleHelp model1
                in
                Expect.all
                    [ \_ -> Expect.equal ( False, False ) ( model0.helpOpen, model0.studio.helpVisible )
                    , \_ -> Expect.equal ( True, True ) ( model1.helpOpen, model1.studio.helpVisible )
                    , \_ -> Expect.equal ( False, False ) ( model2.helpOpen, model2.studio.helpVisible )
                    ]
                    ()
        , test "WindowResized reaches the viewport so picking uses the real canvas size" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        update (WindowResized 705 813) model0
                in
                Expect.equal
                    ( ( 705, 813 ), ( 705, 813 ) )
                    ( ( model1.studio.viewport.width, model1.studio.viewport.height )
                    , ( model1.windowWidth, model1.windowHeight )
                    )
        , describe "Kit Initialization and Multi-item Inventory"
            [ test "initializes inventory from parts in hash" <|
                \_ ->
                    let
                        flags =
                            { testFlags | initialHash = "#parts=3001.dat:14:2,3002.dat:4:3" }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 2 (List.length m.inventory)
                        , \m -> Expect.equal 5 (remainingCount m)
                        ]
                        model
            , test "AddPart decrements only that part's inventory and respects limit" <|
                \_ ->
                    let
                        flags =
                            { testFlags | initialHash = "#parts=3001.dat:14:2,3002.dat:4:3" }

                        ( model0, _ ) =
                            init flags

                        item1 =
                            { part = "3001.dat", color = 14, count = 2 }

                        ( model1, _ ) =
                            update (AddPart item1) model0

                        ( model2, _ ) =
                            update (AddPart item1) model1

                        ( model3, _ ) =
                            update (AddPart item1) model2
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 (Main.remainingForItem model1 item1)
                        , \_ -> Expect.equal 0 (Main.remainingForItem model2 item1)
                        , \_ -> Expect.equal 0 (Main.remainingForItem model3 item1)
                        , \_ -> Expect.equal 2 (List.length (Core.getPlacements model3.studio))
                        ]
                        ()
            , test "pieces added from the kit never overlap; the first lands in the middle" <|
                \_ ->
                    let
                        flags =
                            { testFlags | initialHash = "#parts=3001.dat:19:3,3003.dat:4:2" }

                        ( model0, _ ) =
                            init flags

                        items =
                            [ { part = "3001.dat", color = 19, count = 3 }
                            , { part = "3001.dat", color = 19, count = 3 }
                            , { part = "3001.dat", color = 19, count = 3 }
                            , { part = "3003.dat", color = 4, count = 2 }
                            , { part = "3003.dat", color = 4, count = 2 }
                            ]

                        final =
                            List.foldl (\item m -> Tuple.first (update (AddPart item) m)) model0 items

                        placed =
                            Core.getPlacements final.studio
                                |> List.map (\p -> ( p.part, Mat4.transform p.matrix (vec3 0 0 0) ))

                        -- Plan-view half sizes: 3001 is 2 x 4 studs, 3003 2 x 2.
                        half part =
                            if part == "3001.dat" then
                                ( 40, 20 )

                            else
                                ( 20, 20 )

                        apart ( pa, a ) ( pb, b ) =
                            let
                                ( ax, az ) =
                                    half pa

                                ( bx, bz ) =
                                    half pb
                            in
                            abs (Vec3.getX a - Vec3.getX b) >= ax + bx - 0.5 || abs (Vec3.getZ a - Vec3.getZ b) >= az + bz - 0.5

                        pairs =
                            List.indexedMap (\i a -> List.indexedMap (\j b -> ( i < j, a, b )) placed) placed
                                |> List.concat
                                |> List.filter (\( keep, _, _ ) -> keep)
                    in
                    Expect.all
                        [ \_ -> Expect.equal 5 (List.length placed)
                        , \_ -> Expect.equal True (List.all (\( _, a, b ) -> apart a b) pairs)
                        , \_ ->
                            List.head placed
                                |> Maybe.map (\( _, p ) -> ( Vec3.getX p, Vec3.getZ p ))
                                |> Expect.equal (Just ( 0, 0 ))
                        ]
                        ()
            , test "initializes document when modelText is present in hash" <|
                \_ ->
                    let
                        flags =
                            { testFlags | initialHash = "#model=1%2014%200%200%200%201%200%200%200%201%200%200%200%201%203001.dat&parts=3001.dat:14:5" }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 1 (List.length (Core.getPlacements m.studio))
                        , \m -> Expect.equal 4 (remainingCount m)
                        ]
                        model
            , test "init with cottage example loads 4 placed parts and spare kit inventory" <|
                \_ ->
                    let
                        cottageModel =
                            "1 19 0 -24 0 1 0 0 0 1 0 0 0 1 3001.dat\n1 15 0 -72 10 1 0 0 0 1 0 0 0 1 60592.dat\n1 19 -30 -48 10 1 0 0 0 1 0 0 0 1 3005.dat\n1 19 30 -48 10 1 0 0 0 1 0 0 0 1 3005.dat"

                        cottageParts =
                            "3001.dat:19:3,3005.dat:19:6,3001.dat:14:2,3039.dat:4:4,60592.dat:15:1,3004.dat:19:4"

                        flags =
                            { testFlags | initialHash = "#model=" ++ Url.percentEncode cottageModel ++ "&parts=" ++ Url.percentEncode cottageParts }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 4 (List.length (Core.getPlacements m.studio))
                        , \m -> Expect.equal 6 (List.length m.inventory)
                        , \m -> Expect.equal (3 + 6 + 2 + 4 + 1 + 4 - 4) (remainingCount m)
                        ]
                        model
            , test "init with castle watchtower example loads 4 foundation blocks and kit" <|
                \_ ->
                    let
                        towerModel =
                            "1 71 -20 -24 -20 1 0 0 0 1 0 0 0 1 3003.dat\n1 71 20 -24 -20 1 0 0 0 1 0 0 0 1 3003.dat\n1 71 -20 -24 20 1 0 0 0 1 0 0 0 1 3003.dat\n1 71 20 -24 20 1 0 0 0 1 0 0 0 1 3003.dat"

                        towerParts =
                            "3003.dat:71:12,3005.dat:71:8,3005.dat:14:2,3024.dat:4:4"

                        flags =
                            { testFlags | initialHash = "#model=" ++ Url.percentEncode towerModel ++ "&parts=" ++ Url.percentEncode towerParts }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 4 (List.length (Core.getPlacements m.studio))
                        , \m -> Expect.equal 4 (List.length m.inventory)
                        , \m -> Expect.equal (12 + 8 + 2 + 4 - 4) (remainingCount m)
                        ]
                        model
            , test "init with arch bridge example loads 4 pier blocks and kit" <|
                \_ ->
                    let
                        bridgeModel =
                            "1 15 -40 -24 0 1 0 0 0 1 0 0 0 1 3003.dat\n1 15 -40 -48 0 1 0 0 0 1 0 0 0 1 3003.dat\n1 15 40 -24 0 1 0 0 0 1 0 0 0 1 3003.dat\n1 15 40 -48 0 1 0 0 0 1 0 0 0 1 3003.dat"

                        bridgeParts =
                            "3003.dat:15:4,3001.dat:14:3,3710.dat:1:4,3004.dat:4:4"

                        flags =
                            { testFlags | initialHash = "#model=" ++ Url.percentEncode bridgeModel ++ "&parts=" ++ Url.percentEncode bridgeParts }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 4 (List.length (Core.getPlacements m.studio))
                        , \m -> Expect.equal 4 (List.length m.inventory)
                        , \m -> Expect.equal (4 + 3 + 4 + 4 - 4) (remainingCount m)
                        ]
                        model
            ]
        , thumbnailTests
        , mouseTests
        ]


mouseTests : Test
mouseTests =
    let
        ( model0, _ ) =
            init testFlags

        send msg model =
            update (StudioMsg (Core.ViewportMsg msg)) model |> Tuple.first

        camera m =
            m.studio.viewport.camera

        rows =
            List.concatMap Tuple.second (helpRows model0)
    in
    describe "Mouse and ground"
        [ test "uses DragPartsOrPan with one-finger pan on touch (the globe orbits)" <|
            \_ -> Expect.equal (Core.DragPartsOrPan Core.OneFingerPans) model0.studio.dragMode
        , test "left-drag on empty canvas pans: the target moves, the angle does not" <|
            \_ ->
                let
                    pressed =
                        send (Viewport.MouseButtonDown 100 100 0 False False) model0

                    moved =
                        send (Viewport.MouseMove 180 140) pressed
                in
                Expect.all
                    [ \_ -> Expect.notEqual (camera pressed).target (camera moved).target
                    , \_ -> Expect.within (Expect.Absolute 1.0e-9) (camera pressed).azimuth (camera moved).azimuth
                    ]
                    ()
        , test "right-drag orbits" <|
            \_ ->
                let
                    pressed =
                        send (Viewport.MouseButtonDown 100 100 2 False False) model0

                    moved =
                        send (Viewport.MouseMove 180 140) pressed
                in
                Expect.greaterThan 1.0e-6 (abs ((camera pressed).azimuth - (camera moved).azimuth))
        , test "help lists drag-empty-space pan and right-drag orbit" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal True (List.member ( "Drag empty space", "Pan" ) rows)
                    , \_ -> Expect.equal True (List.member ( "Right-drag", "Orbit" ) rows)
                    ]
                    ()
        , test "a newly added brick rests on the ground (origin Y = -24)" <|
            \_ ->
                addFirst model0
                    |> Tuple.first
                    |> .studio
                    |> Core.getPlacements
                    |> List.head
                    |> Maybe.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0)))
                    |> Expect.equal (Just -24)
        ]


kitFlags : Flags
kitFlags =
    { testFlags | initialHash = "#parts=3001.dat:14:4,3004.dat:4:4,3001.dat:14:1,3001.dat:1:2" }


thumbnailTests : Test
thumbnailTests =
    describe "Kit thumbnails"
        [ test "init asks for one thumbnail per distinct part and colour, in kit order" <|
            \_ ->
                let
                    ( model, _ ) =
                        init kitFlags
                in
                thumbnailRequests (kitPairs model) model.thumbnails
                    |> List.map (\r -> ( r.file, r.color ))
                    |> Expect.equal [ ( "3001.dat", 14 ), ( "3004.dat", 4 ), ( "3001.dat", 1 ) ]
        , test "requests carry the part colour as rgb from the colour table" <|
            \_ ->
                let
                    ( model, _ ) =
                        init testFlags
                in
                case thumbnailRequests (kitPairs model) model.thumbnails of
                    [ request ] ->
                        Expect.all
                            [ \r -> Expect.equal 3 (List.length r.rgb)
                            , \r -> Expect.notEqual [ 0, 0, 0 ] r.rgb
                            ]
                            request

                    other ->
                        Expect.fail ("expected one request, got " ++ String.fromInt (List.length other))
        , test "thumbnailLoaded stores the image under file and colour" <|
            \_ ->
                let
                    ( model, _ ) =
                        init kitFlags
                            |> Tuple.first
                            |> update (ThumbnailLoaded { file = "3004.dat", color = 4, url = "data:image/png;base64,AA", title = "Brick  1 x  2" })
                in
                Expect.all
                    [ \m -> Expect.equal 1 (Dict.size m.thumbnails)
                    , \m ->
                        thumbnailRequests (kitPairs m) m.thumbnails
                            |> List.map .file
                            |> Expect.equal [ "3001.dat", "3001.dat" ]
                    ]
                    model
        , test "a loaded title names the card; the part number stands in before" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init kitFlags

                    item =
                        { part = "3004.dat", color = 4, count = 4 }

                    ( model1, _ ) =
                        update (ThumbnailLoaded { file = "3004.dat", color = 4, url = "data:,", title = "Brick 1 x 2" }) model0
                in
                Expect.equal ( "3004", "Brick 1 x 2" ) ( kitItemName model0 item, kitItemName model1 item )
        , test "a thumbnail in another colour does not name the item" <|
            \_ ->
                let
                    ( model, _ ) =
                        init kitFlags
                            |> Tuple.first
                            |> update (ThumbnailLoaded { file = "3001.dat", color = 1, url = "data:,", title = "Brick 2 x 4" })
                in
                Expect.equal "3001" (kitItemName model { part = "3001.dat", color = 14, count = 4 })
        ]


{-| The (file, colour) pairs of a model's kit, as the dock asks for pictures.
-}
kitPairs : Model -> List ( String, Int )
kitPairs model =
    List.map (\item -> ( item.part, item.color )) model.inventory


{-| A puzzle with the default kit (a bare `model=` link). No hash would be free
building, which has its own tests in FreeTest.
-}
testFlags : Flags
testFlags =
    { ldrawBase = "/ldraw"
    , ldrawFallbackBase = ""
    , initialHash = "#model="
    , initialBytes = Nothing
    , supportsPointerEvents = True
    , hintSeen = False
    , coarsePointer = False
    , language = Just "en"
    , testApiEnabled = False
    , baseUrl = Nothing
    }


{-| Add the first kit item to the studio, as a tap on the dock does.
-}
addFirst : Model -> ( Model, Cmd Msg )
addFirst model =
    case model.inventory of
        first :: _ ->
            update (AddPart first) model

        [] ->
            ( model, Cmd.none )


{-| The top-left menu: it opens and closes, Escape closes it before it
deselects anything, and its items send the messages their buttons used to.
-}
menuTests : Test
menuTests =
    describe "Top-left menu"
        [ test "the menu opens, closes, and pauses the editing keys while open" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        update ToggleMenu model0

                    ( model2, _ ) =
                        update CloseMenu model1
                in
                Expect.equal
                    [ ( False, False ), ( True, True ), ( False, False ) ]
                    [ ( model0.menuOpen, model0.studio.helpVisible )
                    , ( model1.menuOpen, model1.studio.helpVisible )
                    , ( model2.menuOpen, model2.studio.helpVisible )
                    ]
        , test "Escape closes an open menu and keeps the selection" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    selected =
                        Tuple.first (addFirst model0)
                            |> (\m -> Tuple.first (update (StudioMsg (Core.SelectOnly 0)) m))

                    ( menuOpen, _ ) =
                        update ToggleMenu selected

                    ( afterEscape, _ ) =
                        update (pressKey "Escape") menuOpen
                in
                Expect.equal [ True, False, True ]
                    [ Core.hasSelection selected.studio
                    , afterEscape.menuOpen
                    , Core.hasSelection afterEscape.studio
                    ]
        , test "Escape with the menu closed still deselects" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    selected =
                        Tuple.first (addFirst model0)
                            |> (\m -> Tuple.first (update (StudioMsg (Core.SelectOnly 0)) m))

                    ( afterEscape, _ ) =
                        update (pressKey "Escape") selected
                in
                Expect.equal False (Core.hasSelection afterEscape.studio)
        , test "Undo from the menu undoes the last add, as the toolbar button did" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        addFirst model0

                    ( model2, _ ) =
                        update (MenuPicked Undo) model1
                in
                Expect.equal [ 0, 10 ]
                    [ List.length (Core.getPlacements model2.studio), remainingCount model2 ]
        , test "the visible ? button toggles the shortcut list, as the ? key does" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags
                in
                Query.fromHtml (view model0)
                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Keyboard shortcuts (?)") ]
                    |> Event.simulate Event.click
                    |> Event.expect ToggleHelp
        , test "the open menu lists its actions with their shortcuts" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( opened, _ ) =
                        update ToggleMenu model0
                in
                Query.fromHtml (view opened)
                    |> Expect.all
                        [ Query.findAll [ Selector.attribute (Attr.attribute "role" "menuitem") ] >> Query.count (Expect.equal 5)
                        , Query.has [ Selector.attribute (Attr.attribute "aria-label" "Share...") ]
                        , Query.hasNot [ Selector.text "Share editable model" ]
                        , Query.hasNot [ Selector.text "Share parts set" ]
                        , Query.hasNot [ Selector.text "Share fixed model" ]
                        , Query.has [ Selector.text "Suomi" ]
                        , Query.has [ Selector.text "In English" ]
                        , Query.has [ Selector.text "Download BrickLink Studio (.io)" ]
                        , Query.has [ Selector.text "Ctrl+Shift+Z" ]
                        ]
        , test "Download BrickLink Studio (.io) is disabled when empty and enabled when parts exist" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( opened0, _ ) =
                        update ToggleMenu model0

                    ( model1, _ ) =
                        addFirst model0

                    ( opened1, _ ) =
                        update ToggleMenu model1
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view opened0)
                            |> Query.find [ Selector.attribute (Attr.attribute "role" "menuitem"), Selector.containing [ Selector.text "Download BrickLink Studio (.io)" ] ]
                            |> Query.has [ Selector.disabled True ]
                    , \_ ->
                        Query.fromHtml (view opened1)
                            |> Query.find [ Selector.attribute (Attr.attribute "role" "menuitem"), Selector.containing [ Selector.text "Download BrickLink Studio (.io)" ] ]
                            |> Query.has [ Selector.disabled False ]
                    ]
                    ()
        , test "DownloadIo update produces a command without modifying model" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        addFirst model0

                    ( modelAfterDownload, _ ) =
                        update DownloadIo model1
                in
                Expect.equal (List.length (Core.getPlacements modelAfterDownload.studio)) 1
        , test "the kit cards show their count on the image and name it in the accessible label" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags
                in
                Query.fromHtml (view model0)
                    |> Expect.all
                        [ Query.has [ Selector.text "10/10" ]
                        , Query.has [ Selector.attribute (Attr.attribute "aria-label" "Add 3001 (Yellow), 10 of 10 left") ]
                        ]
        ]


{-| Sharing and toast notification lifecycle tests.
-}
sharingAndToastTests : Test
sharingAndToastTests =
    let
        ( start, _ ) =
            init testFlags
    in
    describe "Sharing and toast lifecycle"
        [ test "SharePartsSet in set mode and ShareFixedModel in editable/fixed mode set toast state and close menu" <|
            \_ ->
                let
                    ( menuOpenModel, _ ) =
                        init testFlags |> Tuple.first |> update ToggleMenu

                    ( sharedSetModel, _ ) =
                        update SharePartsSet menuOpenModel

                    ( blockedFixedInSet, _ ) =
                        update ShareFixedModel menuOpenModel

                    ( fixedMenuOpen, _ ) =
                        init { testFlags | initialHash = "#model=&parts=" } |> Tuple.first |> update ToggleMenu

                    ( sharedFixedModel, _ ) =
                        update ShareFixedModel fixedMenuOpen
                in
                Expect.all
                    [ \_ -> Expect.equal False sharedSetModel.menuOpen
                    , \_ -> Expect.equal (Just "Link copied to clipboard!") (Maybe.map .message sharedSetModel.toast)
                    , \_ -> Expect.equal Nothing blockedFixedInSet.toast
                    , \_ -> Expect.equal False sharedFixedModel.menuOpen
                    , \_ -> Expect.equal (Just "Link copied to clipboard!") (Maybe.map .message sharedFixedModel.toast)
                    ]
                    ()
        , test "ShareEditableModel is ignored in set mode but sets toast in editable mode" <|
            \_ ->
                let
                    ( blockedModel, _ ) =
                        update ShareEditableModel start

                    ( freeStart, _ ) =
                        init { testFlags | initialHash = "" }

                    ( sharedFreeModel, _ ) =
                        update ShareEditableModel freeStart
                in
                Expect.all
                    [ \_ -> Expect.equal Nothing blockedModel.toast
                    , \_ -> Expect.equal (Just "Link copied to clipboard!") (Maybe.map .message sharedFreeModel.toast)
                    ]
                    ()
        , test "DismissToast with matching id clears toast; mismatching id preserves toast" <|
            \_ ->
                let
                    ( sharedModel, _ ) =
                        update SharePartsSet start

                    toastId =
                        sharedModel.toast |> Maybe.map .id |> Maybe.withDefault 0

                    ( mismatchModel, _ ) =
                        update (DismissToast (toastId + 999)) sharedModel

                    ( matchModel, _ ) =
                        update (DismissToast toastId) sharedModel
                in
                Expect.all
                    [ \_ -> Expect.notEqual Nothing mismatchModel.toast
                    , \_ -> Expect.equal Nothing matchModel.toast
                    ]
                    ()
        , test "viewToast renders message with role='status'" <|
            \_ ->
                Query.fromHtml (viewToast "Link copied to clipboard!")
                    |> Expect.all
                        [ Query.has [ Selector.attribute (Attr.attribute "role" "status") ]
                        , Query.has [ Selector.text "Link copied to clipboard!" ]
                        ]
        , test "shareHash returns binary kit payload when no placements exist" <|
            \_ ->
                let
                    hash =
                        shareHash start
                in
                case HashPayload.decodeBytes hash of
                    Just payload ->
                        Expect.all
                            [ \p -> Expect.equal (Just start.inventory) p.kit
                            , \p -> Expect.equal 0 (List.length (Document.mainPlacements p.document))
                            ]
                            payload

                    Nothing ->
                        Expect.fail "Failed to decode shareHash payload"
        , test "fixed model mode disables paste" <|
            \_ ->
                let
                    ( fixedModel, _ ) =
                        init { testFlags | initialHash = "#model=&parts=" }
                in
                Expect.equal False fixedModel.studio.pasteEnabled
        , test "Ctrl+C and Ctrl+V canvas shortcuts seamlessly reach Core" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( addedModel, _ ) =
                        addFirst model0

                    copyEvent =
                        { key = "c", shift = False, ctrl = True, meta = False, alt = False, repeat = False, targetTag = "CANVAS", targetEditable = False, targetRole = "" }

                    pasteEvent =
                        { key = "v", shift = False, ctrl = True, meta = False, alt = False, repeat = False, targetTag = "CANVAS", targetEditable = False, targetRole = "" }

                    ( copiedModel, _ ) =
                        update (StudioMsg (Core.ViewportMsg (Viewport.KeyPressed copyEvent))) addedModel

                    ( pastedModel, _ ) =
                        update (StudioMsg (Core.ViewportMsg (Viewport.KeyPressed pasteEvent))) copiedModel
                in
                Expect.all
                    [ \_ -> Expect.equal True (Core.hasClipboard copiedModel.studio)
                    , \_ -> Expect.equal 2 (List.length (Core.getPlacements pastedModel.studio))
                    ]
                    ()
        ]


liveSharingTests : Test
liveSharingTests =
    describe "Live sharing and viewer mode"
        [ test "viewer mode omits the snapshot button from globe control" <|
            \_ ->
                let
                    ( viewerModel, _ ) =
                        init { testFlags | initialHash = "#/watch/ABCD123" }

                    ( normalModel, _ ) =
                        init testFlags
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view viewerModel)
                            |> Expect.all
                                [ Query.has [ Selector.attribute (Attr.attribute "aria-label" (I18n.cameraRealistic viewerModel.language)) ]
                                , Query.hasNot [ Selector.attribute (Attr.attribute "aria-label" (I18n.cameraTakeSnapshot viewerModel.language)) ]
                                ]
                    , \_ ->
                        Query.fromHtml (view normalModel)
                            |> Expect.all
                                [ Query.has [ Selector.attribute (Attr.attribute "aria-label" (I18n.cameraRealistic normalModel.language)) ]
                                , Query.has [ Selector.attribute (Attr.attribute "aria-label" (I18n.cameraTakeSnapshot normalModel.language)) ]
                                ]
                    ]
                    ()
        , test "CaptureSnapshot is ignored in viewer mode" <|
            \_ ->
                let
                    ( viewerModel, _ ) =
                        init { testFlags | initialHash = "#/watch/ABCD123" }

                    ( afterSnapshot, _ ) =
                        update CaptureSnapshot viewerModel
                in
                Expect.equal Nothing afterSnapshot.snapshotModal
        , test "TogglePlay is ignored in viewer mode" <|
            \_ ->
                let
                    ( viewerModel, _ ) =
                        init { testFlags | initialHash = "#/watch/ABCD123" }

                    ( docWithGear, _ ) =
                        Document.addPlacement "" { part = "3647.dat", color = 7, matrix = Mat4.identity } Document.empty

                    bytes =
                        HashPayload.encodeBytes
                            { mode = HashPayload.FreeBuilding
                            , document = docWithGear
                            , kit = Nothing
                            }

                    ( modelWithGear, _ ) =
                        update (LiveSnapshotReceived bytes) viewerModel

                    ( afterToggle, _ ) =
                        update TogglePlay modelWithGear
                in
                Expect.equal False afterToggle.playing
        , test "LiveAnimationReceived plays and stops animation in Technic mode" <|
            \_ ->
                let
                    ( viewerModel, _ ) =
                        init { testFlags | initialHash = "#/watch/ABCD123" }

                    ( docWithGear, _ ) =
                        Document.addPlacement "" { part = "3647.dat", color = 7, matrix = Mat4.identity } Document.empty

                    bytes =
                        HashPayload.encodeBytes
                            { mode = HashPayload.FreeBuilding
                            , document = docWithGear
                            , kit = Nothing
                            }

                    ( modelWithGear, _ ) =
                        update (LiveSnapshotReceived bytes) viewerModel

                    ( playingModel, _ ) =
                        update (LiveAnimationReceived True) modelWithGear

                    ( stoppedModel, _ ) =
                        update (LiveAnimationReceived False) playingModel
                in
                Expect.all
                    [ \_ -> Expect.equal True playingModel.playing
                    , \_ -> Expect.equal False stoppedModel.playing
                    ]
                    ()
        ]


reverseParticipantTests : Test
reverseParticipantTests =
    describe "Reverse Sharing participant builder integration"
        [ describe "parseReverseParticipantRoom"
            [ test "parses valid roomId from #/join/:roomId/b/:base64url" <|
                \_ ->
                    Expect.equal (Just "ROOM123") (parseReverseParticipantRoom "#/join/ROOM123/b/eJxL")
            , test "normalizes lowercase room ID to uppercase with /join/" <|
                \_ ->
                    Expect.equal (Just "ABCD789") (parseReverseParticipantRoom "#/join/abcd789/b/xyz")
            , test "parses legacy roomId from #/reverse/:roomId/b/:base64url for backwards compatibility" <|
                \_ ->
                    Expect.equal (Just "ROOM123") (parseReverseParticipantRoom "#/reverse/ROOM123/b/eJxL")
            , test "normalizes lowercase room ID to uppercase with legacy /reverse/" <|
                \_ ->
                    Expect.equal (Just "ABCD789") (parseReverseParticipantRoom "#/reverse/abcd789/b/xyz")
            , test "parses join hash without leading slash" <|
                \_ ->
                    Expect.equal (Just "ROOM123") (parseReverseParticipantRoom "join/ROOM123/b/eJxL")
            , test "parses legacy reverse hash without leading slash" <|
                \_ ->
                    Expect.equal (Just "ROOM123") (parseReverseParticipantRoom "reverse/ROOM123/b/eJxL")
            , test "rejects empty room ID" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal Nothing (parseReverseParticipantRoom "#/join//b/xyz")
                        , \_ -> Expect.equal Nothing (parseReverseParticipantRoom "#/reverse//b/xyz")
                        ]
                        ()
            , test "rejects non-reverse paths" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal Nothing (parseReverseParticipantRoom "#/watch/ABCD123")
                        , \_ -> Expect.equal Nothing (parseReverseParticipantRoom "#model=abc&parts=123")
                        , \_ -> Expect.equal Nothing (parseReverseParticipantRoom "")
                        ]
                        ()
            ]
        , test "init decodes participant room, model document and kit from base64url URL" <|
            \_ ->
                let
                    participantUrl =
                        ShareModal.computeReverseParticipantUrl
                            "https://example.com/play/"
                            "ROOM789"
                            ShareModal.PartsSet
                            False
                            Document.empty
                            [ { part = "3001.dat", color = 14, count = 10 } ]

                    ( model, cmd ) =
                        init { testFlags | initialHash = participantUrl }
                in
                Expect.all
                    [ \m -> Expect.equal (Just "ROOM789") m.reverseParticipantRoom
                    , \m -> Expect.equal "connecting" m.reverseStatus
                    , \m -> Expect.equal Nothing m.viewerRoom
                    , \m -> Expect.equal False m.free
                    , \m -> Expect.equal 10 (remainingCount m)
                    , \_ -> Expect.notEqual Cmd.none cmd
                    ]
                    model
        , test "init decodes legacy #/reverse/ participant URL for backwards compatibility" <|
            \_ ->
                let
                    legacyUrl =
                        "https://example.com/play/#/reverse/LEGACY1/b/eJxLzUtVsgZAAQ4A7g=="

                    ( model, _ ) =
                        init { testFlags | initialHash = legacyUrl }
                in
                Expect.equal (Just "LEGACY1") model.reverseParticipantRoom
        , test "header toolbar renders reverse participant badge" <|
            \_ ->
                let
                    participantUrl =
                        ShareModal.computeReverseParticipantUrl
                            "https://example.com/play/"
                            "ROOM123"
                            ShareModal.PartsSet
                            False
                            Document.empty
                            [ { part = "3001.dat", color = 14, count = 10 } ]

                    ( modelConnecting, _ ) =
                        init { testFlags | initialHash = participantUrl }

                    ( modelLive, _ ) =
                        update (ReverseStatusChanged "live") modelConnecting

                    ( normalModel, _ ) =
                        init testFlags
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view modelConnecting)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "reverse-participant-badge") ]
                            |> Expect.all
                                [ Query.has [ Selector.containing [ Selector.text "Connecting to session… · ROOM123" ] ]
                                , Query.has [ Selector.containing [ Selector.text "Builder (0 bricks)" ] ]
                                , Query.has [ Selector.class "bg-amber-400" ]
                                ]
                    , \_ ->
                        Query.fromHtml (view modelLive)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "reverse-participant-badge") ]
                            |> Expect.all
                                [ Query.has [ Selector.containing [ Selector.text "Linked to session · ROOM123" ] ]
                                , Query.has [ Selector.containing [ Selector.text "Builder (0 bricks)" ] ]
                                , Query.has [ Selector.class "bg-emerald-500" ]
                                ]
                    , \_ ->
                        Query.fromHtml (view normalModel)
                            |> Query.findAll [ Selector.attribute (Attr.attribute "data-testid" "reverse-participant-badge") ]
                            |> Query.count (Expect.equal 0)
                    ]
                    ()
        , test "reverse participant badge supports Finnish localization" <|
            \_ ->
                let
                    participantUrl =
                        ShareModal.computeReverseParticipantUrl
                            "https://example.com/play/"
                            "ROOM123"
                            ShareModal.PartsSet
                            False
                            Document.empty
                            [ { part = "3001.dat", color = 14, count = 10 } ]

                    ( modelConnectingFi, _ ) =
                        init { testFlags | initialHash = participantUrl, language = Just "fi" }

                    ( modelLiveFi, _ ) =
                        update (ReverseStatusChanged "live") modelConnectingFi
                in
                Query.fromHtml (view modelLiveFi)
                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "reverse-participant-badge") ]
                    |> Expect.all
                        [ Query.has [ Selector.containing [ Selector.text "Yhdistetty istuntoon · ROOM123" ] ]
                        , Query.has [ Selector.containing [ Selector.text "Rakentaja (0 palikkaa)" ] ]
                        ]
        , test "snapshot dispatch command triggers on ReverseStatusChanged live and on brick placement" <|
            \_ ->
                let
                    participantUrl =
                        ShareModal.computeReverseParticipantUrl
                            "https://example.com/play/"
                            "ROOM123"
                            ShareModal.PartsSet
                            False
                            Document.empty
                            [ { part = "3001.dat", color = 14, count = 10 } ]

                    ( modelConnecting, _ ) =
                        init { testFlags | initialHash = participantUrl }

                    ( modelLive, liveCmd ) =
                        update (ReverseStatusChanged "live") modelConnecting

                    ( modelPlaced, placeCmd ) =
                        addFirst modelLive
                in
                Expect.all
                    [ \_ -> Expect.notEqual Cmd.none liveCmd
                    , \_ -> Expect.notEqual Cmd.none placeCmd
                    , \_ ->
                        Query.fromHtml (view modelPlaced)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "reverse-participant-badge") ]
                            |> Query.has [ Selector.containing [ Selector.text "Builder (1 brick)" ] ]
                    ]
                    ()
        ]


reverseHostTests : Test
reverseHostTests =
    describe "Reverse Sharing Host Grid Studio & 3D Inspector"
        [ describe "parseReverseHostRoom"
            [ test "parses valid roomId from #/grid/:roomId" <|
                \_ ->
                    Expect.equal (Just "ROOM123") (parseReverseHostRoom "#/grid/ROOM123")
            , test "normalizes lowercase room ID to uppercase" <|
                \_ ->
                    Expect.equal (Just "ROOM123") (parseReverseHostRoom "#/grid/room123")
            , test "parses hash without leading slash" <|
                \_ ->
                    Expect.equal (Just "ROOM123") (parseReverseHostRoom "grid/ROOM123")
            , test "rejects empty room ID" <|
                \_ ->
                    Expect.equal Nothing (parseReverseHostRoom "#/grid/")
            , test "rejects non-grid routes" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal Nothing (parseReverseHostRoom "#/watch/ROOM123")
                        , \_ -> Expect.equal Nothing (parseReverseHostRoom "#/reverse/ROOM123/b/xyz")
                        , \_ -> Expect.equal Nothing (parseReverseHostRoom "#model=")
                        , \_ -> Expect.equal Nothing (parseReverseHostRoom "")
                        ]
                        ()
            ]
        , test "init decodes reverse host room and starts with empty participants" <|
            \_ ->
                let
                    ( model, _ ) =
                        init { testFlags | initialHash = "#/grid/ROOM123" }
                in
                Expect.all
                    [ \m -> Expect.equal (Just "ROOM123") m.reverseHostRoom
                    , \m -> Expect.equal Dict.empty m.participants
                    , \m -> Expect.equal Nothing m.inspectedParticipant
                    ]
                    model
        , test "grid view renders waiting state when participants list is empty" <|
            \_ ->
                let
                    ( model, _ ) =
                        init { testFlags | initialHash = "#/grid/ROOM123" }
                in
                Query.fromHtml (view model)
                    |> Expect.all
                        [ Query.has [ Selector.attribute (Attr.attribute "data-testid" "grid-waiting-state") ]
                        , Query.has [ Selector.containing [ Selector.text "GRID STUDIO · ROOM123" ] ]
                        , Query.has [ Selector.containing [ Selector.text "0 participants" ] ]
                        , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "grid-copy-link") ]
                        , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "grid-waiting-copy-btn") ]
                        ]
        , test "adding and updating participant via ReverseParticipantUpdated" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init { testFlags | initialHash = "#/grid/ROOM123" }

                    ( model1, _ ) =
                        update
                            (ReverseParticipantUpdated
                                { id = "viewer-1"
                                , name = "Liisa"
                                , bytes = []
                                , thumbnail = "data:image/webp;base64,abc"
                                , brickCount = 3
                                , lastSeen = 1000
                                }
                            )
                            model0
                in
                Expect.all
                    [ \_ ->
                        case Dict.get "viewer-1" model1.participants of
                            Just p ->
                                Expect.all
                                    [ \part -> Expect.equal "Liisa" part.name
                                    , \part -> Expect.equal 3 part.brickCount
                                    , \part -> Expect.equal "connected" part.status
                                    , \part -> Expect.equal "data:image/webp;base64,abc" part.thumbnail
                                    ]
                                    p

                            Nothing ->
                                Expect.fail "Participant not found"
                    , \_ ->
                        Query.fromHtml (view model1)
                            |> Expect.all
                                [ Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "grid-waiting-state") ]
                                , Query.has [ Selector.attribute (Attr.attribute "data-testid" "participant-card") ]
                                , Query.has [ Selector.containing [ Selector.text "Liisa" ] ]
                                , Query.has [ Selector.containing [ Selector.text "3 bricks" ] ]
                                , Query.has [ Selector.class "bg-emerald-500" ]
                                , Query.has [ Selector.attribute (Attr.attribute "data-testid" "inspect-participant-btn") ]
                                , Query.has [ Selector.containing [ Selector.text "1 participant" ] ]
                                ]
                    ]
                    ()
        , test "handling ReverseParticipantLeft updates status to disconnected" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init { testFlags | initialHash = "#/grid/ROOM123" }

                    ( model1, _ ) =
                        update
                            (ReverseParticipantUpdated
                                { id = "viewer-1"
                                , name = "Liisa"
                                , bytes = []
                                , thumbnail = ""
                                , brickCount = 3
                                , lastSeen = 1000
                                }
                            )
                            model0

                    ( model2, _ ) =
                        update (ReverseParticipantLeft "viewer-1") model1
                in
                Expect.all
                    [ \_ ->
                        case Dict.get "viewer-1" model2.participants of
                            Just p ->
                                Expect.equal "disconnected" p.status

                            Nothing ->
                                Expect.fail "Participant not found"
                    , \_ ->
                        Query.fromHtml (view model2)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "participant-card") ]
                            |> Query.has [ Selector.class "bg-amber-400" ]
                    ]
                    ()
        , test "switching between grid view and 3D inspector via InspectParticipant" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init { testFlags | initialHash = "#/grid/ROOM123" }

                    ( model1, _ ) =
                        update
                            (ReverseParticipantUpdated
                                { id = "viewer-1"
                                , name = "Liisa"
                                , bytes = []
                                , thumbnail = ""
                                , brickCount = 2
                                , lastSeen = 1000
                                }
                            )
                            model0

                    -- Inspect participant in 3D
                    ( modelInspecting, _ ) =
                        update (InspectParticipant (Just "viewer-1")) model1

                    -- Return to grid
                    ( modelGrid, _ ) =
                        update (InspectParticipant Nothing) modelInspecting
                in
                Expect.all
                    [ \_ ->
                        Expect.equal (Just "viewer-1") modelInspecting.inspectedParticipant
                    , \_ ->
                        Query.fromHtml (view modelInspecting)
                            |> Expect.all
                                [ Query.has [ Selector.attribute (Attr.attribute "data-testid" "close-inspector") ]
                                , Query.has [ Selector.attribute (Attr.attribute "data-testid" "inspector-participant-pill") ]
                                , Query.has [ Selector.containing [ Selector.text "Liisa" ] ]
                                , Query.has [ Selector.containing [ Selector.text "2 bricks" ] ]
                                , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "participant-card") ]
                                ]
                    , \_ ->
                        Expect.equal Nothing modelGrid.inspectedParticipant
                    , \_ ->
                        Query.fromHtml (view modelGrid)
                            |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "participant-card") ]
                    ]
                    ()
        , test "live document update when inspected participant sends snapshot" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init { testFlags | initialHash = "#/grid/ROOM123" }

                    ( model1, _ ) =
                        update
                            (ReverseParticipantUpdated
                                { id = "viewer-1"
                                , name = "Liisa"
                                , bytes = []
                                , thumbnail = ""
                                , brickCount = 0
                                , lastSeen = 1000
                                }
                            )
                            model0

                    ( modelInspecting, _ ) =
                        update (InspectParticipant (Just "viewer-1")) model1

                    ( docWithPart, _ ) =
                        Document.addPlacement "" { part = "3001.dat", color = 4, matrix = Mat4.identity } Document.empty

                    bytesWithPart =
                        HashPayload.encodeBytes
                            { mode = HashPayload.FreeBuilding
                            , document = docWithPart
                            , kit = Nothing
                            }

                    -- Participant sends updated build snapshot
                    ( modelUpdated, _ ) =
                        update
                            (ReverseParticipantUpdated
                                { id = "viewer-1"
                                , name = "Liisa"
                                , bytes = bytesWithPart
                                , thumbnail = ""
                                , brickCount = 1
                                , lastSeen = 2000
                                }
                            )
                            modelInspecting
                in
                Expect.equal 1 (List.length (Document.placements modelUpdated.studio.document))
        ]


{-| The snapshot modal: Escape closes it and does nothing else, so the part under
the modal keeps its selection.
-}
snapshotTests : Test
snapshotTests =
    let
        start =
            Tuple.first (init testFlags)

        ( studioWithGear, _, _ ) =
            Core.addPart "3647.dat" 7 (vec3 0 0 0) start.studio

        selected =
            Tuple.first (update (StudioMsg (Core.SelectOnly 0)) { start | studio = studioWithGear })
    in
    describe "Snapshot modal"
        [ test "Escape closes an open snapshot and keeps the selection" <|
            \_ ->
                let
                    ( opened, _ ) =
                        update CaptureSnapshot selected

                    ( afterEscape, _ ) =
                        update (pressKey "Escape") opened
                in
                Expect.equal [ True, True, True ]
                    [ opened.snapshotModal /= Nothing
                    , afterEscape.snapshotModal == Nothing
                    , Core.hasSelection afterEscape.studio
                    ]
        , test "progress and a failure reach the open snapshot" <|
            \_ ->
                let
                    ( opened, _ ) =
                        update CaptureSnapshot selected

                    ( progressed, _ ) =
                        update (SnapshotProgress { fraction = 0.25, samples = 4 }) opened

                    ( failed, _ ) =
                        update (SnapshotFailed "Tracer ran out of memory") progressed
                in
                Expect.equal [ True, True ]
                    [ Maybe.map .progress progressed.snapshotModal == Just { fraction = 0.25, samples = 4 }
                    , Maybe.map .phase failed.snapshotModal == Just (SnapshotModal.Failed "Tracer ran out of memory")
                    ]
        ]


shareModalTests : Test
shareModalTests =
    describe "Share Modal"
        [ test "Share... menu item opens modal and pauses keyboard shortcuts" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( menuOpen, _ ) =
                        update ToggleMenu model0

                    ( modalOpen, _ ) =
                        update OpenShareModal menuOpen
                in
                Expect.all
                    [ \_ -> Expect.equal False modalOpen.menuOpen
                    , \_ -> Expect.notEqual Nothing modalOpen.shareModal
                    , \_ -> Expect.equal (Just (ShareModal.init ShareModal.PartsSet)) modalOpen.shareModal
                    , \_ -> Expect.equal True modalOpen.studio.helpVisible
                    , \_ ->
                        Query.fromHtml (view modalOpen)
                            |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-modal") ]
                    ]
                    ()
        , test "Escape key closes the open share modal" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( modalOpen, _ ) =
                        update OpenShareModal model0

                    ( afterEscape, _ ) =
                        update (pressKey "Escape") modalOpen
                in
                Expect.all
                    [ \_ -> Expect.notEqual Nothing modalOpen.shareModal
                    , \_ -> Expect.equal Nothing afterEscape.shareModal
                    ]
                    ()
        , test "Close button and backdrop dismiss the share modal" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( modalOpen, _ ) =
                        update OpenShareModal model0
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view modalOpen)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-modal-close") ]
                            |> Event.simulate Event.click
                            |> Event.expect CloseShareModal
                    , \_ ->
                        Query.fromHtml (view modalOpen)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-modal-backdrop") ]
                            |> Event.simulate Event.click
                            |> Event.expect CloseShareModal
                    , \_ ->
                        let
                            ( closed, _ ) =
                                update CloseShareModal modalOpen
                        in
                        Expect.equal Nothing closed.shareModal
                    ]
                    ()
        , test "Feature selector offers applicable options and switches selection" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( placedModel, _ ) =
                        addFirst model0

                    ( modalOpen, _ ) =
                        update OpenShareModal placedModel

                    ( selectedFixed, _ ) =
                        update (SelectShareKind ShareModal.FixedModel) modalOpen
                in
                Expect.all
                    [ \_ ->
                        Expect.equal [ ShareModal.PartsSet, ShareModal.FixedModel, ShareModal.LivePreview ]
                            (ShareModal.availableKinds { free = False, hasInventory = True, hasPlacedUnglued = False })
                    , \_ ->
                        Expect.equal [ ShareModal.PartsSet, ShareModal.PartialSet, ShareModal.FixedModel, ShareModal.LivePreview ]
                            (ShareModal.availableKinds { free = False, hasInventory = True, hasPlacedUnglued = True })
                    , \_ ->
                        Expect.equal [ ShareModal.FreeBuilding, ShareModal.PartsSet, ShareModal.FixedModel, ShareModal.LivePreview ]
                            (ShareModal.availableKinds { free = True, hasInventory = False, hasPlacedUnglued = False })
                    , \_ ->
                        Query.fromHtml (view modalOpen)
                            |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-kind-partial") ]
                    , \_ ->
                        Query.fromHtml (view modalOpen)
                            |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-kind-live") ]
                    , \_ ->
                        case selectedFixed.shareModal of
                            Just sm ->
                                Expect.equal ShareModal.FixedModel sm.selected

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    ]
                    ()
        , test "Radio cards provide accessibility attributes" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( modalOpen, _ ) =
                        update OpenShareModal model0
                in
                Query.fromHtml (view modalOpen)
                    |> Expect.all
                        [ Query.has [ Selector.attribute (Attr.attribute "role" "radiogroup") ]
                        , Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-kind-parts") ]
                            >> Expect.all
                                [ Query.has [ Selector.attribute (Attr.attribute "role" "radio") ]
                                , Query.has [ Selector.attribute (Attr.attribute "aria-checked" "true") ]
                                , Query.has [ Selector.attribute (Attr.attribute "tabindex" "0") ]
                                ]
                        , Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-kind-fixed") ]
                            >> Expect.all
                                [ Query.has [ Selector.attribute (Attr.attribute "role" "radio") ]
                                , Query.has [ Selector.attribute (Attr.attribute "aria-checked" "false") ]
                                ]
                        ]
        , test "QR code SVG generation with quiet zone padding" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( modalOpen, _ ) =
                        update OpenShareModal model0

                    hash =
                        ShareModal.computeHash ShareModal.PartsSet model0.free model0.studio.document model0.inventory

                    url =
                        ShareModal.computeUrl "https://palikkaharrastajat.fi/play/" ShareModal.PartsSet model0.free model0.studio.document model0.inventory
                in
                Expect.all
                    [ \_ -> Expect.equal True (String.startsWith "#b/" hash)
                    , \_ -> Expect.equal ("https://palikkaharrastajat.fi/play/" ++ hash) url
                    , \_ ->
                        Query.fromHtml (view modalOpen)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-qr-code") ]
                            |> Query.has [ Selector.tag "svg" ]
                    , \_ ->
                        Query.fromHtml (view modalOpen)
                            |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-qr-hint") ]
                    ]
                    ()
        , test "QR code is hidden when the shareable link is too big for a QRCode" <|
            \_ ->
                let
                    hugeFlags =
                        { testFlags | baseUrl = Just ("https://palikkaharrastajat.fi/play/" ++ String.repeat 3000 "x") }

                    ( model0, _ ) =
                        init hugeFlags

                    ( modalOpen, _ ) =
                        update OpenShareModal model0
                in
                Query.fromHtml (view modalOpen)
                    |> Expect.all
                        [ Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-modal") ]
                        , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "share-qr-container") ]
                        , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "share-qr-code") ]
                        , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "share-qr-hint") ]
                        , Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-copy-button") ]
                        ]
        , test "Copy button dispatches CopyShareUrl, displays feedback, and dismisses" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( modalOpen, _ ) =
                        update OpenShareModal model0

                    ( copiedModel, _ ) =
                        update CopyShareUrl modalOpen

                    ( dismissedModel, _ ) =
                        update DismissShareCopied copiedModel
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view modalOpen)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-copy-button") ]
                            |> Event.simulate Event.click
                            |> Event.expect CopyShareUrl
                    , \_ ->
                        case copiedModel.shareModal of
                            Just sm ->
                                Expect.equal True sm.copied

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    , \_ ->
                        Query.fromHtml (view copiedModel)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-copy-button") ]
                            |> Query.has [ Selector.text "Copied!" ]
                    , \_ ->
                        case dismissedModel.shareModal of
                            Just sm ->
                                Expect.equal False sm.copied

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    , \_ ->
                        Query.fromHtml (view dismissedModel)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-copy-button") ]
                            |> Query.has [ Selector.text "Copy link" ]
                    ]
                    ()
        , test "Mobile layout and accessibility attributes in Finnish and English" <|
            \_ ->
                let
                    ( modelEn, _ ) =
                        init testFlags |> Tuple.first |> update OpenShareModal

                    ( modelFi, _ ) =
                        init { testFlags | language = Just "fi" } |> Tuple.first |> update OpenShareModal
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view modelEn)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-modal") ]
                            |> Expect.all
                                [ Query.has [ Selector.text "Share build" ]
                                , Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-modal-close") ]
                                ]
                    , \_ ->
                        Query.fromHtml (view modelEn)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-modal-close") ]
                            |> Query.has [ Selector.attribute (Attr.attribute "aria-label" "Close") ]
                    , \_ ->
                        Query.fromHtml (view modelFi)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-modal") ]
                            |> Expect.all
                                [ Query.has [ Selector.text "Jaa rakennelma" ]
                                , Query.has [ Selector.attribute (Attr.attribute "aria-label" "Sulje") ]
                                , Query.has [ Selector.text "Kopioi linkki" ]
                                , Query.has [ Selector.text "Lue puhelimen kameralla avataksesi suoraan" ]
                                ]
                    ]
                    ()
        , test "Toggling reverse sharing on FreeBuilding, PartsSet, and FixedModel" <|
            \_ ->
                let
                    ( modelParts, _ ) =
                        init testFlags |> Tuple.first |> update OpenShareModal

                    ( partsToggled, _ ) =
                        update ToggleReverseSharing modelParts

                    ( partsToggledBack, _ ) =
                        update ToggleReverseSharing partsToggled

                    ( modelFixed, _ ) =
                        update (SelectShareKind ShareModal.FixedModel) modelParts

                    ( fixedToggled, _ ) =
                        update ToggleReverseSharing modelFixed

                    ( modelFree0, _ ) =
                        init { testFlags | initialHash = "" }

                    ( modelFree, _ ) =
                        update OpenShareModal modelFree0

                    ( freeToggled, _ ) =
                        update ToggleReverseSharing modelFree
                in
                Expect.all
                    [ \_ ->
                        case partsToggled.shareModal of
                            Just sm ->
                                Expect.equal True sm.reverseSharing

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    , \_ ->
                        case partsToggledBack.shareModal of
                            Just sm ->
                                Expect.equal False sm.reverseSharing

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    , \_ ->
                        case fixedToggled.shareModal of
                            Just sm ->
                                Expect.all
                                    [ \_ -> Expect.equal ShareModal.FixedModel sm.selected
                                    , \_ -> Expect.equal True sm.reverseSharing
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    , \_ ->
                        case freeToggled.shareModal of
                            Just sm ->
                                Expect.all
                                    [ \_ -> Expect.equal ShareModal.FreeBuilding sm.selected
                                    , \_ -> Expect.equal True sm.reverseSharing
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    ]
                    ()
        , test "Reverse sharing participant and host grid URLs format" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    baseUrl =
                        "https://palikkaharrastajat.fi/play/"

                    roomId =
                        "7K9P2X4"

                    participantPartsUrl =
                        ShareModal.computeReverseParticipantUrl baseUrl roomId ShareModal.PartsSet model0.free model0.studio.document model0.inventory

                    participantFixedUrl =
                        ShareModal.computeReverseParticipantUrl baseUrl roomId ShareModal.FixedModel model0.free model0.studio.document model0.inventory

                    participantFreeUrl =
                        ShareModal.computeReverseParticipantUrl baseUrl roomId ShareModal.FreeBuilding True model0.studio.document model0.inventory

                    hostUrl =
                        ShareModal.computeReverseHostUrl baseUrl roomId
                in
                Expect.all
                    [ \_ ->
                        Expect.equal True
                            (String.startsWith "https://palikkaharrastajat.fi/play/#/join/7K9P2X4/b/" participantPartsUrl)
                    , \_ ->
                        Expect.equal True
                            (String.startsWith "https://palikkaharrastajat.fi/play/#/join/7K9P2X4/b/" participantFixedUrl)
                    , \_ ->
                        Expect.equal True
                            (String.startsWith "https://palikkaharrastajat.fi/play/#/join/7K9P2X4/b/" participantFreeUrl)
                    , \_ ->
                        Expect.equal "https://palikkaharrastajat.fi/play/#/grid/7K9P2X4" hostUrl
                    ]
                    ()
        , test "Reverse sharing UI presence of copy-participant-url, copy-host-url, and participant QR code" <|
            \_ ->
                let
                    ( modelOpen, _ ) =
                        init testFlags |> Tuple.first |> update OpenShareModal

                    ( reverseActive, _ ) =
                        update ToggleReverseSharing modelOpen

                    ( copiedParticipant, _ ) =
                        update CopyParticipantUrl reverseActive

                    ( copiedHost, _ ) =
                        update CopyHostUrl reverseActive
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view modelOpen)
                            |> Expect.all
                                [ Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-copy-button") ]
                                , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "copy-participant-url") ]
                                , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "copy-host-url") ]
                                , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "participant-qr-code") ]
                                ]
                    , \_ ->
                        Query.fromHtml (view reverseActive)
                            |> Expect.all
                                [ Query.has [ Selector.attribute (Attr.attribute "data-testid" "reverse-sharing-toggle") ]
                                , Query.find [ Selector.attribute (Attr.attribute "data-testid" "reverse-sharing-toggle") ]
                                    >> Query.has [ Selector.attribute (Attr.attribute "aria-checked" "true") ]
                                , Query.find [ Selector.attribute (Attr.attribute "data-testid" "copy-participant-url") ]
                                    >> Event.simulate Event.click
                                    >> Event.expect CopyParticipantUrl
                                , Query.find [ Selector.attribute (Attr.attribute "data-testid" "copy-host-url") ]
                                    >> Event.simulate Event.click
                                    >> Event.expect CopyHostUrl
                                , Query.has [ Selector.attribute (Attr.attribute "data-testid" "open-host-grid") ]
                                , Query.find [ Selector.attribute (Attr.attribute "data-testid" "participant-qr-code") ]
                                    >> Query.has [ Selector.tag "svg" ]
                                , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "share-copy-button") ]
                                ]
                    , \_ ->
                        case copiedParticipant.shareModal of
                            Just sm ->
                                Expect.equal True sm.copiedParticipant

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    , \_ ->
                        case copiedHost.shareModal of
                            Just sm ->
                                Expect.equal True sm.copiedHost

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    ]
                    ()
        , test "Reverse sharing toggle renders 'Rinnakkaisleikki' in Finnish" <|
            \_ ->
                let
                    ( modelOpenFi, _ ) =
                        init { testFlags | language = Just "fi" } |> Tuple.first |> update OpenShareModal
                in
                Query.fromHtml (view modelOpenFi)
                    |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "reverse-sharing-toggle") ]
                    |> Query.has [ Selector.text "Rinnakkaisleikki" ]
        , test "Square copy buttons render accessible aria-label and title attributes with state updates" <|
            \_ ->
                let
                    ( modelOpenFi, _ ) =
                        init { testFlags | language = Just "fi" } |> Tuple.first |> update OpenShareModal

                    ( reverseActiveFi, _ ) =
                        update ToggleReverseSharing modelOpenFi

                    ( copiedParticipantFi, _ ) =
                        update CopyParticipantUrl reverseActiveFi

                    ( copiedHostFi, _ ) =
                        update CopyHostUrl reverseActiveFi
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view reverseActiveFi)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "copy-participant-url") ]
                            |> Query.has
                                [ Selector.attribute (Attr.attribute "aria-label" "Kopioi linkki")
                                , Selector.attribute (Attr.attribute "title" "Kopioi linkki")
                                ]
                    , \_ ->
                        Query.fromHtml (view reverseActiveFi)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "copy-host-url") ]
                            |> Query.has
                                [ Selector.attribute (Attr.attribute "aria-label" "Kopioi ruudukkolinkki")
                                , Selector.attribute (Attr.attribute "title" "Kopioi ruudukkolinkki")
                                ]
                    , \_ ->
                        Query.fromHtml (view copiedParticipantFi)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "copy-participant-url") ]
                            |> Query.has
                                [ Selector.attribute (Attr.attribute "aria-label" "Kopioitu!")
                                , Selector.attribute (Attr.attribute "title" "Kopioitu!")
                                ]
                    , \_ ->
                        Query.fromHtml (view copiedHostFi)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "copy-host-url") ]
                            |> Query.has
                                [ Selector.attribute (Attr.attribute "aria-label" "Kopioitu!")
                                , Selector.attribute (Attr.attribute "title" "Kopioitu!")
                                ]
                    ]
                    ()
        , test "Two-column desktop layout divides selectors and actions" <|
            \_ ->
                let
                    ( modelOpen, _ ) =
                        init testFlags |> Tuple.first |> update OpenShareModal
                in
                Query.fromHtml (view modelOpen)
                    |> Expect.all
                        [ Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-modal") ]
                        , Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-modal-left-column") ]
                            >> Expect.all
                                [ Query.has [ Selector.attribute (Attr.attribute "role" "radiogroup") ]
                                , Query.has [ Selector.attribute (Attr.attribute "data-testid" "reverse-sharing-toggle") ]
                                ]
                        , Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-modal-right-column") ]
                            >> Expect.all
                                [ Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-qr-container") ]
                                , Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-copy-button") ]
                                ]
                        ]
        , test "Mobile QR code toggle expands and collapses QR code on request with i18n labels" <|
            \_ ->
                let
                    ( modelEn, _ ) =
                        init testFlags |> Tuple.first |> update OpenShareModal

                    ( toggledEn, _ ) =
                        update ToggleShareQr modelEn

                    ( collapsedEn, _ ) =
                        update ToggleShareQr toggledEn

                    ( modelFi, _ ) =
                        init { testFlags | language = Just "fi" } |> Tuple.first |> update OpenShareModal

                    ( toggledFi, _ ) =
                        update ToggleShareQr modelFi
                in
                Expect.all
                    [ \_ ->
                        case modelEn.shareModal of
                            Just sm ->
                                Expect.equal False sm.showQr

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    , \_ ->
                        Query.fromHtml (view modelEn)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-qr-toggle") ]
                            |> Expect.all
                                [ Query.has [ Selector.text "Show QR code" ]
                                , Query.has [ Selector.attribute (Attr.attribute "aria-expanded" "false") ]
                                ]
                    , \_ ->
                        Query.fromHtml (view modelEn)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-qr-container") ]
                            |> Query.has [ Selector.class "qr-container-collapsed" ]
                    , \_ ->
                        Query.fromHtml (view modelEn)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-qr-toggle") ]
                            |> Event.simulate Event.click
                            |> Event.expect ToggleShareQr
                    , \_ ->
                        case toggledEn.shareModal of
                            Just sm ->
                                Expect.equal True sm.showQr

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    , \_ ->
                        Query.fromHtml (view toggledEn)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-qr-toggle") ]
                            |> Expect.all
                                [ Query.has [ Selector.text "Hide QR code" ]
                                , Query.has [ Selector.attribute (Attr.attribute "aria-expanded" "true") ]
                                ]
                    , \_ ->
                        Query.fromHtml (view toggledEn)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-qr-container") ]
                            |> Query.hasNot [ Selector.class "qr-container-collapsed" ]
                    , \_ ->
                        case collapsedEn.shareModal of
                            Just sm ->
                                Expect.equal False sm.showQr

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    , \_ ->
                        Query.fromHtml (view modelFi)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-qr-toggle") ]
                            |> Query.has [ Selector.text "Näytä QR-koodi" ]
                    , \_ ->
                        Query.fromHtml (view toggledFi)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-qr-toggle") ]
                            |> Query.has [ Selector.text "Piilota QR-koodi" ]
                    ]
                    ()
        , test "Participant QR code toggle in reverse sharing mode" <|
            \_ ->
                let
                    ( modelOpen, _ ) =
                        init testFlags |> Tuple.first |> update OpenShareModal

                    ( reverseActive, _ ) =
                        update ToggleReverseSharing modelOpen

                    ( qrExpanded, _ ) =
                        update ToggleShareQr reverseActive
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view reverseActive)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "participant-qr-toggle") ]
                            |> Expect.all
                                [ Query.has [ Selector.text "Show QR code" ]
                                , Query.has [ Selector.attribute (Attr.attribute "aria-expanded" "false") ]
                                ]
                    , \_ ->
                        Query.fromHtml (view reverseActive)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "participant-qr-container") ]
                            |> Query.has [ Selector.class "qr-container-collapsed" ]
                    , \_ ->
                        Query.fromHtml (view reverseActive)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "participant-qr-toggle") ]
                            |> Event.simulate Event.click
                            |> Event.expect ToggleShareQr
                    , \_ ->
                        case qrExpanded.shareModal of
                            Just sm ->
                                Expect.equal True sm.showQr

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    , \_ ->
                        Query.fromHtml (view qrExpanded)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "participant-qr-toggle") ]
                            |> Expect.all
                                [ Query.has [ Selector.text "Hide QR code" ]
                                , Query.has [ Selector.attribute (Attr.attribute "aria-expanded" "true") ]
                                ]
                    , \_ ->
                        Query.fromHtml (view qrExpanded)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "participant-qr-container") ]
                            |> Query.hasNot [ Selector.class "qr-container-collapsed" ]
                    ]
                    ()
        , test "QR toggle button is not displayed when shareable link is too big" <|
            \_ ->
                let
                    hugeFlags =
                        { testFlags | baseUrl = Just ("https://palikkaharrastajat.fi/play/" ++ String.repeat 3000 "x") }

                    ( model0, _ ) =
                        init hugeFlags

                    ( modalOpen, _ ) =
                        update OpenShareModal model0
                in
                Query.fromHtml (view modalOpen)
                    |> Expect.all
                        [ Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-modal") ]
                        , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "share-qr-toggle") ]
                        , Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "share-qr-container") ]
                        ]
        , test "Selecting 'Suora lähetys' (LivePreview) disables 'Rinnakkaisleikki' toggle" <|
            \_ ->
                let
                    ( modelOpen, _ ) =
                        init testFlags |> Tuple.first |> update OpenShareModal

                    ( liveSelected, _ ) =
                        update (SelectShareKind ShareModal.LivePreview) modelOpen

                    ( toggledWhileDisabled, _ ) =
                        update ToggleReverseSharing liveSelected
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view liveSelected)
                            |> Query.find [ Selector.attribute (Attr.attribute "data-testid" "reverse-sharing-toggle") ]
                            |> Expect.all
                                [ Query.has [ Selector.attribute (Attr.attribute "aria-disabled" "true") ]
                                , Query.has [ Selector.attribute (Attr.attribute "tabindex" "-1") ]
                                ]
                    , \_ ->
                        case toggledWhileDisabled.shareModal of
                            Just sm ->
                                Expect.equal False sm.reverseSharing

                            Nothing ->
                                Expect.fail "Share modal was not open"
                    ]
                    ()
        , test "Standard sharing methods render rounded container, visible URL field and square copy button" <|
            \_ ->
                let
                    ( modelOpen, _ ) =
                        init testFlags |> Tuple.first |> update OpenShareModal
                in
                Query.fromHtml (view modelOpen)
                    |> Expect.all
                        [ Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-default-card") ]
                        , Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-url-field") ]
                        , Query.has [ Selector.attribute (Attr.attribute "data-testid" "share-copy-button") ]
                        , Query.find [ Selector.attribute (Attr.attribute "data-testid" "share-copy-button") ]
                            >> Expect.all
                                [ Query.has [ Selector.attribute (Attr.attribute "aria-label" "Copy link") ]
                                , Query.has [ Selector.attribute (Attr.attribute "title" "Copy link") ]
                                , Event.simulate Event.click >> Event.expect CopyShareUrl
                                ]
                        ]
        ]


{-| A key press delivered to the studio the way the browser's keydown reaches it.
-}
pressKey : String -> Msg
pressKey key =
    StudioMsg
        (Core.ViewportMsg
            (Viewport.KeyPressed
                { key = key
                , ctrl = False
                , meta = False
                , alt = False
                , shift = False
                , repeat = False
                , targetTag = "BODY"
                , targetEditable = False
                , targetRole = ""
                }
            )
        )


{-| Touch layout: the shared camera globe, the phone-width kit/camera toggle and
the names shown where tooltips never show.
-}
touchTests : Test
touchTests =
    let
        ( start, _ ) =
            init testFlags

        at width model =
            update (WindowResized width 844) model |> Tuple.first

        toggled model =
            update ToggleCameraPanel model |> Tuple.first

        countLabelled label model =
            Query.fromHtml (view model)
                |> Query.findAll [ Selector.attribute (Attr.attribute "aria-label" label) ]

        expectCount n label model =
            countLabelled label model |> Query.count (Expect.equal n)

        azimuth model =
            model.studio.viewport.camera.azimuth
    in
    describe "Touch layout"
        [ test "the Fit view item is gone from the menu, the globe's reset replaces it" <|
            \_ ->
                let
                    opened =
                        update ToggleMenu start |> Tuple.first
                in
                Query.fromHtml (view opened)
                    |> Query.findAll [ Selector.text "Fit view" ]
                    |> Query.count (Expect.equal 0)
        , test "a wide window shows the kit and the camera globe side by side" <|
            \_ ->
                let
                    wide =
                        at 964 start
                in
                Expect.all
                    [ \_ -> expectCount 1 "Kit" wide
                    , \_ -> expectCount 1 "Camera" wide
                    , \_ -> expectCount 0 "Camera controls" wide
                    ]
                    ()
        , test "a phone shows the kit first, and the camera globe when the toggle is pressed" <|
            \_ ->
                let
                    phone =
                        at 390 start

                    cameraView =
                        toggled phone
                in
                Expect.all
                    [ \_ -> expectCount 1 "Kit" phone
                    , \_ -> expectCount 0 "Camera" phone
                    , \_ -> expectCount 0 "Kit" cameraView
                    , \_ -> expectCount 1 "Camera" cameraView
                    ]
                    ()
        , test "the phone toggle is a labelled button that reports whether the camera is shown" <|
            \_ ->
                let
                    pressedState model =
                        Query.fromHtml (view model)
                            |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Camera controls") ]
                            |> Query.has
                                [ Selector.attribute (Attr.attribute "aria-pressed" "true")
                                , Selector.style "background" "rgba(5, 19, 29, 0.9)"
                                , Selector.style "color" "var(--color-text-on-dark)"
                                ]
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view (at 390 start))
                            |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Camera controls") ]
                            |> Query.has [ Selector.attribute (Attr.attribute "aria-pressed" "false") ]
                    , \_ -> pressedState (toggled (at 390 start))
                    ]
                    ()
        , test "the globe drag orbits the camera" <|
            \_ ->
                let
                    pressed =
                        update (GlobePressed ( 0, 0 )) start |> Tuple.first

                    moved =
                        update (GlobeMoved ( 22, 0 )) pressed |> Tuple.first
                in
                Expect.greaterThan 1.0e-6 (abs (azimuth moved - azimuth start))
        , test "realistic mode shows the sun marker on the globe and dragging it orbits the light without moving the camera" <|
            \_ ->
                let
                    wideDraft =
                        at 964 start

                    wideRealistic =
                        update ToggleRealistic wideDraft |> Tuple.first

                    initialLight =
                        Core.lightDirection wideRealistic.studio
                            |> Maybe.withDefault (vec3 0 1 0)

                    sunAngles =
                        CameraGlobe.lightAngles initialLight

                    sunPos =
                        CameraGlobe.markerPosition sunAngles.azimuth sunAngles.elevation

                    afterSunDrag =
                        wideRealistic
                            |> (\m -> update (GlobePressed ( sunPos.x, sunPos.y )) m |> Tuple.first)
                            |> (\m -> update (GlobeMoved ( sunPos.x + 18, sunPos.y - 10 )) m |> Tuple.first)
                            |> (\m -> update GlobeReleased m |> Tuple.first)

                    nextLight =
                        Core.lightDirection afterSunDrag.studio
                            |> Maybe.withDefault initialLight
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view wideDraft)
                            |> Query.hasNot [ Selector.attribute (Attr.attribute "data-testid" "globe-light-marker") ]
                    , \_ ->
                        Query.fromHtml (view wideRealistic)
                            |> Query.has [ Selector.attribute (Attr.attribute "data-testid" "globe-light-marker") ]
                    , \_ ->
                        Expect.within (Expect.Absolute 1.0e-6) (azimuth wideRealistic) (azimuth afterSunDrag)
                    , \_ ->
                        Expect.greaterThan 0.1 (Vec3.distance initialLight nextLight)
                    ]
                    ()
        , test "a kit card tap records the card's name for touch screens" <|
            \_ ->
                let
                    tapped =
                        addFirst start |> Tuple.first
                in
                Expect.equal (Just "3001") tapped.lastTapped
        , test "the touch label is only in the DOM once a card has been tapped" <|
            \_ ->
                Expect.all
                    [ \_ -> Query.fromHtml (view start) |> Query.findAll [ Selector.class "touch-only" ] |> Query.count (Expect.equal 0)
                    , \_ -> Query.fromHtml (view (addFirst start |> Tuple.first)) |> Query.findAll [ Selector.class "touch-only" ] |> Query.count (Expect.greaterThan 0)
                    ]
                    ()
        , describe "Localization (i18n)"
            [ test "defaults to Finnish when flags.language is Nothing" <|
                \_ ->
                    let
                        fiFlags =
                            { testFlags | language = Nothing }

                        model =
                            Tuple.first (init fiFlags)
                    in
                    Expect.equal I18n.Fi model.language
            , test "Finnish menu shows Finnish items and toggle to English" <|
                \_ ->
                    let
                        fiFlags =
                            { testFlags | language = Nothing }

                        ( opened, _ ) =
                            update ToggleMenu (Tuple.first (init fiFlags))
                    in
                    Query.fromHtml (view opened)
                        |> Expect.all
                            [ Query.has [ Selector.containing [ Selector.text "Suomi" ], Selector.attribute (Attr.attribute "aria-pressed" "true") ]
                            , Query.has [ Selector.containing [ Selector.text "In English" ], Selector.attribute (Attr.attribute "aria-pressed" "false") ]
                            , Query.has [ Selector.text "Lataa BrickLink Studio (.io)" ]
                            , Query.has [ Selector.text "Kumoa" ]
                            , Query.has [ Selector.text "Tee uudelleen" ]
                            , Query.has [ Selector.text "Poista" ]
                            , Query.has [ Selector.text "Tyhjennä" ]
                            ]
            , test "switching language updates model and menu label" <|
                \_ ->
                    let
                        fiFlags =
                            { testFlags | language = Nothing }

                        ( modelFi, _ ) =
                            init fiFlags

                        ( modelEn, _ ) =
                            update (SetLanguage I18n.En) modelFi

                        ( openedEn, _ ) =
                            update ToggleMenu modelEn
                    in
                    Expect.all
                        [ \_ -> Expect.equal I18n.En modelEn.language
                        , \_ ->
                            Query.fromHtml (view openedEn)
                                |> Query.has [ Selector.containing [ Selector.text "In English" ], Selector.attribute (Attr.attribute "aria-pressed" "true") ]
                        ]
                        ()
            , test "Finnish free dock shows Finnish tab labels" <|
                \_ ->
                    let
                        fiFlags =
                            { testFlags | initialHash = "", language = Just "fi" }

                        ( model, _ ) =
                            init fiFlags
                    in
                    Query.fromHtml (view model)
                        |> Expect.all
                            [ Query.has [ Selector.text "Palikat" ]
                            , Query.has [ Selector.text "Levyt" ]
                            , Query.has [ Selector.text "Viistot ja laatat" ]
                            , Query.has [ Selector.text "Kulmapalat ja SNOT" ]
                            , Query.has [ Selector.text "Technic" ]
                            ]
            ]
        ]


dockPlacementTests : Test
dockPlacementTests =
    describe "dockPlacement"
        [ test "dockOverlapWidth constant is 1100px" <|
            \_ ->
                Expect.equal 1100 dockOverlapWidth
        , describe "viewport widths < 600px (narrow portrait phone)"
            (List.map
                (\w ->
                    test (String.fromInt w ++ "px pins left and right without transform") <|
                        \_ ->
                            Query.fromHtml (Html.div (dockPlacement w) [])
                                |> Expect.all
                                    [ Query.has [ Selector.style "left" "0.75rem" ]
                                    , Query.has [ Selector.style "right" "0.75rem" ]
                                    , Query.hasNot [ Selector.style "transform" "translateX(-50%)" ]
                                    , Query.hasNot [ Selector.style "left" "50%" ]
                                    ]
                )
                [ 375, 599 ]
            )
        , describe "viewport widths between 600px and 1099px (dockOverlapWidth)"
            (List.map
                (\w ->
                    test (String.fromInt w ++ "px pins left with 16rem margin and no transform") <|
                        \_ ->
                            Query.fromHtml (Html.div (dockPlacement w) [])
                                |> Expect.all
                                    [ Query.has [ Selector.style "left" "0.75rem" ]
                                    , Query.has [ Selector.style "max-width" "calc(100vw - 16rem)" ]
                                    , Query.hasNot [ Selector.style "transform" "translateX(-50%)" ]
                                    , Query.hasNot [ Selector.style "left" "50%" ]
                                    ]
                )
                [ 600, 800, 964, 1064, 1099 ]
            )
        , describe "viewport widths >= 1100px (centered)"
            (List.map
                (\w ->
                    test (String.fromInt w ++ "px centers dock with translateX(-50%) and 24rem margin") <|
                        \_ ->
                            Query.fromHtml (Html.div (dockPlacement w) [])
                                |> Expect.all
                                    [ Query.has [ Selector.style "left" "50%" ]
                                    , Query.has [ Selector.style "transform" "translateX(-50%)" ]
                                    , Query.has [ Selector.style "max-width" "calc(100vw - 24rem)" ]
                                    ]
                )
                [ 1100, 1280, 1920 ]
            )
        , describe "view integration"
            [ test "at 800px bottom area pins left to avoid overlapping camera globe" <|
                \_ ->
                    let
                        ( baseModel, _ ) =
                            init testFlags

                        model800 =
                            { baseModel | windowWidth = 800 }
                    in
                    Query.fromHtml (view model800)
                        |> Query.find [ Selector.style "pointer-events" "none", Selector.style "bottom" "0.75rem" ]
                        |> Expect.all
                            [ Query.has [ Selector.style "left" "0.75rem" ]
                            , Query.has [ Selector.style "max-width" "calc(100vw - 16rem)" ]
                            , Query.hasNot [ Selector.style "transform" "translateX(-50%)" ]
                            , Query.hasNot [ Selector.style "left" "50%" ]
                            ]
            , test "at 1280px bottom area centers horizontally" <|
                \_ ->
                    let
                        ( baseModel, _ ) =
                            init testFlags

                        model1280 =
                            { baseModel | windowWidth = 1280 }
                    in
                    Query.fromHtml (view model1280)
                        |> Query.find [ Selector.style "pointer-events" "none", Selector.style "bottom" "0.75rem" ]
                        |> Expect.all
                            [ Query.has [ Selector.style "left" "50%" ]
                            , Query.has [ Selector.style "transform" "translateX(-50%)" ]
                            , Query.has [ Selector.style "max-width" "calc(100vw - 24rem)" ]
                            ]
            ]
        ]
