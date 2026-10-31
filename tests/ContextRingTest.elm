module ContextRingTest exposing (suite)

{-| Tests for Brick Play's context ring: how it opens and closes, its buttons and
their accessible names, the W A S D Q E moves (with and without a selection, and
in Walk mode), the move steps, Tab selection, and the first-select hint.

Moves are checked in render space (Y up, the camera's frame) against the snapping
the studio uses: the camera's azimuth snapped to the nearest world axis.

-}

import Dict
import Expect
import Html
import Html.Attributes as Attr
import LDraw.Document as Document
import LDraw.Parser as Parser
import LDraw.Resolve exposing (PartStatus(..))
import Main exposing (Flags, Model, Msg(..), init, update, view)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera as Camera
import Ring
import Set
import Step
import Studio.Core as Core
import Studio.Gizmo exposing (DragHandle(..))
import Studio.Shortcuts exposing (NudgeDirection(..), TurnDirection(..))
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time
import Viewport
import Viewport.Shortcuts exposing (KeyEvent)


suite : Test
suite =
    describe "Context ring"
        [ openTests
        , closeTests
        , ringTests
        , moveTests
        , stepTests
        , tabTests
        , hintTests
        , layoutTests
        ]



-- Opening and closing


openTests : Test
openTests =
    describe "opening"
        [ test "a double-click on a part opens the ring on it" <|
            \_ ->
                let
                    ( x, y ) =
                        screenCentre builtWithOneBrick
                in
                send (StudioMsg (Core.ViewportMsg (Viewport.DoubleClick x y))) builtWithOneBrick
                    |> (\m -> Expect.equal (Just 0) m.ringPart)
        , test "a double-click on empty canvas opens nothing" <|
            \_ ->
                send (StudioMsg (Core.ViewportMsg (Viewport.DoubleClick 2 2))) builtWithOneBrick
                    |> (\m -> Expect.equal Nothing m.ringPart)
        , test "two taps close in time and place are a double-tap that opens the ring" <|
            \_ ->
                let
                    ( x, y ) =
                        screenCentre builtWithOneBrick
                in
                builtWithOneBrick
                    |> send (TapTimed x y 0)
                    |> send (TapTimed x y 200)
                    |> (\m -> Expect.equal (Just 0) m.ringPart)
        , test "double-tapping a brick with touch events opens the ring even when the camera glides after the first tap" <|
            \_ ->
                let
                    -- Two bricks so the second brick is off-centre and tapping it starts a camera glide.
                    start =
                        builtWithTwoBricks
                            |> send (StudioMsg Core.ClearSelection)

                    -- Aim at brick 1 (at x = 20, in front of brick 0).
                    centre1 =
                        start.studio.viewport.instancedScene
                            |> Maybe.andThen (\scene -> List.filter (\p -> p.source == 1) scene.placements |> List.head)
                            |> Maybe.map .center
                            |> Maybe.withDefault (vec3 0 0 0)

                    ( x, y ) =
                        Ring.screenPoint start.studio.viewport.camera start.studio.viewport.width start.studio.viewport.height centre1
                            |> Maybe.withDefault ( 0, 0 )

                    touchTap millis model =
                        let
                            ( pressed, _, _ ) =
                                Core.update (Core.ViewportMsg (Viewport.PointerTouchStart { id = 1, x = x, y = y })) model.studio

                            ( released, _, events ) =
                                Core.update (Core.ViewportMsg (Viewport.PointerTouchEnd 1)) pressed

                            afterTouch =
                                { model | studio = released }
                        in
                        List.foldl
                            (\ev acc ->
                                case ev of
                                    Core.TappedAt tx ty ->
                                        send (TapTimed tx ty millis) acc

                                    _ ->
                                        acc
                            )
                            afterTouch
                            events
                in
                start
                    |> touchTap 100
                    -- Advance the camera glide so brick 1 moves away from (x, y) before the second tap.
                    |> send (StudioMsg (Core.ViewportMsg (Viewport.AnimationFrame (Time.millisToPosix 116))))
                    |> send (StudioMsg (Core.ViewportMsg (Viewport.AnimationFrame (Time.millisToPosix 260))))
                    |> touchTap 280
                    |> (\m -> Expect.equal (Just 1) m.ringPart)
        , test "two taps a second apart are not a double-tap" <|
            \_ ->
                let
                    ( x, y ) =
                        screenCentre builtWithOneBrick
                in
                builtWithOneBrick
                    |> send (TapTimed x y 0)
                    |> send (TapTimed x y 1000)
                    |> (\m -> Expect.equal Nothing m.ringPart)
        , test "a single tap does not open the ring" <|
            \_ ->
                let
                    ( x, y ) =
                        screenCentre builtWithOneBrick
                in
                builtWithOneBrick
                    |> send (TapTimed x y 0)
                    |> (\m -> Expect.equal Nothing m.ringPart)
        , test "a single click selects but does not open the ring" <|
            \_ ->
                builtWithOneBrick
                    |> send (StudioMsg (Core.SelectOnly 0))
                    |> (\m -> Expect.equal Nothing m.ringPart)
        ]


closeTests : Test
closeTests =
    describe "closing"
        [ test "Escape closes the ring but keeps the selection" <|
            \_ ->
                let
                    m =
                        openRing builtWithOneBrick
                            |> press "Escape"
                in
                Expect.all
                    [ \_ -> Expect.equal Nothing m.ringPart
                    , \_ -> Expect.equal (Set.singleton 0) m.studio.selection
                    ]
                    ()
        , test "a second Escape deselects, as before" <|
            \_ ->
                openRing builtWithOneBrick
                    |> press "Escape"
                    |> press "Escape"
                    |> (\m -> Expect.equal Set.empty m.studio.selection)
        , test "a click on empty canvas closes it (the selection goes)" <|
            \_ ->
                openRing builtWithOneBrick
                    |> send (StudioMsg Core.ClearSelection)
                    |> (\m -> Expect.equal Nothing m.ringPart)
        , test "selecting a different part with a single click closes it" <|
            \_ ->
                openRing (send (StudioMsg (Core.SelectOnly 0)) builtWithTwoBricks)
                    |> send (StudioMsg (Core.SelectOnly 1))
                    |> (\m -> Expect.equal Nothing m.ringPart)
        , test "clicking the same part again keeps it open" <|
            \_ ->
                openRing builtWithOneBrick
                    |> send (StudioMsg (Core.SelectOnly 0))
                    |> (\m -> Expect.equal (Just 0) m.ringPart)
        , test "starting to drag the part closes it" <|
            \_ ->
                let
                    opened =
                        openRing builtWithOneBrick

                    studio =
                        opened.studio

                    dragging =
                        { studio
                            | drag =
                                Just
                                    { kind = PlaneDrag (vec3 0 0 0)
                                    , plane = Nothing
                                    , ids = [ 0 ]
                                    , start = ( 400, 300 )
                                    , lastPoint = ( 430, 300 )
                                    , original = studio.document
                                    , amount = 0
                                    , elevationOffset = 0
                                    , axisLock = Nothing
                                    , modeAnchor = ( 400, 300 )
                                    , baseElevation = 0
                                    , shiftToggle = Nothing
                                    }
                        }
                in
                send (StudioMsg (Core.SelectOnly 0)) { opened | studio = dragging }
                    |> (\m -> Expect.equal Nothing m.ringPart)
        , test "opening Tidy up closes it" <|
            \_ ->
                let
                    tidy =
                        openRing (send (StudioMsg (Core.ResetDocument (Document.parse misaligned))) builtOnly)
                in
                Expect.all
                    [ \_ -> Expect.equal (Just 0) tidy.ringPart
                    , \_ -> Expect.equal Nothing (send OpenTidy tidy).ringPart
                    ]
                    ()
        , test "opening the menu closes it" <|
            \_ ->
                openRing builtWithOneBrick
                    |> send ToggleMenu
                    |> (\m -> Expect.equal Nothing m.ringPart)
        , test "selecting a movable brick shows the Shift · Bit button in the top-left toolbar, which toggles shiftHeld and resets on drag release" <|
            \_ ->
                let
                    unselected =
                        send (StudioMsg Core.ClearSelection) builtWithOneBrick

                    ( x, y ) =
                        screenCentre builtWithOneBrick

                    shifted =
                        send ToggleDragShift builtWithOneBrick

                    dragging =
                        shifted
                            |> send (StudioMsg (Core.ViewportMsg (Viewport.TouchStart [ { id = 1, x = x, y = y } ])))
                            |> send (StudioMsg (Core.ViewportMsg (Viewport.TouchMove [ { id = 1, x = x + 30, y = y } ])))

                    released =
                        send (StudioMsg (Core.ViewportMsg (Viewport.TouchEnd []))) dragging
                in
                Expect.all
                    [ \_ ->
                        unselected
                            |> view
                            |> Query.fromHtml
                            |> Query.findAll [ Selector.attribute (Attr.attribute "data-shift-drag" "") ]
                            |> Query.count (Expect.equal 0)
                    , \_ ->
                        builtWithOneBrick
                            |> view
                            |> Query.fromHtml
                            |> Query.find [ Selector.attribute (Attr.attribute "data-shift-drag" "") ]
                            |> Query.has [ Selector.attribute (Attr.attribute "aria-pressed" "false") ]
                    , \_ -> Expect.equal True (Core.shiftHeld shifted.studio)
                    , \_ ->
                        shifted
                            |> view
                            |> Query.fromHtml
                            |> Query.find [ Selector.attribute (Attr.attribute "data-shift-drag" "") ]
                            |> Query.has [ Selector.attribute (Attr.attribute "aria-pressed" "true") ]
                    , \_ -> Expect.equal False (Core.shiftHeld released.studio)
                    ]
                    ()
        , test "on mobile (narrow window), opening the context ring replaces the bottom palette with the context toolbar" <|
            \_ ->
                let
                    narrow =
                        send (WindowResized 375 667) builtWithOneBrick

                    opened =
                        openRing narrow
                in
                Expect.all
                    [ \_ ->
                        narrow
                            |> view
                            |> Query.fromHtml
                            |> Query.findAll [ Selector.attribute (Attr.attribute "role" "tablist") ]
                            |> Query.count (Expect.equal 1)
                    , \_ ->
                        opened
                            |> view
                            |> Query.fromHtml
                            |> Query.findAll [ Selector.attribute (Attr.attribute "role" "tablist") ]
                            |> Query.count (Expect.equal 0)
                    , \_ ->
                        opened
                            |> view
                            |> Query.fromHtml
                            |> Query.findAll [ Selector.attribute (Attr.attribute "data-context-ring" "") ]
                            |> Query.count (Expect.equal 1)
                    , \_ ->
                        opened
                            |> view
                            |> Query.fromHtml
                            |> Query.find [ Selector.attribute (Attr.attribute "data-shift-drag" "") ]
                            |> Query.has [ Selector.attribute (Attr.attribute "aria-pressed" "false") ]
                    ]
                    ()
        , test "the top-left header holds the menu, help, Shift · Bit, Tidy up and hint pills so space above the palette stays free" <|
            \_ ->
                let
                    withProblems =
                        builtOnly
                            |> send (StudioMsg (Core.ResetDocument (Document.parse misaligned)))
                            |> send (StudioMsg (Core.SelectOnly 0))

                    headerNode =
                        withProblems
                            |> view
                            |> Query.fromHtml
                            |> Query.find [ Selector.tag "header" ]
                in
                Expect.all
                    [ \_ -> headerNode |> Query.findAll [ Selector.attribute (Attr.attribute "aria-label" "Menu") ] |> Query.count (Expect.equal 1)
                    , \_ -> headerNode |> Query.findAll [ Selector.attribute (Attr.attribute "aria-label" "Keyboard shortcuts (?)") ] |> Query.count (Expect.equal 1)
                    , \_ -> headerNode |> Query.findAll [ Selector.attribute (Attr.attribute "data-shift-drag" "") ] |> Query.count (Expect.equal 1)
                    , \_ -> headerNode |> Query.findAll [ Selector.attribute (Attr.attribute "data-tidy-pill" "") ] |> Query.count (Expect.equal 1)
                    , \_ -> headerNode |> Query.findAll [ Selector.attribute (Attr.attribute "data-play-hint" "") ] |> Query.count (Expect.equal 1)
                    ]
                    ()
        ]



-- The controls' messages


ringTests : Test
ringTests =
    describe "the ring's controls"
        [ test "is hidden when its part is off screen" <|
            \_ ->
                Ring.view
                    { part = Nothing
                    , avoid = []
                    , canvas = ( 800, 600 )
                    , free = True
                    , glued = False
                    , movable = True
                    , step = Step.Stud
                    , coarsePointer = False
                    , onTurn = \_ -> NoOp
                    , onRaise = NoOp
                    , onLower = NoOp
                    , onDelete = NoOp
                    , onGlue = NoOp
                    , onMove = \_ -> NoOp
                    , onStep = \_ -> NoOp
                    , noOp = NoOp
                    }
                    |> List.singleton
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.attribute (Attr.attribute "data-context-ring" "") ]
                    |> Query.count (Expect.equal 0)
        , test "Turn left sends a left turn" <|
            \_ ->
                openRing builtWithOneBrick
                    |> view
                    |> Query.fromHtml
                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Turn left") ]
                    |> Event.simulate Event.click
                    |> Event.expect (TurnSelection TurnLeft)
        , test "Delete sends a delete" <|
            \_ ->
                openRing builtWithOneBrick
                    |> view
                    |> Query.fromHtml
                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Delete") ]
                    |> Event.simulate Event.click
                    |> Event.expect Delete
        , test "the W pad button sends a move away" <|
            \_ ->
                openRing builtWithOneBrick
                    |> view
                    |> Query.fromHtml
                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move away (W)") ]
                    |> Event.simulate Event.click
                    |> Event.expect (MoveSelection Away)
        , test "the Raise button sends a raise" <|
            \_ ->
                openRing builtWithOneBrick
                    |> view
                    |> Query.fromHtml
                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Raise") ]
                    |> Event.simulate Event.click
                    |> Event.expect RaiseSelection
        , test "the Lower button sends a lower" <|
            \_ ->
                openRing builtWithOneBrick
                    |> view
                    |> Query.fromHtml
                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Lower") ]
                    |> Event.simulate Event.click
                    |> Event.expect LowerSelection
        , test "the step segments send their step" <|
            \_ ->
                openRing builtWithOneBrick
                    |> view
                    |> Query.fromHtml
                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Bit step") ]
                    |> Event.simulate Event.click
                    |> Event.expect (SetStepSize Step.Bit)
        , test "desktop shows keyboard characters on the movement pad" <|
            \_ ->
                let
                    ring =
                        openRing builtWithOneBrick
                            |> view
                            |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Lower") ] |> Query.has [ Selector.text "Q" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move away (W)") ] |> Query.has [ Selector.text "W" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Raise") ] |> Query.has [ Selector.text "E" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move left (A)") ] |> Query.has [ Selector.text "A" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move toward (S)") ] |> Query.has [ Selector.text "S" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move right (D)") ] |> Query.has [ Selector.text "D" ]
                    ]
                    ()
        , test "mobile / touch device shows arrow icons on the movement pad" <|
            \_ ->
                let
                    ring =
                        freshModel False True
                            |> withBrickGeometry
                            |> send (AddFreePart { file = "3001.dat", color = 14, name = "Brick 2 x 4" })
                            |> scened
                            |> openRing
                            |> view
                            |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Lower") ] |> Query.has [ Selector.tag "svg" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move away (W)") ] |> Query.has [ Selector.tag "svg" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Raise") ] |> Query.has [ Selector.tag "svg" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move left (A)") ] |> Query.has [ Selector.tag "svg" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move toward (S)") ] |> Query.has [ Selector.tag "svg" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move right (D)") ] |> Query.has [ Selector.tag "svg" ]
                    ]
                    ()
        , test "mobile toolbar view shows arrow icons on the movement pad" <|
            \_ ->
                let
                    ring =
                        send (WindowResized 375 667) builtWithOneBrick
                            |> openRing
                            |> view
                            |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Lower") ] |> Query.has [ Selector.tag "svg" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move away (W)") ] |> Query.has [ Selector.tag "svg" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Raise") ] |> Query.has [ Selector.tag "svg" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move left (A)") ] |> Query.has [ Selector.tag "svg" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move toward (S)") ] |> Query.has [ Selector.tag "svg" ]
                    , \_ -> ring |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Move right (D)") ] |> Query.has [ Selector.tag "svg" ]
                    ]
                    ()
        ]



-- Moves: W A S D Q E


moveTests : Test
moveTests =
    describe "W A S D Q E on a selected part"
        (List.map
            (\degrees_ ->
                test ("follows the camera at " ++ String.fromFloat degrees_ ++ " degrees") <|
                    \_ ->
                        let
                            azimuth =
                                degrees degrees_

                            base =
                                builtWithOneBrick |> atAzimuth azimuth

                            right =
                                vec3 (cos azimuth) 0 -(sin azimuth)

                            away =
                                vec3 -(sin azimuth) 0 -(cos azimuth)
                        in
                        Expect.all
                            [ \_ -> Expect.equal (snapToAxis 20 away) (renderMove "w" base)
                            , \_ -> Expect.equal (snapToAxis 20 (Vec3.negate right)) (renderMove "a" base)
                            , \_ -> Expect.equal (snapToAxis 20 right) (renderMove "d" base)
                            , \_ -> Expect.equal (snapToAxis 20 (Vec3.negate away)) (renderMove "s" base)
                            , \_ -> Expect.equal (vec3 0 24 0) (renderMove "e" base)
                            , \_ -> Expect.equal (vec3 0 -24 0) (renderMove "q" base)
                            ]
                            ()
            )
            [ 0, 90, 180, 270, 44, -44 ]
            ++ [ keysWithoutSelectionTests ]
        )


keysWithoutSelectionTests : Test
keysWithoutSelectionTests =
    describe "keys with no part, in Walk mode, and glued parts"
        [ test "with nothing selected W moves the camera, not the part" <|
            \_ ->
                let
                    m =
                        send (StudioMsg Core.ClearSelection) builtWithOneBrick

                    moved =
                        press "w" m
                in
                Expect.all
                    [ \_ -> Expect.equal (positionOf 0 m) (positionOf 0 moved)
                    , \_ -> Expect.equal True (Set.member "w" moved.studio.viewport.heldKeys)
                    ]
                    ()
        , test "in Walk mode W moves the camera even with a part selected" <|
            \_ ->
                let
                    walking =
                        send (StudioMsg (Core.ViewportMsg Viewport.ToggleWalkMode)) builtWithOneBrick

                    moved =
                        press "w" walking
                in
                Expect.all
                    [ \_ -> Expect.equal (positionOf 0 walking) (positionOf 0 moved)
                    , \_ -> Expect.equal True (Set.member "w" moved.studio.viewport.heldKeys)
                    ]
                    ()
        , test "a glued part does not move on W A S D or the pad" <|
            \_ ->
                let
                    glued =
                        send ToggleGlue builtWithOneBrick
                in
                Expect.all
                    [ \_ -> Expect.equal (positionOf 0 glued) (positionOf 0 (press "d" glued))
                    , \_ -> Expect.equal (positionOf 0 glued) (positionOf 0 (send (MoveSelection Right) glued))
                    ]
                    ()
        , test "the pad sends the same move as the key" <|
            \_ ->
                Expect.equal (positionOf 0 (press "d" builtWithOneBrick))
                    (positionOf 0 (send (MoveSelection Right) builtWithOneBrick))
        , test "each move is one undo step" <|
            \_ ->
                let
                    moved =
                        press "d" builtWithOneBrick
                in
                Expect.all
                    [ \_ -> Expect.equal (positionOf 0 builtWithOneBrick) (positionOf 0 (send Undo moved))
                    , \_ -> Expect.notEqual (positionOf 0 builtWithOneBrick) (positionOf 0 moved)
                    ]
                    ()
        ]



-- Steps


stepTests : Test
stepTests =
    describe "the move step"
        [ test "Stud steps move 20 LDU sideways and 24 LDU up or down" <|
            \_ ->
                let
                    base =
                        atAzimuth 0 builtWithOneBrick |> send (SetStepSize Step.Stud)
                in
                Expect.all
                    [ \_ -> Expect.equal (vec3 20 0 0) (delta (send (MoveSelection Right) base) base)
                    , \_ -> Expect.equal (vec3 0 -24 0) (delta (send RaiseSelection base) base)
                    ]
                    ()
        , test "half-stud steps move 10 LDU sideways and 8 LDU up or down" <|
            \_ ->
                let
                    base =
                        atAzimuth 0 builtWithOneBrick |> send (SetStepSize Step.HalfStud)
                in
                Expect.all
                    [ \_ -> Expect.equal (vec3 10 0 0) (delta (send (MoveSelection Right) base) base)
                    , \_ -> Expect.equal (vec3 0 -8 0) (delta (send RaiseSelection base) base)
                    ]
                    ()
        , test "bit steps move 1 LDU on every axis" <|
            \_ ->
                let
                    base =
                        atAzimuth 0 builtWithOneBrick |> send (SetStepSize Step.Bit)
                in
                Expect.all
                    [ \_ -> Expect.equal (vec3 1 0 0) (delta (send (MoveSelection Right) base) base)
                    , \_ -> Expect.equal (vec3 0 -1 0) (delta (send RaiseSelection base) base)
                    , \_ -> Expect.equal True (Core.fineMove base.studio)
                    ]
                    ()
        , test "Shift moves a half-stud on a key, whatever the step is" <|
            \_ ->
                let
                    base =
                        atAzimuth 0 builtWithOneBrick |> send (SetStepSize Step.Stud)
                in
                Expect.all
                    [ \_ -> Expect.equal (vec3 10 0 0) (delta (pressShift "d" base) base)
                    , \_ -> Expect.equal (vec3 0 -8 0) (delta (pressShift "e" base) base)
                    ]
                    ()
        , test "the choice persists across selections" <|
            \_ ->
                let
                    chosen =
                        atAzimuth 0 builtWithOneBrick
                            |> send (SetStepSize Step.HalfStud)
                            |> send (StudioMsg Core.ClearSelection)
                            |> send (AddFreePart { file = "3001.dat", color = 4, name = "Brick 2 x 4" })
                            |> scened
                in
                Expect.all
                    [ \_ -> Expect.equal Step.HalfStud chosen.stepSize
                    , \_ -> Expect.equal (vec3 10 0 0) (selectedDelta (send (MoveSelection Right) chosen) chosen)
                    ]
                    ()
        , test "the step control shows the chosen step with aria-pressed" <|
            \_ ->
                let
                    v =
                        openRing (send (SetStepSize Step.HalfStud) builtWithOneBrick)
                            |> view
                            |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> v |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Half-stud step") ] |> Query.has [ Selector.attribute (Attr.attribute "aria-pressed" "true") ]
                    , \_ -> v |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Stud step") ] |> Query.has [ Selector.attribute (Attr.attribute "aria-pressed" "false") ]
                    ]
                    ()
        ]



-- Tab


tabTests : Test
tabTests =
    describe "Tab selection"
        [ test "Tab walks the build's Tab order and wraps round" <|
            \_ ->
                let
                    m =
                        send (StudioMsg Core.ClearSelection) builtWithTwoBricks

                    order =
                        Core.partOrder m.studio

                    selectedAfter n =
                        Set.toList (List.foldl (\_ acc -> press "Tab" acc) m (List.range 1 n)).studio.selection
                in
                Expect.equal
                    [ List.take 1 order, List.drop 1 order, List.take 1 order ]
                    [ selectedAfter 1, selectedAfter 2, selectedAfter 3 ]
        , test "Shift+Tab goes backward, and wraps to the last part from nothing" <|
            \_ ->
                let
                    m =
                        send (StudioMsg Core.ClearSelection) builtWithTwoBricks

                    order =
                        Core.partOrder m.studio
                in
                Expect.equal (List.drop 1 order)
                    (Set.toList (pressShift "Tab" m).studio.selection)
        , test "Tab does not open the ring" <|
            \_ ->
                press "Tab" builtWithTwoBricks
                    |> (\m -> Expect.equal Nothing m.ringPart)
        , test "Tab does not take a focused button's key" <|
            \_ ->
                Expect.equal builtWithTwoBricks.studio.selection
                    (pressFrom "Tab" "BUTTON" builtWithTwoBricks).studio.selection
        , test "Tab in a form field keeps its normal meaning" <|
            \_ ->
                Expect.equal builtWithTwoBricks.studio.selection
                    (pressFrom "Tab" "INPUT" builtWithTwoBricks).studio.selection
        , test "Tab glides the camera onto the part it selects" <|
            \_ ->
                let
                    m =
                        send (StudioMsg Core.ClearSelection) builtWithTwoBricks

                    tabbed =
                        press "Tab" m
                in
                Expect.equal True (tabbed.studio.viewport.pivotTween /= Nothing || tabbed.studio.viewport.camera.target /= m.studio.viewport.camera.target)
        ]



-- The first-select hint


hintTests : Test
hintTests =
    describe "the first-select hint"
        [ test "shows the first time a part is selected, with the pointer wording" <|
            \_ ->
                let
                    m =
                        send (AddFreePart { file = "3001.dat", color = 14, name = "Brick 2 x 4" }) (freshModel False False)
                in
                Expect.all
                    [ \_ -> Query.fromHtml (view m) |> Query.has [ Selector.text "Double-click a brick for moves" ]
                    , \_ -> Expect.equal True m.hintSeen
                    ]
                    ()
        , test "uses the touch wording on a touch screen" <|
            \_ ->
                send (AddFreePart { file = "3001.dat", color = 14, name = "Brick 2 x 4" }) (freshModel False True)
                    |> view
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Double-tap a brick for moves" ]
        , test "the stored flag suppresses it" <|
            \_ ->
                let
                    m =
                        send (AddFreePart { file = "3001.dat", color = 14, name = "Brick 2 x 4" }) (freshModel True False)
                in
                Expect.all
                    [ \_ -> Query.fromHtml (view m) |> Query.findAll [ Selector.attribute (Attr.attribute "data-play-hint" "") ] |> Query.count (Expect.equal 0)
                    , \_ -> Expect.equal Nothing m.hint
                    ]
                    ()
        , test "goes after its five seconds, and never comes back" <|
            \_ ->
                let
                    shown =
                        send (AddFreePart { file = "3001.dat", color = 14, name = "Brick 2 x 4" }) (freshModel False False)

                    gone =
                        Maybe.map (\id -> send (HideHint id) shown) shown.hint
                            |> Maybe.withDefault shown

                    again =
                        send (StudioMsg (Core.SelectOnly 0)) (send (StudioMsg Core.ClearSelection) gone)
                in
                Expect.all
                    [ \_ -> Expect.equal Nothing gone.hint
                    , \_ -> Expect.equal Nothing again.hint
                    ]
                    ()
        , test "opening the ring puts it away" <|
            \_ ->
                let
                    shown =
                        send (AddFreePart { file = "3001.dat", color = 14, name = "Brick 2 x 4" }) (freshModel False False)
                in
                Expect.equal Nothing (openRing shown).hint
        ]



-- Accessibility and placement


layoutTests : Test
layoutTests =
    describe "the ring's accessibility and placement"
        [ test "every control is a button with an accessible name" <|
            \_ ->
                let
                    v =
                        Query.fromHtml (view (openRing builtWithOneBrick))
                in
                Expect.all
                    (List.map
                        (\label ->
                            \_ ->
                                v
                                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" label) ]
                                    |> Query.has [ Selector.tag "button" ]
                        )
                        [ "Turn left"
                        , "Turn right"
                        , "Tip"
                        , "Raise"
                        , "Lower"
                        , "Delete"
                        , "Glue"
                        , "Move away (W)"
                        , "Move left (A)"
                        , "Move toward (S)"
                        , "Move right (D)"
                        , "Stud step"
                        , "Half-stud step"
                        , "Bit step"
                        ]
                    )
                    ()
        , test "the ring is a labelled group" <|
            \_ ->
                Query.fromHtml (view (openRing builtWithOneBrick))
                    |> Query.find [ Selector.attribute (Attr.attribute "data-context-ring" "") ]
                    |> Query.has [ Selector.attribute (Attr.attribute "aria-label" "Part controls") ]
        , test "the Glue toggle is not offered in a puzzle" <|
            \_ ->
                let
                    puzzle =
                        send (AddFreePart { file = "3001.dat", color = 14, name = "Brick" }) (Tuple.first (init (flagsFor "#model=" False False)))
                in
                Query.fromHtml (view (openRing puzzle))
                    |> Query.findAll [ Selector.attribute (Attr.attribute "aria-label" "Glue") ]
                    |> Query.count (Expect.equal 0)
        , test "the ring sits 12 px to the right of a part's screen box, centred on it" <|
            \_ ->
                Ring.panelPosition
                    { part = screenBox 100 300 160 400, canvas = ( 1000, 800 ), avoid = [] }
                    |> Expect.equal ( 172, 254 )
        , test "the ring flips to the left of a part near the right edge" <|
            \_ ->
                Ring.panelPosition
                    { part = screenBox 900 300 960 400, canvas = ( 1000, 800 ), avoid = [] }
                    |> Expect.equal ( 664, 254 )
        , test "the ring clamps inside the top of the canvas" <|
            \_ ->
                Ring.panelPosition
                    { part = screenBox 100 0 160 40, canvas = ( 1000, 800 ), avoid = [] }
                    |> Expect.equal ( 172, 8 )
        , test "the ring clamps inside the bottom of the canvas" <|
            \_ ->
                Ring.panelPosition
                    { part = screenBox 100 780 160 800, canvas = ( 1000, 800 ), avoid = [] }
                    |> Expect.equal ( 172, 600 )
        , test "the ring moves above a part when the right side would cover the dock" <|
            \_ ->
                let
                    dock =
                        { left = 0, top = 520, right = 1000, bottom = 800 }
                in
                Ring.panelPosition
                    { part = screenBox 100 400 160 500, canvas = ( 1000, 800 ), avoid = [ dock ] }
                    |> Expect.equal ( 18, 196 )
        , test "the canvas centre projects to the centre of the canvas" <|
            \_ ->
                let
                    camera =
                        builtWithOneBrick.studio.viewport.camera
                in
                case Ring.screenPoint camera 800 600 camera.target of
                    Just ( x, y ) ->
                        Expect.all
                            [ \_ -> Expect.within (Expect.Absolute 0.5) 400 x
                            , \_ -> Expect.within (Expect.Absolute 0.5) 300 y
                            ]
                            ()

                    Nothing ->
                        Expect.fail "the camera target should be on screen"
        , test "a point behind the eye has no screen point" <|
            \_ ->
                let
                    camera =
                        builtWithOneBrick.studio.viewport.camera

                    eye =
                        Camera.position camera

                    behind =
                        Vec3.add eye (Vec3.sub eye camera.target)
                in
                Ring.screenPoint camera 800 600 behind
                    |> Expect.equal Nothing
        ]



-- Models and helpers


{-| A screen rectangle from its left, top, right and bottom edges.
-}
screenBox : Float -> Float -> Float -> Float -> Ring.Rect
screenBox left top right bottom =
    { left = left, top = top, right = right, bottom = bottom }


flagsFor : String -> Bool -> Bool -> Flags
flagsFor hash hintSeen coarse =
    { ldrawBase = "/ldraw"
    , ldrawFallbackBase = ""
    , initialHash = hash
    , initialBytes = Nothing
    , supportsPointerEvents = True
    , hintSeen = hintSeen
    , coarsePointer = coarse
    }


{-| Free building (no hash) with the stored hint flag and the touch flag given.
-}
freshModel : Bool -> Bool -> Model
freshModel hintSeen coarse =
    Tuple.first (init (flagsFor "" hintSeen coarse))


builtOnly : Model
builtOnly =
    freshModel False False
        |> withBrickGeometry


{-| Free building with one brick placed and selected (placement id 0), the
camera turned to azimuth 0 unless a test turns it.
-}
builtWithOneBrick : Model
builtWithOneBrick =
    send (AddFreePart { file = "3001.dat", color = 14, name = "Brick 2 x 4" }) builtOnly
        |> scened


{-| Free building with two bricks placed, the second selected.
-}
builtWithTwoBricks : Model
builtWithTwoBricks =
    send (AddFreePart { file = "3001.dat", color = 4, name = "Brick 2 x 4" }) builtWithOneBrick
        |> scened


{-| A 2 x 4 brick's geometry (a 20 x 24 x 20 box), seeded into the part cache as
the studio's own tests do, since the bundle does not embed it.
-}
withBrickGeometry : Model -> Model
withBrickGeometry model =
    let
        studio =
            model.studio

        vp =
            studio.viewport

        -- A closed box, 20 x 24 x 20: the faces a click ray can hit.
        geometry =
            Parser.parseFile
                (String.join "\n"
                    [ "4 16 -10 0 -10 10 0 -10 10 0 10 -10 0 10"
                    , "4 16 -10 24 -10 10 24 -10 10 24 10 -10 24 10"
                    , "4 16 -10 0 -10 -10 0 10 -10 24 10 -10 24 -10"
                    , "4 16 10 0 -10 10 0 10 10 24 10 10 24 -10"
                    , "4 16 -10 0 -10 10 0 -10 10 24 -10 -10 24 -10"
                    , "4 16 -10 0 10 10 0 10 10 24 10 -10 24 10"
                    ]
                )
    in
    { model | studio = { studio | viewport = { vp | partCache = Dict.insert "3001.dat" (Loaded geometry) vp.partCache } } }


{-| Load the build into the viewport, as the viewer does when a model arrives:
the scene, the bounds and the pick target exist afterwards.
-}
scened : Model -> Model
scened model =
    let
        studio =
            model.studio

        ( loaded, _, _ ) =
            Viewport.loadDocument studio.document studio.viewport
    in
    { model | studio = { studio | viewport = loaded } }


{-| The ring open on placement 0.
-}
openRing : Model -> Model
openRing model =
    send (OpenRing 0) model


misaligned : String
misaligned =
    String.join "\n"
        [ "1 14 0 -24 0 1 0 0 0 1 0 0 0 1 3001.dat"
        , "1 14 5 -48 0 1 0 0 0 1 0 0 0 1 3005.dat"
        ]


send : Msg -> Model -> Model
send msg model =
    Tuple.first (update msg model)


press : String -> Model -> Model
press key model =
    pressFrom key "BODY" model


pressFrom : String -> String -> Model -> Model
pressFrom key tag model =
    send (StudioMsg (Core.ViewportMsg (Viewport.KeyPressed (keyEvent key tag False)))) model


pressShift : String -> Model -> Model
pressShift key model =
    send (StudioMsg (Core.ViewportMsg (Viewport.KeyPressed (keyEvent key "BODY" True)))) model


keyEvent : String -> String -> Bool -> KeyEvent
keyEvent key tag shift =
    { key = key
    , ctrl = False
    , meta = False
    , alt = False
    , shift = shift
    , repeat = False
    , targetTag = tag
    , targetEditable = False
    , targetRole = ""
    }


{-| The camera at `azimuth` (radians), as an orbit leaves it.
-}
atAzimuth : Float -> Model -> Model
atAzimuth azimuth model =
    let
        studio =
            model.studio

        vp =
            studio.viewport

        cam =
            vp.camera
    in
    { model | studio = { studio | viewport = { vp | camera = { cam | azimuth = azimuth } } } }


{-| The LDraw origin of placement `id`.
-}
positionOf : Int -> Model -> Vec3
positionOf id model =
    Core.getPlacements model.studio
        |> List.filter (\p -> p.id == id)
        |> List.head
        |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))
        |> Maybe.withDefault (vec3 0 0 0)


{-| The LDraw move of the first brick from `before` to `after`.
-}
delta : Model -> Model -> Vec3
delta after before =
    Vec3.sub (positionOf 0 after) (positionOf 0 before)


{-| The LDraw move of the selected brick from `before` to `after`.
-}
selectedDelta : Model -> Model -> Vec3
selectedDelta after before =
    let
        id =
            Set.toList before.studio.selection |> List.head |> Maybe.withDefault 0
    in
    Vec3.sub (positionOf id after) (positionOf id before)


{-| The render-space move of the first brick for a key press.
-}
renderMove : String -> Model -> Vec3
renderMove key model =
    toRender (Vec3.sub (positionOf 0 (press key model)) (positionOf 0 model))


{-| The screen point of the first brick's centre.
-}
screenCentre : Model -> ( Float, Float )
screenCentre model =
    let
        studio =
            model.studio

        -- The same centre the ring anchors on: the bounding sphere's centre.
        centre =
            studio.viewport.instancedScene
                |> Maybe.andThen (\scene -> List.filter (\p -> p.source == 0) scene.placements |> List.head)
                |> Maybe.map .center
                |> Maybe.withDefault (vec3 0 0 0)
    in
    Ring.screenPoint studio.viewport.camera studio.viewport.width studio.viewport.height centre
        |> Maybe.withDefault ( 0, 0 )


{-| LDraw to render space: Y is up, and Z points the other way.
-}
toRender : Vec3 -> Vec3
toRender v =
    vec3 (Vec3.getX v) -(Vec3.getY v) -(Vec3.getZ v)


{-| A direction snapped to the nearest world axis: its larger ground component,
`step` LDU long.
-}
snapToAxis : Float -> Vec3 -> Vec3
snapToAxis step direction =
    let
        dx =
            Vec3.getX direction

        dz =
            Vec3.getZ direction

        sign v =
            if v < 0 then
                -1

            else
                1
    in
    if abs dx >= abs dz then
        vec3 (step * sign dx) 0 0

    else
        vec3 0 0 (step * sign dz)
