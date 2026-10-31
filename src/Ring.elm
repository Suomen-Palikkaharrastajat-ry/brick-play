module Ring exposing
    ( Config
    , Rect
    , mobileView
    , panelHeight
    , panelPosition
    , panelWidth
    , screenPoint
    , screenRect
    , view
    )

{-| The context ring: the controls that float beside a selected part in Brick
Play. It opens on a double-click or double-tap of a part, and holds the part's
Turn left, Turn right, Tip, Raise, Lower and Delete buttons, Glue (free building
only), a 3×2 movement pad (QWE / ASD), and the move-step control (`Step.StepSize`: Stud,
½ stud, Bit).

The ring is a compact 224×192 px pod of 44 px touch buttons. `panelPosition`
places it beside the part's screen rectangle (its projected bounding box), 12 px
to the right, flipped to the left, then above or below when neither side has room.
It is kept inside the canvas and clear of the dock and camera globe when it can be.
`screenPoint` and `screenRect` project render-space points with the camera, the
same matrices the studio's outlines use. All are pure.

Every button is a real `<button>` with an `aria-label`, and its tooltip gives the
shortcut. Buttons do not take focus on a mouse press (`UI.Focus.keepFocusOnCanvas`),
so the keyboard shortcuts keep working after a click on the ring.

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)
import Render.Camera as Camera exposing (Camera)
import Step
import Studio.Shortcuts exposing (NudgeDirection(..), TurnDirection(..))
import UI.Focus as Focus
import UI.Icon as Icon


{-| A rectangle on screen, in canvas pixels.
-}
type alias Rect =
    { left : Float
    , top : Float
    , right : Float
    , bottom : Float
    }


{-| What the ring shows and what its buttons send.

`part` is the part's screen rectangle, `Nothing` while it is off screen (the ring
then stays hidden). `canvas` is the canvas size in pixels, and `avoid` lists the
rect rectangles the ring keeps clear of when it can (the dock and the camera globe).
`movable` is False when the selection is glued, and `glued` says whether it is.
`coarsePointer` is True on touch/mobile devices, which shows directional arrow icons
on the movement pad instead of keyboard characters (Q, W, E, A, S, D).

-}
type alias Config msg =
    { part : Maybe Rect
    , canvas : ( Float, Float )
    , avoid : List Rect
    , free : Bool
    , glued : Bool
    , movable : Bool
    , step : Step.StepSize
    , coarsePointer : Bool
    , onTurn : TurnDirection -> msg
    , onRaise : msg
    , onLower : msg
    , onDelete : msg
    , onGlue : msg
    , onMove : NudgeDirection -> msg
    , onStep : Step.StepSize -> msg
    , noOp : msg
    }


{-| The ring's width in pixels.
-}
panelWidth : Float
panelWidth =
    224


{-| The ring's height in pixels, for keeping it inside the canvas.
-}
panelHeight : Float
panelHeight =
    192


{-| The gap, in pixels, between a part's screen rectangle and the ring.
-}
partGap : Float
partGap =
    12


{-| The top-left corner of the ring beside a part's screen rectangle, on a canvas
of `canvas` pixels.

It tries four places in order: right of the part, left of it, above it and below
it. Right and left are centred on the part vertically, above and below are centred
on it horizontally, and each place is kept inside the canvas with an 8 px margin.
The first place that fits the canvas and clears every `avoid` rectangle wins; failing
that, the first that fits the canvas; failing that, the right side clamped inside it.

-}
panelPosition : { part : Rect, canvas : ( Float, Float ), avoid : List Rect } -> ( Float, Float )
panelPosition { part, canvas, avoid } =
    let
        ( canvasWidth, canvasHeight ) =
            canvas

        inset =
            8

        centreX =
            (part.left + part.right) / 2

        centreY =
            (part.top + part.bottom) / 2

        clampX x =
            clampTo inset (canvasWidth - panelWidth - inset) x

        clampY y =
            clampTo inset (canvasHeight - panelHeight - inset) y

        sideY =
            clampY (centreY - panelHeight / 2)

        candidates =
            [ ( part.right + partGap + panelWidth <= canvasWidth - inset
              , ( part.right + partGap, sideY )
              )
            , ( part.left - partGap - panelWidth >= inset
              , ( part.left - partGap - panelWidth, sideY )
              )
            , ( part.top - partGap - panelHeight >= inset
              , ( clampX (centreX - panelWidth / 2), part.top - partGap - panelHeight )
              )
            , ( part.bottom + partGap + panelHeight <= canvasHeight - inset
              , ( clampX (centreX - panelWidth / 2), part.bottom + partGap )
              )
            ]

        fitting =
            candidates
                |> List.filter Tuple.first
                |> List.map Tuple.second

        clear =
            List.filter (\( x, y ) -> not (List.any (overlaps (rectAt x y)) avoid)) fitting
    in
    case clear of
        first :: _ ->
            first

        [] ->
            case fitting of
                first :: _ ->
                    first

                [] ->
                    ( clampX (part.right + partGap), sideY )


{-| The rectangle of a ring whose top-left corner is at (x, y).
-}
rectAt : Float -> Float -> Rect
rectAt x y =
    { left = x, top = y, right = x + panelWidth, bottom = y + panelHeight }


{-| Whether two rectangles share any area.
-}
overlaps : Rect -> Rect -> Bool
overlaps a b =
    a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom


clampTo : Float -> Float -> Float -> Float
clampTo low high value =
    max low (min high value)


{-| The pixel a render-space point appears at on a canvas of `width` by `height`,
or `Nothing` when it is behind the eye or the canvas is empty.
-}
screenPoint : Camera -> Int -> Int -> Vec3 -> Maybe ( Float, Float )
screenPoint camera width height point =
    if width <= 0 || height <= 0 then
        Nothing

    else
        let
            viewMatrix =
                Camera.viewMatrix camera

            projection =
                Camera.projectionMatrix
                    (toFloat width / toFloat height)
                    (Camera.nearPlane camera)
                    (Camera.farPlane camera)

            -- The eye looks down -Z, so a point in front has a negative depth.
            depth =
                Vec3.getZ (Mat4.transform viewMatrix point)

            ndc =
                Mat4.transform (Mat4.mul projection viewMatrix) point

            x =
                (Vec3.getX ndc + 1) / 2 * toFloat width

            y =
                (1 - Vec3.getY ndc) / 2 * toFloat height
        in
        if depth >= 0 || isNaN x || isNaN y || isInfinite x || isInfinite y then
            Nothing

        else
            Just ( x, y )


{-| The smallest screen rectangle around the points that project onto a canvas of
`width` by `height`, or `Nothing` when none does.
-}
screenRect : Camera -> Int -> Int -> List Vec3 -> Maybe Rect
screenRect camera width height points =
    case List.filterMap (screenPoint camera width height) points of
        ( x0, y0 ) :: rest ->
            Just
                (List.foldl
                    (\( x, y ) r ->
                        { left = min r.left x
                        , top = min r.top y
                        , right = max r.right x
                        , bottom = max r.bottom y
                        }
                    )
                    { left = x0, top = y0, right = x0, bottom = y0 }
                    rest
                )

        [] ->
            Nothing


{-| The ring over the canvas. Empty when `part` is `Nothing`.
-}
view : Config msg -> Html msg
view config =
    case config.part of
        Nothing ->
            text ""

        Just part ->
            let
                ( left, top ) =
                    panelPosition { part = part, canvas = config.canvas, avoid = config.avoid }
            in
            div
                [ Attr.attribute "role" "group"
                , Attr.attribute "aria-label" "Part controls"
                , Attr.attribute "data-context-ring" ""
                , Attr.style "position" "absolute"
                , Attr.style "left" (px left)
                , Attr.style "top" (px top)
                , Attr.style "width" (px panelWidth)
                , Attr.style "height" (px panelHeight)
                , Attr.style "box-sizing" "border-box"
                , Attr.style "padding" "0.375rem"
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "justify-content" "space-between"
                , Attr.style "align-items" "center"
                , Attr.style "background" "rgba(5, 19, 29, 0.68)"
                , Attr.style "backdrop-filter" "blur(6px)"
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
                , Attr.style "border-radius" "1rem"
                , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
                , Attr.style "pointer-events" "auto"
                , Attr.style "z-index" "12"
                ]
                [ headerRow config
                , mainControlsPod config
                ]


{-| The context toolbar on mobile, replacing the bottom palette while a part's
context menu is open.
-}
mobileView : Config msg -> Html msg
mobileView config =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Part controls"
        , Attr.attribute "data-context-ring" ""
        , Attr.style "width" "100%"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "padding" "0.5rem"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "justify-content" "space-between"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.375rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.82)"
        , Attr.style "backdrop-filter" "blur(10px)"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "1rem"
        , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
        , Attr.style "pointer-events" "auto"
        ]
        [ headerRow config
        , mainControlsPod { config | coarsePointer = True }
        ]


px : Float -> String
px value =
    String.fromInt (round value) ++ "px"


{-| The top header row: step size selector pill and utility actions (Glue, Delete).
-}
headerRow : Config msg -> Html msg
headerRow config =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "width" "100%"
        , Attr.style "gap" "0.25rem"
        ]
        [ stepRow config
        , utilityRow config
        ]


{-| The utility actions: Glue toggle (free building only) and Delete.
-}
utilityRow : Config msg -> Html msg
utilityRow config =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.25rem"
        ]
        ((if config.free then
            [ glueButton config ]

          else
            []
         )
            ++ [ actionButton config config.onDelete "trash-2" "Delete" "Delete" ]
        )


{-| The main controls pod: a 3×2 movement pad on the left (QWE / ASD) and
a rotation column on the right (Turn left, Turn right, Tip).
-}
mainControlsPod : Config msg -> Html msg
mainControlsPod config =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "width" "100%"
        ]
        [ movementPad config
        , rotationColumn config
        ]


{-| Symmetric 3×2 movement pad:
Row 1: Lower (Q), Move away (W), Raise (E)
Row 2: Move left (A), Move toward (S), Move right (D)

On desktop (`coarsePointer` False), the buttons show keyboard characters
("Q", "W", "E", "A", "S", "D"). On touch/mobile devices (`coarsePointer` True),
they show directional arrow icons.

-}
movementPad : Config msg -> Html msg
movementPad config =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Move the part"
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(3, 2.75rem)"
        , Attr.style "grid-template-rows" "repeat(2, 2.75rem)"
        , Attr.style "gap" "0.25rem"
        , Attr.style "justify-items" "center"
        , Attr.style "align-items" "center"
        ]
        [ padButton config config.onLower "chevrons-down" "Q" "Lower" "Lower (Q)"
        , padButton config (config.onMove Away) "chevron-up" "W" "Move away (W)" "Move away (W)"
        , padButton config config.onRaise "chevrons-up" "E" "Raise" "Raise (E)"
        , padButton config (config.onMove Left) "chevron-left" "A" "Move left (A)" "Move left (A)"
        , padButton config (config.onMove Toward) "chevron-down" "S" "Move toward (S)" "Move toward (S)"
        , padButton config (config.onMove Right) "chevron-right" "D" "Move right (D)" "Move right (D)"
        ]


padButton : Config msg -> msg -> String -> String -> String -> String -> Html msg
padButton config msg icon char label tooltip =
    roundButton
        { noOp = config.noOp
        , enabled = config.movable
        , onPress = msg
        , label = label
        , tooltip = tooltip
        , pressed = Nothing
        }
        (padButtonContent config.coarsePointer icon char)


padButtonContent : Bool -> String -> String -> Html msg
padButtonContent coarsePointer icon char =
    if coarsePointer then
        Icon.featherSized 18 icon

    else
        span
            [ Attr.style "font-weight" "600"
            , Attr.style "font-size" "1rem"
            , Attr.style "line-height" "1"
            ]
            [ text char ]


{-| Right rotation column: Turn left (rotate-ccw), Turn right (rotate-cw), and Tip (corner-right-down).
-}
rotationColumn : Config msg -> Html msg
rotationColumn config =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Rotation controls"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "0.125rem"
        , Attr.style "width" "2.75rem"
        ]
        [ actionButton config (config.onTurn TurnLeft) "rotate-ccw" "Turn left" "←"
        , actionButton config (config.onTurn TurnRight) "rotate-cw" "Turn right" "→"
        , actionButtonWithTooltip config (config.onTurn TipForward) "corner-right-down" "Tip" "Tip forward (↑)"
        ]


actionButton : Config msg -> msg -> String -> String -> String -> Html msg
actionButton config msg icon label shortcut =
    actionButtonWithTooltip config msg icon label (label ++ " (" ++ shortcut ++ ")")


actionButtonWithTooltip : Config msg -> msg -> String -> String -> String -> Html msg
actionButtonWithTooltip config msg icon label tooltip =
    roundButton
        { noOp = config.noOp
        , enabled = config.movable
        , onPress = msg
        , label = label
        , tooltip = tooltip
        , pressed = Nothing
        }
        (Icon.featherSized 18 icon)


{-| The Glue toggle: `aria-pressed` is on while the selection is glued, and a
press glues the unglued parts or unglues the glued ones.
-}
glueButton : Config msg -> Html msg
glueButton config =
    roundButton
        { noOp = config.noOp
        , enabled = True
        , onPress = config.onGlue
        , label =
            if config.glued then
                "Glued"

            else
                "Glue"
        , tooltip = "Glue the selection to the board, locked (L)"
        , pressed = Just config.glued
        }
        (Icon.featherSized 18
            (if config.glued then
                "lock"

             else
                "unlock"
            )
        )


{-| The move-step control: three round icon buttons for Stud, ½ stud, Bit.
-}
stepRow : Config msg -> Html msg
stepRow config =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Move step"
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.125rem"
        , Attr.style "padding" "0.125rem"
        , Attr.style "border-radius" "9999px"
        , Attr.style "background" "rgba(255, 255, 255, 0.08)"
        ]
        (List.map (stepSegment config) Step.all)


stepSegment : Config msg -> Step.StepSize -> Html msg
stepSegment config size =
    let
        pressed =
            config.step == size
    in
    button
        [ Attr.type_ "button"
        , Attr.attribute "aria-label" (stepName size)
        , Attr.attribute "aria-pressed" (boolText pressed)
        , Attr.title (Step.title size)
        , onClick (config.onStep size)
        , Focus.keepFocusOnCanvas config.noOp
        , Attr.style "width" "2.25rem"
        , Attr.style "height" "2.25rem"
        , Attr.style "min-width" "2.25rem"
        , Attr.style "min-height" "2.25rem"
        , Attr.style "padding" "0"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "9999px"
        , Attr.style "background" (fill pressed)
        , Attr.style "color" (ink pressed)
        , Attr.style "cursor" "pointer"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        ]
        [ Icon.featherSized 16 (Step.icon size) ]


stepName : Step.StepSize -> String
stepName size =
    case size of
        Step.Stud ->
            "Stud step"

        Step.HalfStud ->
            "Half-stud step"

        Step.Bit ->
            "Bit step"


{-| A 44 px round button. Disabled buttons are dimmed and send nothing.
-}
roundButton :
    { noOp : msg
    , enabled : Bool
    , onPress : msg
    , label : String
    , tooltip : String
    , pressed : Maybe Bool
    }
    -> Html msg
    -> Html msg
roundButton spec content =
    button
        ([ Attr.type_ "button"
         , Attr.attribute "aria-label" spec.label
         , Attr.title spec.tooltip
         , Attr.disabled (not spec.enabled)
         , Focus.keepFocusOnCanvas spec.noOp
         , Attr.style "width" "2.75rem"
         , Attr.style "height" "2.75rem"
         , Attr.style "min-width" "2.75rem"
         , Attr.style "min-height" "2.75rem"
         , Attr.style "padding" "0"
         , Attr.style "display" "flex"
         , Attr.style "align-items" "center"
         , Attr.style "justify-content" "center"
         , Attr.style "border" "none"
         , Attr.style "border-radius" "9999px"
         , Attr.style "background" (fill (spec.pressed == Just True))
         , Attr.style "color" (ink (spec.pressed == Just True))
         , Attr.style "font-size" "0.875rem"
         , Attr.style "opacity" (opacity spec.enabled)
         , Attr.style "cursor" (cursor spec.enabled)
         ]
            ++ (case spec.pressed of
                    Just pressed ->
                        [ Attr.attribute "aria-pressed" (boolText pressed) ]

                    Nothing ->
                        []
               )
            ++ (if spec.enabled then
                    [ onClick spec.onPress ]

                else
                    []
               )
        )
        [ content ]


fill : Bool -> String
fill on =
    if on then
        "var(--color-brand-yellow)"

    else
        "rgba(255, 255, 255, 0.1)"


ink : Bool -> String
ink on =
    if on then
        "var(--color-brand)"

    else
        "var(--color-text-on-dark)"


opacity : Bool -> String
opacity enabled =
    if enabled then
        "1"

    else
        "0.4"


cursor : Bool -> String
cursor enabled =
    if enabled then
        "pointer"

    else
        "not-allowed"


boolText : Bool -> String
boolText on =
    if on then
        "true"

    else
        "false"
