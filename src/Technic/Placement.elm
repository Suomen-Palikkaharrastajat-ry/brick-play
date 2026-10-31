module Technic.Placement exposing
    ( Target(..)
    , axleLineY
    , onAxleLine
    , rowSpot
    , targetFor
    )

{-| Where a part added from the dock goes.

Every Technic part in the catalog has its origin on its own axis or hole line
(measured from the part files' bounds):

  - gears (`3647` 8T, `4019` 16T, `3648` 24T, `3649` 40T): the origin is the
    gear's centre, its axis is local Z (bounds symmetric in X/Y, ±10 in Z; the
    40T reaches 52.4 LDU from the centre)
  - axles (`3704`–`3707`): the origin is the middle of the axle, which runs
    along local X (cross-section ±6)
  - beams (`32316`, `32524`, `40490`): the origin is the middle hole; the holes
    are spaced along Z, each with its axis along X — the default axle direction
  - pin `2780` (axis X) and bush `6590` (axis Z): the origin is their centre

So all of them are added with their origin on one **axle line** at
`axleLineY` (LDraw, Y down: 60 LDU above the ground), high enough for a 40T to
clear the ground (its teeth reach Y −7.6). Gears on that line are always at the
same height whatever their size, so two of them can be brought to mesh distance
by sliding along the ground only (W A S D, drags with settling off). Parts that
are not in the catalog (a project's own parts) rest on the ground as in Brick
Play.

A new gear goes into the selected part's plane (same Z, same height), on the
first free spot along X next to it, so the next press of A or D in the front
view moves it towards a mesh. Everything else takes the first free grid spot
around the selection (`Studio.Core.freeSpotForPlacement`).

-}

import Data
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set
import Studio.Core as Core
import Studio.DragPlane as DragPlane exposing (Box)
import Technic.Catalog as Catalog exposing (Category(..))


{-| How a dock part is added: at exactly this placement (`Core.addPartAt`), or
resting on the ground at this spot (`Core.addPart`).
-}
type Target
    = AtPlacement Mat4
    | OnGround Vec3


{-| The height (LDraw Y, negative is up) of the common axle line every gear,
axle, beam, pin and bush is added on.
-}
axleLineY : Float
axleLineY =
    -60


{-| Whether a part is added on the axle line: a catalog Technic part or any
known gear.
-}
onAxleLine : String -> Bool
onAxleLine file =
    List.member file technicFiles || isGear file


technicFiles : List String
technicFiles =
    [ GearsCategory, BeamsCategory, AxlesCategory, PinsCategory ]
        |> List.concatMap Catalog.partsForCategory
        |> List.map .file


isGear : String -> Bool
isGear file =
    List.any (\spec -> spec.partFile == file) Data.gearParts


{-| A part's local box (`Core.partBox`), grown for a gear to at least its tip
circle (pitch radius + 2.5 LDU, ±10 along its Z axis): until a part's geometry
is built the studio only knows its connectivity envelope, which for a gear is
smaller than its teeth.
-}
localBox : String -> Core.Model -> Box
localBox file studio =
    let
        box =
            Core.partBox file studio
    in
    case List.filter (\spec -> spec.partFile == file) Data.gearParts of
        spec :: _ ->
            let
                tip =
                    spec.pitchRadius + 2.5
            in
            DragPlane.unionBox [ box.min, box.max, vec3 -tip -tip -10, vec3 tip tip 10 ]
                |> Maybe.withDefault box

        [] ->
            box


{-| Where to add `file` in this studio (see the module documentation).
-}
targetFor : String -> Core.Model -> Target
targetFor file studio =
    if not (onAxleLine file) then
        OnGround (Core.freeSpotFor file studio)

    else
        let
            onLine =
                Mat4.makeTranslate3 0 axleLineY 0

            others =
                Core.getPlacements studio
                    |> List.map (\p -> DragPlane.worldBox p.matrix (localBox p.part studio))

            step =
                toFloat (max 1 studio.gridStep)

            inRow =
                if isGear file then
                    rowSpot
                        { box = DragPlane.worldBox onLine (localBox file studio)
                        , start = rowStart step studio
                        , step = step
                        , others = others
                        }

                else
                    Nothing
        in
        case inRow of
            Just ( x, z ) ->
                AtPlacement (Mat4.makeTranslate3 x axleLineY z)

            Nothing ->
                AtPlacement (Core.freeSpotForPlacement file onLine studio)


{-| Where a gear row starts (X, Z in LDraw): the selection's centre exactly, so
the new gear stays in the selected gear's plane and a whole number of grid steps
from it; else the camera target, rounded to the grid.
-}
rowStart : Float -> Core.Model -> ( Float, Float )
rowStart step studio =
    let
        selected =
            Core.getPlacements studio
                |> List.filter (\p -> Set.member p.id studio.selection)
                |> List.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))

        count =
            toFloat (List.length selected)

        snap v =
            toFloat (round (v / step)) * step
    in
    if List.isEmpty selected then
        -- Render space negates Y and Z: LDraw Z is −render Z.
        ( snap (Vec3.getX studio.viewport.camera.target), snap -(Vec3.getZ studio.viewport.camera.target) )

    else
        ( List.sum (List.map Vec3.getX selected) / count, List.sum (List.map Vec3.getZ selected) / count )


{-| The first free spot for a part along one row: `start` itself, then one grid
`step` to +X, one to −X, two to +X, … (Z stays `start`'s), up to 64 steps each
way. `box` is the part's world box (LDraw) with its origin at X = Z = 0;
`others` are the placed parts' world boxes. `Nothing` when the whole row is
taken.
-}
rowSpot : { box : Box, start : ( Float, Float ), step : Float, others : List Box } -> Maybe ( Float, Float )
rowSpot { box, start, step, others } =
    let
        ( sx, sz ) =
            start

        free x =
            let
                shift =
                    vec3 x 0 sz

                candidate =
                    { min = Vec3.add box.min shift, max = Vec3.add box.max shift }
            in
            not (List.any (DragPlane.boxesOverlap candidate) others)

        offsets =
            0 :: List.concatMap (\k -> [ k, -k ]) (List.range 1 64)
    in
    offsets
        |> List.map (\k -> sx + toFloat k * max 1 step)
        |> List.filter free
        |> List.head
        |> Maybe.map (\x -> ( x, sz ))
