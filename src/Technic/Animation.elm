module Technic.Animation exposing
    ( Animated
    , isAnimating
    , none
    , rotating
    )

{-| Which parts Brick Play draws turning in Technic mode, and which static copies
it hides while it does.

The turning parts are drawn by the shared `Simulation` component: the gears
(`Simulation.gearEntitiesFor`), one baked mesh per gear rotated about its own
axle, and the driven parts riding on them (`Simulation.drivenEntitiesFor`) —
axles a gear sits on and parts threaded onto a gear's axle. The studio's
instanced scene still holds every one of those placements, so while the machine
is animated their placements are hidden from it (`Viewport.setHiddenSources`);
otherwise the static copy would show through the moving one. Gears outside the
motor's gear train, and axles no turning gear drives, are neither animated nor
hidden: they stay in the static scene.

-}

import Dict exposing (Dict)
import Gear.Types exposing (GearId)
import Set exposing (Set)


{-| The gears drawn turning (their driven parts too), and the document
placements (viewport sources) hidden from the static scene because those
turning parts stand in for them.
-}
type alias Animated =
    { gears : Set GearId
    , sources : Set Int
    }


{-| Nothing animated: every part drawn by the static scene.
-}
none : Animated
none =
    { gears = Set.empty, sources = Set.empty }


{-| The machine is shown turning while it plays, and also while it is paused
part-way (time > 0), so a pause freezes the gears where they are. At time 0 the
gears sit exactly where the static parts are, so the static scene shows them.
-}
isAnimating : { playing : Bool, currentTime : Float } -> Bool
isAnimating { playing, currentTime } =
    playing || currentTime > 0


{-| From the gears that turn with the motor (`Simulation.rotatingGearIds`), the
placement of each gear, and the placement of each driven part paired with its
driving gear (`Placements.partSources`) — the gears to animate, and the
placements to hide: the gears' own placements, plus the driven parts whose
driving gear is animated. A gear without a main-model placement is not animated.
-}
rotating : Set GearId -> Dict GearId Int -> List ( GearId, Int ) -> Animated
rotating turning gearSources drivenSources =
    let
        gearPairs =
            Dict.toList gearSources
                |> List.filter (\( gid, _ ) -> Set.member gid turning)

        gears =
            Set.fromList (List.map Tuple.first gearPairs)

        partPairs =
            drivenSources
                |> List.filter (\( gid, _ ) -> Set.member gid gears)
    in
    { gears = gears
    , sources =
        Set.fromList (List.map Tuple.second gearPairs ++ List.map Tuple.second partPairs)
    }
