module Technic.Machine exposing
    ( MachineState
    , analyseMachine
    , isAccomplished
    )

{-| Technic machine analysis and accomplishment evaluation.

Determines whether all placed gears in the document form a single unified
machine where every gear is meshed and rotating together in real time.

-}

import Data
import Dict
import Gear.Detect as Detect
import Gear.Physics as Physics
import Gear.Types exposing (GearGraph, GearId, GearInstance)
import LDraw.Document as Document exposing (Document)
import LDraw.Resolve exposing (PartCache)


{-| Analyzed state of the Technic machine.
-}
type alias MachineState =
    { gears : List GearInstance
    , gearGraph : GearGraph
    , rotatingCount : Int
    , isSingleMachine : Bool
    , motorGearId : Maybe GearId
    }


{-| Analyze the document for Technic gears and mesh connections.
-}
analyseMachine : Document -> PartCache -> MachineState
analyseMachine document partCache =
    let
        lines =
            Document.toLines document

        gears =
            Detect.extractGears Data.gearParts lines partCache

        graph =
            Detect.buildGearGraph gears
    in
    case gears of
        [] ->
            { gears = []
            , gearGraph = graph
            , rotatingCount = 0
            , isSingleMachine = False
            , motorGearId = Nothing
            }

        firstGear :: _ ->
            let
                motorId =
                    firstGear.id

                -- Propagate with arbitrary angle to identify all reachable gears
                reachedAngles =
                    Physics.propagate graph motorId 1.0

                connectedCount =
                    Dict.size reachedAngles

                singleMachine =
                    List.length gears >= 2 && connectedCount == List.length gears
            in
            { gears = gears
            , gearGraph = graph
            , rotatingCount = connectedCount
            , isSingleMachine = singleMachine
            , motorGearId = Just motorId
            }


{-| Evaluates whether the machine goal is accomplished:
"Technic play is accomplished when all gears are rotating and part of the same machine".
-}
isAccomplished : MachineState -> Bool
isAccomplished state =
    state.isSingleMachine && state.rotatingCount >= 2 && state.rotatingCount == List.length state.gears
