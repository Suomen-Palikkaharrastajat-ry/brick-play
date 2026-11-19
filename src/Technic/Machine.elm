module Technic.Machine exposing
    ( MachineState
    , analyseMachine
    , analyseMachineWithMotor
    , isAccomplished
    )

{-| Technic machine analysis and accomplishment evaluation.

Determines whether all placed gears in the document form a single unified
machine where every gear is meshed and rotating together in real time.

-}

import Array
import Data
import Dict
import Gear.Detect as Detect
import Gear.Physics as Physics
import Gear.Types exposing (GearGraph, GearId, GearInstance)
import LDraw.Document as Document exposing (Document)
import LDraw.Resolve exposing (PartCache)


{-| Analyzed state of the Technic machine.

`gearGraph` retains every extracted gear instance (and its original ID),
including fixed parts. `gears` is the rotatable display/accomplishment set when
there is a motor-capable gear; it is empty when the graph has no motor source.

-}
type alias MachineState =
    { gears : List GearInstance
    , gearGraph : GearGraph
    , rotatingCount : Int
    , isSingleMachine : Bool
    , motorGearId : Maybe GearId
    }


{-| Analyze the document for Technic gears and mesh connections using default automatic motor.
-}
analyseMachine : Document -> PartCache -> MachineState
analyseMachine document partCache =
    analyseMachineWithMotor Nothing document partCache


{-| Analyze the document for Technic gears and mesh connections with an optional requested motor gear ID.
-}
analyseMachineWithMotor : Maybe GearId -> Document -> PartCache -> MachineState
analyseMachineWithMotor requestedMotor document partCache =
    let
        lines =
            Document.toLines document

        allGears =
            Detect.extractGears Data.gearParts lines partCache

        rotatingGears =
            List.filter (\gear -> gear.spec.rotates) allGears

        graph =
            Detect.buildGearGraph allGears

        selectedMotor =
            case requestedMotor of
                Just reqId ->
                    case Array.get reqId graph.instances of
                        Just g ->
                            if g.spec.rotates && g.spec.motorCapable then
                                Just reqId

                            else
                                Physics.defaultMotorGearId graph

                        Nothing ->
                            Physics.defaultMotorGearId graph

                Nothing ->
                    Physics.defaultMotorGearId graph
    in
    case selectedMotor of
        Nothing ->
            { gears = []
            , gearGraph = graph
            , rotatingCount = 0
            , isSingleMachine = False
            , motorGearId = Nothing
            }

        Just motorId ->
            let
                -- Propagate with arbitrary angle to identify all reachable gears
                reachedAngles =
                    Physics.propagate graph motorId 1.0

                connectedCount =
                    Dict.size reachedAngles

                singleMachine =
                    List.length rotatingGears >= 2 && connectedCount == List.length rotatingGears
            in
            { gears = rotatingGears
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
