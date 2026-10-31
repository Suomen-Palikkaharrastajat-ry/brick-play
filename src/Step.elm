module Step exposing
    ( StepSize(..)
    , all
    , apply
    , icon
    , steps
    , title
    )

{-| The move step Brick Play uses for keyboard nudges, the context ring's moves
and body drags. It is the segmented control in the ring (Stud, ½ stud, Bit):

  - `Stud`: 20 LDU sideways and 24 LDU (one brick) up or down. The default.
  - `HalfStud`: 10 LDU sideways and 8 LDU (one plate) up or down, a deliberate
    half-stud offset.
  - `Bit`: 1 LDU on every axis, the fine move that Shift gives for a single key
    or drag.

`apply` writes the choice into the studio (`Studio.Core.setNudgeSteps` and
`setFineMove`), so keys, the ring and body drags all follow it. The choice lives
in Brick Play's model and persists across selections.

-}

import Studio.Core as Core


{-| The three move steps.
-}
type StepSize
    = Stud
    | HalfStud
    | Bit


{-| The steps in the order the control shows them.
-}
all : List StepSize
all =
    [ Stud, HalfStud, Bit ]


{-| The sideways and vertical move of a step, in LDU.
-}
steps : StepSize -> { horizontal : Float, vertical : Float }
steps size =
    case size of
        Stud ->
            { horizontal = 20, vertical = 24 }

        HalfStud ->
            { horizontal = 10, vertical = 8 }

        Bit ->
            { horizontal = 1, vertical = 1 }


{-| The icon name for a step size button.
-}
icon : StepSize -> String
icon size =
    case size of
        Stud ->
            "grid"

        HalfStud ->
            "columns"

        Bit ->
            "crosshair"


{-| The accessible name and tooltip of a segment, with what it moves.
-}
title : StepSize -> String
title size =
    case size of
        Stud ->
            "Stud step: 20 LDU sideways, 24 LDU up and down"

        HalfStud ->
            "Half-stud step: 10 LDU sideways, 8 LDU up and down"

        Bit ->
            "Bit step: 1 LDU on every axis (Shift is a temporary bit step)"


{-| Set the studio's move steps and fine mode to a step.
-}
apply : StepSize -> Core.Model -> Core.Model
apply size studio =
    studio
        |> Core.setNudgeSteps (steps size)
        |> Core.setFineMove (size == Bit)
