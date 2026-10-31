module Link exposing
    ( editableHash
    , editablePayload
    , fixedModelHash
    , fixedModelPayload
    , freeHash
    , freePayload
    , freeShareHash
    , freeSharePayload
    , partialSetHash
    , partialSetPayload
    , puzzleHash
    , puzzlePayload
    , setShareHash
    , setSharePayload
    )

{-| High-density binary link payloads for Brick Play builds.

  - Editable model (`freePayload` / `editablePayload`): encodes the whole board,
    glued and unglued placements included, opening in free building mode with the
    full catalog.
  - Parts set (`freeSharePayload` / `setSharePayload`): keeps only the glued
    parts on the board and puts every unglued part into the limited counted kit,
    opening as a parts set.
  - Partially built set (`puzzlePayload` / `partialSetPayload`): encodes the
    board (both glued and placed unglued parts) and the counted kit.
  - Fixed model (`fixedModelPayload`): glues every part on the board and clears
    the kit (`Just []`), opening as a fixed model with no catalog.

-}

import LDraw.Document as Document exposing (Document)
import Studio.HashPayload as HashPayload exposing (KitItem)


{-| The binary payload of an editable model (free building): the whole board,
glue included, with the full catalog.
-}
editableHash : Document -> List Int
editableHash =
    editablePayload


{-| The binary payload of an editable model (free building): the whole board,
glue included, with the full catalog.
-}
editablePayload : Document -> List Int
editablePayload =
    freePayload


{-| The binary payload of free building: the whole board, glue included.
-}
freeHash : Document -> List Int
freeHash =
    freePayload


{-| The binary payload of free building: the whole board, glue included.
-}
freePayload : Document -> List Int
freePayload doc =
    HashPayload.encodeBytes
        { mode = HashPayload.FreeBuilding
        , document = doc
        , kit = Nothing
        }


{-| The binary payload of a partially built set (puzzle): the board (both glued
and placed unglued parts) and the kit.
-}
partialSetHash : Document -> List KitItem -> List Int
partialSetHash =
    partialSetPayload


{-| The binary payload of a partially built set (puzzle): the board (both glued
and placed unglued parts) and the kit.
-}
partialSetPayload : Document -> List KitItem -> List Int
partialSetPayload =
    puzzlePayload


{-| The binary payload of a puzzle: the board and the kit.
-}
puzzleHash : Document -> List KitItem -> List Int
puzzleHash =
    puzzlePayload


{-| The binary payload of a puzzle: the board and the kit.
-}
puzzlePayload : Document -> List KitItem -> List Int
puzzlePayload doc kit =
    HashPayload.encodeBytes
        { mode = HashPayload.Puzzle
        , document = doc
        , kit = Just kit
        }


{-| The parts set a free build shares: its glued parts stay on the board, and
every other part becomes a kit count, grouped by file and colour.
-}
freeShareHash : Document -> List Int
freeShareHash =
    freeSharePayload


{-| The parts set a free build shares: its glued parts stay on the board, and
every other part becomes a kit count, grouped by file and colour.
-}
freeSharePayload : Document -> List Int
freeSharePayload doc =
    let
        unglued =
            ungluedPlacements doc
    in
    puzzlePayload (gluedOnlyDocument doc) (HashPayload.kitOfPlacements unglued)


{-| The parts set a set build shares: its glued parts stay on the board, any
loose placements placed on the board are removed, and the set's full unglued kit
is offered in the limited catalog.
-}
setShareHash : Document -> List KitItem -> List Int
setShareHash =
    setSharePayload


{-| The parts set a set build shares: its glued parts stay on the board, any
loose placements placed on the board are removed, and the set's full unglued kit
is offered in the limited catalog.
-}
setSharePayload : Document -> List KitItem -> List Int
setSharePayload doc kit =
    puzzlePayload (gluedOnlyDocument doc) kit


{-| The fixed model payload: every placement on the board is glued and the kit
is empty (`Just []`), so it opens with all parts glued and no catalog.
-}
fixedModelHash : Document -> List Int
fixedModelHash =
    fixedModelPayload


{-| The fixed model payload: every placement on the board is glued and the kit
is empty (`Just []`), so it opens with all parts glued and no catalog.
-}
fixedModelPayload : Document -> List Int
fixedModelPayload doc =
    puzzlePayload (glueAllDocument doc) []


ungluedPlacements : Document -> List Document.Placement
ungluedPlacements doc =
    Document.placements doc
        |> List.filter (\placement -> not (Document.isGlued placement.id doc))


gluedOnlyDocument : Document -> Document
gluedOnlyDocument doc =
    List.foldl (\placement acc -> Document.removePlacement placement.id acc) doc (ungluedPlacements doc)


glueAllDocument : Document -> Document
glueAllDocument doc =
    List.foldl (\placement acc -> Document.setGlued True placement.id acc) doc (Document.placements doc)
