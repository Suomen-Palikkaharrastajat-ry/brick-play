module ExamplesTest exposing (examples, suite)

{-| The Brick Play example models of docs/examples.md, decoded from their hash
fragments (the production link of each example), must be stud-aligned.

A studded part's X and Z origin sits on the stud lattice (its
`Connectivity.studOffset` mod 20), and it rests on the ground or on the top of
the part under it. Technic parts have no stud points, so the stud rule does not
apply to the Technic examples; they are checked to stay stud-free. If an example
in docs/examples.md changes, update its model here too.

-}

import Expect
import LDraw.Connectivity as Connectivity exposing (PartConnectivity)
import LDraw.Document as Document
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Studio.DragPlane as DragPlane
import Test exposing (Test, describe, test)


{-| Each example's name and the lines of its decoded `model=` (docs/examples.md).
-}
examples : List ( String, List String )
examples =
    [ ( "Cozy Cottage with Window"
      , [ "1 19 0 -24 0 1 0 0 0 1 0 0 0 1 3001.dat"
        , "1 15 0 -72 10 1 0 0 0 1 0 0 0 1 60592.dat"
        , "1 19 -30 -48 10 1 0 0 0 1 0 0 0 1 3005.dat"
        , "1 19 30 -48 10 1 0 0 0 1 0 0 0 1 3005.dat"
        ]
      )
    , ( "Castle Watchtower"
      , [ "1 71 -20 -24 -20 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 71 20 -24 -20 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 71 -20 -24 20 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 71 20 -24 20 1 0 0 0 1 0 0 0 1 3003.dat"
        ]
      )
    , ( "Classic Arch Bridge"
      , [ "1 15 -40 -24 0 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 15 -40 -48 0 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 15 40 -24 0 1 0 0 0 1 0 0 0 1 3003.dat"
        , "1 15 40 -48 0 1 0 0 0 1 0 0 0 1 3003.dat"
        ]
      )
    , ( "Twin 24T Gearbox"
      , [ "1 1 0 -60 -20 0 0 -1 0 1 0 1 0 0 32524.dat"
        , "1 0 0 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat"
        , "1 0 60 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat"
        , "1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3648.dat"
        , "1 7 0 -60 12 1 0 0 0 1 0 0 0 1 6590.dat"
        ]
      )
    , ( "5:1 Speed Reducer"
      , [ "1 1 0 -60 -20 0 0 -1 0 1 0 1 0 0 32524.dat"
        , "1 0 0 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat"
        , "1 0 60 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat"
        , "1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3647.dat"
        , "1 7 0 -60 12 1 0 0 0 1 0 0 0 1 6590.dat"
        ]
      )
    , ( "3-Gear Train"
      , [ "1 1 0 -60 -20 0 0 -1 0 1 0 1 0 0 32524.dat"
        , "1 0 -40 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat"
        , "1 0 0 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat"
        , "1 0 60 -60 0 0 0 -1 0 1 0 1 0 0 3705.dat"
        , "1 7 -40 -60 0 1 0 0 0 1 0 0 0 1 3647.dat"
        , "1 7 0 -60 0 1 0 0 0 1 0 0 0 1 3648.dat"
        ]
      )
    ]


suite : Test
suite =
    describe "docs/examples.md models"
        (List.map
            (\( name, lines ) ->
                test name <|
                    \_ ->
                        alignmentProblems (String.join "\n" lines)
                            |> Expect.equal []
            )
            examples
        )


{-| The problems with a model's placements, one message each; none when every
studded part is stud-aligned and rests on something.
-}
alignmentProblems : String -> List String
alignmentProblems model =
    let
        placements =
            Document.placements (Document.parse model)

        boxOf p =
            DragPlane.worldBox p.matrix (localBox (connectivityOf p.part))

        others p =
            placements
                |> List.filter (\q -> q.id /= p.id)
                |> List.map boxOf

        problemsOf p =
            let
                conn =
                    connectivityOf p.part

                origin =
                    Mat4.transform p.matrix (vec3 0 0 0)

                offset =
                    Connectivity.studOffset conn p.matrix

                footprint =
                    boxOf p

                bottom =
                    Vec3.getY footprint.max

                -- On the ground, or on the top of an overlapping part at exactly this bottom.
                restsOn =
                    abs bottom
                        < 0.01
                        || List.any (\o -> DragPlane.footprintOverlaps footprint o && abs (Vec3.getY o.min - bottom) < 0.01) (others p)
            in
            if not (Connectivity.isStudded conn) then
                []

            else
                List.filterMap identity
                    [ if onLattice offset.x (Vec3.getX origin) && onLattice offset.z (Vec3.getZ origin) then
                        Nothing

                      else
                        Just (p.part ++ " at " ++ showPosition origin ++ " is off the stud lattice")
                    , if restsOn then
                        Nothing

                      else
                        Just (p.part ++ " floats: its bottom is at " ++ String.fromFloat bottom ++ " with nothing at that height under it")
                    ]
    in
    if List.length placements /= List.length (String.lines model) then
        [ "the model parsed to " ++ String.fromInt (List.length placements) ++ " placements, not one per line" ]

    else
        List.concatMap problemsOf placements


connectivityOf : String -> PartConnectivity
connectivityOf part =
    Connectivity.extractPartConnectivity part (\_ -> Nothing)


{-| A part's local box as the studio's drag uses it: the connectivity envelope.
-}
localBox : PartConnectivity -> DragPlane.Box
localBox conn =
    case conn.boundingBoxes of
        box :: _ ->
            Connectivity.effectiveEnvelope box conn.connections

        [] ->
            { min = vec3 -10 -4 -10, max = vec3 10 24 10 }


{-| Whether `value` is on the stud lattice of stud offset `offset` (LDU).
-}
onLattice : Float -> Float -> Bool
onLattice offset value =
    modBy 20 (round (value - offset)) == 0


showPosition : Vec3 -> String
showPosition v =
    "(" ++ String.fromFloat (Vec3.getX v) ++ ", " ++ String.fromFloat (Vec3.getY v) ++ ", " ++ String.fromFloat (Vec3.getZ v) ++ ")"
