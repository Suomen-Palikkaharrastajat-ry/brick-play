module TidyTest exposing (suite)

{-| Tidy up in Brick Play: the pill shows only for a build with problems and
counts its loose parts; the preview is modal (editing paused); Apply moves the
parts as one undo step; Cancel and Escape change nothing; glued parts stay put;
and every example of docs/examples.md is healthy (no pill).
-}

import ExamplesTest
import Expect exposing (Expectation)
import Html
import Html.Attributes as Attr
import LDraw.Document as Document
import LDraw.Writer as Writer
import Main exposing (Flags, Model, Msg(..), init, update, view)
import Studio.Core as Core
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Viewport


suite : Test
suite =
    describe "Brick Play Tidy up"
        [ pillTests
        , previewTests
        , titleTests
        , glueTests
        , exampleTests
        ]


{-| The move list names a part by its curated name when it has one, else by the
.dat title the thumbnail pipeline sent (lower-cased), else by its number.
-}
titleTests : Test
titleTests =
    describe "the move list names parts"
        [ test "a part outside the Free catalog is named by its .dat title" <|
            \_ ->
                freshModel
                    |> withLines [ brick "60592.dat" 0 -48 0 ]
                    |> update (ThumbnailLoaded { file = "60592.dat", color = 14, url = "", title = "Window 1 x 2 x 2" })
                    |> Tuple.first
                    |> update OpenTidy
                    |> Tuple.first
                    |> view
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "li" ]
                    |> Query.has [ Selector.text "Move window 1 x 2 x 2 half a stud" ]
        , test "a part with no title yet is named by its number" <|
            \_ ->
                freshModel
                    |> withLines [ brick "60592.dat" 0 -48 0 ]
                    |> update OpenTidy
                    |> Tuple.first
                    |> view
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "li" ]
                    |> Query.has [ Selector.text "Move part 60592 half a stud" ]
        ]


pillTests : Test
pillTests =
    describe "the pill"
        [ test "a healthy build shows nothing" <|
            \_ ->
                freshModel
                    |> withLines [ brick "3001.dat" 0 -24 0, brick "3005.dat" 10 -48 10 ]
                    |> expectPills 0
        , test "one loose brick says so, and offers Tidy up" <|
            \_ ->
                freshModel
                    |> withLines misaligned
                    |> (\model ->
                            Query.fromHtml (view model)
                                |> Query.find [ Selector.attribute pillAttribute ]
                                |> Query.has [ Selector.text "1 loose brick · Tidy up" ]
                       )
        , test "two loose bricks are pluralised" <|
            \_ ->
                freshModel
                    |> withLines
                        [ brick "3001.dat" 0 -24 0
                        , brick "3005.dat" 5 -48 0
                        , brick "3005.dat" -35 -48 0
                        ]
                    |> (\model ->
                            Query.fromHtml (view model)
                                |> Query.find [ Selector.attribute pillAttribute ]
                                |> Query.has [ Selector.text "2 loose bricks · Tidy up" ]
                       )
        ]


previewTests : Test
previewTests =
    describe "the preview"
        [ test "Apply moves the parts as one undo step" <|
            \_ ->
                let
                    before =
                        freshModel |> withLines misaligned

                    applied =
                        before
                            |> update OpenTidy
                            |> Tuple.first
                            |> update ApplyTidy
                            |> Tuple.first

                    undone =
                        applied |> update Undo |> Tuple.first
                in
                Expect.all
                    [ \_ -> Expect.notEqual (written before) (written applied)
                    , \_ -> expectPills 0 applied
                    , \_ -> Expect.equal (written before) (written undone)
                    , \_ -> expectPills 1 undone
                    ]
                    ()
        , test "Cancel changes nothing" <|
            \_ ->
                let
                    before =
                        freshModel |> withLines misaligned

                    after =
                        before
                            |> update OpenTidy
                            |> Tuple.first
                            |> update CancelTidy
                            |> Tuple.first
                in
                Expect.all
                    [ \_ -> Expect.equal (written before) (written after)
                    , \_ -> Expect.equal False after.tidyOpen
                    , \_ -> expectPills 1 after
                    ]
                    ()
        , test "Escape cancels the preview and changes nothing" <|
            \_ ->
                let
                    before =
                        freshModel |> withLines misaligned

                    after =
                        before
                            |> update OpenTidy
                            |> Tuple.first
                            |> press "Escape"
                in
                Expect.all
                    [ \_ -> Expect.equal (written before) (written after)
                    , \_ -> Expect.equal False after.tidyOpen
                    ]
                    ()
        , test "while the preview is open, a key edit does nothing" <|
            \_ ->
                let
                    selected =
                        freshModel
                            |> withLines misaligned
                            |> update (StudioMsg Core.SelectAll)
                            |> Tuple.first

                    opened =
                        selected
                            |> update OpenTidy
                            |> Tuple.first

                    after =
                        press "Delete" opened
                in
                Expect.all
                    [ \_ -> Expect.equal (written selected) (written after)
                    , \_ -> Expect.equal True after.tidyOpen
                    ]
                    ()
        , test "the preview lists each move, with Apply and Cancel" <|
            \_ ->
                freshModel
                    |> withLines misaligned
                    |> update OpenTidy
                    |> Tuple.first
                    |> view
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ Selector.attribute (Attr.attribute "role" "dialog") ]
                        , Query.findAll [ Selector.tag "li" ] >> Query.count (Expect.equal 1)
                        , Query.has [ Selector.text "Apply" ]
                        , Query.has [ Selector.text "Cancel" ]
                        ]
        ]


glueTests : Test
glueTests =
    describe "glue"
        [ test "a glued part stays where it is when Tidy up applies" <|
            \_ ->
                let
                    gluedLine =
                        brick "3001.dat" 0 -24 0

                    before =
                        freshModel
                            |> withLines [ "0 !PALIKKA GLUED", gluedLine, brick "3005.dat" 5 -48 0 ]

                    after =
                        before
                            |> update OpenTidy
                            |> Tuple.first
                            |> update ApplyTidy
                            |> Tuple.first

                    afterLines =
                        String.lines (written after)
                in
                Expect.all
                    [ \_ -> Expect.equal True (List.member gluedLine afterLines)
                    , \_ -> Expect.equal True (List.member "0 !PALIKKA GLUED" afterLines)
                    , \_ -> Expect.notEqual (written before) (written after)
                    ]
                    ()
        ]


exampleTests : Test
exampleTests =
    describe "docs/examples.md"
        (List.map
            (\( name, exampleLines ) ->
                test (name ++ " is healthy") <|
                    \_ ->
                        freshModel
                            |> withLines exampleLines
                            |> expectPills 0
            )
            ExamplesTest.examples
        )


{-| A free-building model with an empty board.
-}
freshModel : Model
freshModel =
    Tuple.first (init (flagsFor "#free=1"))


flagsFor : String -> Flags
flagsFor hash =
    { ldrawBase = "/ldraw"
    , ldrawFallbackBase = ""
    , initialHash = hash
    , initialBytes = Nothing
    , supportsPointerEvents = True
    , hintSeen = False
    , coarsePointer = False
    }


{-| The model with the board replaced by the document of these lines.
-}
withLines : List String -> Model -> Model
withLines text model =
    Tuple.first
        (update (StudioMsg (Core.ResetDocument (Document.parse (String.join "\n" text)))) model)


{-| Two parts that are off the studs: a 2 x 4 on the ground with a 1 x 1 half a stud
off on it. Tidy up moves the 1 x 1 onto a stud.
-}
misaligned : List String
misaligned =
    [ brick "3001.dat" 0 -24 0, brick "3005.dat" 5 -48 0 ]


{-| A placement line for a part at a position, unrotated, in colour 14.
-}
brick : String -> Float -> Float -> Float -> String
brick file x y z =
    "1 14 "
        ++ String.join " " (List.map String.fromFloat [ x, y, z ])
        ++ " 1 0 0 0 1 0 0 0 1 "
        ++ file


{-| The attribute that marks the Tidy up pill.
-}
pillAttribute : Html.Attribute Never
pillAttribute =
    Attr.attribute "data-tidy-pill" ""


{-| Expect the screen to show exactly `n` Tidy up pills (0 or 1).
-}
expectPills : Int -> Model -> Expectation
expectPills n model =
    Query.fromHtml (view model)
        |> Query.findAll [ Selector.attribute pillAttribute ]
        |> Query.count (Expect.equal n)


{-| The document as LDraw text, for "did not change" comparisons.
-}
written : Model -> String
written model =
    Writer.writeDocument model.studio.document


{-| A key press, delivered the way the browser's keydown reaches the studio.
-}
press : String -> Model -> Model
press key model =
    Tuple.first
        (update
            (StudioMsg
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
            )
            model
        )
