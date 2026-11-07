module Main exposing
    ( Flags
    , Model
    , Msg(..)
    , Participant
    , accomplishmentBanner
    , accomplishmentBannerIn
    , clearable
    , dockOverlapWidth
    , dockPlacement
    , hashFor
    , helpRows
    , init
    , kitItemName
    , main
    , parseReverseHostRoom
    , parseReverseParticipantRoom
    , remainingCount
    , remainingForItem
    , shareHash
    , subscriptions
    , technicMode
    , thumbnailRequests
    , update
    , view
    , viewToast
    )

{-| Main application for Brick Play.

A free-building studio with kit inventory tracking and stud snapping. There is
no axis gizmo: a drag on a part moves it relative to the view, a drag on empty
space pans and a right-drag orbits. A double-click (or double-tap) on a part
opens its context ring (`Ring`), a cluster of round buttons beside the part that
turns, tips, raises, lowers and deletes it, glues it (free building), and moves it
with a W A S D Q E pad. The ring's move step (`Step`: Stud, ½ stud, or Bit) also
sets the keyboard nudges and body drags. Shift+drag prefers the closest snap
candidate. Tab and Shift+Tab select the next or
previous part, and W A S D Q E move the selected part (the camera moves when
nothing is selected). The camera has its own control bottom-right, the shared
camera globe (`UI.CameraGlobe`): drag it to orbit, tap it to fly there, and use
its zoom slider and reset button. On a narrow portrait window (a phone) the bottom
area shows either the dock or the camera globe, switched by a toggle button, and
the dock is shown first.

There are three modes, chosen by the URL hash (see `Link` for the formats):

  - **Free building / Editable model** (no hash, `free=1`, or an editable binary hash):
    the dock is the curated catalog of `Free.Catalog`, unlimited, with a colour row
    that sets the colour of the parts added next and a search box over the whole
    part library (`parts-index.json`, loaded on first use). Parts can be glued to
    the board (the Glue button, or `L`): a glued part is locked, and a glued part
    is written as `0 !PALIKKA GLUED`.
  - **Parts set / Puzzle** (a set or partially built set hash with a non-empty kit):
    the dock is the counted kit (`parts=`, or the default 10 yellow bricks for a
    bare `model=`). Glued parts are locked and cannot be unglued, and the Glue
    button is hidden. Clear keeps the glued parts.
  - **Fixed model** (a fixed model hash with an empty kit): every part on the board
    is glued and locked, and there is no catalog dock.

Each dock card is a rendered picture of the part (`UI.PartCard.compactView`, fed
by the shared thumbnail pipeline through `Ports`).

Technic mode is derived, not set: it is on when the document holds a gear that
`Technic.Machine` recognises, or when a puzzle's kit lists a gear. In Technic
mode the play button shows and Space plays and pauses the machine; the gear mesh
status pill, automatic tooth alignment and the accomplishment banner come on, and
drags keep their height. Outside it, Space is unbound and the play button is
hidden. Technic parts (gears, axles, beams, pins) go on the shared axle line in
either mode (`Technic.Placement`), and other parts rest on the ground. Any
edit stops a running machine and rewinds it to 0.

The top-left toolbar holds the menu (`UI.HamburgerMenu` — share options, download
.io, undo, redo, delete and clear), a visible ? button that opens the shortcut
list, the play button (in Technic mode), and any temporary info pills (Bit step,
Tidy up, gear mesh status, first-select hint, touch part name, and the
accomplishment banner), growing horizontally so the vertical space above the
brick palette stays completely free. Escape closes the menu first. An opened link
offers only similar or lesser shares in the menu: editable offers Share editable
model, Share parts set and Share fixed model; a parts set offers Share parts set
and Share fixed model; a fixed model offers only Share fixed model.

Tidy up (`Studio.Health`) checks the build after each change: a studded part that
sits off the stud grid, or parts that overlap, show a pill in the top-left bar
("2 loose bricks · Tidy up"). A healthy build shows nothing. The pill opens a
preview: the parts that move are outlined, a translucent ghost shows each new
place, and Apply makes all the moves one undo step. While the preview is open
editing is paused; Escape or Cancel leaves the build as it was.

The first time a part is selected a small hint in the top-left bar says how to open
the ring. It goes after five seconds, or when the ring opens, and is remembered
per browser (`hintSeen`, a flag the JS side keeps in localStorage).

-}

import Array
import Base64
import Browser
import Browser.Dom
import Browser.Events
import Bytes exposing (Bytes)
import Data
import Dict exposing (Dict)
import EmbeddedParts
import FeatherIcons
import Flate
import Free.Catalog as FreeCatalog
import Gear.Placements as Placements
import Html exposing (Html, button, div, h1, h2, header, input, li, p, span, text, ul)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onFocus, onInput, preventDefaultOn)
import Html.Keyed as Keyed
import Http
import I18n exposing (Language(..))
import Json.Decode as Decode
import Json.Encode as Encode
import LDraw.Colors as Colors
import LDraw.Connectivity as Connectivity
import LDraw.Document as Document
import LDraw.Resolve as Resolve
import LDraw.Types exposing (LDrawLine)
import LDraw.Writer
import Link
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)
import Ports
import Process
import Render.Camera as Camera
import Render.Style as Style
import Ring
import Set
import ShareModal
import Shortcuts as PlayShortcuts
import Simulation
import Step
import Studio.Colours as Colours
import Studio.Core as Core
import Studio.GearFeedback as GearFeedback
import Studio.Gizmo as Gizmo
import Studio.HashPayload as HashPayload
import Studio.Health as Health
import Studio.PartIndex as PartIndex
import Studio.PartSearch as PartSearch
import Studio.Shortcuts as StudioShortcuts exposing (TurnDirection(..))
import Studio.UrlPayload as UrlPayload exposing (PartKitItem)
import Task
import Technic.Animation as Animation exposing (Animated)
import Technic.Catalog as Catalog
import Technic.Machine as Machine exposing (MachineState)
import Technic.Placement as Placement
import Time
import UI.Button as Button
import UI.CameraGlobe as CameraGlobe
import UI.Focus as Focus
import UI.HamburgerMenu as HamburgerMenu
import UI.Icon as Icon
import UI.PartCard as PartCard
import UI.ShortcutHelp as ShortcutHelp
import UI.SnapshotModal as SnapshotModal
import Viewport
import Viewport.Framing exposing (ViewPreset(..))
import Viewport.Shortcuts as ViewportShortcuts


{-| Application flags received from JavaScript runtime.
-}
type alias Flags =
    { ldrawBase : String
    , ldrawFallbackBase : String
    , initialHash : String
    , initialBytes : Maybe (List Int)
    , supportsPointerEvents : Bool
    , hintSeen : Bool
    , coarsePointer : Bool
    , language : Maybe String
    , testApiEnabled : Bool
    , baseUrl : Maybe String
    }


{-| Default inventory when none is provided: 10 yellow 2x4 bricks.
-}
defaultInventory : List PartKitItem
defaultInventory =
    [ { part = "3001.dat", color = 14, count = 10 } ]


{-| The height of the motor, in revolutions per minute, for the Technic machine.
-}
motorRpm : Float
motorRpm =
    30.0


{-| Main application model.
-}
type alias Model =
    { studio : Core.Model
    , free : Bool
    , inventory : List PartKitItem
    , ldrawBase : String
    , machine : MachineState
    , playing : Bool
    , currentTime : Float
    , simulation : Simulation.Model
    , animated : Animated
    , activeTab : FreeCatalog.Tab
    , paintColour : Int
    , search : Search
    , helpOpen : Bool
    , menuOpen : Bool
    , windowWidth : Int
    , windowHeight : Int
    , thumbnails : Dict String Thumbnail
    , globeDrag : Maybe CameraGlobe.Drag
    , cameraShown : Bool
    , lastTapped : Maybe String
    , toast : Maybe { message : String, id : Int }
    , toastCounter : Int
    , snapshotModal : Maybe SnapshotModal.Model
    , shareModal : Maybe ShareModal.Model
    , baseUrl : String
    , tidy : List Health.Fix
    , tidyOpen : Bool
    , stepSize : Step.StepSize
    , ringPart : Maybe Int
    , lastTap : Maybe { x : Float, y : Float, millis : Int, part : Maybe Int }
    , hintSeen : Bool
    , coarsePointer : Bool
    , language : Language
    , testApiEnabled : Bool
    , hint : Maybe Int
    , hintCounter : Int
    , liveBroadcasting : Bool
    , liveRoomId : Maybe String
    , liveWatchUrl : Maybe String
    , liveViewerCount : Int
    , viewerRoom : Maybe String
    , viewerStatus : String
    , reverseParticipantRoom : Maybe String
    , reverseStatus : String
    , participantName : String
    , reverseHostRoom : Maybe String
    , participants : Dict String Participant
    , inspectedParticipant : Maybe String
    }


{-| A participant in a reverse sharing session.
-}
type alias Participant =
    { id : String
    , name : String
    , bytes : List Int
    , thumbnail : String
    , brickCount : Int
    , lastSeen : Int
    , status : String
    }


{-| A rendered dock image: its data URL and the part's .dat title (`""` when
unknown). Kept under `PartCard.thumbnailKey file color`.
-}
type alias Thumbnail =
    { url : String
    , title : String
    }


{-| The dock's part search: what was typed, the part library once it has been
fetched (`parts-index.json` is only read when the search is first used), and the
matches for the query.
-}
type alias Search =
    { query : String
    , index : SearchIndex
    , results : List PartIndex.Part
    }


{-| Whether the part library has arrived.
-}
type SearchIndex
    = NotLoaded
    | Loading
    | Loaded PartIndex.PartIndex
    | Failed String


{-| The most matches the dock lists at once.
-}
resultLimit : Int
resultLimit =
    40


{-| The colours the free dock's colour row offers, in row order (LDraw codes:
black, blue, green, red, white, light bluish gray, tan, yellow).
-}
paintColours : List Int
paintColours =
    -- Original favorites
    [ 14
    , 15
    , 4
    , 1
    , 2
    , 0
    , 71
    , 19

    -- Additional solids
    , 5
    , 3
    , 6
    , 27
    , 28
    , 72
    , 84
    , 86
    , 89
    , 212
    , 226
    , 268
    , 85

    -- Translucent / water colors
    , 47
    , 43
    , 41
    , 33
    , 34
    , 35
    , 42
    , 36
    , 57
    , 46
    , 52
    , 10375
    , 293
    ]


{-| Messages handled by the Brick Play application.
-}
type Msg
    = StudioMsg Core.Msg
    | AddPart PartKitItem
    | AddFreePart { file : String, color : Int, name : String }
    | SetFreeTab FreeCatalog.Tab
    | SetPaintColour Int
    | SearchInput String
    | SearchOpened
    | SearchLoaded (Result Http.Error PartIndex.PartIndex)
    | ToggleGlue
    | Undo
    | Redo
    | Delete
    | Cut
    | Copy
    | Paste
    | Group
    | Ungroup
    | TurnSelection TurnDirection
    | RaiseSelection
    | LowerSelection
    | MoveSelection StudioShortcuts.NudgeDirection
    | SetStepSize Step.StepSize
    | ToggleBitDrag
    | OpenRing Int
    | CloseRing
    | SelectNeighbour Bool
    | TapTimed Float Float Int
    | HideHint Int
    | Clear
    | FitView
    | DownloadIo
    | TogglePlay
    | Tick Float
    | ToggleRealistic
    | CaptureSnapshot
    | SnapshotReady { dataUrl : String, filename : String }
    | SnapshotProgress { fraction : Float, samples : Int }
    | SnapshotFailed String
    | SnapshotSave { dataUrl : String, filename : String }
    | SnapshotClose
    | TestCommand Decode.Value
    | GlobePressed ( Float, Float )
    | GlobeMoved ( Float, Float )
    | GlobeReleased
    | ZoomTo Float
    | ZoomDone
    | ResetView
    | ToggleCameraPanel
    | ToggleHelp
    | ToggleMenu
    | CloseMenu
    | SetLanguage Language
    | OpenTidy
    | CancelTidy
    | ApplyTidy
    | OpenShareModal
    | CloseShareModal
    | SelectShareKind ShareModal.ShareKind
    | CopyShareUrl
    | DismissShareCopied
    | ToggleReverseSharing
    | CopyParticipantUrl
    | CopyHostUrl
    | ShareModalMsg ShareModal.Msg
    | ShareEditableModel
    | SharePartsSet
    | ShareFixedModel
    | ToggleBroadcast
    | BroadcastStarted { roomId : String, watchUrl : String }
    | BroadcastStopped
    | BroadcastViewersChanged Int
    | BroadcastError String
    | LiveSnapshotReceived (List Int)
    | LiveAnimationReceived Bool
    | LiveStatusChanged String
    | LiveError String
    | ReverseStatusChanged String
    | ReverseParticipantUpdated
        { id : String
        , name : String
        , bytes : List Int
        , thumbnail : String
        , brickCount : Int
        , lastSeen : Int
        }
    | ReverseParticipantLeft String
    | InspectParticipant (Maybe String)
    | DismissToast Int
    | MenuPicked Msg
    | NoOp
    | WindowResized Int Int
    | ThumbnailLoaded { file : String, color : Int, url : String, title : String }


{-| Parse room ID from #/watch/:roomId or #/live/:roomId
-}
parseViewerRoom : String -> Maybe String
parseViewerRoom hash =
    let
        clean =
            String.replace "#" "" hash
                |> String.trim
    in
    if String.startsWith "/watch/" clean then
        case String.split "/" clean of
            _ :: _ :: roomId :: _ ->
                if String.isEmpty roomId then
                    Nothing

                else
                    Just (String.toUpper roomId)

            _ ->
                Nothing

    else if String.startsWith "/live/" clean then
        case String.split "/" clean of
            _ :: _ :: roomId :: _ ->
                if String.isEmpty roomId then
                    Nothing

                else
                    Just (String.toUpper roomId)

            _ ->
                Nothing

    else
        Nothing


{-| Extract the hash fragment from a URL or hash string.
-}
extractFragment : String -> String
extractFragment hash =
    case String.split "#" hash of
        _ :: after :: _ ->
            after

        _ ->
            hash


{-| Parse room ID from #/reverse/:roomId/b/:base64url
-}
parseReverseParticipantRoom : String -> Maybe String
parseReverseParticipantRoom hash =
    let
        clean =
            extractFragment hash
                |> String.trim

        stripped =
            if String.startsWith "/" clean then
                String.dropLeft 1 clean

            else
                clean
    in
    if String.startsWith "reverse/" stripped then
        case String.split "/" stripped of
            _ :: roomId :: _ ->
                if String.isEmpty roomId then
                    Nothing

                else
                    Just (String.toUpper roomId)

            _ ->
                Nothing

    else
        Nothing


{-| Extract the base64url payload string from #/reverse/:roomId/b/:base64url
-}
parseReverseParticipantBase64 : String -> Maybe String
parseReverseParticipantBase64 hash =
    let
        clean =
            extractFragment hash
                |> String.trim

        stripped =
            if String.startsWith "/" clean then
                String.dropLeft 1 clean

            else
                clean
    in
    if String.startsWith "reverse/" stripped then
        case String.split "/" stripped of
            _ :: _ :: "b" :: rest ->
                let
                    b64 =
                        String.join "/" rest
                in
                if String.isEmpty b64 then
                    Nothing

                else
                    Just b64

            _ ->
                Nothing

    else
        Nothing


{-| Parse room ID from #/grid/:roomId
-}
parseReverseHostRoom : String -> Maybe String
parseReverseHostRoom hash =
    let
        clean =
            extractFragment hash
                |> String.trim

        stripped =
            if String.startsWith "/" clean then
                String.dropLeft 1 clean

            else
                clean
    in
    if String.startsWith "grid/" stripped then
        case String.split "/" stripped of
            _ :: roomId :: _ ->
                if String.isEmpty roomId then
                    Nothing

                else
                    Just (String.toUpper roomId)

            _ ->
                Nothing

    else
        Nothing


{-| Extract direct base64url payload string from #b/:base64url or b/:base64url
-}
parseDirectBase64 : String -> Maybe String
parseDirectBase64 hash =
    let
        clean =
            extractFragment hash
                |> String.trim
    in
    if String.startsWith "b/" clean then
        Just (String.dropLeft 2 clean)

    else if String.startsWith "/b/" clean then
        Just (String.dropLeft 3 clean)

    else
        Nothing


{-| Decode a base64url-encoded compressed binary payload into HashPayload.Payload.
-}
decodeBase64UrlPayload : String -> Maybe HashPayload.Payload
decodeBase64UrlPayload b64 =
    let
        standardB64 =
            b64
                |> String.replace "-" "+"
                |> String.replace "_" "/"

        padded =
            case modBy 4 (String.length standardB64) of
                2 ->
                    standardB64 ++ "=="

                3 ->
                    standardB64 ++ "="

                _ ->
                    standardB64
    in
    Base64.toBytes padded
        |> Maybe.andThen Flate.inflate
        |> Maybe.andThen HashPayload.decode


{-| Default participant display name in chosen language.
-}
participantDisplayName : Language -> String -> String
participantDisplayName lang customName =
    let
        trimmed =
            String.trim customName
    in
    if not (String.isEmpty trimmed) then
        trimmed

    else
        case lang of
            Fi ->
                "Rakentaja"

            En ->
                "Builder"


{-| Send participant snapshot through Ports.pushReverseSnapshot.
-}
pushReverseSnapshotFor : Model -> Cmd msg
pushReverseSnapshotFor m =
    Ports.pushReverseSnapshot
        { name = participantDisplayName m.language m.participantName
        , bytes = liveHash m
        , brickCount = List.length (Core.getPlacements m.studio)
        }


{-| Initialize the Brick Play application.
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( rawDoc, free, inventory ) =
            case flags.initialBytes |> Maybe.andThen HashPayload.decodeBytes of
                Just binaryPayload ->
                    let
                        isFree =
                            binaryPayload.mode == HashPayload.FreeBuilding

                        inv =
                            if isFree then
                                []

                            else
                                case binaryPayload.kit of
                                    Just k ->
                                        k

                                    Nothing ->
                                        defaultInventory
                    in
                    ( binaryPayload.document, isFree, inv )

                Nothing ->
                    case
                        parseReverseParticipantBase64 flags.initialHash
                            |> Maybe.andThen decodeBase64UrlPayload
                    of
                        Just binaryPayload ->
                            let
                                isFree =
                                    binaryPayload.mode == HashPayload.FreeBuilding

                                inv =
                                    if isFree then
                                        []

                                    else
                                        case binaryPayload.kit of
                                            Just k ->
                                                k

                                            Nothing ->
                                                defaultInventory
                            in
                            ( binaryPayload.document, isFree, inv )

                        Nothing ->
                            case
                                parseDirectBase64 flags.initialHash
                                    |> Maybe.andThen decodeBase64UrlPayload
                            of
                                Just binaryPayload ->
                                    let
                                        isFree =
                                            binaryPayload.mode == HashPayload.FreeBuilding

                                        inv =
                                            if isFree then
                                                []

                                            else
                                                case binaryPayload.kit of
                                                    Just k ->
                                                        k

                                                    Nothing ->
                                                        defaultInventory
                                    in
                                    ( binaryPayload.document, isFree, inv )

                                Nothing ->
                                    let
                                        payload =
                                            UrlPayload.decodeHash flags.initialHash

                                        doc =
                                            case payload.modelText of
                                                Just text ->
                                                    Document.parse text

                                                Nothing ->
                                                    Document.empty

                                        isFree =
                                            payload.free || String.isEmpty (String.trim (String.replace "#" "" flags.initialHash))

                                        inv =
                                            if isFree then
                                                []

                                            else if payload.partsListed then
                                                payload.parts

                                            else
                                                defaultInventory
                                    in
                                    ( doc, isFree, inv )

        vRoom =
            parseViewerRoom flags.initialHash

        rRoom =
            parseReverseParticipantRoom flags.initialHash

        rHostRoom =
            parseReverseHostRoom flags.initialHash

        isViewerMode =
            vRoom /= Nothing

        initialDoc =
            if isViewerMode || (not free && List.isEmpty inventory) then
                List.foldl (\placement acc -> Document.setGlued True placement.id acc) rawDoc (Document.placements rawDoc)

            else
                rawDoc

        config =
            { ldrawBase = flags.ldrawBase
            , ldrawFallbackBase = flags.ldrawFallbackBase
            , defaultModelUrl = ""
            , initialHash = flags.initialHash
            , supportsPointerEvents = flags.supportsPointerEvents
            , document = initialDoc
            , gridStep = Just 20
            , rotateStep = Just 90
            , snapDistance = Just 24.0
            , collisionMode = Nothing
            }

        ( initialStudio, studioCmd ) =
            Core.init config
                |> Tuple.mapFirst
                    (Core.setGizmoStyle Core.NoGizmo
                        -- Mouse: left-drag empty space pans, right-drag orbits.
                        -- Touch: one finger on empty space pans, two fingers orbit
                        -- (and pinch-zoom); the globe orbits too.
                        >> Core.setDragMode (Core.DragPartsOrPan Core.OneFingerPans)
                        >> Core.setTwoFingerOrbits True
                        -- Glue locks parts in a puzzle; free building may glue and unglue.
                        >> Core.setGlueEditable (free && not isViewerMode)
                        >> Core.setPasteEnabled ((free || not (List.isEmpty inventory)) && not isViewerMode)
                        >> Step.apply Step.Stud
                    )

        initialModel =
            { studio = initialStudio
            , free = free
            , inventory = inventory
            , ldrawBase = flags.ldrawBase
            , machine = Machine.analyseMachine initialDoc initialStudio.viewport.partCache
            , playing = False
            , currentTime = 0.0
            , simulation =
                Simulation.init
                    { gearSpecs = Data.gearParts
                    , lodCache = EmbeddedParts.lodCache
                    , maxRpm = motorRpm
                    , initialRpm = motorRpm
                    , requestedMotorIndex = Nothing
                    }
            , animated = Animation.none
            , activeTab = FreeCatalog.BricksTab
            , paintColour = 14
            , search = emptySearch
            , helpOpen = False
            , menuOpen = False
            , windowWidth = 1280
            , windowHeight = 800
            , thumbnails = Dict.empty
            , globeDrag = Nothing
            , cameraShown = False
            , lastTapped = Nothing
            , toast = Nothing
            , toastCounter = 0
            , snapshotModal = Nothing
            , shareModal = Nothing
            , baseUrl = Maybe.withDefault "https://palikkaharrastajat.fi/play/" flags.baseUrl
            , tidy = []
            , tidyOpen = False
            , stepSize = Step.Stud
            , ringPart = Nothing
            , lastTap = Nothing
            , hintSeen = flags.hintSeen
            , coarsePointer = flags.coarsePointer
            , language = Maybe.map I18n.fromString flags.language |> Maybe.withDefault I18n.defaultLanguage
            , testApiEnabled = flags.testApiEnabled
            , hint = Nothing
            , hintCounter = 0
            , liveBroadcasting = False
            , liveRoomId = Nothing
            , liveWatchUrl = Nothing
            , liveViewerCount = 0
            , viewerRoom = parseViewerRoom flags.initialHash
            , viewerStatus =
                if parseViewerRoom flags.initialHash /= Nothing then
                    "connecting"

                else
                    ""
            , reverseParticipantRoom = rRoom
            , reverseStatus =
                if rRoom /= Nothing then
                    "connecting"

                else
                    ""
            , participantName = ""
            , reverseHostRoom = rHostRoom
            , participants = Dict.empty
            , inspectedParticipant = Nothing
            }

        fetchViewportSize =
            Browser.Dom.getViewport
                |> Task.perform (\info -> WindowResized (round info.viewport.width) (round info.viewport.height))

        initialCmds =
            [ Cmd.map StudioMsg studioCmd
            , fetchViewportSize
            , requestDockThumbnails initialModel
            ]
                ++ (case rRoom of
                        Just _ ->
                            [ pushReverseSnapshotFor initialModel ]

                        Nothing ->
                            []
                   )
    in
    ( withTechnicSettings (withTidy initialModel)
    , Cmd.batch initialCmds
    )


{-| Technic mode, derived from the model: on when the document holds a gear that
`Technic.Machine` recognises, or when a puzzle's kit lists a gear part. Free
building has no kit, so only the board's gears count there.
-}
technicMode : Model -> Bool
technicMode model =
    not (List.isEmpty model.machine.gears)
        || List.any (\item -> isGearFile item.part) model.inventory


isGearFile : String -> Bool
isGearFile file =
    List.any (\spec -> spec.partFile == file) Data.gearParts


{-| The search box is empty (or only spaces), so the dock shows its tab.
-}
notSearching : Model -> Bool
notSearching model =
    PartSearch.normaliseQuery model.search.query == ""


{-| The parts the free dock lists: the tab's entries, or the search results while
a search is typed. Results are offered in the chosen colour like the tabs.
-}
freeEntries : Model -> List FreeCatalog.Entry
freeEntries model =
    if notSearching model then
        FreeCatalog.entriesFor model.activeTab

    else
        List.map (\part -> { file = part.file, name = part.description, description = part.description }) model.search.results


{-| The (file, colour) pairs the dock shows: the free entries in the paint colour
in free building, else the kit items.
-}
dockPairs : Model -> List ( String, Int )
dockPairs model =
    if model.free then
        List.map (\entry -> ( entry.file, model.paintColour )) (freeEntries model)

    else
        List.map (\item -> ( item.part, item.color )) model.inventory


{-| The search state for a query, with its matches when the library is loaded.
-}
searchState : String -> SearchIndex -> Search
searchState query index =
    { query = query
    , index = index
    , results =
        case index of
            Loaded library ->
                PartSearch.matching resultLimit query library.parts

            _ ->
                []
    }


emptySearch : Search
emptySearch =
    searchState "" NotLoaded


{-| The part library, fetched on first use: returns the index state and the
command that fetches it, if it has to.
-}
ensureLibrary : String -> SearchIndex -> ( SearchIndex, Cmd Msg )
ensureLibrary ldrawBase index =
    case index of
        NotLoaded ->
            ( Loading, PartIndex.fetch ldrawBase SearchLoaded )

        _ ->
            ( index, Cmd.none )


{-| Ask for the images of the parts in the dock that have none yet.
-}
requestDockThumbnails : Model -> Cmd Msg
requestDockThumbnails model =
    case thumbnailRequests (dockPairs model) model.thumbnails of
        [] ->
            Cmd.none

        requests ->
            Ports.requestThumbnails requests


{-| The thumbnails to ask the pipeline for: one per distinct (file, colour) in
the list that has none yet. `rgb` comes from the colour table the 3D view uses,
so the pictures match the parts on the canvas.
-}
thumbnailRequests : List ( String, Int ) -> Dict String Thumbnail -> List { file : String, color : Int, rgb : List Float }
thumbnailRequests items thumbnails =
    let
        addRequest ( file, color ) ( seen, acc ) =
            let
                key =
                    PartCard.thumbnailKey file color
            in
            if List.member key seen || Dict.member key thumbnails then
                ( seen, acc )

            else
                ( key :: seen
                , { file = file
                  , color = color
                  , rgb =
                        Colors.resolveColor color 16
                            |> (\c -> [ c.r, c.g, c.b ])
                  }
                    :: acc
                )
    in
    List.foldl addRequest ( [], [] ) items
        |> Tuple.second
        |> List.reverse


{-| The readable name of a kit item: the part's .dat title once the pipeline has
sent it ("Brick 2 x 4"), else the part number ("3001").
-}
kitItemName : Model -> PartKitItem -> String
kitItemName model item =
    case Dict.get (PartCard.thumbnailKey item.part item.color) model.thumbnails of
        Just thumb ->
            if String.isEmpty (String.trim thumb.title) then
                partNumber item.part

            else
                thumb.title

        Nothing ->
            partNumber item.part


partNumber : String -> String
partNumber file =
    String.replace ".dat" "" file


{-| A placed part's name: its Technic catalogue name when it has one, else the
title the pipeline sent, else its part number.
-}
placedName : Model -> String -> Int -> String
placedName model file color =
    case List.filter (\item -> item.file == file) Catalog.curatedParts of
        item :: _ ->
            item.name

        [] ->
            case Dict.get (PartCard.thumbnailKey file color) model.thumbnails of
                Just thumb ->
                    if String.isEmpty (String.trim thumb.title) then
                        partNumber file

                    else
                        thumb.title

                Nothing ->
                    partNumber file


{-| Remaining count for a specific kit item: its count less the unglued parts of
that file and colour on the board. A glued part is part of the puzzle, not drawn
from the kit, so it does not count.
-}
remainingForItem : Model -> PartKitItem -> Int
remainingForItem model item =
    let
        placed =
            Core.getPlacements model.studio
                |> List.filter (\p -> p.part == item.part && p.color == item.color && not (Core.isGlued p.id model.studio))
                |> List.length
    in
    max 0 (item.count - placed)


{-| Total remaining parts across the entire kit inventory.
-}
remainingCount : Model -> Int
remainingCount model =
    List.sum (List.map (remainingForItem model) model.inventory)


{-| The binary payload the address bar keeps for the build (see `Link`): the whole board
with its glue while free building, the fixed model when there is no catalog, else the
partially built set with its kit.
-}
liveHash : Model -> List Int
liveHash model =
    if model.free then
        Link.freeHash model.studio.document

    else if List.isEmpty model.inventory then
        Link.fixedModelHash model.studio.document

    else
        Link.partialSetHash model.studio.document model.inventory


{-| The default parts-set (or fixed-model) link for the current build: from free
building its glued parts stay on the board and the rest become the kit; from a parts
set its glued parts stay on the board and the full kit is offered in the dock; from
a fixed model all parts stay glued with no catalog.
-}
shareHash : Model -> List Int
shareHash model =
    if model.free then
        Link.freeShareHash model.studio.document

    else if List.isEmpty model.inventory then
        Link.fixedModelHash model.studio.document

    else
        Link.setShareHash model.studio.document model.inventory


{-| The hash to write after an update from `before` to `after`: the live hash
when the build changed (its document, its mode or its kit), else nothing. It
compares the models rather than reading the studio's events, so an edit that
emits no `DocumentChanged` (or a new one that forgets to) cannot leave the
address bar behind.
-}
hashFor : Model -> Model -> Maybe (List Int)
hashFor before after =
    if before.studio.document /= after.studio.document || before.free /= after.free || before.inventory /= after.inventory then
        Just (liveHash after)

    else
        Nothing


{-| Update the application model.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TestCommand payload ->
            runTestCommand payload model

        _ ->
            if closesShareModal msg model then
                ( syncKeyPause { model | shareModal = Nothing }, Cmd.none )

            else if model.tidyOpen then
                updateTidying msg model

            else if closesSnapshot msg model then
                ( { model | snapshotModal = Nothing }, cancelSnapshotCmd model.snapshotModal )

            else if closesRing msg model then
                ( { model | ringPart = Nothing }, Cmd.none )

            else
                updateApp msg model


{-| Escape closes the context ring before it deselects anything, so with the ring
open Escape only closes it. Not while the help list is open: that dialog takes
Escape first.
-}
closesRing : Msg -> Model -> Bool
closesRing msg model =
    case ( model.ringPart, msg ) of
        ( Just _, StudioMsg (Core.ViewportMsg (Viewport.KeyPressed keyEvent)) ) ->
            not model.helpOpen && ViewportShortcuts.closesDialog keyEvent

        _ ->
            False


{-| While the Tidy up preview is open editing is paused. Escape cancels, Apply
and Cancel act, and only camera, loading and layout messages pass through, so a
pointer press cannot start a drag and no key can edit the build.
-}
updateTidying : Msg -> Model -> ( Model, Cmd Msg )
updateTidying msg model =
    case msg of
        StudioMsg (Core.ViewportMsg (Viewport.KeyPressed keyEvent)) ->
            if ViewportShortcuts.closesDialog keyEvent then
                updateApp CancelTidy model

            else
                ( model, Cmd.none )

        CancelTidy ->
            updateApp msg model

        ApplyTidy ->
            updateApp msg model

        _ ->
            if passesWhileTidying msg then
                updateApp msg model

            else
                ( model, Cmd.none )


{-| The messages a Tidy up preview lets through: loading, layout, and the
camera's own controls (none of which edits the document).
-}
passesWhileTidying : Msg -> Bool
passesWhileTidying msg =
    case msg of
        StudioMsg (Core.ViewportMsg viewportEvent) ->
            case viewportEvent of
                Viewport.WindowResize _ _ ->
                    True

                Viewport.TopLevelLoaded _ _ ->
                    True

                Viewport.PartLoaded _ _ ->
                    True

                Viewport.FileContentReceived _ ->
                    True

                Viewport.FileLoadError _ ->
                    True

                Viewport.GeometryFlattened _ ->
                    True

                Viewport.GeometryFlattenFailed _ ->
                    True

                Viewport.AnimationFrame _ ->
                    True

                Viewport.Wheel _ ->
                    True

                Viewport.MouseMove _ _ ->
                    True

                Viewport.MouseUp _ _ ->
                    True

                Viewport.RotateCameraBy _ _ ->
                    True

                Viewport.StartHoldRotate _ _ ->
                    True

                Viewport.EndHoldRotate ->
                    True

                Viewport.FitView ->
                    True

                Viewport.ToggleWalkMode ->
                    True

                Viewport.SetViewPreset _ ->
                    True

                Viewport.SetRenderQuality _ ->
                    True

                Viewport.OrbitLightBy _ _ ->
                    True

                Viewport.KeyReleased _ ->
                    True

                Viewport.AllKeysReleased ->
                    True

                Viewport.DismissError ->
                    True

                Viewport.ContextMenuOpened ->
                    True

                _ ->
                    False

        StudioMsg (Core.GeometryFlattened _) ->
            True

        StudioMsg (Core.GeometryFlattenFailed _) ->
            True

        WindowResized _ _ ->
            True

        ThumbnailLoaded _ ->
            True

        Tick _ ->
            True

        DismissToast _ ->
            True

        GlobePressed _ ->
            True

        GlobeMoved _ ->
            True

        GlobeReleased ->
            True

        ZoomTo _ ->
            True

        ZoomDone ->
            True

        ResetView ->
            True

        NoOp ->
            True

        _ ->
            False


{-| Stop the path tracer when the snapshot modal closes while it is still rendering.
-}
cancelSnapshotCmd : Maybe SnapshotModal.Model -> Cmd msg
cancelSnapshotCmd modal =
    case modal of
        Just rendering ->
            if SnapshotModal.isRendering rendering then
                Ports.cancelSnapshot ()

            else
                Cmd.none

        Nothing ->
            Cmd.none


{-| Escape closes an open snapshot modal before anything else sees the key, so
the editing keys and the layers underneath do not react as well.
-}
closesSnapshot : Msg -> Model -> Bool
closesSnapshot msg model =
    case ( model.snapshotModal, msg ) of
        ( Just _, StudioMsg (Core.ViewportMsg (Viewport.KeyPressed keyEvent)) ) ->
            ViewportShortcuts.closesDialog keyEvent

        _ ->
            False


{-| Escape closes an open share modal before anything else sees the key.
-}
closesShareModal : Msg -> Model -> Bool
closesShareModal msg model =
    case ( model.shareModal, msg ) of
        ( Just _, StudioMsg (Core.ViewportMsg (Viewport.KeyPressed keyEvent)) ) ->
            ViewportShortcuts.closesDialog keyEvent

        _ ->
            False


{-| Run a message, then bring the derived state up to date (`settle`). The dock
asks for the pictures of its new parts when the parts it lists change.
-}
updateApp : Msg -> Model -> ( Model, Cmd Msg )
updateApp msg model =
    let
        ( next, cmd ) =
            step msg model

        settled =
            settle model next

        dockCmd =
            if dockPairs model /= dockPairs settled then
                requestDockThumbnails settled

            else
                Cmd.none

        hashCmd =
            case hashFor model settled of
                Just hash ->
                    Ports.setUrlHash hash

                Nothing ->
                    Cmd.none

        liveCmd =
            if settled.liveBroadcasting then
                case hashFor model settled of
                    Just hash ->
                        Ports.pushLiveSnapshot hash

                    Nothing ->
                        Cmd.none

            else
                Cmd.none

        liveAnimationCmd =
            if settled.liveBroadcasting && model.playing /= settled.playing then
                Ports.pushLiveAnimation settled.playing

            else
                Cmd.none

        reverseSnapshotCmd =
            case settled.reverseParticipantRoom of
                Just _ ->
                    if hashFor model settled /= Nothing || List.length (Core.getPlacements model.studio) /= List.length (Core.getPlacements settled.studio) then
                        pushReverseSnapshotFor settled

                    else
                        Cmd.none

                Nothing ->
                    Cmd.none

        ( hinted, hintCmd ) =
            withHint settled
    in
    ( hinted, Cmd.batch [ cmd, dockCmd, hashCmd, liveCmd, liveAnimationCmd, reverseSnapshotCmd, hintCmd ] )


{-| Handle the opt-in dev browser bridge. `dragLDraw` projects world-space
points into the same canvas coordinates used by pointer events, then feeds those
events through `updateApp`/`Core.update` so acceptance tests exercise the normal
selection, drag, snap, collision, and drop path.
-}
runTestCommand : Decode.Value -> Model -> ( Model, Cmd Msg )
runTestCommand value model =
    let
        requestId =
            Decode.decodeValue (Decode.field "id" Decode.string) value
                |> Result.withDefault ""

        commandType =
            Decode.decodeValue (Decode.field "type" Decode.string) value
                |> Result.withDefault ""

        reply result =
            Ports.testReply
                (Encode.object
                    [ ( "id", Encode.string requestId )
                    , ( "result", result )
                    ]
                )

        failure message =
            Encode.object [ ( "error", Encode.string message ) ]

        pointField name =
            Decode.decodeValue (decodeTestPoint name) value

        coordinateField name =
            Decode.decodeValue (Decode.field name Decode.float) value

        shiftField =
            Decode.decodeValue (Decode.field "shift" Decode.bool) value
                |> Result.withDefault False

        mutatesDocument =
            List.member commandType [ "mouseDown", "mouseMove", "mouseUp", "dragLDraw" ]
    in
    if not model.testApiEnabled then
        ( model, reply (failure "The test bridge is disabled") )

    else if mutatesDocument && model.tidyOpen then
        ( model, reply (failure "Cannot send test input while the tidy preview is open") )

    else if mutatesDocument && model.snapshotModal /= Nothing then
        ( model, reply (failure "Cannot send test input while the snapshot modal is open") )

    else if mutatesDocument && model.shareModal /= Nothing then
        ( model, reply (failure "Cannot send test input while the share modal is open") )

    else
        case commandType of
            "snapshot" ->
                ( model, reply (testSnapshot model) )

            "projectLDrawPoint" ->
                case pointField "point" of
                    Ok point ->
                        case projectLDrawPoint model point of
                            Just ( x, y ) ->
                                ( model, reply (encodeTestScreenPoint x y) )

                            Nothing ->
                                ( model, reply (failure "The point is outside the camera projection") )

                    Err error ->
                        ( model, reply (failure (Decode.errorToString error)) )

            "mouseDown" ->
                case ( coordinateField "x", coordinateField "y" ) of
                    ( Ok x, Ok y ) ->
                        let
                            ( next, cmd ) =
                                update
                                    (StudioMsg
                                        (Core.ViewportMsg
                                            (Viewport.CanvasPressed ( 0, 0 ) (Viewport.MouseButtonDown x y 0 shiftField False))
                                        )
                                    )
                                    model
                        in
                        ( next, Cmd.batch [ cmd, reply (testSnapshot next) ] )

                    _ ->
                        ( model, reply (failure "mouseDown requires numeric x and y") )

            "mouseMove" ->
                case ( coordinateField "x", coordinateField "y" ) of
                    ( Ok x, Ok y ) ->
                        let
                            ( next, cmd ) =
                                update (StudioMsg (Core.ViewportMsg (Viewport.MouseMove x y))) model
                        in
                        ( next, Cmd.batch [ cmd, reply (testSnapshot next) ] )

                    _ ->
                        ( model, reply (failure "mouseMove requires numeric x and y") )

            "mouseUp" ->
                case ( coordinateField "x", coordinateField "y" ) of
                    ( Ok x, Ok y ) ->
                        let
                            ( next, cmd ) =
                                update (StudioMsg (Core.ViewportMsg (Viewport.MouseUp x y))) model
                        in
                        ( next, Cmd.batch [ cmd, reply (testSnapshot next) ] )

                    _ ->
                        ( model, reply (failure "mouseUp requires numeric x and y") )

            "dragLDraw" ->
                case ( pointField "from", pointField "to" ) of
                    ( Ok fromPoint, Ok toPoint ) ->
                        testDragLDraw requestId fromPoint toPoint model

                    ( Err error, _ ) ->
                        ( model, reply (failure (Decode.errorToString error)) )

                    ( _, Err error ) ->
                        ( model, reply (failure (Decode.errorToString error)) )

            _ ->
                ( model, reply (failure ("Unknown test command: " ++ commandType)) )


{-| Project an LDraw-space point into canvas-local coordinates.
-}
projectLDrawPoint : Model -> Vec3 -> Maybe ( Float, Float )
projectLDrawPoint model point =
    Gizmo.project
        model.studio.viewport.camera
        model.studio.viewport.width
        model.studio.viewport.height
        (Gizmo.toRender point)


decodeTestPoint : String -> Decode.Decoder Vec3
decodeTestPoint name =
    Decode.field name
        (Decode.map3 Vec3.vec3
            (Decode.field "x" Decode.float)
            (Decode.field "y" Decode.float)
            (Decode.field "z" Decode.float)
        )


encodeTestScreenPoint : Float -> Float -> Encode.Value
encodeTestScreenPoint x y =
    Encode.object
        [ ( "x", Encode.float x )
        , ( "y", Encode.float y )
        ]


encodeTestVector : Vec3 -> Encode.Value
encodeTestVector vector =
    Encode.object
        [ ( "x", Encode.float (Vec3.getX vector) )
        , ( "y", Encode.float (Vec3.getY vector) )
        , ( "z", Encode.float (Vec3.getZ vector) )
        ]


testSnapshot : Model -> Encode.Value
testSnapshot model =
    let
        placementValue placement =
            let
                connections =
                    Connectivity.extractPartConnectivity placement.part (testResolvedLines model)
                        |> .connections
                        |> List.map
                            (\connection ->
                                Encode.object
                                    [ ( "id", Encode.int connection.id )
                                    , ( "localPosition", encodeTestVector connection.localPosition )
                                    , ( "normal", encodeTestVector connection.normal )
                                    ]
                            )
            in
            Encode.object
                [ ( "id", Encode.int placement.id )
                , ( "part", Encode.string placement.part )
                , ( "position", encodeTestVector (Mat4.transform placement.matrix (Vec3.vec3 0 0 0)) )
                , ( "glued", Encode.bool (Core.isGlued placement.id model.studio) )
                , ( "connections", Encode.list identity connections )
                ]

        snapValue =
            case model.studio.activeSnap of
                Just snap ->
                    Encode.object
                        [ ( "draggedPointId", Encode.int snap.draggedPoint.id )
                        , ( "draggedLocalPosition", encodeTestVector snap.draggedPoint.localPosition )
                        , ( "targetPlacementId", Encode.int snap.targetPlacementId )
                        , ( "targetPointId", Encode.int snap.targetPoint.pointId )
                        , ( "targetWorldPosition", encodeTestVector snap.targetWorldPosition )
                        , ( "distance", Encode.float snap.distance )
                        ]

                Nothing ->
                    Encode.null
    in
    Encode.object
        [ ( "mainPlacements", Encode.list placementValue (Document.mainPlacements model.studio.document) )
        , ( "selection", Encode.list Encode.int (Set.toList model.studio.selection) )
        , ( "viewport"
          , Encode.object
                [ ( "width", Encode.int model.studio.viewport.width )
                , ( "height", Encode.int model.studio.viewport.height )
                ]
          )
        , ( "dragging", Encode.bool (model.studio.drag /= Nothing) )
        , ( "activeSnap", snapValue )
        , ( "collidingPlacements", Encode.list Encode.int (Set.toList model.studio.collidingPlacements) )
        ]


testResolvedLines : Model -> String -> Maybe (List LDrawLine)
testResolvedLines model name =
    case Dict.get name model.studio.viewport.partCache of
        Just (Resolve.Loaded lines) ->
            Just lines

        _ ->
            Nothing


testDragLDraw : String -> Vec3 -> Vec3 -> Model -> ( Model, Cmd Msg )
testDragLDraw requestId fromPoint toPoint model =
    case ( projectLDrawPoint model fromPoint, projectLDrawPoint model toPoint ) of
        ( Just ( startX, startY ), Just ( endX, endY ) ) ->
            let
                pressEvent =
                    Viewport.CanvasPressed ( 0, 0 ) (Viewport.MouseButtonDown startX startY 0 False False)

                ( pressed, pressCmd ) =
                    update (StudioMsg (Core.ViewportMsg pressEvent)) model

                ( moved, moveCmd ) =
                    update (StudioMsg (Core.ViewportMsg (Viewport.MouseMove endX endY))) pressed

                preview =
                    testSnapshot moved

                ( released, releaseCmd ) =
                    update (StudioMsg (Core.ViewportMsg (Viewport.MouseUp endX endY))) moved

                result =
                    Encode.object
                        [ ( "projectedFrom", encodeTestScreenPoint startX startY )
                        , ( "projectedTo", encodeTestScreenPoint endX endY )
                        , ( "preview", preview )
                        , ( "final", testSnapshot released )
                        ]

                reply =
                    Ports.testReply
                        (Encode.object
                            [ ( "id", Encode.string requestId )
                            , ( "result", result )
                            ]
                        )
            in
            ( released, Cmd.batch [ pressCmd, moveCmd, releaseCmd, reply ] )

        _ ->
            ( model
            , Ports.testReply
                (Encode.object
                    [ ( "id", Encode.string requestId )
                    , ( "result", Encode.object [ ( "error", Encode.string "A drag point is outside the camera projection" ) ] )
                    ]
                )
            )


{-| The first-selection hint: the first time a part is selected, and the hint
has not been seen in this browser, it shows in the top-left bar for five seconds
and is remembered as seen. Any selection counts, however it was made.
-}
withHint : Model -> ( Model, Cmd Msg )
withHint model =
    if model.hintSeen || Set.isEmpty model.studio.selection then
        ( model, Cmd.none )

    else
        let
            id =
                model.hintCounter + 1
        in
        ( { model | hintSeen = True, hint = Just id, hintCounter = id }
        , Cmd.batch
            [ Ports.rememberHintSeen ()
            , Process.sleep 5000 |> Task.perform (\_ -> HideHint id)
            ]
        )


{-| Whether a part is being dragged across the build (not just pressed): the
drag has moved since it started.
-}
partDragged : Model -> Bool
partDragged model =
    case model.studio.drag of
        Just drag ->
            drag.start /= drag.lastPoint

        Nothing ->
            False


{-| The ring closes when its part stops being the whole selection (a click
elsewhere, a different part, Tab, a deletion), when a part is dragged, and when
the Tidy up preview or the menu opens.
-}
ringClosed : Model -> Model
ringClosed model =
    let
        clearedTap =
            if partDragged model then
                { model | lastTap = Nothing }

            else
                model
    in
    case clearedTap.ringPart of
        Just id ->
            if clearedTap.tidyOpen || clearedTap.menuOpen || partDragged clearedTap || Set.isEmpty clearedTap.studio.selection then
                { clearedTap | ringPart = Nothing }

            else if Set.member id clearedTap.studio.selection then
                clearedTap

            else if Core.shiftHeld clearedTap.studio then
                case Set.toList clearedTap.studio.selection of
                    nextId :: _ ->
                        { clearedTap | ringPart = Just nextId }

                    [] ->
                        { clearedTap | ringPart = Nothing }

            else
                { clearedTap | ringPart = Nothing }

        Nothing ->
            clearedTap


{-| Brings the derived state in line with what a message just did. A changed
document is analysed again for its gears, and the machine comes to rest. Then
Technic mode sets the studio's gear alignment and drag settling, and a machine
that is no longer Technic stops.
-}
settle : Model -> Model -> Model
settle before after =
    let
        edited =
            after.studio.document.submodels /= before.studio.document.submodels

        -- A part's geometry arriving can change what the build check can see.
        partsArrived =
            Dict.size before.studio.viewport.partCache /= Dict.size after.studio.viewport.partCache

        analysed =
            if edited then
                { after | machine = Machine.analyseMachine after.studio.document after.studio.viewport.partCache }

            else
                after

        rested =
            if edited || (not (technicMode analysed) && (analysed.playing || analysed.currentTime /= 0)) then
                stopForEdit analysed

            else
                analysed

        -- Mid-drag the document changes on every move, so the check waits for the drop.
        checked =
            if (edited || partsArrived) && after.studio.drag == Nothing then
                withTidy rested

            else
                rested

        unshifted =
            if before.studio.drag /= Nothing && checked.studio.drag == Nothing && Core.shiftHeld checked.studio then
                { checked | studio = Core.setShiftHeld False checked.studio }

            else
                checked
    in
    withTechnicSettings (ringClosed unshifted)


{-| The Tidy up plan for the studio's document: what `Studio.Health` would move.
Empty when the build is healthy.
-}
tidyPlan : Core.Model -> List Health.Fix
tidyPlan studio =
    let
        document =
            studio.document

        cache =
            studio.viewport.partCache

        glued =
            Document.gluedIds document
    in
    Health.plan cache document glued (Health.check cache document glued)


withTidy : Model -> Model
withTidy model =
    { model | tidy = tidyPlan model.studio }


{-| Technic mode turns on gear alignment (a meshing gear turns its axle line so
the teeth interlock). Drag settling is off in both modes so parts keep their plane height.
-}
withTechnicSettings : Model -> Model
withTechnicSettings model =
    let
        technic =
            technicMode model
    in
    { model
        | studio =
            model.studio
                |> Core.setGearAlignment technic
                |> Core.setDragSettles False
    }


{-| An edit puts the machine back at rest: paused, at time 0, nothing animated,
so every part is drawn by the static scene again.
-}
stopForEdit : Model -> Model
stopForEdit model =
    { model
        | playing = False
        , currentTime = 0
        , simulation = Simulation.setTime 0 model.simulation
        , animated = Animation.none
    }


{-| Play from rest analyses the current document for the turning gears and
their meshes; resuming a pause keeps the analysis and the clock.
-}
startPlaying : Model -> Model
startPlaying model =
    if model.currentTime > 0 then
        { model | playing = True }

    else
        let
            ( simulation, _ ) =
                Simulation.analyse
                    { lines = Document.toLines model.studio.document
                    , cache = model.studio.viewport.partCache
                    }
                    (Simulation.setTime 0 model.simulation)

            gears =
                simulation.gearGraph
                    |> Maybe.map (.instances >> Array.toList)
                    |> Maybe.withDefault []
        in
        { model
            | playing = True
            , simulation = Simulation.setTime 0 simulation
            , animated =
                let
                    turning =
                        Simulation.rotatingGearIds simulation
                in
                Animation.rotating turning
                    (Placements.gearSources model.studio.document gears)
                    (Placements.partSources model.studio.document (Simulation.drivenPartsFor turning simulation))
        }


{-| Handle one message, without the derived-state bookkeeping of `updateApp`.
-}
step : Msg -> Model -> ( Model, Cmd Msg )
step msg model =
    case msg of
        TestCommand _ ->
            ( model, Cmd.none )

        StudioMsg studioMsg ->
            if model.viewerRoom /= Nothing then
                let
                    ( nextStudio, studioCmd, _ ) =
                        Core.update studioMsg model.studio
                in
                -- In viewer mode, keep document locked to what the broadcaster sent
                ( { model | studio = { nextStudio | document = model.studio.document } }
                , Cmd.map StudioMsg studioCmd
                )

            else
                let
                    ( nextStudio, studioCmd, events ) =
                        Core.update studioMsg model.studio

                    portCmds =
                        handleCoreEvents events

                    nextModel =
                        { model | studio = nextStudio }
                in
                List.foldl applyEvent
                    ( nextModel
                    , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
                    )
                    events

        TogglePlay ->
            -- Outside Technic mode there is no machine, so Space is unbound.
            -- In viewer mode, the animation is driven by the broadcaster.
            if model.viewerRoom /= Nothing || not (technicMode model) then
                ( model, Cmd.none )

            else if model.playing then
                ( { model | playing = False }, Cmd.none )

            else
                ( startPlaying model, Cmd.none )

        AddPart item ->
            if remainingForItem model item <= 0 then
                ( model, Cmd.none )

            else
                placePart item.part item.color (kitItemName model item) model

        AddFreePart part ->
            placePart part.file part.color part.name model

        SetFreeTab tab ->
            ( { model | activeTab = tab }, Cmd.none )

        SetPaintColour colour ->
            let
                ( nextModel, cmd ) =
                    if Core.hasSelection model.studio then
                        runCore (Core.recolorSelection colour) model

                    else
                        ( model, Cmd.none )
            in
            ( { nextModel | paintColour = colour }, cmd )

        SearchInput query ->
            let
                ( index, loadCmd ) =
                    ensureLibrary model.ldrawBase model.search.index
            in
            ( { model | search = searchState query index }, loadCmd )

        SearchOpened ->
            let
                ( index, loadCmd ) =
                    ensureLibrary model.ldrawBase model.search.index
            in
            ( { model | search = searchState model.search.query index }, loadCmd )

        SearchLoaded result ->
            let
                index =
                    case result of
                        Ok library ->
                            Loaded library

                        Err _ ->
                            Failed "the part library could not be fetched"
            in
            ( { model | search = searchState model.search.query index }, Cmd.none )

        ToggleGlue ->
            runCore (\studio -> Core.setGlued (not (Core.selectionGlued studio)) studio) model

        Undo ->
            runCore Core.undo model

        Redo ->
            runCore Core.redo model

        Delete ->
            runCore Core.deleteSelection model

        Cut ->
            runCore Core.cutSelection model

        Copy ->
            runCore (\s -> ( Core.copySelection s, Cmd.none, [] )) model

        Paste ->
            runCore Core.pasteClipboard model

        Group ->
            runCore Core.groupSelection model

        Ungroup ->
            runCore Core.explodeSelection model

        TurnSelection direction ->
            runCore (Core.turnSelected direction) model

        RaiseSelection ->
            runCore Core.raiseSelected model

        LowerSelection ->
            runCore Core.lowerSelected model

        Clear ->
            if model.free then
                runCore (Core.resetDocument Document.empty) model

            else
                -- A puzzle keeps its glued parts.
                runCore Core.clearUnglued model

        MoveSelection direction ->
            step (StudioMsg (Core.NudgeSelection (StudioShortcuts.Nudge { direction = direction, fine = False }))) model

        SetStepSize size ->
            ( { model | stepSize = size, studio = Step.apply size model.studio }
            , Cmd.none
            )

        ToggleBitDrag ->
            let
                active =
                    Core.fineMove model.studio

                nextStep =
                    if active then
                        Step.Stud

                    else
                        Step.Bit

                nextStudio =
                    model.studio
                        |> Step.apply nextStep
            in
            ( { model | stepSize = nextStep, studio = nextStudio }
            , Cmd.none
            )

        OpenRing id ->
            openRing id model

        CloseRing ->
            ( { model | ringPart = Nothing }, Cmd.none )

        SelectNeighbour backwards ->
            runCore (Core.selectNeighbour { backwards = backwards }) model

        TapTimed x y millis ->
            tapTimed x y millis model

        HideHint id ->
            ( if model.hint == Just id then
                { model | hint = Nothing }

              else
                model
            , Cmd.none
            )

        ToggleRealistic ->
            let
                nextQuality =
                    case Core.renderQuality model.studio of
                        Style.Draft ->
                            Style.Realistic

                        Style.Realistic ->
                            Style.Draft
            in
            ( { model | studio = Core.setRenderQuality nextQuality model.studio }
            , Cmd.none
            )

        CaptureSnapshot ->
            if model.viewerRoom /= Nothing then
                ( model, Cmd.none )

            else
                ( { model | snapshotModal = Just (SnapshotModal.init { filename = "brick-play-render.png" }) }
                , Ports.captureRender
                    (Viewport.snapshotRequest
                        { filename = "brick-play-render.png"
                        , extraTriangles = snapshotTriangles model
                        }
                        model.studio.viewport
                    )
                )

        SnapshotReady payload ->
            ( { model | snapshotModal = Maybe.map (SnapshotModal.setReady payload) model.snapshotModal }
            , Cmd.none
            )

        SnapshotProgress progress ->
            ( { model | snapshotModal = Maybe.map (SnapshotModal.setProgress progress) model.snapshotModal }
            , Cmd.none
            )

        SnapshotFailed message ->
            ( { model | snapshotModal = Maybe.map (SnapshotModal.setFailed message) model.snapshotModal }
            , Cmd.none
            )

        SnapshotSave payload ->
            ( { model | snapshotModal = Maybe.map SnapshotModal.markSaved model.snapshotModal }
            , Ports.saveSnapshot payload
            )

        SnapshotClose ->
            ( { model | snapshotModal = Nothing }
            , cancelSnapshotCmd model.snapshotModal
            )

        FitView ->
            let
                ( nextStudio, studioCmd ) =
                    Core.fitView model.studio
            in
            ( { model | studio = nextStudio }, Cmd.map StudioMsg studioCmd )

        DownloadIo ->
            let
                ldrText =
                    LDraw.Writer.writeDocument (Core.getDocument model.studio)
            in
            ( model, Ports.downloadStudioIo { filename = "model.io", ldrText = ldrText } )

        Tick delta ->
            if model.playing then
                let
                    newTime =
                        model.currentTime + delta / 1000.0
                in
                ( { model
                    | currentTime = newTime
                    , simulation = Simulation.setTime newTime model.simulation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        GlobePressed point ->
            ( { model
                | globeDrag =
                    Just
                        (CameraGlobe.startDragWithLight
                            model.studio.viewport.camera
                            (Core.lightDirection model.studio)
                            point
                        )
              }
            , Cmd.none
            )

        GlobeMoved point ->
            case model.globeDrag of
                Just drag ->
                    let
                        ( ( azimuthDelta, elevationDelta ), nextDrag ) =
                            CameraGlobe.moveDrag drag point

                        moveMsg =
                            case CameraGlobe.dragTarget drag of
                                CameraGlobe.LightTarget ->
                                    Viewport.OrbitLightBy azimuthDelta elevationDelta

                                CameraGlobe.CameraTarget ->
                                    Viewport.RotateCameraBy azimuthDelta elevationDelta
                    in
                    update (viewportMsg moveMsg)
                        { model | globeDrag = Just nextDrag }

                Nothing ->
                    ( model, Cmd.none )

        GlobeReleased ->
            let
                released =
                    { model | globeDrag = Nothing }
            in
            case model.globeDrag of
                Just drag ->
                    case CameraGlobe.dragTarget drag of
                        CameraGlobe.LightTarget ->
                            case
                                Core.lightDirection model.studio
                                    |> Maybe.andThen (\dir -> CameraGlobe.releaseLightDrag dir drag)
                            of
                                Just ( azimuthDelta, elevationDelta ) ->
                                    update (viewportMsg (Viewport.OrbitLightBy azimuthDelta elevationDelta)) released

                                Nothing ->
                                    ( released, Cmd.none )

                        CameraGlobe.CameraTarget ->
                            case CameraGlobe.releaseDrag model.studio.viewport.camera drag of
                                Just ( azimuthDelta, elevationDelta ) ->
                                    update (viewportMsg (Viewport.RotateCameraBy azimuthDelta elevationDelta)) released

                                Nothing ->
                                    ( released, Cmd.none )

                Nothing ->
                    ( released, Cmd.none )

        ZoomTo level ->
            update (viewportMsg (Viewport.Wheel (CameraGlobe.wheelDeltaForZoom model.studio.viewport.camera level))) model

        ZoomDone ->
            ( model, Task.attempt (\_ -> NoOp) (Browser.Dom.blur CameraGlobe.zoomSliderId) )

        ResetView ->
            let
                ( fitted, fitCmd ) =
                    update FitView model
            in
            ( fitted
            , Cmd.batch [ fitCmd, Task.attempt (\_ -> NoOp) (Browser.Dom.blur CameraGlobe.resetButtonId) ]
            )

        ToggleCameraPanel ->
            ( { model
                | cameraShown = not model.cameraShown
                , ringPart =
                    if model.cameraShown then
                        model.ringPart

                    else
                        Nothing
              }
            , Cmd.none
            )

        ToggleHelp ->
            let
                open =
                    not model.helpOpen
            in
            ( syncKeyPause { model | helpOpen = open, menuOpen = False }, Cmd.none )

        OpenTidy ->
            if List.isEmpty model.tidy then
                ( model, Cmd.none )

            else
                ( syncKeyPause
                    { model
                        | tidyOpen = True
                        , menuOpen = False
                        , studio = Core.setPreview model.tidy model.studio
                    }
                , requestTidyTitles model
                )

        CancelTidy ->
            ( syncKeyPause { model | tidyOpen = False, studio = Core.setPreview [] model.studio }, Cmd.none )

        ApplyTidy ->
            let
                ( studio, studioCmd, events ) =
                    Core.applyFixes model.tidy model.studio
            in
            ( syncKeyPause { model | tidyOpen = False, studio = studio }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, handleCoreEvents events ]
            )

        ToggleMenu ->
            ( syncKeyPause { model | menuOpen = not model.menuOpen && not model.helpOpen }, Cmd.none )

        CloseMenu ->
            ( syncKeyPause { model | menuOpen = False }, Cmd.none )

        SetLanguage lang ->
            ( syncKeyPause { model | language = lang, menuOpen = False }
            , Ports.storeLanguage (I18n.toString lang)
            )

        OpenShareModal ->
            let
                initialKind =
                    ShareModal.defaultKind
                        { free = model.free
                        , hasInventory = not (List.isEmpty model.inventory)
                        }
            in
            ( syncKeyPause { model | menuOpen = False, shareModal = Just (ShareModal.init initialKind) }
            , Cmd.none
            )

        CloseShareModal ->
            ( syncKeyPause { model | shareModal = Nothing }
            , Cmd.none
            )

        SelectShareKind kind ->
            ( { model
                | shareModal =
                    Maybe.map
                        (\sm ->
                            { sm
                                | selected = kind
                                , copied = False
                                , copiedParticipant = False
                                , copiedHost = False
                                , reverseSharing =
                                    if kind == ShareModal.LivePreview then
                                        False

                                    else
                                        sm.reverseSharing
                            }
                        )
                        model.shareModal
              }
            , Cmd.none
            )

        CopyShareUrl ->
            case model.shareModal of
                Just sm ->
                    case sm.selected of
                        ShareModal.LivePreview ->
                            let
                                url =
                                    ShareModal.computeUrlWithLive model.baseUrl sm.selected model.free model.studio.document model.inventory model.liveWatchUrl
                            in
                            ( { model | shareModal = Just { sm | copied = True } }
                            , Cmd.batch
                                [ Ports.copyTextToClipboard url
                                , Process.sleep 2500 |> Task.perform (\_ -> DismissShareCopied)
                                ]
                            )

                        _ ->
                            let
                                payload =
                                    ShareModal.payloadFor sm.selected model.free model.studio.document model.inventory
                            in
                            ( { model | shareModal = Just { sm | copied = True } }
                            , Cmd.batch
                                [ Ports.copyToClipboard payload
                                , Process.sleep 2500 |> Task.perform (\_ -> DismissShareCopied)
                                ]
                            )

                Nothing ->
                    ( model, Cmd.none )

        DismissShareCopied ->
            ( { model | shareModal = Maybe.map (\sm -> { sm | copied = False, copiedParticipant = False, copiedHost = False }) model.shareModal }
            , Cmd.none
            )

        ToggleReverseSharing ->
            ( { model | shareModal = Maybe.map (ShareModal.update ShareModal.ToggleReverseSharing) model.shareModal }
            , Cmd.none
            )

        ShareModalMsg subMsg ->
            ( { model | shareModal = Maybe.map (ShareModal.update subMsg) model.shareModal }
            , Cmd.none
            )

        CopyParticipantUrl ->
            case model.shareModal of
                Just sm ->
                    let
                        url =
                            ShareModal.computeReverseParticipantUrl model.baseUrl sm.reverseRoomId sm.selected model.free model.studio.document model.inventory
                    in
                    ( { model | shareModal = Just { sm | copiedParticipant = True } }
                    , Cmd.batch
                        [ Ports.copyTextToClipboard url
                        , Process.sleep 2500 |> Task.perform (\_ -> DismissShareCopied)
                        ]
                    )

                Nothing ->
                    case model.reverseHostRoom of
                        Just roomId ->
                            let
                                url =
                                    ShareModal.computeReverseParticipantUrl model.baseUrl roomId ShareModal.PartsSet model.free model.studio.document model.inventory

                                toastMsg =
                                    case model.language of
                                        Fi ->
                                            "Osallistujan linkki kopioitu leikepöydälle"

                                        En ->
                                            "Participant link copied to clipboard"

                                ( toastModel, toastCmd ) =
                                    showToast toastMsg model
                            in
                            ( toastModel
                            , Cmd.batch
                                [ Ports.copyTextToClipboard url
                                , toastCmd
                                ]
                            )

                        Nothing ->
                            ( model, Cmd.none )

        CopyHostUrl ->
            case model.shareModal of
                Just sm ->
                    let
                        url =
                            ShareModal.computeReverseHostUrl model.baseUrl sm.reverseRoomId
                    in
                    ( { model | shareModal = Just { sm | copiedHost = True } }
                    , Cmd.batch
                        [ Ports.copyTextToClipboard url
                        , Process.sleep 2500 |> Task.perform (\_ -> DismissShareCopied)
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        ShareEditableModel ->
            if model.free then
                copyShareHash (Link.editableHash model.studio.document) model

            else
                ( model, Cmd.none )

        SharePartsSet ->
            if model.free then
                copyShareHash (Link.freeShareHash model.studio.document) model

            else if List.isEmpty model.inventory then
                ( model, Cmd.none )

            else
                copyShareHash (Link.setShareHash model.studio.document model.inventory) model

        ShareFixedModel ->
            if model.free || List.isEmpty model.inventory then
                copyShareHash (Link.fixedModelHash model.studio.document) model

            else
                ( model, Cmd.none )

        ToggleBroadcast ->
            if model.liveBroadcasting then
                ( { model | liveBroadcasting = False, liveRoomId = Nothing, liveWatchUrl = Nothing, liveViewerCount = 0 }
                , Ports.stopBroadcast ()
                )

            else
                ( model, Ports.startBroadcast () )

        BroadcastStarted { roomId, watchUrl } ->
            let
                snapshotBytes =
                    liveHash model
            in
            ( { model
                | liveBroadcasting = True
                , liveRoomId = Just roomId
                , liveWatchUrl = Just watchUrl
              }
            , Cmd.batch
                [ Ports.pushLiveSnapshot snapshotBytes
                , Ports.pushLiveAnimation model.playing
                ]
            )

        BroadcastStopped ->
            ( { model | liveBroadcasting = False, liveRoomId = Nothing, liveWatchUrl = Nothing, liveViewerCount = 0 }
            , Cmd.none
            )

        BroadcastViewersChanged count ->
            ( { model | liveViewerCount = count }, Cmd.none )

        BroadcastError err ->
            showToast err model

        LiveSnapshotReceived bytes ->
            case HashPayload.decodeBytes bytes of
                Just payload ->
                    let
                        doc =
                            if model.viewerRoom /= Nothing then
                                List.foldl (\p acc -> Document.setGlued True p.id acc)
                                    payload.document
                                    (Document.placements payload.document)

                            else
                                payload.document

                        ( resetStudio, studioCmd, _ ) =
                            Core.resetDocument doc model.studio

                        updatedStudio =
                            resetStudio
                                |> Core.setGlueEditable False
                                |> Core.clearSelection
                    in
                    ( { model | studio = updatedStudio }
                    , Cmd.map StudioMsg studioCmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        LiveAnimationReceived playing ->
            if playing then
                if technicMode model then
                    ( startPlaying model, Cmd.none )

                else
                    ( model, Cmd.none )

            else
                ( { model | playing = False }, Cmd.none )

        LiveStatusChanged status ->
            ( { model | viewerStatus = status }, Cmd.none )

        ReverseStatusChanged status ->
            let
                cmd =
                    if status == "live" && model.reverseParticipantRoom /= Nothing then
                        pushReverseSnapshotFor model

                    else
                        Cmd.none
            in
            ( { model | reverseStatus = status }, cmd )

        ReverseParticipantUpdated data ->
            let
                participant =
                    { id = data.id
                    , name = data.name
                    , bytes = data.bytes
                    , thumbnail = data.thumbnail
                    , brickCount = data.brickCount
                    , lastSeen = data.lastSeen
                    , status = "connected"
                    }

                newParticipants =
                    Dict.insert data.id participant model.participants

                newModel =
                    { model | participants = newParticipants }
            in
            if model.inspectedParticipant == Just data.id then
                case HashPayload.decodeBytes data.bytes of
                    Just payload ->
                        let
                            doc =
                                List.foldl (\p acc -> Document.setGlued True p.id acc)
                                    payload.document
                                    (Document.placements payload.document)

                            ( resetStudio, studioCmd, _ ) =
                                Core.resetDocument doc model.studio

                            updatedStudio =
                                resetStudio
                                    |> Core.setGlueEditable False
                                    |> Core.clearSelection
                        in
                        ( { newModel | studio = updatedStudio }
                        , Cmd.map StudioMsg studioCmd
                        )

                    Nothing ->
                        ( newModel, Cmd.none )

            else
                ( newModel, Cmd.none )

        ReverseParticipantLeft id ->
            case Dict.get id model.participants of
                Just p ->
                    let
                        updated =
                            { p | status = "disconnected" }
                    in
                    ( { model | participants = Dict.insert id updated model.participants }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        InspectParticipant maybeId ->
            case maybeId of
                Just id ->
                    let
                        modelWithInspect =
                            { model | inspectedParticipant = Just id }
                    in
                    case Dict.get id model.participants of
                        Just p ->
                            case HashPayload.decodeBytes p.bytes of
                                Just payload ->
                                    let
                                        doc =
                                            List.foldl (\pl acc -> Document.setGlued True pl.id acc)
                                                payload.document
                                                (Document.placements payload.document)

                                        ( resetStudio, studioCmd, _ ) =
                                            Core.resetDocument doc model.studio

                                        updatedStudio =
                                            resetStudio
                                                |> Core.setGlueEditable False
                                                |> Core.clearSelection
                                    in
                                    ( { modelWithInspect | studio = updatedStudio }
                                    , Cmd.map StudioMsg studioCmd
                                    )

                                Nothing ->
                                    ( modelWithInspect, Cmd.none )

                        Nothing ->
                            ( modelWithInspect, Cmd.none )

                Nothing ->
                    ( { model | inspectedParticipant = Nothing }, Cmd.none )

        LiveError err ->
            showToast err model

        DismissToast id ->
            if Maybe.map .id model.toast == Just id then
                ( { model | toast = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        MenuPicked action ->
            -- The menu closes, then the chosen message runs as if its button were pressed.
            let
                ( next, cmd ) =
                    update action (syncKeyPause { model | menuOpen = False })
            in
            ( syncKeyPause next, cmd )

        NoOp ->
            ( model, Cmd.none )

        WindowResized w h ->
            -- The viewport projects picks and gizmo handles through its own
            -- size, so it must track the canvas, not keep its 800×600 default.
            update (viewportMsg (Viewport.WindowResize w h)) { model | windowWidth = w, windowHeight = h }

        ThumbnailLoaded { file, color, url, title } ->
            ( { model
                | thumbnails =
                    Dict.insert (PartCard.thumbnailKey file color) { url = url, title = title } model.thumbnails
              }
            , Cmd.none
            )


{-| Show a temporary toast message for 3 seconds.
-}
showToast : String -> Model -> ( Model, Cmd Msg )
showToast message model =
    let
        nextId =
            model.toastCounter + 1
    in
    ( { model
        | toast = Just { message = message, id = nextId }
        , toastCounter = nextId
      }
    , Process.sleep 3000 |> Task.perform (\_ -> DismissToast nextId)
    )


{-| Copy a share link payload to the clipboard, close the menu and show a toast.
-}
copyShareHash : List Int -> Model -> ( Model, Cmd Msg )
copyShareHash hash model =
    let
        nextId =
            model.toastCounter + 1
    in
    ( { model
        | menuOpen = False
        , toast = Just { message = I18n.linkCopied model.language, id = nextId }
        , toastCounter = nextId
      }
    , Cmd.batch
        [ Ports.copyToClipboard hash
        , Process.sleep 3000 |> Task.perform (\_ -> DismissToast nextId)
        ]
    )


{-| Put a dock part into the studio: a Technic part on the shared axle line, next
to the selection (`Technic.Placement`), anything else on a free ground spot
clear of every piece already placed.
-}
placePart : String -> Int -> String -> Model -> ( Model, Cmd Msg )
placePart file color name model =
    let
        ( nextStudio, studioCmd, events ) =
            case Placement.targetFor file model.studio of
                Placement.AtPlacement matrix ->
                    Core.addPartAt file color matrix model.studio

                Placement.OnGround spot ->
                    Core.addPart file color spot model.studio

        nextModel =
            { model | studio = nextStudio, lastTapped = Just name }
    in
    ( nextModel
    , Cmd.batch [ Cmd.map StudioMsg studioCmd, handleCoreEvents events ]
    )


{-| The gears and driven parts drawn turning in the snapshot: only in Technic
mode, where the machine has a simulation to draw.
-}
snapshotTriangles : Model -> List Float
snapshotTriangles model =
    if technicMode model then
        Simulation.snapshotTriangles model.simulation

    else
        []


{-| Open the context ring on the part under a canvas point (a double-click). Empty
canvas opens nothing.
-}
openRingAt : Float -> Float -> Model -> ( Model, Cmd Msg )
openRingAt x y model =
    case pickedAt x y model of
        Just id ->
            openRing id model

        Nothing ->
            ( model, Cmd.none )


{-| Which part is under a canvas point, including a gear's axle hole.
-}
pickedAt : Float -> Float -> Model -> Maybe Int
pickedAt x y model =
    Viewport.pickBodyPoint x y model.studio.viewport
        |> Maybe.map .source


{-| Open the ring on a part: the part becomes the explicit selection, and the
hint (if it is still due) is put away and remembered as seen.
-}
openRing : Int -> Model -> ( Model, Cmd Msg )
openRing id model =
    if List.any (\p -> p.id == id) (Core.getPlacements model.studio) then
        let
            ( studio, studioCmd, events ) =
                if Set.member id model.studio.selection then
                    ( model.studio, Cmd.none, [] )

                else
                    Core.update (Core.SelectOnly id) model.studio
        in
        ( { model
            | studio = studio
            , ringPart = Just id
            , cameraShown = False
            , lastTap = Nothing
            , hint = Nothing
            , hintSeen = True
          }
        , Cmd.batch
            [ Cmd.map StudioMsg studioCmd
            , handleCoreEvents events
            , if model.hintSeen then
                Cmd.none

              else
                Ports.rememberHintSeen ()
            ]
        )

    else
        ( model, Cmd.none )


{-| Two taps close in time and place are a double-tap: the second opens the ring
on the part it lands on (or the part the first tap hit, if the camera glided it
toward the centre between the two taps). Taps arrive with their time in
milliseconds (`TapTimed`).
-}
tapTimed : Float -> Float -> Int -> Model -> ( Model, Cmd Msg )
tapTimed x y millis model =
    let
        isDouble previous =
            millis
                - previous.millis
                <= doubleTapMillis
                && ((x - previous.x) ^ 2 + (y - previous.y) ^ 2 <= doubleTapPixels ^ 2)

        hitPart =
            case pickedAt x y model of
                Just id ->
                    Just id

                Nothing ->
                    case Set.toList model.studio.selection of
                        [ id ] ->
                            Just id

                        _ ->
                            Nothing

        here =
            Just { x = x, y = y, millis = millis, part = hitPart }
    in
    case model.lastTap of
        Just previous ->
            if isDouble previous then
                case previous.part of
                    Just id ->
                        openRing id { model | lastTap = Nothing }

                    Nothing ->
                        openRingAt x y { model | lastTap = Nothing }

            else
                ( { model | lastTap = here }, Cmd.none )

        Nothing ->
            ( { model | lastTap = here }, Cmd.none )


{-| The longest gap, in milliseconds, between the taps of a double-tap.
-}
doubleTapMillis : Int
doubleTapMillis =
    350


{-| How far, in pixels, the second tap of a double-tap may land from the first.
-}
doubleTapPixels : Float
doubleTapPixels =
    30


{-| Where the ring anchors: the screen rectangle of the part's bounding box (the
projection of its eight corners), or `Nothing` while the part is off screen or
behind the eye.
-}
partScreenRect : Model -> Int -> Maybe Ring.Rect
partScreenRect model id =
    let
        selectedIds =
            if Set.member id model.studio.selection then
                model.studio.selection

            else
                Set.singleton id

        placements =
            Core.getPlacements model.studio
                |> List.filter (\p -> Set.member p.id selectedIds)

        cornersFor placed =
            boxCorners (Core.partBox placed.part model.studio)
                |> List.map (\corner -> Gizmo.toRender (Mat4.transform placed.matrix corner))

        allCorners =
            List.concatMap cornersFor placements
    in
    if List.isEmpty placements then
        Nothing

    else
        Ring.screenRect
            model.studio.viewport.camera
            model.studio.viewport.width
            model.studio.viewport.height
            allCorners


{-| The eight corners of a part's local box (LDraw).
-}
boxCorners : { min : Vec3, max : Vec3 } -> List Vec3
boxCorners box =
    let
        xs =
            [ Vec3.getX box.min, Vec3.getX box.max ]

        ys =
            [ Vec3.getY box.min, Vec3.getY box.max ]

        zs =
            [ Vec3.getZ box.min, Vec3.getZ box.max ]
    in
    List.concatMap
        (\x ->
            List.concatMap (\y -> List.map (\z -> Vec3.vec3 x y z) zs) ys
        )
        xs


{-| The screen areas the context ring keeps clear of when it can: the top-left
toolbar row, the 130 px dock band along the bottom, and, on a wide window, the
camera globe in the bottom-right corner.
-}
ringAvoid : Model -> List Ring.Rect
ringAvoid model =
    let
        width =
            toFloat model.windowWidth

        height =
            toFloat model.windowHeight

        topToolbar =
            { left = 0, top = 0, right = width, bottom = 12 + 44 }

        dock =
            { left = 0, top = height - 12 - 130, right = width, bottom = height }

        globe =
            { left = width - 12 - 200, top = height - 12 - 200, right = width, bottom = height }
    in
    if isNarrow model then
        [ topToolbar, dock ]

    else
        [ topToolbar, dock, globe ]


syncKeyPause : Model -> Model
syncKeyPause model =
    { model | studio = Core.setHelpVisible (model.helpOpen || model.menuOpen || model.tidyOpen || model.shareModal /= Nothing) model.studio }


{-| Run a studio edit and forward its effects.
-}
runCore : (Core.Model -> ( Core.Model, Cmd Core.Msg, List Core.Event )) -> Model -> ( Model, Cmd Msg )
runCore edit model =
    if model.viewerRoom /= Nothing then
        ( model, Cmd.none )

    else
        let
            ( nextStudio, studioCmd, events ) =
                edit model.studio

            nextModel =
                { model | studio = nextStudio }
        in
        ( nextModel
        , Cmd.batch [ Cmd.map StudioMsg studioCmd, handleCoreEvents events ]
        )


{-| Act on one studio event: a key the studio did not take becomes a shortcut,
and a double-click opens the context ring on the part under it.
-}
applyEvent : Core.Event -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applyEvent event ( model, cmd ) =
    case event of
        Core.KeyAccepted keyEvent ->
            applyShortcut keyEvent ( model, cmd )

        Core.DoubleClickedAt x y ->
            let
                ( next, openCmd ) =
                    openRingAt x y model
            in
            ( next, Cmd.batch [ cmd, openCmd ] )

        _ ->
            ( model, cmd )


{-| Run the shortcut a key press stands for, after the studio has had its turn.
-}
applyShortcut : ViewportShortcuts.KeyEvent -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applyShortcut keyEvent ( model, cmd ) =
    if model.menuOpen && closesMenu keyEvent then
        let
            ( next, actionCmd ) =
                update CloseMenu model
        in
        ( next, Cmd.batch [ cmd, actionCmd ] )

    else
        case PlayShortcuts.actionFor { glueEditable = model.free, helpVisible = model.helpOpen, menuOpen = model.menuOpen } keyEvent of
            Just PlayShortcuts.ToggleGlue ->
                let
                    ( next, actionCmd ) =
                        update ToggleGlue model
                in
                ( next, Cmd.batch [ cmd, actionCmd ] )

            Just PlayShortcuts.SelectNext ->
                let
                    ( next, actionCmd ) =
                        update (SelectNeighbour False) model
                in
                ( next, Cmd.batch [ cmd, actionCmd ] )

            Just PlayShortcuts.SelectPrevious ->
                let
                    ( next, actionCmd ) =
                        update (SelectNeighbour True) model
                in
                ( next, Cmd.batch [ cmd, actionCmd ] )

            Nothing ->
                case ViewportShortcuts.actionFor (shortcutContext model) keyEvent of
                    Just action ->
                        let
                            ( next, actionCmd ) =
                                update (msgForAction action) model
                        in
                        ( next, Cmd.batch [ cmd, actionCmd ] )

                    Nothing ->
                        ( model, cmd )


{-| Escape closes an open menu first. Asked of the studio's own shortcut table,
so the key is still matched in one place.
-}
closesMenu : ViewportShortcuts.KeyEvent -> Bool
closesMenu keyEvent =
    StudioShortcuts.actionFor
        { hasSelection = False
        , helpVisible = True
        , walking = False
        , inSubmodel = False
        , menuOpen = True
        }
        keyEvent
        == Just StudioShortcuts.CloseMenu


{-| What `Escape` and `?` need to know to pick between their meanings.
-}
shortcutContext : Model -> { helpVisible : Bool, walking : Bool }
shortcutContext model =
    { helpVisible = model.helpOpen
    , walking = model.studio.viewport.camera.navigation == Camera.Walk
    }


{-| Map a viewport shortcut to the message it stands for. Space is
`TogglePlay`, which does nothing outside Technic mode.
-}
msgForAction : ViewportShortcuts.Action -> Msg
msgForAction action =
    case action of
        ViewportShortcuts.TogglePlayback ->
            TogglePlay

        ViewportShortcuts.FitView ->
            FitView

        ViewportShortcuts.ToggleWalk ->
            viewportMsg Viewport.ToggleWalkMode

        ViewportShortcuts.LeaveWalk ->
            viewportMsg Viewport.ToggleWalkMode

        ViewportShortcuts.ToggleHelp ->
            ToggleHelp

        ViewportShortcuts.CloseHelp ->
            ToggleHelp

        ViewportShortcuts.ZoomIn ->
            viewportMsg (Viewport.Wheel -keyboardZoomStep)

        ViewportShortcuts.ZoomOut ->
            viewportMsg (Viewport.Wheel keyboardZoomStep)

        ViewportShortcuts.PresetFront ->
            viewportMsg (Viewport.SetViewPreset FrontView)

        ViewportShortcuts.PresetBack ->
            viewportMsg (Viewport.SetViewPreset BackView)

        ViewportShortcuts.PresetLeft ->
            viewportMsg (Viewport.SetViewPreset LeftView)

        ViewportShortcuts.PresetRight ->
            viewportMsg (Viewport.SetViewPreset RightView)

        ViewportShortcuts.PresetTop ->
            viewportMsg (Viewport.SetViewPreset TopView)

        ViewportShortcuts.PresetBottom ->
            viewportMsg (Viewport.SetViewPreset BottomView)

        ViewportShortcuts.PresetIsometric ->
            viewportMsg (Viewport.SetViewPreset IsometricView)


viewportMsg : Viewport.Msg -> Msg
viewportMsg =
    StudioMsg << Core.ViewportMsg


{-| How far `+` and `-` zoom, in wheel units.
-}
keyboardZoomStep : Float
keyboardZoomStep =
    200.0


handleCoreEvents : List Core.Event -> Cmd Msg
handleCoreEvents events =
    let
        toCmd ev =
            case ev of
                Core.FlattenRequested payload ->
                    Ports.requestGeometryFlatten payload

                Core.TappedAt x y ->
                    Task.perform (\now -> TapTimed x y (Time.posixToMillis now)) Time.now

                _ ->
                    Cmd.none
    in
    Cmd.batch (List.map toCmd events)


{-| Global subscriptions for Brick Play. The machine's clock runs only while it
plays.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StudioMsg (Core.subscriptions model.studio)
        , Ports.geometryFlattened (StudioMsg << Core.geometryFlattened)
        , Ports.geometryFlattenFailed (StudioMsg << Core.geometryFlattenFailed)
        , Browser.Events.onResize WindowResized
        , Ports.thumbnailLoaded ThumbnailLoaded
        , Ports.snapshotReady SnapshotReady
        , Ports.snapshotProgress SnapshotProgress
        , Ports.snapshotFailed SnapshotFailed
        , Ports.testCommand TestCommand
        , Ports.broadcastStarted BroadcastStarted
        , Ports.broadcastStopped (\_ -> BroadcastStopped)
        , Ports.broadcastViewersChanged BroadcastViewersChanged
        , Ports.broadcastError BroadcastError
        , Ports.liveSnapshotReceived LiveSnapshotReceived
        , Ports.liveAnimationReceived LiveAnimationReceived
        , Ports.liveStatusChanged LiveStatusChanged
        , Ports.liveError LiveError
        , Ports.reverseStatusChanged ReverseStatusChanged
        , Ports.reverseParticipantUpdated ReverseParticipantUpdated
        , Ports.reverseParticipantLeft ReverseParticipantLeft
        , if model.playing then
            Browser.Events.onAnimationFrameDelta Tick

          else
            Sub.none
        ]


{-| Render the Brick Play interface.
-}
view : Model -> Html Msg
view model =
    case model.viewerRoom of
        Just roomId ->
            viewChromelessViewer model roomId

        Nothing ->
            case model.reverseHostRoom of
                Just roomId ->
                    case model.inspectedParticipant of
                        Just participantId ->
                            viewInspectedParticipant model participantId

                        Nothing ->
                            viewReverseGrid model roomId

                Nothing ->
                    viewNormalBuilder model


viewNormalBuilder : Model -> Html Msg
viewNormalBuilder model =
    let
        accomplished =
            technicMode model && Machine.isAccomplished model.machine

        gearCount =
            List.length model.machine.gears
    in
    div
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "var(--color-brand)"
        , Attr.style "font-family" "var(--font-sans)"
        ]
        [ -- Top-left toolbar and status bar: the menu, help and play buttons sit at
          -- the top left, and temporary info pills (Bit drag, Tidy up, gear mesh
          -- status, first-select hint, touch part name, and accomplishment banner)
          -- grow horizontally beside them, keeping the vertical space above the
          -- brick palette free.
          header
            [ Attr.style "position" "absolute"
            , Attr.style "top" "0.75rem"
            , Attr.style "left" "0.75rem"
            , Attr.style "right" "0.75rem"
            , Attr.style "display" "flex"
            , Attr.style "flex-wrap" "wrap"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "0.5rem"
            , Attr.style "z-index" "20"
            , Attr.style "pointer-events" "none"
            , keepFocusOnCanvas
            ]
            [ h1 [ Attr.class "sr-only" ] [ text (I18n.appPlaygroundTitle model.language) ]
            , HamburgerMenu.viewWithLanguage model.language (menuConfig model)
            , shareButton model.language
            , helpButton model.language
            , if model.liveBroadcasting then
                livePill model

              else
                text ""
            , case model.reverseParticipantRoom of
                Just roomId ->
                    reverseParticipantBadge model roomId

                Nothing ->
                    text ""
            , if technicMode model then
                playButton model.language model.playing

              else
                text ""
            , bitDragRow model
            , tidyRow model
            , if technicMode model then
                gearRow model

              else
                text ""
            , hintRow model
            , touchLabel model
            , if accomplished then
                accomplishmentBannerIn model.language gearCount

              else
                text ""
            ]

        -- 3D WebGL Canvas
        , Html.map StudioMsg (viewCanvas model)

        -- The context ring beside the selected part, over the canvas
        , ringView model

        -- Camera control: on a wide window the camera globe sits bottom-right,
        -- beside the dock. On a narrow window it shares the bottom row instead.
        , if isNarrow model then
            text ""

          else
            div
                [ Attr.style "position" "absolute"
                , Attr.style "right" "0.75rem"
                , Attr.style "bottom" "0.75rem"
                , Attr.style "z-index" "10"
                ]
                [ globeControl model ]
        , if model.helpOpen then
            -- Above the header and bottom area (z-index 10 and 20), which would
            -- otherwise cover the top and bottom of the dialog.
            div
                [ Attr.style "position" "absolute"
                , Attr.style "top" "0"
                , Attr.style "left" "0"
                , Attr.style "right" "0"
                , Attr.style "bottom" "0"
                , Attr.style "z-index" "30"
                , Attr.style "pointer-events" "none"
                ]
                [ ShortcutHelp.viewWithLanguage model.language { rows = helpRows model, onClose = ToggleHelp } ]

          else
            text ""

        -- Bottom: the brick palette / kit dock (or narrow row with camera toggle)
        , bottomArea model
        , case model.toast of
            Just t ->
                viewToast t.message

            Nothing ->
                text ""
        , case model.snapshotModal of
            Just modal ->
                SnapshotModal.viewWithLanguage model.language
                    { onSave = SnapshotSave
                    , onClose = SnapshotClose
                    }
                    modal

            Nothing ->
                text ""
        , case model.shareModal of
            Just sm ->
                ShareModal.viewWithLanguage model.language
                    { language = model.language
                    , baseUrl = model.baseUrl
                    , free = model.free
                    , document = model.studio.document
                    , inventory = model.inventory
                    , liveBroadcasting = model.liveBroadcasting
                    , liveWatchUrl = model.liveWatchUrl
                    , liveViewerCount = model.liveViewerCount
                    , onToggleBroadcast = ToggleBroadcast
                    , onClose = CloseShareModal
                    , onSelectKind = SelectShareKind
                    , onCopyUrl = CopyShareUrl
                    , onToggleReverseSharing = ToggleReverseSharing
                    , onCopyParticipantUrl = CopyParticipantUrl
                    , onCopyHostUrl = CopyHostUrl
                    }
                    sm

            Nothing ->
                text ""
        , if model.tidyOpen then
            tidyPanel model

          else
            text ""
        ]


viewChromelessViewer : Model -> String -> Html Msg
viewChromelessViewer model roomId =
    div
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "var(--color-brand)"
        , Attr.style "font-family" "var(--font-sans)"
        ]
        [ -- 3D WebGL Canvas
          Html.map StudioMsg (viewCanvas model)

        -- Top-left Live status badge
        , div
            [ Attr.attribute "data-testid" "viewer-live-badge"
            , Attr.style "position" "absolute"
            , Attr.style "top" "1rem"
            , Attr.style "left" "1rem"
            , Attr.style "z-index" "20"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "0.5rem"
            , Attr.style "padding" "6px 14px"
            , Attr.style "border-radius" "9999px"
            , Attr.style "background" "rgba(0, 0, 0, 0.75)"
            , Attr.style "backdrop-filter" "blur(8px)"
            , Attr.style "-webkit-backdrop-filter" "blur(8px)"
            , Attr.style "color" "#ffffff"
            , Attr.style "font-size" "13px"
            , Attr.style "font-weight" "600"
            , Attr.style "pointer-events" "none"
            , Attr.style "box-shadow" "0 4px 12px rgba(0, 0, 0, 0.3)"
            ]
            [ div
                [ Attr.style "width" "8px"
                , Attr.style "height" "8px"
                , Attr.style "border-radius" "50%"
                , Attr.style "background"
                    (if model.viewerStatus == "live" then
                        "#ef4444"

                     else
                        "#eab308"
                    )
                ]
                []
            , text
                (if model.viewerStatus == "live" then
                    "LIVE · " ++ roomId

                 else if model.viewerStatus == "ended" then
                    case model.language of
                        Fi ->
                            "Lähetys päättyi"

                        En ->
                            "Broadcast ended"

                 else
                    case model.language of
                        Fi ->
                            "Yhdistetään…"

                        En ->
                            "Connecting…"
                )
            ]

        -- Camera Globe control for 3D inspection
        , div
            [ Attr.style "position" "absolute"
            , Attr.style "right" "0.75rem"
            , Attr.style "bottom" "0.75rem"
            , Attr.style "z-index" "10"
            ]
            [ globeControl model ]

        -- Toast messages (if any)
        , case model.toast of
            Just t ->
                viewToast t.message

            Nothing ->
                text ""
        ]


viewReverseGrid : Model -> String -> Html Msg
viewReverseGrid model roomId =
    let
        totalCount =
            Dict.size model.participants

        badgeText =
            case model.language of
                Fi ->
                    "SEURANTARUUDUKKO · " ++ roomId

                En ->
                    "GRID STUDIO · " ++ roomId

        activeCountText =
            case model.language of
                Fi ->
                    String.fromInt totalCount
                        ++ (if totalCount == 1 then
                                " osallistuja"

                            else
                                " osallistujaa"
                           )

                En ->
                    String.fromInt totalCount
                        ++ (if totalCount == 1 then
                                " participant"

                            else
                                " participants"
                           )

        copyButtonLabel =
            case model.language of
                Fi ->
                    "Kopioi osallistujan linkki"

                En ->
                    "Copy participant link"

        participantUrl =
            ShareModal.computeReverseParticipantUrl model.baseUrl roomId ShareModal.PartsSet model.free model.studio.document model.inventory
    in
    div
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "overflow-y" "auto"
        , Attr.style "background" "var(--color-bg-page)"
        , Attr.style "color" "var(--color-text-primary)"
        , Attr.style "font-family" "var(--font-sans)"
        , Attr.class "flex flex-col min-h-screen"
        ]
        [ -- Sticky top header
          header
            [ Attr.class "sticky top-0 z-20 flex flex-wrap items-center justify-between gap-4 border-b px-4 py-3 shadow-xs"
            , Attr.style "background" "color-mix(in srgb, var(--color-bg-page) 95%, transparent)"
            , Attr.style "border-color" "var(--color-border-default)"
            , Attr.style "backdrop-filter" "blur(8px)"
            , Attr.style "-webkit-backdrop-filter" "blur(8px)"
            ]
            [ div [ Attr.class "flex items-center gap-3" ]
                [ span
                    [ Attr.class "inline-flex items-center px-3 py-1 rounded-full text-xs font-bold tracking-wider uppercase border"
                    , Attr.style "background" "var(--color-bg-subtle)"
                    , Attr.style "border-color" "var(--color-border-default)"
                    , Attr.style "color" "var(--color-text-primary)"
                    ]
                    [ text badgeText ]
                , span
                    [ Attr.class "text-sm font-medium"
                    , Attr.style "color" "var(--color-text-muted)"
                    ]
                    [ text activeCountText ]
                ]
            , div [ Attr.class "flex items-center gap-2" ]
                [ button
                    [ Attr.type_ "button"
                    , Attr.attribute "data-testid" "grid-copy-link"
                    , onClick CopyParticipantUrl
                    , Attr.class "inline-flex items-center gap-2 px-3.5 py-1.5 rounded-lg text-sm font-medium transition cursor-pointer border shadow-xs"
                    , Attr.style "background" "var(--color-brand-yellow)"
                    , Attr.style "color" "var(--color-text-primary)"
                    , Attr.style "border-color" "var(--color-border-default)"
                    ]
                    [ Icon.feather "copy"
                    , text copyButtonLabel
                    ]
                ]
            ]
        , -- Main body
          div [ Attr.class "flex-1 flex flex-col p-4" ]
            [ if Dict.isEmpty model.participants then
                viewGridWaitingState model participantUrl

              else
                viewGridParticipants model
            ]
        , -- Toast notifications
          case model.toast of
            Just t ->
                viewToast t.message

            Nothing ->
                text ""
        ]


viewGridWaitingState : Model -> String -> Html Msg
viewGridWaitingState model participantUrl =
    let
        title =
            case model.language of
                Fi ->
                    "Odotetaan osallistujia…"

                En ->
                    "Waiting for participants to open the link…"

        subtitle =
            case model.language of
                Fi ->
                    "Jaa alla oleva linkki osallistujille. Heidän rakennelmansa ilmestyvät tähän ruudukkoon reaaliajassa."

                En ->
                    "Share the link below with participants. Their builds will appear in this grid in real time."

        copyBtnText =
            case model.language of
                Fi ->
                    "Kopioi linkki"

                En ->
                    "Copy link"
    in
    div
        [ Attr.attribute "data-testid" "grid-waiting-state"
        , Attr.class "max-w-lg mx-auto my-auto w-full p-8 rounded-2xl border text-center shadow-md flex flex-col items-center gap-4"
        , Attr.style "background" "var(--color-bg-page)"
        , Attr.style "border-color" "var(--color-border-default)"
        ]
        [ div
            [ Attr.class "w-16 h-16 rounded-full flex items-center justify-center mb-2"
            , Attr.style "background" "var(--color-bg-subtle)"
            , Attr.style "color" "var(--color-text-muted)"
            ]
            [ FeatherIcons.users
                |> FeatherIcons.withSize 32
                |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
            ]
        , h2
            [ Attr.class "text-xl font-bold"
            , Attr.style "color" "var(--color-text-primary)"
            ]
            [ text title ]
        , p
            [ Attr.class "text-sm max-w-sm"
            , Attr.style "color" "var(--color-text-muted)"
            ]
            [ text subtitle ]
        , div [ Attr.class "w-full flex items-center gap-2 mt-2" ]
            [ input
                [ Attr.type_ "text"
                , Attr.readonly True
                , Attr.value participantUrl
                , Attr.class "flex-1 px-3 py-2 text-xs rounded-lg border font-mono select-all truncate"
                , Attr.style "background" "var(--color-bg-subtle)"
                , Attr.style "border-color" "var(--color-border-default)"
                , Attr.style "color" "var(--color-text-primary)"
                ]
                []
            , button
                [ Attr.type_ "button"
                , Attr.attribute "data-testid" "grid-waiting-copy-btn"
                , onClick CopyParticipantUrl
                , Attr.class "px-4 py-2 text-xs font-semibold rounded-lg shrink-0 border transition cursor-pointer shadow-xs"
                , Attr.style "background" "var(--color-brand-yellow)"
                , Attr.style "color" "var(--color-text-primary)"
                , Attr.style "border-color" "var(--color-border-default)"
                ]
                [ text copyBtnText ]
            ]
        ]


viewGridParticipants : Model -> Html Msg
viewGridParticipants model =
    let
        sortedParticipants =
            Dict.values model.participants
                |> List.sortBy .name
    in
    div
        [ Attr.class "grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4 p-4" ]
        (List.map (viewParticipantCard model) sortedParticipants)


viewParticipantCard : Model -> Participant -> Html Msg
viewParticipantCard model p =
    let
        isConnected =
            p.status == "connected"

        brickCountLabel =
            case model.language of
                Fi ->
                    String.fromInt p.brickCount
                        ++ (if p.brickCount == 1 then
                                " palikka"

                            else
                                " palikkaa"
                           )

                En ->
                    String.fromInt p.brickCount
                        ++ (if p.brickCount == 1 then
                                " brick"

                            else
                                " bricks"
                           )

        inspectLabel =
            case model.language of
                Fi ->
                    "Tarkastele 3D"

                En ->
                    "Inspect in 3D"

        statusLabel =
            case model.language of
                Fi ->
                    if isConnected then
                        "Yhdistetty"

                    else
                        "Yhteys katkennut"

                En ->
                    if isConnected then
                        "Connected"

                    else
                        "Disconnected"
    in
    div
        [ Attr.attribute "data-testid" "participant-card"
        , Attr.class "flex flex-col rounded-xl border overflow-hidden shadow-xs transition hover:shadow-md"
        , Attr.style "background" "var(--color-bg-page)"
        , Attr.style "border-color" "var(--color-border-default)"
        ]
        [ -- 2D Thumbnail or fallback
          div
            [ Attr.class "w-full aspect-[4/3] flex items-center justify-center relative overflow-hidden"
            , Attr.style "background" "var(--color-bg-subtle)"
            ]
            [ if String.isEmpty p.thumbnail then
                div
                    [ Attr.class "flex flex-col items-center justify-center gap-1.5"
                    , Attr.style "color" "var(--color-text-muted)"
                    ]
                    [ Icon.featherSized 28 "box"
                    , span [ Attr.class "text-xs" ]
                        [ text
                            (case model.language of
                                Fi ->
                                    "Ei esikatselua"

                                En ->
                                    "No preview"
                            )
                        ]
                    ]

              else
                Html.img
                    [ Attr.src p.thumbnail
                    , Attr.alt p.name
                    , Attr.class "w-full h-full object-contain"
                    ]
                    []
            ]
        , -- Info and inspect action
          div [ Attr.class "p-3.5 flex flex-col gap-2.5 flex-1 justify-between" ]
            [ div [ Attr.class "flex items-start justify-between gap-2" ]
                [ div [ Attr.class "flex flex-col min-w-0" ]
                    [ span
                        [ Attr.class "font-bold text-sm truncate"
                        , Attr.style "color" "var(--color-text-primary)"
                        ]
                        [ text p.name ]
                    , span
                        [ Attr.class "text-xs"
                        , Attr.style "color" "var(--color-text-muted)"
                        ]
                        [ text brickCountLabel ]
                    ]
                , div
                    [ Attr.class "flex items-center gap-1 shrink-0 pt-0.5"
                    , Attr.title statusLabel
                    ]
                    [ span
                        [ Attr.class
                            (if isConnected then
                                "bg-emerald-500"

                             else
                                "bg-amber-400"
                            )
                        , Attr.style "width" "8px"
                        , Attr.style "height" "8px"
                        , Attr.style "border-radius" "50%"
                        , Attr.style "display" "inline-block"
                        ]
                        []
                    , span [ Attr.class "sr-only" ] [ text statusLabel ]
                    ]
                ]
            , button
                [ Attr.type_ "button"
                , Attr.attribute "data-testid" "inspect-participant-btn"
                , onClick (InspectParticipant (Just p.id))
                , Attr.class "w-full py-1.5 px-3 rounded-lg text-xs font-semibold border transition cursor-pointer flex items-center justify-center gap-1.5"
                , Attr.style "background" "var(--color-bg-subtle)"
                , Attr.style "color" "var(--color-text-primary)"
                , Attr.style "border-color" "var(--color-border-default)"
                ]
                [ FeatherIcons.eye
                    |> FeatherIcons.withSize 14
                    |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                , text inspectLabel
                ]
            ]
        ]


viewInspectedParticipant : Model -> String -> Html Msg
viewInspectedParticipant model participantId =
    let
        maybeP =
            Dict.get participantId model.participants

        participantName =
            Maybe.map .name maybeP
                |> Maybe.withDefault ("Rakentaja " ++ String.left 4 participantId)

        brickCount =
            Maybe.map .brickCount maybeP
                |> Maybe.withDefault 0

        isConnected =
            Maybe.map (\p -> p.status == "connected") maybeP
                |> Maybe.withDefault False

        backLabel =
            case model.language of
                Fi ->
                    "Takaisin ruudukkoon"

                En ->
                    "Back to grid"

        brickCountText =
            case model.language of
                Fi ->
                    String.fromInt brickCount
                        ++ (if brickCount == 1 then
                                " palikka"

                            else
                                " palikkaa"
                           )

                En ->
                    String.fromInt brickCount
                        ++ (if brickCount == 1 then
                                " brick"

                            else
                                " bricks"
                           )

        statusText =
            case model.language of
                Fi ->
                    if isConnected then
                        "Yhdistetty"

                    else
                        "Yhteys katkennut"

                En ->
                    if isConnected then
                        "Connected"

                    else
                        "Disconnected"
    in
    div
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "var(--color-brand)"
        , Attr.style "font-family" "var(--font-sans)"
        ]
        [ -- 3D WebGL Canvas
          Html.map StudioMsg (viewCanvas model)

        -- Top Bar
        , header
            [ Attr.style "position" "absolute"
            , Attr.style "top" "0.75rem"
            , Attr.style "left" "0.75rem"
            , Attr.style "right" "0.75rem"
            , Attr.style "display" "flex"
            , Attr.style "flex-wrap" "wrap"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "0.5rem"
            , Attr.style "z-index" "20"
            , Attr.style "pointer-events" "none"
            ]
            [ -- Back to grid button
              button
                [ Attr.type_ "button"
                , Attr.attribute "data-testid" "close-inspector"
                , onClick (InspectParticipant Nothing)
                , Attr.class "inline-flex items-center gap-1.5 px-3 py-1.5 rounded-full text-xs font-semibold border shadow-md transition cursor-pointer"
                , Attr.style "background" "color-mix(in srgb, var(--color-bg-page) 92%, transparent)"
                , Attr.style "border-color" "var(--color-border-default)"
                , Attr.style "color" "var(--color-text-primary)"
                , Attr.style "backdrop-filter" "blur(8px)"
                , Attr.style "-webkit-backdrop-filter" "blur(8px)"
                , Attr.style "pointer-events" "auto"
                ]
                [ FeatherIcons.arrowLeft
                    |> FeatherIcons.withSize 14
                    |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
                , text backLabel
                ]

            -- Participant Info Pill
            , div
                [ Attr.attribute "data-testid" "inspector-participant-pill"
                , Attr.class "inline-flex items-center gap-2 px-3 py-1.5 rounded-full text-xs font-semibold border shadow-md"
                , Attr.style "background" "color-mix(in srgb, var(--color-bg-page) 92%, transparent)"
                , Attr.style "border-color" "var(--color-border-default)"
                , Attr.style "color" "var(--color-text-primary)"
                , Attr.style "backdrop-filter" "blur(8px)"
                , Attr.style "-webkit-backdrop-filter" "blur(8px)"
                , Attr.style "pointer-events" "auto"
                ]
                [ span [ Attr.class "font-bold text-sm" ] [ text participantName ]
                , span
                    [ Attr.class "text-xs"
                    , Attr.style "color" "var(--color-text-muted)"
                    ]
                    [ text ("(" ++ brickCountText ++ ")") ]
                , span
                    [ Attr.class
                        (if isConnected then
                            "bg-emerald-500"

                         else
                            "bg-amber-400"
                        )
                    , Attr.style "width" "8px"
                    , Attr.style "height" "8px"
                    , Attr.style "border-radius" "50%"
                    , Attr.style "display" "inline-block"
                    ]
                    []
                , span
                    [ Attr.class "text-xs"
                    , Attr.style "color" "var(--color-text-muted)"
                    ]
                    [ text statusText ]
                ]
            ]

        -- Camera Globe control for 3D orbiting/inspection
        , div
            [ Attr.style "position" "absolute"
            , Attr.style "right" "0.75rem"
            , Attr.style "bottom" "0.75rem"
            , Attr.style "z-index" "10"
            ]
            [ globeControl model ]

        -- Toast messages (if any)
        , case model.toast of
            Just t ->
                viewToast t.message

            Nothing ->
                text ""
        ]


livePill : Model -> Html Msg
livePill model =
    button
        [ Attr.type_ "button"
        , Attr.attribute "data-testid" "header-live-pill"
        , Attr.attribute "aria-label" "Live Crafting Broadcast"
        , Html.Events.onClick OpenShareModal
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "4px 10px"
        , Attr.style "border-radius" "9999px"
        , Attr.style "background" "rgba(239, 68, 68, 0.95)"
        , Attr.style "color" "#ffffff"
        , Attr.style "font-size" "12px"
        , Attr.style "font-weight" "700"
        , Attr.style "letter-spacing" "0.02em"
        , Attr.style "border" "none"
        , Attr.style "cursor" "pointer"
        , Attr.style "box-shadow" "0 2px 8px rgba(239, 68, 68, 0.4)"
        , Attr.style "pointer-events" "auto"
        ]
        [ div
            [ Attr.style "width" "6px"
            , Attr.style "height" "6px"
            , Attr.style "border-radius" "50%"
            , Attr.style "background" "#ffffff"
            ]
            []
        , text
            ("LIVE · "
                ++ String.fromInt model.liveViewerCount
                ++ (if model.language == Fi then
                        if model.liveViewerCount == 1 then
                            " katsoja"

                        else
                            " katsojaa"

                    else if model.liveViewerCount == 1 then
                        " viewer"

                    else
                        " viewers"
                   )
            )
        ]


reverseParticipantBadge : Model -> String -> Html Msg
reverseParticipantBadge model roomId =
    let
        isLive =
            model.reverseStatus == "live"

        statusText =
            case model.language of
                Fi ->
                    if isLive then
                        "Yhdistetty istuntoon · " ++ roomId

                    else
                        "Yhdistetään istuntoon… · " ++ roomId

                En ->
                    if isLive then
                        "Linked to session · " ++ roomId

                    else
                        "Connecting to session… · " ++ roomId

        placedCount =
            List.length (Core.getPlacements model.studio)

        unitText =
            case model.language of
                Fi ->
                    if placedCount == 1 then
                        "palikka"

                    else
                        "palikkaa"

                En ->
                    if placedCount == 1 then
                        "brick"

                    else
                        "bricks"

        nameText =
            participantDisplayName model.language model.participantName
                ++ " ("
                ++ String.fromInt placedCount
                ++ " "
                ++ unitText
                ++ ")"
    in
    div
        [ Attr.attribute "data-testid" "reverse-participant-badge"
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "4px 10px"
        , Attr.style "border-radius" "9999px"
        , Attr.style "background" "color-mix(in srgb, var(--color-brand) 85%, transparent)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "font-size" "12px"
        , Attr.style "font-weight" "600"
        , Attr.style "letter-spacing" "0.02em"
        , Attr.style "border" "1px solid color-mix(in srgb, var(--color-text-on-dark) 20%, transparent)"
        , Attr.style "pointer-events" "auto"
        ]
        [ div
            [ Attr.class
                (if isLive then
                    "w-2 h-2 rounded-full bg-emerald-500 inline-block"

                 else
                    "w-2 h-2 rounded-full bg-amber-400 inline-block"
                )
            ]
            []
        , span [] [ text statusText ]
        , span [ Attr.style "opacity" "0.5" ] [ text "·" ]
        , span [] [ text nameText ]
        ]


{-| The Tidy up pill: shown only when the build has problems and no preview is
open. It sits in the top-left toolbar row.
-}
tidyRow : Model -> Html Msg
tidyRow model =
    if model.tidyOpen || List.isEmpty model.tidy then
        text ""

    else
        div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "max-width" "100%"
            , Attr.style "min-width" "0"
            , Attr.style "overflow" "hidden"
            , Attr.style "white-space" "nowrap"
            ]
            [ tidyPill model.language model.tidy ]


tidyPill : Language -> List Health.Fix -> Html Msg
tidyPill lang fixes =
    button
        [ onClick OpenTidy
        , Attr.attribute "data-tidy-pill" ""
        , Attr.title (I18n.tidyPreviewTip lang)
        , Attr.attribute "aria-haspopup" "dialog"
        , Attr.style "pointer-events" "auto"
        , Attr.style "align-self" "center"
        , Attr.style "max-width" "100%"
        , Attr.style "padding" "0.3125rem 0.875rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "backdrop-filter" "blur(8px)"
        , Attr.style "border-radius" "9999px"
        , Attr.style "border" "1px solid var(--color-brand-yellow)"
        , Attr.style "color" "var(--color-brand-yellow)"
        , Attr.style "font-family" "var(--font-sans)"
        , Attr.style "font-size" "0.8125rem"
        , Attr.style "font-weight" "600"
        , Attr.style "white-space" "nowrap"
        , Attr.style "overflow" "hidden"
        , Attr.style "text-overflow" "ellipsis"
        , Attr.style "cursor" "pointer"
        ]
        [ text (tidyLabelIn lang fixes) ]


{-| The pill's words: "1 loose brick · Tidy up", "1 overlapping brick · Tidy up", "3 loose bricks · Tidy up".
-}
tidyLabel : List Health.Fix -> String
tidyLabel fixes =
    tidyLabelIn En fixes


tidyLabelIn : Language -> List Health.Fix -> String
tidyLabelIn lang fixes =
    let
        count =
            List.length fixes

        hasSnap =
            List.any (\f -> f.reason == Health.SnapToStud) fixes

        hasOverlap =
            List.any
                (\f ->
                    case f.reason of
                        Health.LiftOut _ ->
                            True

                        _ ->
                            False
                )
                fixes
    in
    I18n.tidyPillLabel lang { count = count, hasOverlap = hasOverlap, hasSnap = hasSnap }


{-| The Tidy up preview: an accessible dialog listing the moves, with Apply and
Cancel. The moved parts are outlined and their ghosts drawn in the studio.
-}
tidyPanel : Model -> Html Msg
tidyPanel model =
    let
        count =
            List.length model.tidy

        summary =
            I18n.tidySummary model.language count
    in
    div
        [ Attr.attribute "role" "dialog"
        , Attr.attribute "aria-modal" "true"
        , Attr.attribute "aria-labelledby" "tidy-title"
        , Attr.attribute "aria-describedby" "tidy-summary"
        , Attr.attribute "data-tidy-preview" ""
        , Attr.style "position" "absolute"
        , Attr.style "top" "4.5rem"
        , Attr.style "left" "1rem"
        , Attr.style "width" "min(22rem, calc(100% - 2rem))"
        , Attr.style "max-height" "calc(100% - 9rem)"
        , Attr.style "overflow-y" "auto"
        , Attr.style "padding" "1rem 1.125rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.95)"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
        , Attr.style "border-radius" "0.75rem"
        , Attr.style "box-shadow" "0 4px 16px rgba(0, 0, 0, 0.5)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "font-family" "var(--font-sans)"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "z-index" "25"
        , Attr.style "pointer-events" "auto"
        ]
        [ h2
            [ Attr.id "tidy-title"
            , Attr.style "margin" "0 0 0.375rem"
            , Attr.style "font-size" "1rem"
            , Attr.style "font-weight" "700"
            ]
            [ text (I18n.tidyTitle model.language) ]
        , p
            [ Attr.id "tidy-summary"
            , Attr.style "margin" "0 0 0.625rem"
            ]
            [ text summary ]
        , ul
            [ Attr.attribute "aria-label" (I18n.tidyPlannedMoves model.language)
            , Attr.style "margin" "0 0 0.875rem"
            , Attr.style "padding-left" "1.125rem"
            ]
            (List.map
                (\fix -> li [ Attr.style "margin" "0.125rem 0" ] [ text (Health.describeIn model.language (tidyPartTitle model) model.studio.document fix) ])
                model.tidy
            )
        , div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "flex-end"
            , Attr.style "gap" "0.5rem"
            ]
            [ tidyButton False CancelTidy (I18n.tidyCancel model.language)
            , tidyButton True ApplyTidy (I18n.tidyApply model.language)
            ]
        ]


tidyButton : Bool -> Msg -> String -> Html Msg
tidyButton primary msg label =
    button
        [ onClick msg
        , Attr.style "padding" "0.375rem 0.875rem"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "700"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background"
            (if primary then
                "var(--color-brand-yellow)"

             else
                "rgba(255, 255, 255, 0.15)"
            )
        , Attr.style "color"
            (if primary then
                "var(--color-brand)"

             else
                "var(--color-text-on-dark)"
            )
        , Attr.style "cursor" "pointer"
        , Attr.class "touch-target"
        ]
        [ text label ]


{-| A part's readable name for Tidy up's sentences: its curated Free catalog
description ("2 x 4 brick") when it has one, else the .dat title the thumbnail
pipeline sent, lower-cased so it reads inside a sentence, else its part number.
-}
tidyPartTitle : Model -> String -> Int -> String
tidyPartTitle model file color =
    case
        List.concatMap FreeCatalog.entriesFor FreeCatalog.tabs
            |> List.filter (\entry -> String.toLower entry.file == String.toLower file)
            |> List.head
    of
        Just entry ->
            entry.description

        Nothing ->
            case Dict.get (PartCard.thumbnailKey file color) model.thumbnails of
                Just thumb ->
                    if String.isEmpty (String.trim thumb.title) then
                        case model.language of
                            Fi ->
                                "osa " ++ partNumber file

                            En ->
                                "part " ++ partNumber file

                    else
                        String.toLower thumb.title

                Nothing ->
                    case model.language of
                        Fi ->
                            "osa " ++ partNumber file

                        En ->
                            "part " ++ partNumber file


{-| Asks the thumbnail pipeline for the parts a Tidy up move names, so that the
move list can use their .dat titles. A part that already has a thumbnail is
skipped, and the pipeline's title arrives as `ThumbnailLoaded`.
-}
requestTidyTitles : Model -> Cmd Msg
requestTidyTitles model =
    let
        moved =
            List.filterMap
                (\fix ->
                    Document.getPlacement fix.placement model.studio.document
                        |> Maybe.map (\placed -> ( placed.part, placed.color ))
                )
                model.tidy
    in
    case thumbnailRequests moved model.thumbnails of
        [] ->
            Cmd.none

        requests ->
            Ports.requestThumbnails requests


{-| Toast notification overlay.
-}
viewToast : String -> Html Msg
viewToast message =
    div
        [ Attr.attribute "role" "status"
        , Attr.attribute "aria-live" "polite"
        , Attr.style "position" "fixed"
        , Attr.style "top" "1rem"
        , Attr.style "left" "50%"
        , Attr.style "transform" "translateX(-50%)"
        , Attr.style "background" "rgba(5, 19, 29, 0.95)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "padding" "0.625rem 1.125rem"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
        , Attr.style "box-shadow" "0 4px 16px rgba(0, 0, 0, 0.5)"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "500"
        , Attr.style "z-index" "60"
        , Attr.style "pointer-events" "none"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.5rem"
        ]
        [ Icon.featherSized 16 "check"
        , text message
        ]


{-| Below this window width (CSS px, a portrait phone) the bottom area shows
either the dock or the camera globe, never both.
-}
narrowWidth : Int
narrowWidth =
    600


isNarrow : Model -> Bool
isNarrow model =
    model.windowWidth < narrowWidth


{-| The bottom area: the dock on a wide window, or `narrowRow` (the dock, mobile
context toolbar or camera globe, plus the camera toggle) on a narrow window.
All temporary status pills sit in the top-left toolbar, keeping the space above
the dock free.
-}
bottomArea : Model -> Html Msg
bottomArea model =
    div
        ([ Attr.style "position" "absolute"
         , Attr.style "bottom" "0.75rem"
         , Attr.style "display" "flex"
         , Attr.style "flex-direction" "column"
         , Attr.style "align-items" "center"
         , Attr.style "z-index" "10"
         , Attr.style "pointer-events" "none"
         ]
            ++ dockPlacement model.windowWidth
        )
        [ if isNarrow model then
            narrowRow model

          else
            viewDock model
        ]


{-| The Bit-drag toggle pill shown in the top-left toolbar whenever a movable
brick is selected or being dragged: it switches body drags to 1-LDU movement
without magnetic snapping. Shift+drag separately prefers the nearest snap.
-}
bitDragRow : Model -> Html Msg
bitDragRow model =
    if not (Core.selectionMovable model.studio || Core.isDragging model.studio) then
        text ""

    else
        let
            active =
                Core.fineMove model.studio
        in
        div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "max-width" "100%"
            ]
            [ button
                [ onClick ToggleBitDrag
                , preventDefaultOn "touchstart" (Decode.succeed ( ToggleBitDrag, True ))
                , keepFocusOnCanvas
                , Attr.attribute "data-bit-drag" ""
                , Attr.attribute "aria-label" (I18n.ringBitStepTip model.language)
                , Attr.attribute "aria-pressed"
                    (if active then
                        "true"

                     else
                        "false"
                    )
                , Attr.title (I18n.ringBitStepTip model.language)
                , Attr.class "touch-target"
                , Attr.style "pointer-events" "auto"
                , Attr.style "display" "inline-flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.375rem"
                , Attr.style "height" "2.25rem"
                , Attr.style "padding" "0 0.75rem"
                , Attr.style "border-radius" "9999px"
                , Attr.style "border"
                    (if active then
                        "1px solid var(--color-brand-yellow)"

                     else
                        "1px solid rgba(255, 255, 255, 0.25)"
                    )
                , Attr.style "background"
                    (if active then
                        "var(--color-brand-yellow)"

                     else
                        "rgba(5, 19, 29, 0.75)"
                    )
                , Attr.style "color"
                    (if active then
                        "var(--color-brand)"

                     else
                        "var(--color-text-on-dark)"
                    )
                , Attr.style "backdrop-filter" "blur(8px)"
                , Attr.style "font-family" "var(--font-sans)"
                , Attr.style "font-size" "0.8125rem"
                , Attr.style "font-weight" "700"
                , Attr.style "cursor" "pointer"
                ]
                [ Icon.featherSized 16 "crosshair"
                , span [] [ text (I18n.ringBitStep model.language) ]
                ]
            ]


{-| The narrow bottom row: the dock (or the mobile context toolbar while a part's
ring is open, or the camera globe) and the toggle between them, at a finger-sized
width. In a fixed model (no dock), shows the mobile context toolbar or the camera
globe directly without the toggle.
-}
narrowRow : Model -> Html Msg
narrowRow model =
    if not model.free && List.isEmpty model.inventory then
        div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "flex-end"
            , Attr.style "justify-content" "flex-end"
            , Attr.style "width" "100%"
            , Attr.style "min-width" "0"
            , Attr.style "pointer-events" "auto"
            ]
            [ case model.ringPart of
                Just id ->
                    Ring.mobileView (ringConfig model id)

                Nothing ->
                    globeControl model
            ]

    else
        div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "flex-end"
            , Attr.style "gap" "0.5rem"
            , Attr.style "width" "100%"
            , Attr.style "min-width" "0"
            ]
            [ div
                [ Attr.style "display" "flex"
                , Attr.style "justify-content" "center"
                , Attr.style "flex" "1 1 auto"
                , Attr.style "min-width" "0"
                , Attr.style "overflow-x" "auto"
                , Attr.style "overflow-y" "hidden"
                , Attr.style "scrollbar-width" "none"
                , Attr.style "-ms-overflow-style" "none"
                , Attr.class "scroll-none"
                , Attr.style "pointer-events" "auto"
                ]
                [ if model.cameraShown then
                    globeControl model

                  else
                    case model.ringPart of
                        Just id ->
                            Ring.mobileView (ringConfig model id)

                        Nothing ->
                            viewDock model
                ]
            , cameraToggle model
            ]


{-| The toggle between the dock and the camera globe on a narrow window. Its icon
shows what is on screen now; `aria-pressed` is on while the camera is shown.
-}
cameraToggle : Model -> Html Msg
cameraToggle model =
    let
        icon =
            if model.cameraShown then
                FeatherIcons.camera

            else
                FeatherIcons.box
    in
    button
        [ onClick ToggleCameraPanel
        , Attr.attribute "aria-label" (I18n.cameraControls model.language)
        , Attr.attribute "aria-pressed"
            (if model.cameraShown then
                "true"

             else
                "false"
            )
        , Attr.title (I18n.cameraControls model.language)
        , Attr.class "touch-target"
        , Attr.style "display" "flex"
        , Attr.style "flex-shrink" "0"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "2.75rem"
        , Attr.style "height" "2.75rem"
        , Attr.style "padding" "0"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "0.75rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.9)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "cursor" "pointer"
        , Attr.style "pointer-events" "auto"
        , keepFocusOnCanvas
        ]
        [ icon
            |> FeatherIcons.withSize 20
            |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
        ]


{-| The shared camera globe, with the zoom slider, the reset button and the
realistic-rendering and snapshot buttons.
-}
globeControl : Model -> Html Msg
globeControl model =
    let
        extras =
            if model.viewerRoom /= Nothing then
                [ Button.icon
                    { icon = Icon.feather "sun"
                    , label = I18n.cameraRealistic model.language
                    , shortcut = Nothing
                    , pressed = Just (Core.renderQuality model.studio == Style.Realistic)
                    , enabled = True
                    , showLabel = False
                    }
                    ToggleRealistic
                ]

            else
                [ Button.icon
                    { icon = Icon.feather "sun"
                    , label = I18n.cameraRealistic model.language
                    , shortcut = Nothing
                    , pressed = Just (Core.renderQuality model.studio == Style.Realistic)
                    , enabled = True
                    , showLabel = False
                    }
                    ToggleRealistic
                , Button.icon
                    { icon = Icon.feather "camera"
                    , label = I18n.cameraTakeSnapshot model.language
                    , shortcut = Nothing
                    , pressed = Nothing
                    , enabled = True
                    , showLabel = False
                    }
                    CaptureSnapshot
                ]
    in
    CameraGlobe.viewWithLight (Core.lightDirection model.studio)
        { camera = model.studio.viewport.camera
        , onPress = GlobePressed
        , onMove = GlobeMoved
        , onRelease = GlobeReleased
        , onZoom = ZoomTo
        , onZoomDone = ZoomDone
        , onReset = ResetView
        , extras =
            [ div
                [ keepFocusOnCanvas
                , Attr.style "display" "contents"
                ]
                extras
            ]
        }


{-| On touch screens, where tooltips never show: the name of the dock part last
tapped, in the top-left toolbar. Hidden while the camera globe or mobile context
menu takes the bottom row.
-}
touchLabel : Model -> Html Msg
touchLabel model =
    case model.lastTapped of
        Just name ->
            if isNarrow model && (model.cameraShown || model.ringPart /= Nothing) then
                text ""

            else
                div
                    [ Attr.class "touch-only"
                    , Attr.style "max-width" "100%"
                    , Attr.style "overflow" "hidden"
                    , Attr.style "text-overflow" "ellipsis"
                    , Attr.style "white-space" "nowrap"
                    , Attr.style "padding" "0.125rem 0.75rem"
                    , Attr.style "border-radius" "9999px"
                    , Attr.style "background" "rgba(5, 19, 29, 0.85)"
                    , Attr.style "color" "var(--color-text-on-dark)"
                    , Attr.style "font-size" "0.8125rem"
                    , Attr.style "font-weight" "600"
                    , Attr.style "pointer-events" "auto"
                    ]
                    [ Keyed.node "span" [] [ ( name, span [] [ text name ] ) ] ]

        Nothing ->
            text ""


{-| The width of both docks (the free dock and the kit). Fixed, so the cards in
the dock, or the search results, never widen it: a dock sized to its content
would grow under the camera globe. A narrower window clips it to its own width.
-}
dockWidth : String
dockWidth =
    "40rem"


{-| Below this window width (CSS px), centering the palette dock causes its
right edge to overlap the camera globe in the bottom-right corner. The dock
pins to the left instead, reserving clear space for the camera controls.
-}
dockOverlapWidth : Int
dockOverlapWidth =
    1100


{-| Where the bottom area sits. Centred when there is room, leaving the globe its
corner on the right; on a narrow window it spans the width.
-}
dockPlacement : Int -> List (Html.Attribute Msg)
dockPlacement windowWidth =
    if windowWidth < narrowWidth then
        [ Attr.style "left" "0.75rem"
        , Attr.style "right" "0.75rem"
        ]

    else if windowWidth < dockOverlapWidth then
        [ Attr.style "left" "0.75rem"
        , Attr.style "max-width" "calc(100vw - 16rem)"
        ]

    else
        [ Attr.style "left" "50%"
        , Attr.style "transform" "translateX(-50%)"
        , Attr.style "max-width" "calc(100vw - 24rem)"
        ]


{-| The gear mesh status pill, keyed by its text so a changed status replaces it,
and truncated rather than overflowing a narrow screen.
-}
gearRow : Model -> Html Msg
gearRow model =
    case GearFeedback.gearFeedback (Set.toList model.studio.selection) model.studio.document of
        Just feedback ->
            let
                label =
                    GearFeedback.feedbackTextIn model.language feedback
            in
            div
                [ Attr.style "display" "flex"
                , Attr.style "justify-content" "center"
                , Attr.style "max-width" "100%"
                , Attr.style "min-width" "0"
                , Attr.style "overflow" "hidden"
                , Attr.style "white-space" "nowrap"
                ]
                [ Keyed.node "span" [ Attr.style "display" "contents" ] [ ( label, GearFeedback.pillIn model.language feedback ) ] ]

        Nothing ->
            text ""


{-| The top-left menu: download .io, undo, redo, delete and clear. Each item
does what its keyboard shortcut does.
-}
menuConfig : Model -> HamburgerMenu.Config Msg
menuConfig model =
    { id = menuId
    , open = model.menuOpen
    , onToggle = ToggleMenu
    , onClose = CloseMenu
    , languageToggle =
        Just
            { current = model.language
            , onSelect = SetLanguage
            }
    , items =
        [ { label = I18n.menuDownloadIo model.language, shortcut = "", enabled = not (List.isEmpty (Core.getPlacements model.studio)), onSelect = MenuPicked DownloadIo }
        , { label = I18n.menuUndo model.language, shortcut = "Ctrl+Z", enabled = Core.canUndo model.studio, onSelect = MenuPicked Undo }
        , { label = I18n.menuRedo model.language, shortcut = "Ctrl+Shift+Z", enabled = Core.canRedo model.studio, onSelect = MenuPicked Redo }
        , { label = I18n.menuDelete model.language, shortcut = "Del", enabled = Core.hasSelection model.studio, onSelect = MenuPicked Delete }
        , { label = I18n.menuClear model.language, shortcut = "", enabled = clearable model, onSelect = MenuPicked Clear }
        ]
    }


{-| The visible share button: it opens the share modal, situated between the
hamburger menu and the help button.
-}
shareButton : Language -> Html Msg
shareButton lang =
    button
        [ onClick OpenShareModal
        , Attr.attribute "aria-label" (I18n.menuShare lang)
        , Attr.title (I18n.menuShare lang)
        , Attr.style "display" "flex"
        , Attr.style "flex-shrink" "0"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "2.25rem"
        , Attr.style "height" "2.25rem"
        , Attr.style "padding" "0"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "cursor" "pointer"
        , Attr.style "pointer-events" "auto"
        , Attr.style "transition" "all 150ms ease"
        , Attr.class "touch-target"
        ]
        [ Icon.featherSized 18 "share-2" ]


{-| Whether Clear has anything to remove: any part in free building, else any
unglued part (a puzzle's glued parts stay).
-}
clearable : Model -> Bool
clearable model =
    if model.free then
        not (List.isEmpty (Core.getPlacements model.studio))

    else
        List.any (\p -> not (Core.isGlued p.id model.studio)) (Core.getPlacements model.studio)


{-| The visible shortcut-reference button: it toggles the help overlay, as the
`?` key does.
-}
helpButton : Language -> Html Msg
helpButton lang =
    button
        [ onClick ToggleHelp
        , Attr.attribute "aria-label" (I18n.headerShortcuts lang)
        , Attr.title (I18n.headerShortcuts lang)
        , Attr.style "display" "flex"
        , Attr.style "flex-shrink" "0"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "2.25rem"
        , Attr.style "height" "2.25rem"
        , Attr.style "padding" "0"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "cursor" "pointer"
        , Attr.style "pointer-events" "auto"
        , Attr.style "transition" "all 150ms ease"
        , Attr.class "touch-target"
        ]
        [ Icon.featherSized 18 "help-circle" ]


{-| The DOM id of the menu panel, for `aria-controls`.
-}
menuId : String
menuId =
    "play-menu"


{-| The accomplishment banner: a polite live status line, in the green of the
LDraw palette's Green (the theme has no green token). It is one line, the height
of the top buttons, and ellipsizes on a narrow window.
-}
accomplishmentBanner : Int -> Html Msg
accomplishmentBanner =
    accomplishmentBannerIn En


accomplishmentBannerIn : Language -> Int -> Html Msg
accomplishmentBannerIn lang gearCount =
    let
        green =
            Colors.resolveColor 2 16

        channel value =
            String.fromInt (round (value * 255))
    in
    div
        [ Attr.attribute "role" "status"
        , Attr.attribute "aria-live" "polite"
        , Attr.style "height" "2.25rem"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "min-width" "0"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "padding" "0 1.25rem"
        , Attr.style "background" ("rgba(" ++ channel green.r ++ ", " ++ channel green.g ++ ", " ++ channel green.b ++ ", 0.95)")
        , Attr.style "border" "2px solid var(--color-brand-yellow)"
        , Attr.style "border-radius" "9999px"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "700"
        , Attr.style "white-space" "nowrap"
        , Attr.style "overflow" "hidden"
        , Attr.style "text-overflow" "ellipsis"
        , Attr.style "box-shadow" "0 10px 25px rgba(0, 0, 0, 0.6)"
        ]
        [ -- Keyed: a text change replaces the element, so a browser that has
          -- translated the old text cannot leave stale copies behind.
          let
            label =
                I18n.gearAllRotating lang gearCount
          in
          Keyed.node "span" [] [ ( label, span [] [ text label ] ) ]
        ]


{-| The studio canvas. While the machine is animated the turning gears are
drawn by the simulation (with the axles and parts riding on them) and their
static placements are hidden.
-}
viewCanvas : Model -> Html Core.Msg
viewCanvas model =
    let
        animated =
            if technicMode model && Animation.isAnimating { playing = model.playing, currentTime = model.currentTime } then
                model.animated

            else
                Animation.none

        studio =
            model.studio

        shown =
            if Set.isEmpty animated.sources then
                studio

            else
                { studio | viewport = Viewport.setHiddenSources animated.sources studio.viewport }
    in
    Core.viewWithExtra
        { width = model.windowWidth
        , height = model.windowHeight
        , extraEntities =
            \camera aspect ->
                if Set.isEmpty animated.gears then
                    []

                else
                    Simulation.gearEntitiesFor animated.gears camera studio.viewport.renderStyle aspect model.simulation
                        ++ Simulation.drivenEntitiesFor animated.gears camera studio.viewport.renderStyle aspect model.simulation
        }
        shown


{-| The context ring for the selected part (`Ring`) on wide screens. On narrow
screens (`isNarrow`) the context toolbar replaces the bottom palette in `narrowRow`.
-}
ringView : Model -> Html Msg
ringView model =
    if isNarrow model then
        text ""

    else
        case model.ringPart of
            Just id ->
                Ring.view (ringConfig model id)

            Nothing ->
                text ""


ringConfig : Model -> Int -> Ring.Config Msg
ringConfig model id =
    { part = partScreenRect model id
    , avoid = ringAvoid model
    , canvas = ( toFloat model.studio.viewport.width, toFloat model.studio.viewport.height )
    , free = model.free
    , glued = Core.selectionGlued model.studio
    , movable = Core.selectionMovable model.studio
    , canCut = Core.selectionMovable model.studio && Core.hasSelection model.studio
    , canCopy = Core.hasSelection model.studio
    , canPaste = Core.hasClipboard model.studio
    , canGroup = Core.canGroup model.studio
    , canUngroup = Core.canUngroup model.studio
    , step = model.stepSize
    , coarsePointer = model.coarsePointer || isNarrow model
    , onTurn = TurnSelection
    , onRaise = RaiseSelection
    , onLower = LowerSelection
    , onDelete = Delete
    , onGlue = ToggleGlue
    , onCut = Cut
    , onCopy = Copy
    , onPaste = Paste
    , onGroup = Group
    , onUngroup = Ungroup
    , onMove = MoveSelection
    , onStep = SetStepSize
    , noOp = NoOp
    , language = model.language
    }


{-| The first-selection hint, a small pill in the top-left bar that does not take
pointer events. Its words depend on whether the device is a touch screen.
-}
hintRow : Model -> Html Msg
hintRow model =
    case model.hint of
        Just _ ->
            div
                [ Attr.attribute "role" "status"
                , Attr.attribute "aria-live" "polite"
                , Attr.attribute "data-play-hint" ""
                , Attr.style "padding" "0.125rem 0.75rem"
                , Attr.style "border-radius" "9999px"
                , Attr.style "background" "rgba(5, 19, 29, 0.7)"
                , Attr.style "color" "var(--color-text-on-dark)"
                , Attr.style "font-size" "0.75rem"
                , Attr.style "font-weight" "500"
                , Attr.style "white-space" "nowrap"
                , Attr.style "pointer-events" "none"
                ]
                [ text (hintText model.language model.coarsePointer) ]

        Nothing ->
            text ""


hintText : Language -> Bool -> String
hintText lang coarse =
    case lang of
        Fi ->
            if coarse then
                "Kaksoisnapauta palikkaa liikuttaaksesi"

            else
                "Kaksoisnapsauta palikkaa liikuttaaksesi"

        En ->
            if coarse then
                "Double-tap a brick for moves"

            else
                "Double-click a brick for moves"


boolText : Bool -> String
boolText on =
    if on then
        "true"

    else
        "false"


{-| The dock: the free catalog in free building, nothing in a fixed model, else
the counted kit.
-}
viewDock : Model -> Html Msg
viewDock model =
    if model.free then
        freeDockView model

    else if List.isEmpty model.inventory then
        text ""

    else
        kitDockView model


{-| The counted kit: one picture card per kit item, its remaining count on the
image corner.
-}
kitDockView : Model -> Html Msg
kitDockView model =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label"
            (if model.language == I18n.Fi then
                "Sarja"

             else
                "Kit"
            )
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.5rem"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "height" PartCard.compactPanelHeight
        , Attr.style "width" dockWidth
        , Attr.style "max-width" "100%"
        , Attr.style "overflow-x" "auto"
        , Attr.style "overflow-y" "hidden"
        , Attr.style "scrollbar-width" "none"
        , Attr.style "-ms-overflow-style" "none"
        , Attr.class "palette-scroll-x"
        , Attr.style "padding" "0.5rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.9)"
        , Attr.style "backdrop-filter" "blur(12px)"
        , Attr.style "border-radius" "1rem"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
        , Attr.style "pointer-events" "auto"
        , keepFocusOnCanvas
        ]
        (List.map (viewKitCard model) model.inventory)


{-| A kit item as a compact picture card. Its name, part number, colour and
count are in the tooltip and accessible name; the image corner shows "left/kit".
-}
viewKitCard : Model -> PartKitItem -> Html Msg
viewKitCard model item =
    let
        remaining =
            remainingForItem model item

        name =
            kitItemName model item

        colourName =
            Colours.nameOf item.color
    in
    PartCard.compactView
        { imageUrl =
            Dict.get (PartCard.thumbnailKey item.part item.color) model.thumbnails
                |> Maybe.map .url
        , tooltip = name ++ " (" ++ partNumber item.part ++ ", " ++ colourName ++ ") — " ++ String.fromInt remaining ++ " of " ++ String.fromInt item.count ++ " available"
        , ariaLabel = "Add " ++ name ++ " (" ++ colourName ++ "), " ++ String.fromInt remaining ++ " of " ++ String.fromInt item.count ++ " left"
        , badge =
            Just
                { text = String.fromInt remaining ++ "/" ++ String.fromInt item.count
                , muted = remaining == 0
                }
        , onClick =
            if remaining > 0 then
                Just (AddPart item)

            else
                Nothing
        , attributes = [ keepFocusOnCanvas ]
        }


{-| The free-building dock: the colour row on the left, then the tabs and the part
search above the cards of the visible tab (or the search results). The cards are
unlimited.
-}
freeDockView : Model -> Html Msg
freeDockView model =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "stretch"
        , Attr.style "gap" "0.5rem"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "height" PartCard.compactPanelHeight
        , Attr.style "width" dockWidth
        , Attr.style "min-width" "0"
        , Attr.style "max-width" "100%"
        , Attr.style "padding" "0.5rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.92)"
        , Attr.style "backdrop-filter" "blur(12px)"
        , Attr.style "border-radius" "1rem"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
        , Attr.style "pointer-events" "auto"
        , keepFocusOnCanvas
        ]
        [ colourRow model
        , div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "gap" "0.375rem"
            , Attr.style "flex" "1 1 auto"
            , Attr.style "min-width" "0"
            ]
            [ div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.375rem"
                , Attr.style "min-width" "0"
                ]
                [ tabRow model
                , searchField model
                ]
            , cardsRow model
            ]
        ]


{-| The colour row: the swatches of `paintColours`, two to a column, the chosen
one outlined. Parts added from the dock take this colour.
-}
colourRow : Model -> Html Msg
colourRow model =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label"
            (if model.language == I18n.Fi then
                "Lisättävien osien väri"

             else
                "Colour of parts added"
            )
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(2, 1.125rem)"
        , Attr.style "grid-auto-rows" "1.125rem"
        , Attr.style "gap" "0.25rem"
        , Attr.style "max-height" "calc(100% - 0.25rem)"
        , Attr.style "overflow-y" "auto"
        , Attr.style "overflow-x" "hidden"
        , Attr.style "scrollbar-width" "none"
        , Attr.style "-ms-overflow-style" "none"
        , Attr.class "palette-scroll-y"
        , Attr.style "align-content" "start"
        , Attr.style "flex-shrink" "0"
        ]
        (List.map (swatch model.paintColour) paintColours)


swatch : Int -> Int -> Html Msg
swatch chosen code =
    let
        name =
            Colours.nameOf code

        isChosen =
            chosen == code
    in
    button
        [ onClick (SetPaintColour code)
        , Attr.attribute "aria-label" name
        , Attr.attribute "aria-pressed" (boolText isChosen)
        , Attr.title name
        , Attr.style "width" "1.125rem"
        , Attr.style "height" "1.125rem"
        , Attr.style "padding" "0"
        , Attr.style "border-radius" "9999px"
        , Attr.style "background" (swatchBackground code)
        , Attr.style "border"
            (if isChosen then
                "2px solid var(--color-brand-yellow)"

             else
                "1px solid rgba(255, 255, 255, 0.35)"
            )
        , Attr.style "cursor" "pointer"
        ]
        []


{-| A swatch's CSS colour, from the colour table the 3D view uses.
-}
swatchBackground : Int -> String
swatchBackground code =
    let
        rgba =
            Colors.resolveColor code 16
    in
    if rgba.alpha < 0.99 then
        "rgba("
            ++ String.fromInt (round (rgba.r * 255))
            ++ ", "
            ++ String.fromInt (round (rgba.g * 255))
            ++ ", "
            ++ String.fromInt (round (rgba.b * 255))
            ++ ", "
            ++ String.fromFloat rgba.alpha
            ++ ")"

    else
        "rgb("
            ++ String.fromInt (round (rgba.r * 255))
            ++ ", "
            ++ String.fromInt (round (rgba.g * 255))
            ++ ", "
            ++ String.fromInt (round (rgba.b * 255))
            ++ ")"


{-| The free dock's tabs, in a row. The search results replace the cards while a
search is typed, and the tabs stay in place.
-}
tabRow : Model -> Html Msg
tabRow model =
    div
        [ Attr.attribute "role" "tablist"
        , Attr.attribute "aria-label" (I18n.paletteCategoryPicker model.language)
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.25rem"
        , Attr.style "flex" "1 1 auto"
        , Attr.style "min-width" "0"
        , Attr.style "overflow-x" "auto"
        , Attr.style "overflow-y" "hidden"
        , Attr.style "scrollbar-width" "none"
        , Attr.style "-ms-overflow-style" "none"
        , Attr.class "scroll-none"
        ]
        (List.map (tabButton model.language model.activeTab) FreeCatalog.tabs)


tabButton : Language -> FreeCatalog.Tab -> FreeCatalog.Tab -> Html Msg
tabButton lang active tab =
    let
        isSelected =
            active == tab
    in
    button
        [ onClick (SetFreeTab tab)
        , Attr.attribute "role" "tab"
        , Attr.attribute "aria-selected" (boolText isSelected)
        , Attr.style "flex-shrink" "0"
        , Attr.style "padding" "0.25rem 0.5rem"
        , Attr.style "font-size" "0.75rem"
        , Attr.style "font-weight" "600"
        , Attr.style "line-height" "1.2"
        , Attr.style "white-space" "nowrap"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background"
            (if isSelected then
                "var(--color-brand-yellow)"

             else
                "rgba(255, 255, 255, 0.08)"
            )
        , Attr.style "color"
            (if isSelected then
                "var(--color-brand)"

             else
                "var(--color-text-subtle)"
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 150ms ease"
        , Attr.class "touch-target"
        ]
        [ text (tabLabelIn lang tab) ]


tabLabelIn : Language -> FreeCatalog.Tab -> String
tabLabelIn lang tab =
    case tab of
        FreeCatalog.BricksTab ->
            I18n.categoryBricks lang

        FreeCatalog.PlatesTab ->
            I18n.categoryPlates lang

        FreeCatalog.SlopesTab ->
            I18n.categorySlopesAndTiles lang

        FreeCatalog.BracketsTab ->
            I18n.categoryBracketsSnot lang

        FreeCatalog.TechnicTab ->
            I18n.categoryTechnic lang


{-| The part search box. Typing searches the whole part library; a form field, so
no keyboard shortcut fires while it has the focus.
-}
searchField : Model -> Html Msg
searchField model =
    input
        [ Attr.type_ "search"
        , Attr.id "play-part-search"
        , Attr.attribute "aria-label" (I18n.paletteSearchAria model.language)
        , Attr.placeholder (I18n.paletteSearchPlaceholder model.language)
        , Attr.value model.search.query
        , onInput SearchInput
        , onFocus SearchOpened
        , Attr.style "width" "8.5rem"
        , Attr.style "min-width" "0"
        , Attr.style "flex-shrink" "1"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "padding" "0.25rem 0.5rem"
        , Attr.style "font-size" "0.75rem"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background" "rgba(255, 255, 255, 0.08)"
        , Attr.style "color" "var(--color-text-on-dark)"
        ]
        []


{-| The cards row: the tab's parts, or the search results, with a note while the
library loads, when it cannot be read, or when nothing matches.
-}
cardsRow : Model -> Html Msg
cardsRow model =
    let
        note message =
            [ div
                [ Attr.style "color" "var(--color-text-subtle)"
                , Attr.style "font-size" "0.8125rem"
                , Attr.style "white-space" "nowrap"
                , Attr.style "padding" "0 0.25rem"
                ]
                [ text message ]
            ]

        children =
            if notSearching model then
                List.map (freeCard model) (freeEntries model)

            else
                case model.search.index of
                    NotLoaded ->
                        note (I18n.paletteLoading model.language)

                    Loading ->
                        note (I18n.paletteLoading model.language)

                    Failed reason ->
                        note ("No part library: " ++ reason ++ ".")

                    Loaded _ ->
                        case model.search.results of
                            [] ->
                                note ("No parts match \"" ++ String.trim model.search.query ++ "\".")

                            _ ->
                                List.map (freeCard model) (freeEntries model)
    in
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label"
            (if model.language == I18n.Fi then
                "Osat"

             else
                "Parts"
            )
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.5rem"
        , Attr.style "flex" "1 1 auto"
        , Attr.style "min-height" "0"
        , Attr.style "min-width" "0"
        , Attr.style "overflow-x" "auto"
        , Attr.style "overflow-y" "hidden"
        , Attr.style "scrollbar-width" "none"
        , Attr.style "-ms-overflow-style" "none"
        , Attr.class "palette-scroll-x"
        , Attr.style "padding" "0.125rem"
        ]
        children


{-| One free-dock part as a compact picture card, in the paint colour. Its name
and part number are in the tooltip and the accessible name.
-}
freeCard : Model -> FreeCatalog.Entry -> Html Msg
freeCard model entry =
    let
        colourName =
            Colours.nameOf model.paintColour
    in
    PartCard.compactView
        { imageUrl = Maybe.map .url (Dict.get (PartCard.thumbnailKey entry.file model.paintColour) model.thumbnails)
        , tooltip = entry.name ++ " (" ++ partNumber entry.file ++ ", " ++ colourName ++ ")" ++ descriptionSuffix entry.description
        , ariaLabel = "Add " ++ entry.name ++ " (" ++ colourName ++ ")"
        , badge = Nothing
        , onClick = Just (AddFreePart { file = entry.file, color = model.paintColour, name = entry.name })
        , attributes = [ keepFocusOnCanvas ]
        }


descriptionSuffix : String -> String
descriptionSuffix description =
    if String.isEmpty description then
        ""

    else
        " — " ++ description


{-| Clicking a toolbar, dock or pad button must not leave it focused: a focused
button takes Space for itself, so Space would undo or add a part instead of
playing and pausing.
-}
keepFocusOnCanvas : Html.Attribute Msg
keepFocusOnCanvas =
    Focus.keepFocusOnCanvas NoOp


{-| The shortcut list. The Machine section (Space and the gear rules) is listed
only in Technic mode, where it applies.
-}
helpRows : Model -> List ( String, List ( String, String ) )
helpRows model =
    I18n.shortcutsHelpRows model.language
        { free = model.free
        , technic = technicMode model
        , hasInventory = not (List.isEmpty model.inventory)
        }


{-| Play or pause the machine. Highlighted while it runs, like the old
simulate toggle, so the state is visible at a glance.
-}
playButton : I18n.Language -> Bool -> Html Msg
playButton lang playing =
    button
        [ onClick TogglePlay
        , Attr.title
            (if playing then
                I18n.headerPause lang ++ " (Space)"

             else
                I18n.headerPlay lang ++ " (Space)"
            )
        , Attr.attribute "aria-pressed"
            (if playing then
                "true"

             else
                "false"
            )
        , Attr.style "padding" "0.375rem 0.875rem"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "700"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background"
            (if playing then
                "var(--color-brand-yellow)"

             else
                "rgba(255, 255, 255, 0.15)"
            )
        , Attr.style "color"
            (if playing then
                "var(--color-brand)"

             else
                "var(--color-text-on-dark)"
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "pointer-events" "auto"
        , Attr.style "transition" "all 150ms ease"
        , Attr.style "display" "flex"
        , Attr.style "flex-shrink" "0"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.375rem"
        , Attr.style "white-space" "nowrap"
        , Attr.class "touch-target"
        ]
        (if playing then
            [ Icon.feather "pause", text (I18n.headerPause lang) ]

         else
            [ Icon.feather "play", text (I18n.headerPlay lang) ]
        )


{-| Main application program.
-}
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
