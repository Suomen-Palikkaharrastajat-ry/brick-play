module ShareModal exposing
    ( Config
    , Model
    , ShareKind(..)
    , availableKinds
    , computeHash
    , computeUrl
    , computeUrlWithLive
    , defaultKind
    , hasPlacedUnglued
    , init
    , kindToTestId
    , payloadFor
    , view
    , viewWithLanguage
    )

{-| Responsive Share Dialog Modal for Brick Play.

Provides a pure Elm QR code and link generator for builds, supporting:

  - Free building mode
  - Parts set mode (resetting loose parts to dock)
  - Partially built set (puzzle preserving board progress)
  - Fixed model mode (locked viewing without catalog)

Optimized for mobile touch targets (>=44px close/actions, >=48px selector cards)
and camera readability with a 16px white quiet zone.

-}

import Base64
import Flate
import Html exposing (Html, button, div, h2, text)
import Html.Attributes as Attr
import Html.Events
import I18n exposing (Language(..))
import LDraw.Document as Document exposing (Document)
import Link
import QRCode
import Studio.HashPayload as HashPayload exposing (KitItem)
import Svg.Attributes
import UI.Icon as Icon
import UI.Theme as Theme


{-| The share modes supported across Brick Play.
-}
type ShareKind
    = FreeBuilding
    | PartsSet
    | PartialSet
    | FixedModel
    | LivePreview


{-| State of the share modal.
-}
type alias Model =
    { selected : ShareKind
    , copied : Bool
    }


{-| Configuration and callbacks needed to render the modal.
-}
type alias Config msg =
    { language : Language
    , baseUrl : String
    , free : Bool
    , document : Document
    , inventory : List KitItem
    , liveBroadcasting : Bool
    , liveWatchUrl : Maybe String
    , liveViewerCount : Int
    , onToggleBroadcast : msg
    , onClose : msg
    , onSelectKind : ShareKind -> msg
    , onCopyUrl : msg
    }


{-| Initialize the share modal with a selected kind.
-}
init : ShareKind -> Model
init kind =
    { selected = kind
    , copied = False
    }


{-| Determine the default share kind for a given model state.
-}
defaultKind : { free : Bool, hasInventory : Bool } -> ShareKind
defaultKind config =
    if config.free then
        FreeBuilding

    else if config.hasInventory then
        PartsSet

    else
        FixedModel


{-| Check whether the document contains any unglued placements on the board.
-}
hasPlacedUnglued : Document -> Bool
hasPlacedUnglued doc =
    Document.placements doc
        |> List.any (\p -> not (Document.isPlacementGlued p.id doc))


{-| Calculate the list of share kinds applicable to the current build.
-}
availableKinds : { free : Bool, hasInventory : Bool, hasPlacedUnglued : Bool } -> List ShareKind
availableKinds config =
    let
        baseKinds =
            if config.free then
                [ FreeBuilding, PartsSet, FixedModel ]

            else if config.hasInventory then
                if config.hasPlacedUnglued then
                    [ PartsSet, PartialSet, FixedModel ]

                else
                    [ PartsSet, FixedModel ]

            else
                [ FixedModel ]
    in
    baseKinds ++ [ LivePreview ]


{-| Convert a share kind to a string for data-testid attributes.
-}
kindToTestId : ShareKind -> String
kindToTestId kind =
    case kind of
        FreeBuilding ->
            "free"

        PartsSet ->
            "parts"

        PartialSet ->
            "partial"

        FixedModel ->
            "fixed"

        LivePreview ->
            "live"


{-| Compute the wire-format byte list for a share kind.
-}
payloadFor : ShareKind -> Bool -> Document -> List KitItem -> List Int
payloadFor kind free doc inventory =
    case kind of
        FreeBuilding ->
            Link.freeHash doc

        PartsSet ->
            if free then
                Link.freeShareHash doc

            else
                Link.setShareHash doc inventory

        PartialSet ->
            Link.partialSetHash doc inventory

        FixedModel ->
            Link.fixedModelHash doc

        LivePreview ->
            Link.freeHash doc


{-| Compute the pure high-density hash string (`#b/...`) for a share kind.
Uses raw deflate compression via `Flate.deflate` and base64url encoding.
-}
computeHash : ShareKind -> Bool -> Document -> List KitItem -> String
computeHash kind free doc inventory =
    let
        bytesList =
            payloadFor kind free doc inventory

        compressed =
            HashPayload.listToBytes bytesList
                |> Flate.deflate

        base64Url =
            Base64.fromBytes compressed
                |> Maybe.withDefault ""
                |> String.replace "+" "-"
                |> String.replace "/" "_"
                |> String.replace "=" ""
    in
    "#b/" ++ base64Url


{-| Compute the full URL for a share kind, ensuring no double hashes.
-}
computeUrl : String -> ShareKind -> Bool -> Document -> List KitItem -> String
computeUrl baseUrl kind free doc inventory =
    computeUrlWithLive baseUrl kind free doc inventory Nothing


{-| Compute the full URL for a share kind, supporting live broadcast watch URLs.
-}
computeUrlWithLive : String -> ShareKind -> Bool -> Document -> List KitItem -> Maybe String -> String
computeUrlWithLive baseUrl kind free doc inventory liveWatchUrl =
    case kind of
        LivePreview ->
            case liveWatchUrl of
                Just url ->
                    url

                Nothing ->
                    let
                        cleanBase =
                            case String.split "#" baseUrl of
                                first :: _ ->
                                    first

                                [] ->
                                    baseUrl
                    in
                    cleanBase ++ "#/watch/connecting"

        _ ->
            let
                cleanBase =
                    case String.split "#" baseUrl of
                        first :: _ ->
                            first

                        [] ->
                            baseUrl

                hash =
                    computeHash kind free doc inventory
            in
            cleanBase ++ hash


kindLabels : Language -> ShareKind -> ( String, String )
kindLabels lang kind =
    case kind of
        FreeBuilding ->
            ( I18n.shareOptionFreeBuilding lang, I18n.shareOptionFreeBuildingDesc lang )

        PartsSet ->
            ( I18n.shareOptionPartsSet lang, I18n.shareOptionPartsSetDesc lang )

        PartialSet ->
            ( I18n.shareOptionPartialSet lang, I18n.shareOptionPartialSetDesc lang )

        FixedModel ->
            ( I18n.shareOptionFixedModel lang, I18n.shareOptionFixedModelDesc lang )

        LivePreview ->
            ( shareOptionLivePreview lang, shareOptionLivePreviewDesc lang )


shareOptionLivePreview : Language -> String
shareOptionLivePreview lang =
    case lang of
        Fi ->
            "Suora lähetys"

        En ->
            "Live Crafting"


shareOptionLivePreviewDesc : Language -> String
shareOptionLivePreviewDesc lang =
    case lang of
        Fi ->
            "Jaa linkki, josta muut näkevät rakentamisesi reaaliajassa"

        En ->
            "Share a readonly link to let others watch you build in real time"


{-| Render the responsive share modal.
-}
view : Config msg -> Model -> Html msg
view config model =
    viewWithLanguage config.language config model


{-| Render the modal explicitly with a given language.
-}
viewWithLanguage : Language -> Config msg -> Model -> Html msg
viewWithLanguage lang config model =
    let
        hasPlaced =
            hasPlacedUnglued config.document

        kinds =
            availableKinds
                { free = config.free
                , hasInventory = not (List.isEmpty config.inventory)
                , hasPlacedUnglued = hasPlaced
                }

        fullUrl =
            computeUrlWithLive config.baseUrl model.selected config.free config.document config.inventory config.liveWatchUrl
    in
    div
        [ Attr.attribute "role" "dialog"
        , Attr.attribute "aria-modal" "true"
        , Attr.attribute "aria-label" (I18n.shareModalTitle lang)
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "z-index" "99999"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "padding" "12px"
        , Attr.style "box-sizing" "border-box"
        ]
        [ -- Clickable backdrop for closing
          div
            [ Attr.attribute "data-testid" "share-modal-backdrop"
            , Attr.style "position" "absolute"
            , Attr.style "inset" "0"
            , Attr.style "background" "color-mix(in srgb, #000 65%, transparent)"
            , Attr.style "backdrop-filter" "blur(8px)"
            , Attr.style "-webkit-backdrop-filter" "blur(8px)"
            , Html.Events.onClick config.onClose
            ]
            []
        , -- Modal Card
          div
            [ Attr.attribute "data-testid" "share-modal"
            , Attr.style "position" "relative"
            , Attr.style "z-index" "1"
            , Attr.style "max-width" "min(360px, calc(100vw - 24px))"
            , Attr.style "max-height" "calc(100vh - 24px)"
            , Attr.style "width" "100%"
            , Attr.style "background" Theme.panelSurface
            , Attr.style "color" Theme.textPrimary
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "12px"
            , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.4), 0 8px 10px -6px rgba(0, 0, 0, 0.3)"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "overflow-y" "auto"
            , Attr.style "box-sizing" "border-box"
            ]
            [ -- Modal Header
              div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "space-between"
                , Attr.style "padding" "12px 16px"
                , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
                ]
                [ h2
                    [ Attr.style "margin" "0"
                    , Attr.style "font-size" "15px"
                    , Attr.style "font-weight" "600"
                    , Attr.style "letter-spacing" "-0.01em"
                    ]
                    [ text (I18n.shareModalTitle lang) ]
                , button
                    [ Attr.type_ "button"
                    , Attr.attribute "aria-label" (I18n.shareClose lang)
                    , Attr.attribute "data-testid" "share-modal-close"
                    , Html.Events.onClick config.onClose
                    , Attr.style "min-width" "44px"
                    , Attr.style "min-height" "44px"
                    , Attr.style "display" "inline-flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "justify-content" "center"
                    , Attr.style "background" "transparent"
                    , Attr.style "border" "none"
                    , Attr.style "border-radius" "6px"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "color" Theme.textMuted
                    , Attr.style "padding" "0"
                    ]
                    [ Icon.feather "x" ]
                ]
            , -- Modal Body
              div
                [ Attr.style "padding" "16px"
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "12px"
                ]
                [ -- Feature Selector (Radio Cards)
                  div
                    [ Attr.attribute "role" "radiogroup"
                    , Attr.attribute "aria-label" (I18n.shareModalTitle lang)
                    , Attr.style "display" "flex"
                    , Attr.style "flex-direction" "column"
                    , Attr.style "gap" "8px"
                    ]
                    (List.map (viewRadioCard lang model.selected config.onSelectKind) kinds)
                , if model.selected == LivePreview then
                    if config.liveBroadcasting then
                        div
                            [ Attr.style "display" "flex"
                            , Attr.style "flex-direction" "column"
                            , Attr.style "gap" "12px"
                            ]
                            [ viewLiveBadge lang config.liveViewerCount
                            , viewQrCode lang fullUrl
                            , viewCopyButton lang model.copied config.onCopyUrl
                            , viewBroadcastButton lang True config.onToggleBroadcast
                            ]

                    else
                        div
                            [ Attr.style "display" "flex"
                            , Attr.style "flex-direction" "column"
                            , Attr.style "gap" "12px"
                            , Attr.style "padding" "6px 0"
                            ]
                            [ viewBroadcastButton lang False config.onToggleBroadcast ]

                  else
                    div
                        [ Attr.style "display" "flex"
                        , Attr.style "flex-direction" "column"
                        , Attr.style "gap" "12px"
                        ]
                        [ viewQrCode lang fullUrl
                        , viewCopyButton lang model.copied config.onCopyUrl
                        ]
                ]
            ]
        ]


viewLiveBadge : Language -> Int -> Html msg
viewLiveBadge lang count =
    div
        [ Attr.attribute "data-testid" "live-broadcast-badge"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "6px 12px"
        , Attr.style "border-radius" "6px"
        , Attr.style "background" "rgba(239, 68, 68, 0.15)"
        , Attr.style "border" "1px solid rgba(239, 68, 68, 0.3)"
        , Attr.style "color" "#ef4444"
        , Attr.style "font-size" "12px"
        , Attr.style "font-weight" "600"
        ]
        [ div
            [ Attr.style "width" "6px"
            , Attr.style "height" "6px"
            , Attr.style "border-radius" "50%"
            , Attr.style "background" "#ef4444"
            ]
            []
        , text
            (case lang of
                Fi ->
                    "LIVE · "
                        ++ String.fromInt count
                        ++ (if count == 1 then
                                " katsoja"

                            else
                                " katsojaa"
                           )

                En ->
                    "LIVE · "
                        ++ String.fromInt count
                        ++ (if count == 1 then
                                " viewer"

                            else
                                " viewers"
                           )
            )
        ]


viewBroadcastButton : Language -> Bool -> msg -> Html msg
viewBroadcastButton lang isBroadcasting onToggle =
    button
        [ Attr.type_ "button"
        , Attr.attribute "data-testid"
            (if isBroadcasting then
                "stop-broadcast-button"

             else
                "start-broadcast-button"
            )
        , Html.Events.onClick onToggle
        , Attr.style "min-height" "44px"
        , Attr.style "width" "100%"
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "8px"
        , Attr.style "padding" "10px 16px"
        , Attr.style "border-radius" "8px"
        , Attr.style "font-weight" "600"
        , Attr.style "font-size" "13px"
        , Attr.style "cursor" "pointer"
        , Attr.style "border"
            (if isBroadcasting then
                "1px solid #dc2626"

             else
                "1px solid " ++ Theme.borderDefault
            )
        , Attr.style "background"
            (if isBroadcasting then
                "#dc2626"

             else
                Theme.brandYellow
            )
        , Attr.style "color"
            (if isBroadcasting then
                "#ffffff"

             else
                Theme.brand
            )
        ]
        [ Icon.feather
            (if isBroadcasting then
                "square"

             else
                "radio"
            )
        , text
            (if isBroadcasting then
                case lang of
                    Fi ->
                        "Lopeta suora lähetys"

                    En ->
                        "Stop Live Crafting"

             else
                case lang of
                    Fi ->
                        "Aloita suora lähetys"

                    En ->
                        "Start Live Crafting"
            )
        ]


viewRadioCard : Language -> ShareKind -> (ShareKind -> msg) -> ShareKind -> Html msg
viewRadioCard lang currentSelected onSelect kind =
    let
        isSelected =
            currentSelected == kind

        ( title, desc ) =
            kindLabels lang kind
    in
    div
        [ Attr.attribute "role" "radio"
        , Attr.attribute "aria-checked"
            (if isSelected then
                "true"

             else
                "false"
            )
        , Attr.attribute "tabindex" "0"
        , Attr.attribute "data-testid" ("share-kind-" ++ kindToTestId kind)
        , Html.Events.onClick (onSelect kind)
        , Attr.style "min-height" "48px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "10px"
        , Attr.style "padding" "8px 12px"
        , Attr.style "border-radius" "8px"
        , Attr.style "border"
            (if isSelected then
                "2px solid " ++ Theme.brandYellow

             else
                "1px solid " ++ Theme.borderDefault
            )
        , Attr.style "background"
            (if isSelected then
                Theme.panelSubtleBackground

             else
                "transparent"
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "box-sizing" "border-box"
        ]
        [ div
            [ Attr.style "width" "18px"
            , Attr.style "height" "18px"
            , Attr.style "border-radius" "50%"
            , Attr.style "border"
                ("2px solid "
                    ++ (if isSelected then
                            Theme.brandYellow

                        else
                            Theme.textMuted
                       )
                )
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "flex-shrink" "0"
            ]
            [ if isSelected then
                div
                    [ Attr.style "width" "10px"
                    , Attr.style "height" "10px"
                    , Attr.style "border-radius" "50%"
                    , Attr.style "background" Theme.brandYellow
                    ]
                    []

              else
                text ""
            ]
        , div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "gap" "2px"
            ]
            [ div
                [ Attr.style "font-size" "13px"
                , Attr.style "font-weight" "600"
                , Attr.style "color" Theme.textPrimary
                ]
                [ text title ]
            , div
                [ Attr.style "font-size" "11px"
                , Attr.style "color" Theme.textMuted
                , Attr.style "line-height" "1.3"
                ]
                [ text desc ]
            ]
        ]


viewQrCode : Language -> String -> Html msg
viewQrCode lang url =
    case QRCode.fromString url of
        Ok qrCode ->
            div
                [ Attr.attribute "data-testid" "share-qr-container"
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "8px"
                , Attr.style "margin" "8px 0"
                ]
                [ div
                    [ Attr.attribute "data-testid" "share-qr-code"
                    , Attr.style "background" "#ffffff"
                    , Attr.style "padding" "16px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "justify-content" "center"
                    , Attr.style "box-shadow" "0 2px 6px rgba(0, 0, 0, 0.15)"
                    , Attr.style "width" "180px"
                    , Attr.style "height" "180px"
                    , Attr.style "box-sizing" "border-box"
                    ]
                    [ QRCode.toSvg
                        [ Svg.Attributes.width "100%"
                        , Svg.Attributes.height "100%"
                        ]
                        qrCode
                    ]
                , div
                    [ Attr.attribute "data-testid" "share-qr-hint"
                    , Attr.style "font-size" "11px"
                    , Attr.style "color" Theme.textMuted
                    , Attr.style "text-align" "center"
                    ]
                    [ text (I18n.shareScanQrHint lang) ]
                ]

        Err _ ->
            text ""


viewCopyButton : Language -> Bool -> msg -> Html msg
viewCopyButton lang copied onCopy =
    button
        [ Attr.type_ "button"
        , Attr.attribute "data-testid" "share-copy-button"
        , Html.Events.onClick onCopy
        , Attr.style "min-height" "44px"
        , Attr.style "width" "100%"
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "8px"
        , Attr.style "padding" "10px 16px"
        , Attr.style "border-radius" "8px"
        , Attr.style "font-weight" "600"
        , Attr.style "font-size" "13px"
        , Attr.style "cursor" "pointer"
        , Attr.style "border"
            (if copied then
                "1px solid #16a34a"

             else
                "1px solid " ++ Theme.borderDefault
            )
        , Attr.style "background"
            (if copied then
                "#16a34a"

             else
                Theme.brandYellow
            )
        , Attr.style "color"
            (if copied then
                "#ffffff"

             else
                Theme.brand
            )
        , Attr.style "transition" "background-color 0.15s ease, color 0.15s ease, border-color 0.15s ease"
        ]
        [ Icon.feather
            (if copied then
                "check"

             else
                "copy"
            )
        , text
            (if copied then
                I18n.shareCopiedLink lang

             else
                I18n.shareCopyLink lang
            )
        ]
