port module Ports exposing
    ( cancelSnapshot
    , captureRender
    , copyToClipboard
    , downloadStudioIo
    , geometryFlattenFailed
    , geometryFlattened
    , rememberHintSeen
    , requestGeometryFlatten
    , requestThumbnails
    , saveSnapshot
    , setUrlHash
    , snapshotFailed
    , snapshotProgress
    , snapshotReady
    , thumbnailLoaded
    )

{-| Ports for brick-play: the geometry flattening worker and the part thumbnail
pipeline (`initThumbnails` in `packages/brick-frame-viewport/runtime/thumbnail-pipeline.js`).
-}

import Bytes exposing (Bytes)
import Json.Encode


{-| Ask JS to flatten geometry. Payload is JSON text.
-}
port requestGeometryFlatten : String -> Cmd msg


{-| Flattened geometry as JSON text.
-}
port geometryFlattened : (String -> msg) -> Sub msg


{-| The worker failed.
-}
port geometryFlattenFailed : (String -> msg) -> Sub msg


{-| Ask JS to render (or fetch from its cache) a thumbnail of each kit part in
its colour. `rgb` is the colour in 0–1, from the LDraw colour table.
-}
port requestThumbnails : List { file : String, color : Int, rgb : List Float } -> Cmd msg


{-| A rendered thumbnail as a data URL, with the part's .dat title (`""` when
unknown). Parts that cannot be rendered send nothing.
-}
port thumbnailLoaded : ({ file : String, color : Int, url : String, title : String } -> msg) -> Sub msg


{-| Update the browser URL hash with a binary payload.
-}
port setUrlHash : List Int -> Cmd msg


{-| Copy binary link payload to the system clipboard (compressed as #b/<base64url> by JS).
-}
port copyToClipboard : List Int -> Cmd msg


{-| Ask the offline path tracer for a 1920 × 1080 render. The value is the
request built by `Viewport.snapshotRequest`: the camera and every visible triangle.
-}
port captureRender : Json.Encode.Value -> Cmd msg


{-| The snapshot modal was closed while the render was running: stop the path tracer.
-}
port cancelSnapshot : () -> Cmd msg


{-| Path tracer progress: the fraction done and the samples taken so far.
-}
port snapshotProgress : ({ fraction : Float, samples : Int } -> msg) -> Sub msg


{-| The path tracer failed; the string is a message for the user.
-}
port snapshotFailed : (String -> msg) -> Sub msg


{-| Receive when high-resolution render capture is ready for preview.
-}
port snapshotReady : ({ dataUrl : String, filename : String } -> msg) -> Sub msg


{-| Request file download of the approved snapshot.
-}
port saveSnapshot : { dataUrl : String, filename : String } -> Cmd msg


{-| Request download of the model as a BrickLink Studio `.io` archive containing `model.ldr`.
-}
port downloadStudioIo : { filename : String, ldrText : String } -> Cmd msg


{-| Remember in this browser that the first-selection hint has been shown, so it
is not shown again. The JS side writes a flag to localStorage inside a try/catch.
-}
port rememberHintSeen : () -> Cmd msg
