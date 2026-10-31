module ReviewConfig exposing (config)

{-| elm-review configuration for brick-play.
-}

import LlmAgent.NoExposingEverything
import LlmAgent.NoTailwindRawStrings
import LlmAgent.RequireModuleDoc
import LlmAgent.RequireTypeAnnotation
import Review.Rule as Rule exposing (Rule)


generatedDirectories : List String
generatedDirectories =
    [ ".elm-tailwind/" ]


generatedFiles : List String
generatedFiles =
    [ "vendor/brick-studio/packages/brick-frame-data/src/Data.elm"
    , "vendor/brick-studio/packages/brick-frame-simulator/src/LDraw/ColorData.elm"
    ]


config : List Rule
config =
    [ LlmAgent.NoTailwindRawStrings.rule
    , LlmAgent.RequireModuleDoc.rule
    , LlmAgent.RequireTypeAnnotation.rule
    , LlmAgent.NoExposingEverything.rule
    ]
        |> List.map (Rule.ignoreErrorsForDirectories generatedDirectories)
        |> List.map (Rule.ignoreErrorsForFiles generatedFiles)
