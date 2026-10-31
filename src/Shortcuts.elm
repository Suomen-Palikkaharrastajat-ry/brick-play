module Shortcuts exposing
    ( Action(..)
    , actionFor
    )

{-| Brick Play's own keys, on top of the studio's and the viewport's (see
`Viewport.Shortcuts` and `Studio.Shortcuts`).

  - `L` glues or unglues the selection. Glue exists in free building only: a
    puzzle's glued parts are locked, so the key does nothing there.
  - `Tab` and `Shift+Tab` select the next or previous part in the build's Tab
    order (`Studio.Core.selectNeighbour`). They act only while the canvas has
    focus: with a form field or a button focused, Tab keeps its browser meaning.

Like every shortcut table it is guarded: nothing fires while the user is typing
in a form field (the dock's part search included), on a widget key, or behind an
open dialog or menu.

-}

import Viewport.Shortcuts as ViewportShortcuts exposing (KeyEvent)


{-| What a key press means in Brick Play.
-}
type Action
    = ToggleGlue
    | SelectNext
    | SelectPrevious


{-| The Brick Play action for a key press, if any. `glueEditable` is true in free
building, `helpVisible` and `menuOpen` say whether a dialog or the menu has the
keyboard.
-}
actionFor : { glueEditable : Bool, helpVisible : Bool, menuOpen : Bool } -> KeyEvent -> Maybe Action
actionFor context event =
    if ViewportShortcuts.isTextEntryTarget event || ViewportShortcuts.isWidgetKey event then
        Nothing

    else if context.helpVisible || context.menuOpen then
        Nothing

    else if event.ctrl || event.meta || event.alt then
        Nothing

    else
        case ViewportShortcuts.normalizeKey event.key of
            "l" ->
                if context.glueEditable then
                    Just ToggleGlue

                else
                    Nothing

            "Tab" ->
                if isControlTarget event then
                    -- A focused button or link keeps Tab for moving focus.
                    Nothing

                else if event.shift then
                    Just SelectPrevious

                else
                    Just SelectNext

            _ ->
                Nothing


{-| Whether the key went to a control that uses Tab for focus: a button, a link,
a summary, or an element with a button-like ARIA role. Text fields are already
excluded by `ViewportShortcuts.isTextEntryTarget`.
-}
isControlTarget : KeyEvent -> Bool
isControlTarget event =
    List.member (String.toUpper event.targetTag) [ "BUTTON", "A", "SUMMARY" ]
        || List.member (String.toLower event.targetRole)
            [ "button", "link", "checkbox", "switch", "menuitem", "tab", "option", "radio", "slider", "textbox", "combobox" ]
