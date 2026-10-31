# AGENTS.md — Brick Play

Lightweight, touch-friendly 3D LEGO® building sandbox and Technic simulator built with Elm + WebGL.

## Application Architecture

- **Root Component (`src/Main.elm`):** Manages application state, building dock, context ring, and wraps `Studio.Core` from `vendor/brick-studio/packages/brick-frame-studio`.
- **Modes:**
  - *Free Building:* Full curated dock (Bricks, Plates, Slopes, Tiles, Technic), search, and color picker.
  - *Parts Set (`#parts=...`):* Fixed inventory kit mode where parts are deducted as they are placed on the plate.
  - *Fixed Model (`#model=...`):* Read-only inspection mode with interactive gear simulation.
  - *Technic Simulation:* Auto-activates when gears/axles/motors are placed; provides run/stop playback controls.
- **Context Ring (`src/Ring.elm`):** Radial UI anchored on screen beside double-clicked parts providing quick rotation (X, Y, Z), nudging (Stud, ½ stud, Bit), coloring, and deletion.
- **URL Payload (`src/Link.elm`):** Encodes/decodes entire model topologies and inventories directly in URL hash fragments using compact binary serialization.

## Shared Packages Integration

The application imports core engine packages directly from the submodule at `vendor/brick-studio`:
- `vendor/brick-studio/packages/brick-frame-studio/src`
- `vendor/brick-studio/packages/brick-frame-simulator/src`
- `vendor/brick-studio/packages/brick-frame-viewport/src`
- `vendor/brick-studio/packages/brick-frame-simulation/src`
- `vendor/brick-studio/packages/brick-frame-data/src`
- `vendor/brick-studio/packages/brick-frame-ui/src`
- `vendor/brick-studio/vendor/master-builder/packages/` (via `packages/` symlink)

## Development & Test Commands

Run within `devenv shell`:

```bash
# Run the complete test suite (300 tests)
devenv shell -- make test

# Validate Elm formatting and review rules
devenv shell -- make check

# Auto-format Elm code
devenv shell -- make format

# Launch development server
devenv shell -- make dev
```
