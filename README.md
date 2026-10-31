# Brick Play (Palikkaleikki)

Brick Play is a lightweight, touch-friendly, browser-based LEGO® building sandbox and Technic mechanism simulator developed by Suomen Palikkaharrastajat ry.

## Features

- **Free Building Mode:** Intuitive parts palette with search, 3D body dragging, intelligent surface snapping, and full color selection.
- **Limited Parts Set:** Build with a fixed kit of parts exported from Brick Studio.
- **Fixed Model Viewer:** Inspect and interact with pre-assembled models.
- **Technic Mode:** Real-time physics simulation of gear trains, motorized axles, and driven assemblies with interactive playback controls.
- **Context Ring:** Instant radial controls beside clicked parts for rotation, nudging (Stud, ½ stud, Bit), and color changes.
- **Deep Linking:** Zero-backend sharing of custom models and part kits encoded directly in URL hash fragments.

## Architecture

Brick Play is powered by the `brick-studio` shared Elm package suite via git submodule:

```
brick-play/
├── src/                                # Brick Play Elm application
│   ├── Main.elm                        # Application root (dock, modes, context ring)
│   ├── Link.elm                        # URL hash payload encoder/decoder
│   ├── Shortcuts.elm                   # Keyboard shortcut bindings
│   ├── Ring.elm                        # Context ring radial controls
│   ├── Step.elm                        # Nudge increment definitions (Stud, Half, Bit)
│   ├── Free/                           # Curated free building palette
│   └── Technic/                        # Technic machine analysis & animation
├── tests/                              # Elm unit test suite (300 tests)
├── vendor/brick-studio/                # Submodule hosting core Elm packages
├── package.json                        # NPM package configuration
└── devenv.nix                          # Pure Node.js & Elm development shell
```

## Quick Start

Using [devenv](https://devenv.sh):

```bash
# Enter devenv shell
devenv shell

# Initialize git submodules and symlinks
make vendor

# Run test suite
make test

# Start local dev server (http://localhost:5173)
make dev

# Production build
make build
```

## Licensing & Trademarks

- Software is open source under the MIT License (see [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md)).
- LEGO® is a registered trademark of the LEGO Group of companies, which does not sponsor, authorize, or endorse this project.
- LDraw™ is a trademark owned and licensed by the Estate of James Jessiman.
