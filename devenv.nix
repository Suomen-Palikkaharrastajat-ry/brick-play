let
  ci =
    { pkgs, ... }:
    let
      npmTools = pkgs.callPackage ./pkgs/npm-tools.nix { };
    in
    {
      languages.elm.enable = true;

      env.NODE_PATH = "${npmTools}/lib/node_modules";

      packages = [
        npmTools
        pkgs.nodejs_22
        pkgs.elmPackages.elm-review
      ];

      enterShell = ''
        TOP="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
        ln -sfn "${npmTools}/lib/node_modules" "$TOP/node_modules"
      '';
    };

  shell =
    { pkgs, ... }:
    let
      npmTools = pkgs.callPackage ./pkgs/npm-tools.nix { };
    in
    {
      packages = [
        pkgs.entr
        pkgs.git
        pkgs.nodejs_22
        pkgs.treefmt
        pkgs.elmPackages.elm-review
        pkgs.elmPackages.elm-json
        npmTools
      ];

      languages.elm.enable = true;

      dotenv.enable = true;

      env.NODE_PATH = "${npmTools}/lib/node_modules";

      enterShell = ''
        TOP="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
        ln -sfn "${npmTools}/lib/node_modules" "$TOP/node_modules"

        echo ""
        echo "── brick-play dev environment ──────────────────────"
        echo "  Elm:    $(elm --version)"
        echo "  Node:   $(node --version)"
        echo "  Vite:   $(vite --version)"
        echo ""
        echo "  make dev      — start Vite dev server"
        echo "  make build    — production build"
        echo "  make test     — run elm-test"
        echo ""
      '';
    };
in
{
  profiles.shell.module = {
    imports = [ shell ];
  };

  profiles.ci.module = {
    imports = [ ci ];
  };
}
