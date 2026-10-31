# Packages vite CLI and elm-test CLI from the project's npm package-lock.json
# so the versions stay in sync with package.json.
#
# How to update the hash after changing package-lock.json:
#   1. Run: cd pkgs && npm install   (requires npm, available in devenv shell)
#   2. Set hash = pkgs.lib.fakeHash; below
#   3. Run `devenv shell` — the build fails with the correct sha256 in "got:"
#   4. Paste that sha256 here
{ pkgs }:
let
  patchedSrc = pkgs.runCommand "template-npm-src"
    { nativeBuildInputs = [ pkgs.jq ]; }
    ''
      mkdir $out
      jq 'del(.scripts.postinstall)' ${./package.json} > $out/package.json
      cp ${./package-lock.json} $out/package-lock.json
    '';

  npmDeps = pkgs.fetchNpmDeps {
    name = "template-npm-deps";
    src = patchedSrc;
    hash = "sha256-LKW7ipJ4jMNTi4jHJuuiVd8bkjvkQgBw1z9v0UmUDIo=";
  };
in
pkgs.stdenv.mkDerivation {
  pname = "template-npm-tools";
  version = "1.0.0";

  src = patchedSrc;
  inherit npmDeps;

  nativeBuildInputs = [
    pkgs.nodejs_22
    pkgs.npmHooks.npmConfigHook
    pkgs.makeWrapper
  ];

  makeCacheWritable = "1";
  npmRebuildFlags = "--ignore-scripts";

  postPatch = ''
    mkdir -p "$TMPDIR/fake-bin"
    printf '#!/bin/sh\nexec true\n' > "$TMPDIR/fake-bin/elm-tooling"
    chmod +x "$TMPDIR/fake-bin/elm-tooling"
    export PATH="$TMPDIR/fake-bin:$PATH"
  '';

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/lib
    cp -r node_modules $out/lib/

    # Patch broken tailwind-resolver default export (upstream bug in 0.3.x)
    for f in \
      $out/lib/node_modules/tailwind-resolver/dist/index.mjs \
      $out/lib/node_modules/elm-tailwind-classes/node_modules/tailwind-resolver/dist/index.mjs; do
      if [ -f "$f" ]; then
        substituteInPlace "$f" --replace-quiet "u1 as default" "h1 as default"
      fi
    done

    # Patch elm-tailwind vite plugin: copy extractor to a writable tmpdir at runtime
    if [ -f "$out/lib/node_modules/elm-tailwind-classes/vite-plugin/index.js" ]; then
      substituteInPlace \
        "$out/lib/node_modules/elm-tailwind-classes/vite-plugin/index.js" \
        --replace-fail \
        "const bundledReviewConfig = path.resolve(__dirname, '..', 'extractor');" \
        "const bundledReviewConfig = (() => { const src = path.resolve(__dirname, '..', 'extractor'); const dst = path.join(process.env.TMPDIR || '/tmp', 'elm-tailwind-extractor'); try { fs.cpSync(src, dst, { recursive: true, force: true }); } catch(e) {} return dst; })();"
    fi

    # vite CLI
    makeWrapper ${pkgs.nodejs_22}/bin/node $out/bin/vite \
      --add-flags "$out/lib/node_modules/vite/bin/vite.js" \
      --prefix PATH : "$out/lib/node_modules/.bin" \
      --set NODE_PATH "$out/lib/node_modules"

    # elm-test CLI
    makeWrapper ${pkgs.nodejs_22}/bin/node $out/bin/elm-test \
      --add-flags "$out/lib/node_modules/elm-test/bin/elm-test" \
      --prefix PATH : "$out/lib/node_modules/.bin" \
      --prefix PATH : "${pkgs.elmPackages.elm}/bin" \
      --set NODE_PATH "$out/lib/node_modules"

    # elm-tailwind-classes CLI
    makeWrapper ${pkgs.nodejs_22}/bin/node $out/bin/elm-tailwind-classes \
      --add-flags "$out/lib/node_modules/elm-tailwind-classes/vite-plugin/cli.js" \
      --prefix PATH : "$out/lib/node_modules/.bin" \
      --set NODE_PATH "$out/lib/node_modules"

    runHook postInstall
  '';
}
