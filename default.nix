# mutastructura

# Copyright © Josep Bigorra <jjbigorra@gmail.com>

# mutastructura is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# mutastructura is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with mutastructura.  If not, see <https://www.gnu.org/licenses/>.

# The package of this checkout as it stands.  It is the same derivation as the
# `mutastructura' package in nixpkgs -- same phases, same layout under $out --
# so that a change which breaks this build breaks that one for the same reason.
# What differs is the source: this file builds the working tree, while nixpkgs
# pins a released tag and its hash.  Keep `version' in step with the tag you
# release, because nothing here can read it from the library.

{ lib
, stdenv
, guile
, makeWrapper
, bash
, coreutils
, util-linux
, guile-dbi
, guile-dbd-sqlite3
, guile-dbd-mysql
, guile-dbd-postgresql
, guile-gcrypt
}:

let
  # The libraries whose modules the CLI loads at run time.  They are named once
  # and reused by the wrapper and by the check below.
  guileDeps = [
    guile-dbi
    guile-dbd-sqlite3
    guile-dbd-mysql
    guile-dbd-postgresql
    guile-gcrypt
  ];
in
stdenv.mkDerivation {
  pname = "mutastructura";
  version = "0.9.2";

  src = ./.;

  nativeBuildInputs = [
    guile
    makeWrapper
  ];

  buildInputs = [ guile ];

  propagatedBuildInputs = guileDeps;

  buildPhase = ''
    runHook preBuild

    siteDir=$out/${guile.siteDir}
    install -d "$siteDir"
    cp -R src/. "$siteDir"/

    export GUILE_AUTO_COMPILE=0
    export GUILE_LOAD_PATH="$siteDir''${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}"

    find src -name '*.scm' | while IFS= read -r file; do
      rel="''${file#src/}"
      guild compile -o "$siteDir/''${rel%.scm}.go" "$file"
    done

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    install -Dm755 scripts/mutastructura $out/bin/mutastructura
    cp -R resources $out/share/resources

    runHook postInstall
  '';

  postFixup = ''
    wrapProgram $out/bin/mutastructura \
      --prefix PATH : ${
        lib.makeBinPath [
          bash
          coreutils
          util-linux
          guile
        ]
      } \
      --prefix GUILE_LOAD_PATH : "$out/${guile.siteDir}:${
        lib.makeSearchPath "share/guile/site/3.0" guileDeps
      }" \
      --prefix GUILE_LOAD_COMPILED_PATH : "$out/${guile.siteDir}:${
        lib.makeSearchPath "share/guile/site/3.0" guileDeps
      }" \
      --suffix GUILE_DBD_PATH : "${
        lib.makeSearchPath "lib" [
          guile-dbd-sqlite3
          guile-dbd-mysql
          guile-dbd-postgresql
        ]
      }"
  '';

  doCheck = true;

  checkPhase = ''
    runHook preCheck

    # Ask for a module that pulls in both dbi and this package's own code.  The
    # load path is spelled out because a plain build has no setup hook that
    # would put the dependencies on it.
    GUILE_AUTO_COMPILE=0 \
      GUILE_LOAD_PATH="$out/${guile.siteDir}:${
        lib.makeSearchPath "share/guile/site/3.0" guileDeps
      }" \
      guile -c '(use-modules (mutastructura persistence migration-runner))'

    runHook postCheck
  '';

  meta = with lib; {
    description = "Relational database schema migrations powered by GNU Guile";
    homepage = "https://codeberg.org/jjba23/mutastructura";
    license = licenses.agpl3Plus;
    mainProgram = "mutastructura";
    platforms = platforms.linux;
  };
}
