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

{
  description = "Mutastructura - database schema migrations powered by GNU Guile";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # guile-dbi and its drivers have no packages in nixpkgs yet, so the branches
    # of this fork that add them are inputs and the overlay below reads the
    # packages from those branches.  Drop an input and its overlay entry
    # together, once the branch is merged and nixos-unstable carries the
    # package.
    guile-dbi-src = {
      url = "github:jjba23/nixpkgs/guile-dbi-init";
      flake = false;
    };
    guile-dbd-sqlite3-src = {
      url = "github:jjba23/nixpkgs/guile-dbd-sqlite3-init";
      flake = false;
    };
    guile-dbd-mysql-src = {
      url = "github:jjba23/nixpkgs/guile-dbd-mysql-init";
      flake = false;
    };
    guile-dbd-postgresql-src = {
      url = "github:jjba23/nixpkgs/guile-dbd-postgresql-init";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , guile-dbi-src
    , guile-dbd-sqlite3-src
    , guile-dbd-mysql-src
    , guile-dbd-postgresql-src
    }:
    let
      # The launcher is a bash script that shells out to util-linux and
      # coreutils, so the package is as portable as those are.
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          (final: prev: {
            guile-dbi = final.callPackage
              "${guile-dbi-src}/pkgs/by-name/gu/guile-dbi/package.nix" { };
            guile-dbd-sqlite3 = final.callPackage
              "${guile-dbd-sqlite3-src}/pkgs/by-name/gu/guile-dbd-sqlite3/package.nix" { };
            guile-dbd-mysql = final.callPackage
              "${guile-dbd-mysql-src}/pkgs/by-name/gu/guile-dbd-mysql/package.nix" { };
            guile-dbd-postgresql = final.callPackage
              "${guile-dbd-postgresql-src}/pkgs/by-name/gu/guile-dbd-postgresql/package.nix" { };
          })
        ];
      };
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        {
          mutastructura = pkgs.callPackage ./default.nix { };
          default = self.packages.${system}.mutastructura;
        });

      apps = forAllSystems (system: {
        mutastructura = {
          type = "app";
          program = "${self.packages.${system}.mutastructura}/bin/mutastructura";
        };
        default = self.apps.${system}.mutastructura;
      });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        {
          default = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.mutastructura ];
            packages = with pkgs; [
              guile
              guile-dbi
              guile-dbd-sqlite3
              guile-dbd-mysql
              guile-dbd-postgresql
              guile-gcrypt
            ];
            shellHook = ''
              export GUILE_LOAD_PATH="$PWD/src''${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}"
            '';
          };
        });
    };
}
