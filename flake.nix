{
  description = "Elm quickies development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        buildScript = pkgs.writeShellApplication {
          name = "build-all";
          runtimeInputs = with pkgs; [
            elmPackages.elm
            uglify-js
            coreutils
            bash
          ];
          text = builtins.readFile ./build-all.sh;
        };
      in
      {
        packages.default = buildScript;

        apps.default = flake-utils.lib.mkApp {
          drv = buildScript;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs.elmPackages; [
            elm
            elm-live
            elm-format
            pkgs.gnumake
            pkgs.uglify-js
            buildScript
          ];
        };
      }
    );
}
