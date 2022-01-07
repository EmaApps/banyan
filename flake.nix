{
  description = "Banyan";
  inputs = {
    banyan-emanote.url = "github:srid/emanote/master";
    nixpkgs.follows = "banyan-emanote/nixpkgs";

    NanoID = {
      url = "github:srid/NanoID/srid";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        name = "banyan";
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
        tailwindPkgs = import ./tailwind/default.nix {
          inherit pkgs system;
        };
        # Based on https://github.com/input-output-hk/daedalus/blob/develop/yarn2nix.nix#L58-L71
        filter = name: type:
          let
            baseName = baseNameOf (toString name);
            sansPrefix = pkgs.lib.removePrefix (toString ./.) name;
          in
          # Ignore these files when building source package
            !(
              baseName == "README.md" ||
              sansPrefix == "/bin" ||
              sansPrefix == "/content" ||
              sansPrefix == "/.github" ||
              sansPrefix == "/.vscode" ||
              sansPrefix == "/.ghcid"
            );
        m1MacHsBuildTools =
          pkgs.haskellPackages.override {
            overrides = self: super:
              let
                workaround140774 = hpkg: with pkgs.haskell.lib;
                  overrideCabal hpkg (drv: {
                    enableSeparateBinOutput = false;
                  });
              in
              {
                ghcid = workaround140774 super.ghcid;
                ormolu = workaround140774 super.ormolu;
              };
          };
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv name;
            root = pkgs.lib.cleanSourceWith { inherit filter name; src = ./.; };
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              ema = disableCabalFlag inputs.banyan-emanote.inputs.ema.defaultPackage.${system} "with-examples";
              emanote = inputs.banyan-emanote.defaultPackage.${system};
              NanoID = self.callCabal2nix "NanoID" inputs.NanoID { };
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with (if system == "aarch64-darwin"
                then m1MacHsBuildTools
                else pkgs.haskellPackages); [
                  cabal-fmt
                  cabal-install
                  ghcid
                  haskell-language-server
                  ormolu
                  pkgs.nixpkgs-fmt
                  (
                    let
                      p = tailwindPkgs.package;
                      node_modules = "${p}/lib/node_modules/banyon-tailwind/node_modules";
                    in
                    pkgs.writeShellScriptBin "tailwind"
                      ''
                        export NODE_PATH=${node_modules}
                        exec ${node_modules}/.bin/tailwind $*
                      ''
                  )
                ]);
          };
      in
      {
        # Used by `nix build` & `nix run`
        defaultPackage = project false;

        # Used by `nix develop`
        devShell = project true;
      });
}
