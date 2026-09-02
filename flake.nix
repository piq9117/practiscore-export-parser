{
  description = "Basic haskell cabal template";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/3b9f00d7a7bf68acd4c4abb9d43695afb04e03a5;

  outputs = { self, nixpkgs }:
    let
      forAllSystems = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = final: prev: {
        hsPkgs = prev.haskell.packages.ghc9102.override {
          overrides = hfinal: hprev: { };
        };
        ps-tap = final.hsPkgs.callCabal2nix "ps-tap" ./. { };
        ps-gen = final.hsPkgs.callCabal2nix "ps-gen" ./. { };
      };

      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in
        {
          ps-tap = pkgs.ps-tap;
          ps-gen = pkgs.ps-gen;
          check-formatting = pkgs.writeShellApplication {
            name = "check-formatting";
            runtimeInputs = with pkgs; [
              nixpkgs-fmt
              treefmt
              hsPkgs.cabal-fmt
              ormolu
            ];
            text = ''
              ${pkgs.treefmt}/bin/treefmt --version
              ${pkgs.treefmt}/bin/treefmt

              if [[ -n "$(git diff --stat)" ]]; then
                git status
                echo "FAIL: found some changes"
                git diff
                exit 1
              fi
            '';
          };
        });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          libs = with pkgs; [
            zlib
          ];
        in
        {
          default = pkgs.hsPkgs.shellFor {
            packages = hsPkgs: [ pkgs.ps-tap pkgs.ps-gen ];
            buildInputs = with pkgs; [
              hsPkgs.cabal-install
              hsPkgs.cabal-fmt
              hsPkgs.ghc
              ormolu
              treefmt
              nixpkgs-fmt
            ] ++ libs;
            nativeBuildInputs = with pkgs; [
              hsPkgs.cabal-install
            ];
            shellHook = "export PS1='[$PWD]\n❄ '";
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
          };
        });
    };
}
