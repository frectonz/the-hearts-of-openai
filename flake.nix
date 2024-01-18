{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      with pkgs;{
        devShells.default = pkgs.mkShell {
          buildInputs = [
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-language-server

            nodejs
            nodePackages.pnpm
            nodePackages.typescript
            nodePackages.typescript-language-server
            nodePackages.vscode-langservers-extracted
          ];
        };

        formatter = nixpkgs-fmt;
      });
}
