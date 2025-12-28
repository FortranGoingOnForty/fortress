{
  description = "FORTRESS - A command-line file explorer written in modern Fortran";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        fortress = pkgs.stdenv.mkDerivation rec {
          pname = "fortress";
          version = "1.0.2";

          src = ./.;

          nativeBuildInputs = with pkgs; [
            gfortran
            fortran-fpm
            makeWrapper
          ];

          buildInputs = with pkgs; [
            ncurses
          ];

          buildPhase = ''
            # fpm needs a writable home for cache
            export HOME=$(mktemp -d)
            fpm build --flag "-O2"
          '';

          installPhase = ''
            # Install binary to lib directory
            mkdir -p $out/lib/fortress
            cp build/gfortran_*/app/fortress $out/lib/fortress/fortress

            # Create wrapper script in bin
            mkdir -p $out/bin
            makeWrapper $out/lib/fortress/fortress $out/bin/fortress

            # Install shell integration files
            mkdir -p $out/share/fortress
            cp fortress.sh $out/share/fortress/
            cp fortress.fish $out/share/fortress/

            # Patch fortress.sh to find binary in nix store
            substituteInPlace $out/share/fortress/fortress.sh \
              --replace '/usr/lib/fortress/fortress' "$out/lib/fortress/fortress"

            # Install docs
            mkdir -p $out/share/doc/fortress
            cp README.md $out/share/doc/fortress/
            [ -f USAGE.md ] && cp USAGE.md $out/share/doc/fortress/ || true
          '';

          meta = with pkgs.lib; {
            description = "A command-line file explorer written in modern Fortran with fzf integration";
            homepage = "https://github.com/FortranGoingOnForty/fortress";
            license = licenses.mit;
            platforms = platforms.unix;
            mainProgram = "fortress";
          };
        };
      in
      {
        packages = {
          default = fortress;
          fortress = fortress;
        };

        apps.default = {
          type = "app";
          program = "${fortress}/bin/fortress";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            gfortran
            fortran-fpm
            ncurses
            fzf
            git
          ];
        };
      }
    );
}
