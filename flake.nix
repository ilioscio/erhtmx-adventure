{
  description = "ERHTMX Adventure - An Erlang + HTMX + Canvas RPG Game";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Erlang/OTP version - use beam packages for proper integration
        beamPkgs = pkgs.beam.packages.erlang_26;
        erlang = beamPkgs.erlang;

        # Rebar3 from beam packages
        rebar3 = beamPkgs.rebar3;

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            erlang
            rebar3
            pkgs.git
          ];

          shellHook = ''
            echo "ERHTMX Adventure Development Environment"
            echo "Erlang: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>&1 | tr -d '"')"
            echo "Rebar3: $(rebar3 version 2>&1 | head -1)"
            echo ""
            echo "Commands:"
            echo "  rebar3 compile    - Compile the project"
            echo "  rebar3 shell      - Start an Erlang shell with the project"
            echo "  ./run.sh          - Run the game server"
          '';
        };

        packages.default = pkgs.stdenv.mkDerivation {
          pname = "erhtmx-adventure";
          version = "0.1.0";

          src = ./.;

          buildInputs = [ erlang rebar3 ];

          buildPhase = ''
            export HOME=$TMPDIR
            rebar3 as prod release
          '';

          installPhase = ''
            mkdir -p $out
            cp -r _build/prod/rel/erhtmx_adventure/* $out/
          '';
        };
      }
    );
}
