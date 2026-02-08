{
  description = "ERHTMX Adventure - An Erlang + HTMX + Canvas RPG Game";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      # Overlay for use in other NixOS configurations
      overlay = final: prev: {
        erhtmx-adventure = final.callPackage ({ stdenv, beam, lib }:
          let
            beamPkgs = beam.packages.erlang_26;
            erlang = beamPkgs.erlang;
            rebar3 = beamPkgs.rebar3;

            # Build dependencies from hex
            ranch = beamPkgs.buildHex {
              name = "ranch";
              version = "1.8.0";
              sha256 = "sha256-SfvP02gvqx9dEJNRthJXZ22hov2+KVkEF21eUhot3+U=";
            };

            cowlib = beamPkgs.buildHex {
              name = "cowlib";
              version = "2.13.0";
              sha256 = "sha256-4eEoTcP8Awpksa0Ng4KufpnaRsMka4FTGKS4SIc4AKQ=";
              beamDeps = [ ];
            };

            cowboy = beamPkgs.buildHex {
              name = "cowboy";
              version = "2.12.0";
              sha256 = "sha256-inq+bRgzcs6yHKonCb7JKKsrcuGKORGqF3Fjm++CZR4=";
              beamDeps = [ cowlib ranch ];
            };

            jsx = beamPkgs.buildHex {
              name = "jsx";
              version = "3.1.0";
              sha256 = "sha256-DFzI/cEbU8wlz2WsZwWtOeVOzFbRwi5K249aU/uUJ/M=";
            };

          in stdenv.mkDerivation {
            pname = "erhtmx-adventure";
            version = "0.1.0";

            src = self;

            nativeBuildInputs = [ rebar3 ];
            buildInputs = [ erlang ];

            configurePhase = ''
              runHook preConfigure
              export HOME=$TMPDIR

              # Link beamDeps into _build/default/lib and _build/prod/lib
              # buildHex packages have structure: lib/erlang/lib/<name>-<version>/
              mkdir -p _build/default/lib _build/prod/lib
              for dep in ${cowboy} ${cowlib} ${ranch} ${jsx}; do
                for pkgdir in $dep/lib/erlang/lib/*; do
                  pkgname=$(basename $pkgdir | sed 's/-[0-9].*//')
                  if [ ! -e "_build/default/lib/$pkgname" ]; then
                    cp -r $pkgdir _build/default/lib/$pkgname
                    chmod -R u+w _build/default/lib/$pkgname
                  fi
                  if [ ! -e "_build/prod/lib/$pkgname" ]; then
                    cp -r $pkgdir _build/prod/lib/$pkgname
                    chmod -R u+w _build/prod/lib/$pkgname
                  fi
                done
              done

              # Remove the deps from rebar.config to prevent rebar3 from checking them
              # We manually provide the built deps
              cat > rebar.config.new << 'REBAR_CONFIG'
{erl_opts, [debug_info]}.
{deps, []}.
{relx, [
    {release, {erhtmx_adventure, "0.1.0"}, [
        erhtmx_adventure,
        cowboy,
        jsx,
        sasl
    ]},
    {dev_mode, false},
    {include_erts, true},
    {extended_start_script, true},
    {lib_dirs, ["_build/prod/lib"]}
]}.
{profiles, [
    {prod, [
        {relx, [
            {dev_mode, false},
            {include_erts, true}
        ]}
    ]}
]}.
REBAR_CONFIG
              mv rebar.config.new rebar.config

              # Remove the lock file since we're not using deps
              rm -f rebar.lock

              runHook postConfigure
            '';

            buildPhase = ''
              runHook preBuild
              export HOME=$TMPDIR
              rebar3 as prod release
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out/bin
              cp -r _build/prod/rel/erhtmx_adventure/* $out/

              # Create a wrapper script in bin/ for easy execution
              cat > $out/bin/erhtmx-adventure <<'EOF'
#!/bin/sh
exec "$(dirname "$0")/../bin/erhtmx_adventure" foreground "$@"
EOF
              chmod +x $out/bin/erhtmx-adventure
              runHook postInstall
            '';

            meta = {
              description = "An Erlang + HTMX + Canvas RPG Game";
              homepage = "https://github.com/ilioscio/erhtmx-adventure";
              mainProgram = "erhtmx-adventure";
              license = lib.licenses.mit;
            };
          }
        ) {};
      };
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };

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

        packages.default = pkgs.erhtmx-adventure;

        # App for `nix run github:ilioscio/erhtmx-adventure`
        apps.default = {
          type = "app";
          program = "${pkgs.erhtmx-adventure}/bin/erhtmx-adventure";
        };
      }
    ) // {
      # Expose overlay for use in other flakes
      overlays.default = overlay;

      # NixOS module for easy integration
      nixosModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.services.erhtmx-adventure;
        in {
          options.services.erhtmx-adventure = {
            enable = lib.mkEnableOption "ERHTMX Adventure game server";

            port = lib.mkOption {
              type = lib.types.port;
              default = 8080;
              description = "Port for the ERHTMX Adventure server to listen on";
            };

            openFirewall = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = "Whether to open the firewall port";
            };

            nginx = {
              enable = lib.mkEnableOption "nginx reverse proxy for ERHTMX Adventure";

              virtualHost = lib.mkOption {
                type = lib.types.str;
                default = "erhtmx-adventure.localhost";
                description = "Virtual host name for nginx";
              };

              forceSSL = lib.mkOption {
                type = lib.types.bool;
                default = false;
                description = "Force SSL for the virtual host";
              };

              enableACME = lib.mkOption {
                type = lib.types.bool;
                default = false;
                description = "Enable ACME (Let's Encrypt) for the virtual host";
              };
            };
          };

          config = lib.mkIf cfg.enable {
            nixpkgs.overlays = [ overlay ];

            systemd.services.erhtmx-adventure = {
              description = "ERHTMX Adventure Game Server";
              wantedBy = [ "multi-user.target" ];
              after = [ "network.target" ];

              environment = {
                PORT = toString cfg.port;
              };

              serviceConfig = {
                Type = "exec";
                ExecStart = "${pkgs.erhtmx-adventure}/bin/erhtmx-adventure";
                Restart = "on-failure";
                RestartSec = "5s";

                # Security hardening
                DynamicUser = true;
                NoNewPrivileges = true;
                ProtectSystem = "strict";
                ProtectHome = true;
                PrivateTmp = true;
                PrivateDevices = true;
                ProtectKernelTunables = true;
                ProtectKernelModules = true;
                ProtectControlGroups = true;
                RestrictAddressFamilies = [ "AF_INET" "AF_INET6" ];
                RestrictNamespaces = true;
                LockPersonality = true;
                MemoryDenyWriteExecute = false; # Erlang needs this
                RestrictRealtime = true;
              };
            };

            networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ cfg.port ];

            services.nginx = lib.mkIf cfg.nginx.enable {
              enable = true;
              virtualHosts.${cfg.nginx.virtualHost} = {
                forceSSL = cfg.nginx.forceSSL;
                enableACME = cfg.nginx.enableACME;
                locations."/" = {
                  proxyPass = "http://127.0.0.1:${toString cfg.port}";
                  proxyWebsockets = true;
                  extraConfig = ''
                    proxy_set_header Host $host;
                    proxy_set_header X-Real-IP $remote_addr;
                    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                    proxy_set_header X-Forwarded-Proto $scheme;
                  '';
                };
              };
            };
          };
        };
    };
}
