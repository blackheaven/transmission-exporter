{
  description = "transmission-exporter";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    # flake-utils.lib.eachDefaultSystem
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        _jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        haskellPackages = pkgs.haskellPackages.override
          {
            overrides = hself: hsuper: { };
          };

        nixpkgsOverlay = _final: _prev: {
          transmission-exporter = self.packages.${system}.transmission-exporter;
        };

      in
      rec {
        packages.transmission-exporter =
          pkgs.haskell.lib.justStaticExecutables
            (haskellPackages.callCabal2nix "transmission-exporter" ./. {});
        packages.transmission-exporter-image = pkgs.dockerTools.buildImage {
          name = "blackheaven/transmission-exporter";
          tag = "latest";

          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths =
              [ pkgs.cacert self.packages.${system}.transmission-exporter ];
            pathsToLink = [ "/bin" "/etc" ];
          };
          config = {
            Entrypoint = [ "/bin/transmission-exporter" ];
            Env = [
              # "LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive"
              # "LC_TIME=en_US.UTF-8"
              # "LANG=en_US.UTF-8"
              # "LANGUAGE=en"
              # "LC_ALL=en_US.UTF-8"
              "TRANSMISSION_ADDR=http://transmission:9091/transmission/rpc"
            ];
          };
        };

        packages.default = packages.transmission-exporter;

        overlays = nixpkgsOverlay;

        nixosModules.default =
          { pkgs, lib, config, ... }:
          let
            cfg = config.services.transmission-exporter;
          in
          {
            options = with lib; {
                services.transmission-exporter = {
                  enable = mkEnableOption "Efficient Transmission-bt Prometheus exporter";
                  package = lib.mkPackageOption pkgs "transmission-exporter" {};
                  transmissionAddr = lib.mkOption {
                    type = types.str;
                    default = "http://localhost:9091";
                  };
                  transmissionUser = lib.mkOption {
                    type = types.str;
                  };
                  transmissionPasswordFile = lib.mkOption {
                    type = types.path;
                  };
                  openFirewall = lib.mkOption {
                    type = types.bool;
                    default = false;
                  };
                  webListenAddr = lib.mkOption {
                    type = types.str;
                    default = "0.0.0.0";
                  };
                  webListenPort = lib.mkOption {
                    type = types.port;
                    default = 19091;
                  };
                  webEndpointPath = lib.mkOption {
                    type = types.str;
                    default = "/metrics";
                  };
              };
            };
            config = lib.mkIf cfg.enable {
              nixpkgs.overlays = [ nixpkgsOverlay ];

              networking.firewall.allowedTCPPorts = lib.optional cfg.openFirewall cfg.webListenPort;

              systemd.services.transmission-exporter = {
                description = "Efficient Transmission-bt Prometheus exporter";
                after = [ "network.target" ];
                wantedBy = [ "multi-user.target" ];
                script = ''
                  export TRANSMISSION_PASSWORD=$(cat ${cfg.transmissionPasswordFile})
                  exec ${lib.getExe cfg.package}
                '';
                serviceConfig = {
                  Environment = [
                    "WEB_PATH=${cfg.webEndpointPath}"
                    "WEB_ADDR=${cfg.webListenAddr}:${builtins.toString cfg.webListenPort}"
                    "TRANSMISSION_ADDR=${cfg.transmissionAddr}"
                    "TRANSMISSION_USERNAME=${cfg.transmissionUser}"
                  ];
                };
              };
            };
          };

        devShells.default =
          pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
              ormolu
            ];
            inputsFrom = [ self.packages.${system}.default.env ];
          };
      });
}
