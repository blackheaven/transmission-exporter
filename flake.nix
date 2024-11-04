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

        defaultPackage = packages.transmission-exporter;

        devShell =
          pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
              ormolu
            ];
            inputsFrom = [ self.defaultPackage.${system}.env ];
          };
      });
}
