let nixpkgs20 = import (import ./nixpkgs20.nix) {};
    nixpkgs21 = import (import ./nixpkgs21.nix) {};
in
{
  hexOrganization,
  hexApiKey,
  robotSshKey,
  vimBackground ? "light",
  vimColorScheme ? "PaperColor"
}:
[
  (self: super:
    let
      callPackage = self.lib.callPackageWith self.haskellPackages;
      dontCheck = self.haskell.lib.dontCheck;
      doJailbreak = self.haskell.lib.doJailbreak;
    in
      {
        ghcjs = nixpkgs20.haskell.compiler.ghcjs;
        stack2cabal = doJailbreak nixpkgs21.haskellPackages.stack2cabal;
        haskell-ide = import (
          fetchTarball "https://github.com/tim2CF/ultimate-haskell-ide/tarball/605191e71ecc13b07471c8859b91a2d3e2267485"
        ) {inherit vimBackground vimColorScheme;};
        haskellPackages = super.haskell.packages.ghc865.extend(
          self': super': {
            universum = dontCheck super'.universum;
            persistent-migration = dontCheck (
              callPackage ./overlay/persistent-migration.nix {
                stdenv = self.stdenv;
                fetchgit = self.fetchgit;
              });
            hspec-wai = callPackage ./overlay/hspec-wai.nix {
              stdenv = self.stdenv;
            };
            hspec-wai-json = callPackage ./overlay/hspec-wai-json.nix {};
            scotty = callPackage ./overlay/scotty.nix {};
            HaskellNet = callPackage ./overlay/haskell-net.nix {};
            concur-core = callPackage ./overlay/concur-core.nix {
              stdenv = self.stdenv;
              fetchgit = self.fetchgit;
            };
            replica = callPackage ./overlay/replica.nix {
              stdenv = self.stdenv;
              fetchgit = self.fetchgit;
            };
            concur-replica = callPackage ./overlay/concur-replica.nix {
              stdenv = self.stdenv;
              fetchgit = self.fetchgit;
            };
            proto3-suite = dontCheck (doJailbreak super'.proto3-suite);
          }
        );
      })
]
