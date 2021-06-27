{ mkDerivation, async, base, brick, bytestring, containers
, envparse, extra, file-embed, haskell-coinbase-pro, haskii, hpack
, hspec, http-client, http-client-tls, katip, lens, microlens
, pretty-simple, retry, stdenv, stm, template-haskell, text, time
, unbounded-delays, universum, unliftio, vty
}:
mkDerivation {
  pname = "bitcoin-dashboard-tui";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base brick bytestring containers envparse extra file-embed
    haskell-coinbase-pro haskii hspec http-client http-client-tls katip
    lens microlens pretty-simple retry stm template-haskell text time
    unbounded-delays universum unliftio vty
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    async base brick bytestring containers envparse extra file-embed
    haskell-coinbase-pro haskii http-client http-client-tls katip lens
    microlens pretty-simple retry stm template-haskell text time
    unbounded-delays universum unliftio vty
  ];
  testHaskellDepends = [
    async base brick bytestring containers envparse extra file-embed
    haskell-coinbase-pro haskii hspec http-client http-client-tls katip
    lens microlens pretty-simple retry stm template-haskell text time
    unbounded-delays universum unliftio vty
  ];
  prePatch = "hpack";
  homepage = "https://github.com/tkachuk-labs/bitcoin-dashboard-tui#readme";
  license = stdenv.lib.licenses.bsd3;
}
