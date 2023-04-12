let
  myNixPkgs = import <nixpkgs> {};
in
myNixPkgs.mkShell {
  buildInputs = with myNixPkgs; [
    cabal-install
    ghc
    haskell-language-server
  ];
}
