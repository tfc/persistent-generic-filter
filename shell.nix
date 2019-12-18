let
  hsPkgs = import ./default.nix;
in
  hsPkgs.haskell-generics-persistent.components.all
