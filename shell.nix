let
  hsPkgs = import ./default.nix;
in
  hsPkgs.persistent-generic-filter.components.all
