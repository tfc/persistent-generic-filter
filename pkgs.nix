{
  extras = hackage:
    {
      packages = {
        haskell-generics-persistent = ./haskell-generics-persistent.nix;
        };
      };
  resolver = "lts-14.17";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }