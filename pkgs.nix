{
  extras = hackage:
    {
      packages = {
        persistent-generic-filter = ./persistent-generic-filter.nix;
        };
      };
  resolver = "lts-14.17";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }
