let hsPkgs = import ./default.nix {};
in hsPkgs.shellFor {
  packages = ps: [ ps.hasktags ];
  withHoogle = true;
  tools = import ./tools.nix;
}
