with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "none";
  inherit ghc;
  buildInputs = [
    pkgconfig
    zlib

    jq

    # for `git:` references in stack.yml:
    git
  ];
}
