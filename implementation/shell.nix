with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "none";
  inherit ghc;
  buildInputs = [
    # general haskell stuff
    pkgconfig
    zlib

    # for the tests (not currently used)
    jq

    # for `git:` references in stack.yml:
    git

    # for intero
    ncurses
  ];
}
