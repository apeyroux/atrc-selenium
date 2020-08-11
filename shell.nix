((import ./.).atrc-selenium.components.exes.atrc-selenium // {
  env = (import ./.).shellFor {
    packages = p: [ p.atrc-selenium ];
    exactDeps = true;
    tools = {
      cabal = "3.2.0.0";
      hie = "unstable";
    };
    shellHook = ''
      export HIE_HOOGLE_DATABASE=$(realpath "$(dirname "$(realpath "$(which hoogle)")")/../share/doc/hoogle/default.hoo")
    '';
  };
}).env
