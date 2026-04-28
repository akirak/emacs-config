{ delib, ... }:
delib.host {
  name = "default-x86_64-linux";

  useHomeManagerModule = true;

  type = "desktop";

  system = "x86_64-linux";

  home = {
    home.stateVersion = "26.05";
  };
}
