{ delib, pkgs, ... }:
delib.module {
  name = "zsh.starship";

  home.always =
    let
      configFile = ./etc/starship.toml;
    in
    {
      programs.zsh = {
        initContent = ''
          export STARSHIP_CONFIG=${configFile}

          eval "$(${pkgs.starship}/bin/starship init zsh)"
        '';
      };
    };
}
