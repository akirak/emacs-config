{
  delib,
  pkgs,
  enableWayland,
  ...
}:
let
  defaultWrapper = if enableWayland then "lxqt" else "openssh-askpass";
in
delib.module {
  name = "ssh-askpass";

  options =
    with delib;
    moduleOptions {
      enable = boolOption enableWayland;

      wrapper = enumOption [
        "lxqt"
        "openssh-askpass"
        # currently broken
        "wayprompt"
      ] defaultWrapper;
    };

  home.ifEnabled = { cfg, ... }: {
    home.sessionVariables = {
      SSH_ASKPASS =
        if cfg.wrapper == "lxqt" then
          "${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass"
        else if cfg.wrapper == "openssh-askpass" then
          "${pkgs.openssh-askpass}/bin/openssh-askpass"
        else if cfg.wrapper == "wayprompt" then
          "${pkgs.wayprompt}/bin/wayprompt-ssh-askpass"
        else
          throw "Unsupported value for ssh-askpass.wrapper: ${cfg.wrapper}";
      SSH_ASKPASS_REQUIRE = "prefer";
    };
  };
}
