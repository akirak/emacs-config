{
  programs.emacs-unsafe = {
    enable = true;

    extraInitText = builtins.readFile ./extra-init.el;

    extraDirsToTryBind = [
      "/run/user/$(id -u)"
    ];
  };
}
