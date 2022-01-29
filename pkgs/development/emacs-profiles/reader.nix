{ emacsSandboxed }:
emacsSandboxed {
  name = "emacs-reader";

  extraFeatures = [ ];
  extraInitFiles = [ ];
  extraInitText = ''
    (require 'poet-theme)
    (load-theme 'poet t)
  '';

  protectHome = true;

  extraDirsToTryBind = [
    "$HOME/org"
    "$HOME/resources"
  ];

  # For eww
  shareNet = true;

  inheritPath = false;

  userEmacsDirectory = "$HOME/resources/emacs-var";
}
