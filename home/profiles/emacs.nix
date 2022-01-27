{
  programs.emacs-unsafe = {
    enable = true;

    extraInitText = ''
      (require 'doom-themes)
      (load-theme 'doom-one t)
    '';
  };
}
