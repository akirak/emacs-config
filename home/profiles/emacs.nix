{
  programs.emacs-unsafe = {
    enable = true;

    extraInitText = ''
      (require 'sanityinc-tomorrow-eighties-theme)
      (load-theme 'sanityinc-tomorrow-eighties t)
    '';
  };
}
