{
  description =
    "THIS IS AN AUTO-GENERATED FILE. PLEASE DON'T EDIT IT MANUALLY.";
  inputs = {
    beancount = {
      flake = false;
      owner = "beancount";
      repo = "beancount-mode";
      type = "github";
    };
    benchmark-init = {
      flake = false;
      owner = "akirak";
      repo = "benchmark-init-el";
      type = "github";
    };
    dash = {
      flake = false;
      owner = "magnars";
      repo = "dash.el";
      type = "github";
    };
    doom-themes = {
      flake = false;
      owner = "doomemacs";
      repo = "themes";
      type = "github";
    };
    gcmh = {
      flake = false;
      type = "git";
      url = "https://gitlab.com/koral/gcmh.git";
    };
    git-commit = {
      flake = false;
      owner = "magit";
      repo = "magit";
      type = "github";
    };
    magit = {
      flake = false;
      owner = "magit";
      repo = "magit";
      type = "github";
    };
    magit-section = {
      flake = false;
      owner = "magit";
      repo = "magit";
      type = "github";
    };
    poet-theme = {
      flake = false;
      owner = "kunalb";
      repo = "poet";
      type = "github";
    };
    which-key = {
      flake = false;
      owner = "justbur";
      repo = "emacs-which-key";
      type = "github";
    };
    with-editor = {
      flake = false;
      owner = "magit";
      repo = "with-editor";
      type = "github";
    };
  };
  outputs = { ... }: { };
}
