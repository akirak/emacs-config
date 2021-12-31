{
  description =
    "THIS IS AN AUTO-GENERATED FILE. PLEASE DON'T EDIT IT MANUALLY.";
  inputs = {
    benchmark-init = {
      flake = false;
      owner = "akirak";
      repo = "benchmark-init-el";
      type = "github";
    };
    bind-key = {
      flake = false;
      owner = "jwiegley";
      repo = "use-package";
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
    general = {
      flake = false;
      owner = "noctuid";
      repo = "general.el";
      type = "github";
    };
    poet-theme = {
      flake = false;
      owner = "kunalb";
      repo = "poet";
      type = "github";
    };
    use-package = {
      flake = false;
      owner = "jwiegley";
      repo = "use-package";
      type = "github";
    };
  };
  outputs = { ... }: { };
}
