{pkgs, ...}: {
  i18n.inputMethod = {
    enabled = "fcitx";
    fcitx.engines = with pkgs.fcitx-engines; [mozc];
  };
}
