# Profile for working with Android devices.
# See https://nixos.wiki/wiki/Android for information
{site, ...}: {
  programs.adb.enable = true;

  users.users.${site.username}.extraGroups = [
    "adbusers"
  ];
}
