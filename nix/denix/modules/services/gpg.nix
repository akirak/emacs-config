{
  delib,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "gpg";

  options = delib.singleEnableOption host.isDesktop;

  home.ifEnabled = {
    programs.gpg = {
      enable = true;
    };

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 60;
      defaultCacheTtlSsh = 60;
      # Explicitly set the pinentry package when using a non-standard window
      # manager setting
      # https://discourse.nixos.org/t/help-with-pinentrypackage/41393/8
      pinentry.package = pkgs.pinentry-gtk2;
      extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
      # pinentryFlavor = "gtk2";
      sshKeys = [
        "5B3390B01C01D3EE"
      ];
    };

    programs.bash.initExtra = ''
      gpg-connect-agent /bye
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    '';
  };
}
