{pkgs, ...}: {
  home.packages = with pkgs; [
    gnupg
  ];

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 60;
    defaultCacheTtlSsh = 60;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
    # pinentryFlavor = "gtk2";
    sshKeys = [
      "5B3390B01C01D3EE"
    ];
  };

  programs.bash = {
    initExtra = ''
      # Use gpg-agent as ssh-agent.
      gpg-connect-agent /bye
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    '';
  };

  programs.git.signing.key = "5B3390B01C01D3E";
}
