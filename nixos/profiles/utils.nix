{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    binutils
    coreutils
    curl
    dnsutils
    dosfstools
    git
    gptfdisk
    cryptsetup
    iputils
    moreutils
    nmap
    pciutils
    usbutils
    utillinux
    whois
    unzip
    lzip
  ];
}
