{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    binutils
    coreutils
    curl
    dnsutils
    dosfstools
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
  ];
}
