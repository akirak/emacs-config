{ twist, pkgs, lib, linkFarmFromDrvs, writeShellApplication, archiveName, }:
let buildArchive = twist.lib.buildElpaArchive pkgs;
in packageInputs:
let
  archiveRoot = lib.pipe packageInputs [
    (lib.mapAttrsToList (_: buildArchive))
    (linkFarmFromDrvs "elpa-archive")
  ];
in writeShellApplication {
  name = "build-elpa-archive";
  text = ''
    root="${archiveRoot}"
    outFile="${archiveName}.tar.gz"
    tar --mode 'u+w' -czf "$outFile" -C "$root" -h .
    echo >&2 "Created $outFile"
  '';
}
