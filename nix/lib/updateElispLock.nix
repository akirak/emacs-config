{
  writeShellApplication,
  deno,
  lockDirName,
}:
writeShellApplication {
  name = "update-elisp-lock";
  runtimeInputs = [ deno ];
  text = ''
    cd ${lockDirName}
    deno run --allow-read --allow-run ${./update-elisp-lock.ts}
  '';
}
