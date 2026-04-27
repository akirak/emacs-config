{
  runCommandLocal,
  emacs,
}:
runCommandLocal "emacsclient" { propagatedBuildInputs = [ emacs ]; } ''
  mkdir -p $out/bin
  ln -t $out/bin -s ${emacs}/bin/emacsclient
''
