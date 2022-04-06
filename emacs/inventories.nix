inputs: [
  {
    type = "melpa";
    path = inputs.melpa.outPath + "/recipes";
  }
  {
    type = "elpa";
    path = inputs.gnu-elpa.outPath + "/elpa-packages";
    core-src = inputs.emacs.outPath;
    auto-sync-only = true;
  }
  {
    type = "archive";
    url = "https://elpa.gnu.org/packages/";
  }
  {
    type = "archive";
    url = "https://elpa.nongnu.org/nongnu/";
  }
  {
    type = "gitmodules";
    path = inputs.epkgs.outPath + "/.gitmodules";
  }
]
