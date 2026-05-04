{ delib, host, ... }:
delib.module {
  name = "recoll";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.isDesktop;

      # TODO: Add support for extra (private) top directories
      topdirs = listOfOption str [
        "~/resources/bundles"
        "~/resources/ebooks"
        "~/resources/sound"
        "~/notes-and-pdfs/notes"
        "~/notes-and-pdfs/ebooks"
      ];
    };

  home.ifEnabled =
    { cfg, ... }:
    {
      services.recoll = {
        enable = true;
        settings = {
          followLinks = true;

          topdirs = cfg.topdirs;

          excludedmimetypes = [
            "text/html"
            "application/x-mobipocket-ebook"
            "audio/flac"
          ];
        };
      };
    };
}
