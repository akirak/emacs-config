{
  services.recoll = {
    enable = true;
    settings = {
      # TODO: Add support for extra (private) top directories
      topdirs = [
        "~/resources/bundles"
        "~/resources/ebooks"
        "~/resources/sound"
      ];

      excludedmimetypes = [
        "text/html"
        "application/x-mobipocket-ebook"
        "audio/flac"
      ];

      followLinks = true;
    };
  };
}
