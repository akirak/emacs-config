{
  fileSystems."/git-annex/personal" = {
    device = "rpool2/git-annex/personal";
    fsType = "zfs";
    options = ["noatime"];
  };

  fileSystems."/assets/archives/personal/git" = {
    device = "rpool2/git/private";
    fsType = "zfs";
    options = ["noatime"];
  };

  fileSystems."/assets/archives/oss" = {
    device = "rpool2/git/oss";
    fsType = "zfs";
    options = ["noatime"];
  };

  fileSystems."/assets/work" = {
    device = "rpool2/git/work";
    fsType = "zfs";
    options = ["noatime"];
  };

  fileSystems."/assets/archives/personal/appdata" = {
    device = "rpool2/media/appdata";
    fsType = "zfs";
    options = ["noatime"];
  };

  fileSystems."/assets/archives/hoard" = {
    device = "rpool2/media/hoard";
    fsType = "zfs";
    options = ["noatime"];
  };
}
