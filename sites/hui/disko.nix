{ luksKey ? "/persist/luks.key", ... }:
{
  disk = {

    ssd = {
      device = "/dev/nvme0n1";
      type = "disk";
      content = {
        type = "table";
        format = "gpt";
        partitions = [

          {
            type = "partition";
            name = "ESP";
            start = "0";
            end = "100MiB";
            bootable = true;
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [
                "defaults"
              ];
            };
          }

          {
            type = "partition";
            name = "luks";
            start = "100MiB";
            end = "-8G";
            content = {
              type = "luks";
              name = "cryptroot";
              keyFile = luksKey;
              content = {
                type = "btrfs";
                extraArgs = "-f";
                subvolumes = {
                  "/root" = {
                    mountpoint = "/";
                    mountOptions = ["discard=async"];
                  };
                  "/nix" = {
                    mountOptions = ["compress=lz4" "noatime" "discard=async"];
                  };
                  # "/home" = {
                  #   mountOptions = ["discard=async"];
                  # };
                };
              };
            };
          }

          {
            name = "swap";
            type = "partition";
            start = "-8G";
            end = "100%";
            part-type = "primary";
            content = {
              type = "swap";
              randomEncryption = true;
            };
          }
        ];
      };
    };

    # mmc = {
    #   device = "/dev/mmcblk0";
    #   type = "disk";
    #   content = {
    #     type = "table";
    #     format = "gpt";
    #     partitions = [
    #       {
    #         type = "partition";
    #         name = "luks";
    #         start = "0";
    #         end = "100%";
    #         content = {
    #           type = "luks";
    #           name = "cryptdata";
    #           keyFile = luksKey;
    #           content = {
    #             type = "lvm_pv";
    #             vg = "data";
    #           };
    #         };
    #       }
    #     ];
    #   };
    # };
  };

  # lvm_vg = {
  #   data = {
  #     type = "lvm_vg";
  #     lvs = {
  #       home = {
  #         type = "lvm_lv";
  #         size = "50G";
  #         content = {
  #           type = "filesystem";
  #           format = "ext4";
  #           mountpoint = "/home";
  #         };
  #       };
  #     };
  #   };
  # };
}
