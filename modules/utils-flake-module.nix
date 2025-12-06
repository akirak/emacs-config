{
  perSystem =
    { pkgs, ... }:
    {
      packages = {
        update-ai-models =
          pkgs.writers.writePython3Bin "write-ai-models"
            {
              libraries = with pkgs.python3Packages; [ litellm ];
            }
            ''
              import litellm

              FileName = "ai-model-list.txt"

              with open(FileName, "w") as f:
                  for model in sorted(litellm.model_list, key=str.lower):
                      f.write(model + "\n")

              print("Updated " + FileName)
            '';
      };
    };
}
