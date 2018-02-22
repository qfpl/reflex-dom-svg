let
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      # rev = "822212aa6c995bf58a8362c411f157227092c42d";
      # sha256 = "1asqjh5r8kjvil1h7pggv408wamws8hnrdsm7a01nkvcm18gagad";
      rev = "2e2fb7f0d15385a712face0c9cdcece4f3981a90";
      sha256 = "1xxw4w3c3xmx4wjlwxqg7xbd74kdzv13viwh8g49adyl1agg4m12";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform
