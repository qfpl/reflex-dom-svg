let
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "2e2fb7f0d15385a712face0c9cdcece4f3981a90";
      sha256 = "1xxw4w3c3xmx4wjlwxqg7xbd74kdzv13viwh8g49adyl1agg4m12";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform
