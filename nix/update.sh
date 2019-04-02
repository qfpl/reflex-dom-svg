#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix-prefetch-git

cd $(dirname $0)
nix-prefetch-git https://github.com/reflex-frp/reflex-platform > reflex-platform.json
