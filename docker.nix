#
# Run as:
#   docker load -i $(
#     nix-build docker.nix \
#       --argstr name <image-name> \
#       --argstr tag <image-tag>
#   )
let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  pkgs =
    import
      (
        fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
          sha256 = lock.nodes.nixpkgs.locked.narHash;
        }
      )
      { };
  banyan = (import ./.).defaultPackage.x86_64-linux;
in
{ name ? "sridca/banyan"
, tag ? "dev"
}: pkgs.dockerTools.buildImage {
  inherit name tag;
  contents = [
    banyan
    # These are required for the GitLab CI runner
    pkgs.coreutils
    pkgs.bash_5
  ];

  config = {
    WorkingDir = "/data";
    Volumes = {
      "/data" = { };
    };
    Cmd = [ "${banyan}/bin/banyan" ];
  };
}
