let nixpkgs = import ./nixpkgs.nix;
in
{
  pkgs ? import nixpkgs {
    overlays = import ./overlay.nix {
      inherit hexOrganization hexApiKey robotSshKey;
    };
  },
  hexOrganization ? null, # organization account name on hex.pm
  hexApiKey ? null,       # plain text account API key on hex.pm
  robotSshKey ? null      # base64-encoded private id_rsa (for private git)
}:
let pkg = import ./default.nix {inherit hexOrganization hexApiKey robotSshKey;};
in
with pkgs;

dockerTools.buildImage {
  name = "tkachuk-labs/bitcoin-dashboard-tui";
  contents = [ pkg ];

  config = {
    Cmd = [ "${pkg}/bin/bitcoin-dashboard-tui-exe" ];
    ExposedPorts = {
      "80/tcp" = {};
    };
  };
}
