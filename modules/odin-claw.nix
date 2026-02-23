# Build odin-claw from source and package as squashfs module.
{ pkgs, lib, odin-claw-src, mkSquashfsModule }:
let
  odin-claw = pkgs.stdenv.mkDerivation {
    pname = "odin-claw";
    version = "0-unstable";
    src = odin-claw-src;

    nativeBuildInputs = with pkgs; [ odin gcc curl ];
    buildInputs = with pkgs; [ curl openssl ];

    buildPhase = ''
      runHook preBuild
      cc -c src/curl_helpers.c -o curl_helpers.o \
        -I${pkgs.curl.dev}/include
      odin build src -out:odin-claw -o:aggressive \
        -extra-linker-flags:"-L${pkgs.curl}/lib -lcurl"
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin
      cp odin-claw $out/bin/
      runHook postInstall
    '';
  };
in
mkSquashfsModule {
  name = "200-odin-claw";
  buildScript = ''
    mkdir -p "$rootfs/usr/local/bin"
    cp ${odin-claw}/bin/odin-claw "$rootfs/usr/local/bin/"
  '';
}
