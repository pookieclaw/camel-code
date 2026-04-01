FROM ocaml/opam:ubuntu-24.04-ocaml-5.2

USER opam
WORKDIR /home/opam

# System deps for cohttp-eio and other libs
RUN sudo apt-get update && sudo apt-get install -y \
    pkg-config \
    libev-dev \
    libssl-dev \
    libgmp-dev \
    libffi-dev \
    m4 \
    && sudo rm -rf /var/lib/apt/lists/*

# Pre-install core opam packages for faster builds
RUN opam install -y \
    dune \
    eio_main \
    cohttp-eio \
    yojson \
    ppx_deriving_yojson \
    cmdliner \
    uutf \
    fmt \
    logs \
    alcotest \
    ocaml-lsp-server \
    ocamlformat \
    && opam clean -a -c -s --logs

WORKDIR /workspace
