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
    cmake \
    && sudo rm -rf /var/lib/apt/lists/*

# Pre-install core opam packages for faster builds (cached layer)
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

# Install Rust + build fff-c (after opam so opam layer is cached)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/home/opam/.cargo/bin:${PATH}"

RUN git clone --depth 1 https://github.com/dmtrKovalenko/fff.nvim.git /tmp/fff \
    && cd /tmp/fff \
    && cargo build --release -p fff-c 2>&1 \
    && sudo cp target/release/libfff_c.so /usr/local/lib/ \
    && sudo ldconfig \
    && rm -rf /tmp/fff /home/opam/.cargo/registry /home/opam/.cargo/git

WORKDIR /workspace
