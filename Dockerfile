FROM ocaml/opam:debian-11-ocaml-4.14

USER root
RUN apt-get update \
  && apt-get install -y pkg-config libgmp-dev libssl-dev \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

USER opam
RUN opam update
RUN opam install opam-publish
