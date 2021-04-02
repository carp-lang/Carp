FROM debian:buster-slim

ARG DEBIAN_FRONTEND=noninteractive

ENV CARP_DIR=/Carp
ENV CARP_GIT=https://github.com/carp-lang/Carp
ENV PATH=$PATH:/root/.local/bin

RUN apt update && \
    apt install -y git build-essential curl wget \
    haskell-stack libncurses-dev

RUN stack upgrade --binary-only
RUN git clone $CARP_GIT $CARP_DIR

WORKDIR $CARP_DIR
RUN stack build
RUN stack install

RUN mkdir -p /work
WORKDIR /work

CMD carp
