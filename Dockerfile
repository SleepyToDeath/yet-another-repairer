FROM ubuntu:18.04

RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
        software-properties-common \
        unzip \
        wget \
        git \
        git-lfs \
        openssh-client \
        libssl-dev \
        ca-certificates \
        sqlite \
    && echo "Initialization done"

RUN echo "Installing Racket"

ARG RACKET_INSTALLER_URL="https://mirror.racket-lang.org/installers/7.7/racket-7.7-x86_64-linux.sh"
ARG RACKET_VERSION="7.7"

RUN wget --output-document=racket-install.sh -q ${RACKET_INSTALLER_URL} && \
    echo "yes\n1\n" | sh racket-install.sh --create-dir --unix-style --dest /usr/ && \
    rm racket-install.sh

ENV SSL_CERT_FILE="/etc/ssl/certs/ca-certificates.crt"
ENV SSL_CERT_DIR="/etc/ssl/certs"

RUN raco setup
RUN raco pkg config --set catalogs \
    "https://download.racket-lang.org/releases/${RACKET_VERSION}/catalog/" \
    "https://pkg-build.racket-lang.org/server/built/catalog/" \
    "https://pkgs.racket-lang.org" \
    "https://planet-compats.racket-lang.org"

RUN echo "Installing Rosette" \
    && raco pkg install --batch --deps search-auto rosette

WORKDIR /repairer
COPY src /repairer/src
COPY test /repairer/test
CMD ["raco", "test", "test"]

