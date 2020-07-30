FROM ubuntu:18.04

RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
        software-properties-common \
        fontconfig \
        unzip \
        wget \
        git \
        git-lfs \
        openssh-client \
        libssl-dev \
        ca-certificates \
        sqlite \
    && echo "Initialization done"

RUN echo "Installing Racket" \
    && add-apt-repository ppa:plt/racket \
    && apt-get update \
    && apt-get install -y racket

RUN echo "Installing Rosette" \
    && raco pkg install \
        custom-load \
        rfc6455 \
        rosette

CMD ["racket"]

