FROM racket/racket:7.7

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
        vim \
    && echo "Initialization done"

RUN echo "Installing Rosette" \
    && raco pkg install --batch --deps search-auto rosette

RUN echo "Installing brag" \
    && raco pkg install --batch --deps search-auto brag

WORKDIR /repairer
COPY src /repairer/src
COPY test /repairer/test
CMD ["raco", "test", "test"]

