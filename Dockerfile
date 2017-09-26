FROM debian:stretch-slim

RUN apt-get update \
    && apt-get install -y libmpich-dev mpich libgmp-dev cmake \
    && rm -rf /var/lib/apt/lists/*

COPY bin/formura /usr/bin

WORKDIR /work

CMD ["bash"]
