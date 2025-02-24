FROM haskell:9.10.1-slim-bullseye

ARG dist="/tmp/ss"

RUN mkdir -p ${dist}/src
RUN mkdir -p ${dist}/bin

COPY . ${dist}/src
WORKDIR ${dist}/src
RUN stack setup --install-ghc
RUN stack build
RUN stack install --local-bin-path=${dist}/bin

EXPOSE 8080
CMD ["/tmp/ss/bin/ss"]
