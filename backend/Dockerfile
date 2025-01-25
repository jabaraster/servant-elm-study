# FROM alpine:latest
FROM fpco/stack-build

ENV LANG=C.UTF-8


# Install required packages.
RUN apk update
RUN apk add curl xz make

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xvz -C /opt/stack/bin --strip-components=1
RUN /opt/stack/bin/stack --version

# Create /opt/servant-elm-study/bin and /opt/servant-elm-study/src.  Set
# /opt/servant-elm-study/src as the working directory.
RUN mkdir -p /opt/servant-elm-study/src
RUN mkdir -p /opt/servant-elm-study/bin
WORKDIR /opt/servant-elm-study/src

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/servant-elm-study/bin"


RUN stack init

# # Install GHC using stack, based on your app's stack.yaml file.
# COPY ./stack.yaml /opt/servant-elm-study/src/stack.yaml
# RUN stack --no-terminal setup

# # Install all dependencies in app's .cabal file.
# COPY ./servant-elm-study.cabal /opt/servant-elm-study/src/servant-elm-study.cabal
# RUN stack --no-terminal test --only-dependencies

# Build application.
COPY . /opt/servant-elm-study/src
RUN stack --verbose build

# Install application binaries to /opt/servant-elm-study/bin.
RUN stack --no-terminal --local-bin-path /opt/servant-elm-study/bin install

# Remove source code.
RUN rm -rf /opt/servant-elm-study/src

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/servant-elm-study
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/servant-elm-study/bin"

# Set the working directory as /opt/servant-elm-study/.
WORKDIR /opt/servant-elm-study

CMD /opt/servant-elm-study/bin/ss