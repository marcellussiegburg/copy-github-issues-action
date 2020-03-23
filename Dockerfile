FROM haskell:8.6
WORKDIR /copy-github-issues
COPY stack.yaml /copy-github-issues/stack.yaml
COPY package.yaml /copy-github-issues/package.yaml
RUN stack install --dependencies-only
COPY src /copy-github-issues/src
RUN stack install
ENTRYPOINT ["copy-github-issues"]