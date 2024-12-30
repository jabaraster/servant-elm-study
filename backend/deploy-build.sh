stack build \
  --ghc-options ' -static -optl-static -optl-pthread -fPIC' \
  --docker --docker-image "utdemir/ghc-musl:v25-ghc944" \
  --verbose