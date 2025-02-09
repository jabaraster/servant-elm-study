stack build \
  --ghc-options ' -static -optl-static -optl-pthread -fPIC' \
  --docker --docker-image shinsakata/ghc-single-binary-builder:ghc8.10.1-v1.1.0 \
  --verbose
  

# --docker --docker-image "utdemir/ghc-musl:v25-ghc944" \