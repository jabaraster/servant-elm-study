docker build . -t jabaraster/lightsail:haskell-elm --platform linux/x86_64
DOCKER_HOST=unix:///Users/<username>/.docker/run/docker.sock aws lightsail push-container-image --region ap-northeast-1 --service-name container-service-3 --label servant-elm-study --image jabaraster/lightsail:haskell-elm  --profile jabara-admin
