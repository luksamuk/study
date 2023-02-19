#!/bin/bash
TAG="luksamuk/example-webserver:latest"

docker buildx build \
	--platform=linux/amd64,linux/arm64,linux/riscv64 \
	-f Dockerfile \
	-t $TAG \
	--push \
	.

