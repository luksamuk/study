#!/bin/bash
TAG="467769465270.dkr.ecr.sa-east-1.amazonaws.com/webserver:latest"
docker build -f Dockerfile -t $TAG .
docker push $TAG
