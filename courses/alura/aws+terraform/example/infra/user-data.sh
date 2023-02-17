#!/bin/bash

# NOTICE: Tail logs by using "tail -f /var/log/user-data.log".
exec > >(tee /var/log/user-data.log|logger -t user-data -s 2>/dev/console) 2>&1

apt update -y

# Apache2 test server
apt install apache2 -y
systemctl enable apache2
systemctl start apache2
bash -c 'echo "This web server is running in AWS!" > /var/www/html/index.html'

# PostgreSQL psql util
apt install postgresql-client -y

# Docker & Amazon ECR credential helper configuration
apt install docker-compose docker.io awscli amazon-ecr-credential-helper -y
# mkdir -p /root/.docker
# echo "{\"credsStore\":\"ecr-login\"}" > /root/.docker/config.json


# Fetch ECR credentials for every repository
export ECR_REGISTRY="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"
echo "export ECR_REGISTRY=$ECR_REGISTRY" > /etc/profile.d/ecr_registry.sh
aws ecr get-login-password --region sa-east-1 | docker login --username AWS --password-stdin $ECR_REGISTRY

# Install k3s
curl -sfL https://get.k3s.io | INSTALL_K3S_EXEC="--write-kubeconfig=/home/ubuntu/.kube/config --write-kubeconfig-mode=644" sh -s - --docker
