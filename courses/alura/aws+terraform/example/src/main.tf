# Tutorial: https://www.youtube.com/watch?v=SLB_c_ayRMo
# AWS Provider docs: https://registry.terraform.io/providers/hashicorp/aws/latest/docs

terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.0"
    }
  }
}


# Configure the AWS Provider
provider "aws" {
  region = var.region
  # access_key (prefer env variable AWS_ACCESS_KEY)
  # secret_key (prefer env variable AWS_SECRET_ACCESS_KEY)
}


# Network config: network.tf
# Security group config: firewall.tf
# Network interface, elastic IP config: elastic-ip.tf
# IAM roles config: roles.tf

# Create RDS PostgreSQL database instance
resource "aws_db_instance" "pgsql-1" {
  allocated_storage            = 20
  db_name                      = var.default_db
  engine                       = "postgres"
  engine_version               = "13.7"
  instance_class               = "db.t3.micro"
  username                     = var.db_user
  password                     = var.db_password
  skip_final_snapshot          = true
  performance_insights_enabled = true
  publicly_accessible          = false
  db_subnet_group_name         = aws_db_subnet_group.subnet-group-1.name
  vpc_security_group_ids       = [aws_security_group.security-group-4.id]
  availability_zone            = var.availability_zone_1

  tags = {
    Name        = "${var.environment}-pgsql-1"
    environment = var.environment
  }
}


# Create an Elastic Container Registry for Web Server Docker image
resource "aws_ecr_repository" "ecr-1" {
  name                 = "${var.environment}-webserver"
  image_tag_mutability = "MUTABLE"
  force_delete         = true

  image_scanning_configuration {
    scan_on_push = false
  }

  tags = {
    Name        = "${var.environment}-webserver"
    environment = var.environment
  }
}

# Create an Ubuntu server. Runs Apache2, Docker, k3s, and 
resource "aws_instance" "vm-1" {
  ami               = var.ami                 # Ubuntu 22.04 LTS
  instance_type     = "t2.micro"              # 1 vCPU, 1GB RAM
  availability_zone = var.availability_zone_1 # Must be the same as the subnet
  key_name          = var.key_name            # Key pair created for SSH access

  # Profile for accessing ECR
  iam_instance_profile = aws_iam_instance_profile.ecr-pull-profile.name

  # eth0 interface
  network_interface {
    device_index         = 0
    network_interface_id = aws_network_interface.web-server-nic.id
  }

  user_data = <<-EOF
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
export ECR_REGISTRY=${var.account_id}.dkr.ecr.${var.region}.amazonaws.com
echo "export ECR_REGISTRY=$ECR_REGISTRY" > /etc/profile.d/ecr_registry.sh
aws ecr get-login-password --region sa-east-1 | docker login --username AWS --password-stdin $ECR_REGISTRY

# Install k3s
# TODO: maybe change cluster CIDR block, etc? https://github.com/k3s-io/k3s/issues/2854
curl -sfL https://get.k3s.io | INSTALL_K3S_EXEC="--write-kubeconfig=/home/ubuntu/.kube/config --write-kubeconfig-mode=644" sh -s - --docker
EOF

  tags = {
    Name        = "${var.environment}-vm-1"
    environment = var.environment
  }
}
