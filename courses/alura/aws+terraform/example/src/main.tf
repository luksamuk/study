# Tutorial copied from:
# https://www.youtube.com/watch?v=SLB_c_ayRMo
# ...with a few modifications.

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

# Create a VPC
resource "aws_vpc" "main-vpc" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_hostnames = true
  tags = {
    Name        = "${var.environment}-vpc-main"
    environment = var.environment
  }
}


# Create an internet gateway
# This is for assigning a public IP address later
resource "aws_internet_gateway" "gw-1" {
  vpc_id = aws_vpc.main-vpc.id
  tags = {
    Name        = "${var.environment}-gw-1"
    environment = var.environment
  }
}



# Create a custom route table
resource "aws_route_table" "route-table-1" {
  vpc_id = aws_vpc.main-vpc.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.gw-1.id
  }

  route {
    ipv6_cidr_block = "::/0"
    gateway_id      = aws_internet_gateway.gw-1.id
  }

  tags = {
    Name        = "${var.environment}-rt-1"
    environment = var.environment
  }
}

# Create subnets
resource "aws_subnet" "subnet-1" {
  vpc_id            = aws_vpc.main-vpc.id
  cidr_block        = "10.0.1.0/24"
  availability_zone = var.availability_zone_1
  tags = {
    Name        = "${var.environment}-subnet-1"
    environment = var.environment
  }
}

resource "aws_subnet" "subnet-2" {
  vpc_id            = aws_vpc.main-vpc.id
  cidr_block        = "10.0.2.0/24"
  availability_zone = var.availability_zone_2
  tags = {
    Name        = "${var.environment}-subnet-2"
    environment = var.environment
  }
}

# Create a subnet group
resource "aws_db_subnet_group" "subnet-group-1" {
  name       = "${var.environment}-subnet-group-1"
  subnet_ids = [aws_subnet.subnet-1.id, aws_subnet.subnet-2.id]

  tags = {
    Name        = "${var.environment}-subnet-group-1"
    environment = var.environment
  }
}

# Associate subnets with route table
resource "aws_route_table_association" "route-table-assoc-1" {
  subnet_id      = aws_subnet.subnet-1.id
  route_table_id = aws_route_table.route-table-1.id
}

resource "aws_route_table_association" "route-table-assoc-2" {
  subnet_id      = aws_subnet.subnet-2.id
  route_table_id = aws_route_table.route-table-1.id
}

# Create security groups
resource "aws_security_group" "security-group-1" {
  name        = "${var.environment}-sg-1"
  description = "Allow all outbound traffic"
  vpc_id      = aws_vpc.main-vpc.id

  # Allow exit traffic for all addresses, all ports, any protocol
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name        = "${var.environment}-sg-1"
    environment = var.environment
  }
}

# Security group for all inbound traffic (specially debug)
resource "aws_security_group" "security-group-2" {
  name        = "${var.environment}-sg-2"
  description = "Allow web inbound traffic"
  vpc_id      = aws_vpc.main-vpc.id

  ingress {
    description = "HTTPS"
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"] # Anyone can access
  }

  ingress {
    description = "HTTP"
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"] # Anyone can access
  }

  # SSH connection
  ingress {
    description = "SSH"
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"] # Anyone can access
  }

  tags = {
    Name        = "${var.environment}-sg-2"
    environment = var.environment
  }
}

# Security group for Kubernetes
resource "aws_security_group" "security-group-3" {
  name        = "${var.environment}-sg-3"
  description = "Allow k3s inbound traffic"
  vpc_id      = aws_vpc.main-vpc.id

  ingress {
    description = "Kubernetes Controller"
    from_port   = 6443
    to_port     = 6443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"] # Anyone can access
  }

  ingress {
    description = "NodePort #1"
    from_port = 30000
    to_port = 30000
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"] # Anyone can access
  }

  tags = {
    Name = "${var.environment}-sg-3"
    environment = var.environment
  }
}

# Security group for PostgreSQL
resource "aws_security_group" "security-group-4" {
  name        = "${var.environment}-sg-4"
  description = "Allow postgresql inbound traffic (VPC only)"
  vpc_id      = aws_vpc.main-vpc.id

  # PostgreSQL (Internal -- subnets only)
  ingress {
    description = "PostgreSQL"
    from_port   = 5432
    to_port     = 5432
    protocol    = "tcp"
    cidr_blocks = [aws_subnet.subnet-1.cidr_block, aws_subnet.subnet-2.cidr_block]
  }

  tags = {
    Name        = "${var.environment}-sg-4"
    environment = var.environment
  }
}

# Create a network interface with an IP in the subnet
# This is so that the web server has a private IP within the subnet
resource "aws_network_interface" "web-server-nic" {
  subnet_id       = aws_subnet.subnet-1.id
  private_ips     = ["10.0.1.50"]
  security_groups = [
    aws_security_group.security-group-1.id,
    aws_security_group.security-group-2.id,
    aws_security_group.security-group-3.id,
    aws_security_group.security-group-4.id
  ]

  tags = {
    Name = "${var.environment}-nic-1"
    environment = var.environment
  }
}

# Assign an elastic IP to the network interface
# This is so that we can access our web server publicly
# Notice that this depends on the deployment of the gateway, so it must deploy
# after it. This is why we set the depends_on flag
resource "aws_eip" "elastic-ip-1" {
  vpc                       = true
  network_interface         = aws_network_interface.web-server-nic.id
  associate_with_private_ip = "10.0.1.50"
  depends_on                = [aws_internet_gateway.gw-1]

  tags = {
    Name = "${var.environment}-eip-1"
    environment = var.environment
  }
}

# Create RDS database instance
# TODO: Create a proper subnet here. We don't need outbound traffic!!!
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


# Create an Elastic Container Registry for Docker images
resource "aws_ecr_repository" "ecr-1" {
  name                 = "${var.environment}-webserver"
  image_tag_mutability = "MUTABLE"
  force_delete         = true

  image_scanning_configuration {
    scan_on_push = false
  }

  tags = {
    Name = "${var.environment}-webserver"
    environment = var.environment
  }
}


# Add IAM Role for reading ECR containers (no push authorization)
resource "aws_iam_role" "ecr-pull-role" {
  name = "${var.environment}-ecr-pull-role"
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Principal = {
          Service = ["ec2.amazonaws.com"]
        }
        Effect = "Allow"
        Action = "sts:AssumeRole"
      }
    ]
  })
}

# IAM policy for retrieving ECR container images
resource "aws_iam_policy" "ecr-pull-policy" {
  name = "${var.environment}-ecr-pull-policy"

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid = "GrantAllImagesReadOnlyAccess"
        Action = [
          "ecr:BatchCheckLayerAvailability",
          "ecr:GetDownloadUrlForLayer",
          "ecr:GetRepositoryPolicy",
          "ecr:DescribeRepositories",
          "ecr:ListImages",
          "ecr:DescribeImages",
          "ecr:BatchGetImage"
        ]
        Effect   = "Allow"
        Resource = "*"
      },
      {
        Sid = "GrantECRAuthAccess",
        Effect = "Allow",
        Action = [
          "ecr:GetAuthorizationToken",
        ],
        Resource = "*"
      }
    ]
  })
}

# IAM policy attachment for ecr-pull
resource "aws_iam_policy_attachment" "erc-pull-policy-attach" {
  name       = "${var.environment}-ecr-pull-policy-attach"
  roles      = [aws_iam_role.ecr-pull-role.name]
  policy_arn = aws_iam_policy.ecr-pull-policy.arn
}

# IAM profile for EC2 instances that will pull from ECR
resource "aws_iam_instance_profile" "ecr-pull-profile" {
  name = "${var.environment}-ecr-pull-profile"
  role = aws_iam_role.ecr-pull-role.name
}


# Create an Ubuntu server and install/enable apache2
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
