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
resource "aws_vpc" "test-vpc" {
  cidr_block = "10.0.0.0/16"
  enable_dns_hostnames = true
  tags = {
    Name = "test"
  }
}

# Create an internet gateway
# This is for assigning a public IP address later
resource "aws_internet_gateway" "gw" {
  vpc_id = aws_vpc.test-vpc.id
  tags = {
    Name = "test-gateway"
  }
}



# Create a custom route table
resource "aws_route_table" "test-route-table" {
  vpc_id = aws_vpc.test-vpc.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.gw.id
  }

  route {
    ipv6_cidr_block = "::/0"
    gateway_id      = aws_internet_gateway.gw.id
  }

  tags = {
    Name = "test-route-table"
  }
}

# Create subnets
resource "aws_subnet" "subnet-1" {
  vpc_id            = aws_vpc.test-vpc.id
  cidr_block        = "10.0.1.0/24"
  availability_zone = var.availability_zone_1
  tags = {
    Name = "test-subnet-1"
  }
}

resource "aws_subnet" "subnet-2" {
  vpc_id            = aws_vpc.test-vpc.id
  cidr_block        = "10.0.2.0/24"
  availability_zone = var.availability_zone_2
  tags = {
    Name = "test-subnet-2"
  }
}

# Create a subnet group
resource "aws_db_subnet_group" "subnet-group-1" {
  name = "subnet-group-1"
  subnet_ids = [aws_subnet.subnet-1.id, aws_subnet.subnet-2.id]

  tags = {
    Name = "test-subnet-group-1"
  }
}

# Associate subnet with route table
resource "aws_route_table_association" "a" {
  subnet_id      = aws_subnet.subnet-1.id
  route_table_id = aws_route_table.test-route-table.id
}

# Create security group to allow ports 22, 80, 443
resource "aws_security_group" "allow_web" {
  name        = "allow_web_traffic"
  description = "Allow web inbound traffic"
  vpc_id      = aws_vpc.test-vpc.id

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

  ingress {
    description = "PostgreSQL"
    from_port   = 5432
    to_port     = 5432
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"] # Anyone can access
  }

  ingress {
    description = "SSH"
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"] # Anyone can access
  }

  # Allow exit traffic for all addresses, all ports, any protocol
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name = "allow_web"
  }
}

# Create a network interface with an IP in the subnet
# This is so that the web server has a private IP within the subnet
resource "aws_network_interface" "web-server-nic" {
  subnet_id       = aws_subnet.subnet-1.id
  private_ips     = ["10.0.1.50"]
  security_groups = [aws_security_group.allow_web.id]
}

# Assign an elastic IP to the network interface
# This is so that we can access our web server publicly
# Notice that this depends on the deployment of the gateway, so it must deploy
# after it. This is why we set the depends_on flag
resource "aws_eip" "one" {
  vpc                       = true
  network_interface         = aws_network_interface.web-server-nic.id
  associate_with_private_ip = "10.0.1.50"
  depends_on                = [aws_internet_gateway.gw]
}

# Create an Ubuntu server and install/enable apache2
resource "aws_instance" "web-server-instance" {
  ami               = var.ami               # Ubuntu 22.04 LTS
  instance_type     = "t2.micro"            # 1 vCPU, 1GB RAM
  availability_zone = var.availability_zone_1 # Must be the same as the subnet
  key_name          = var.key_name          # Key pair created for SSH access


  # eth0 interface
  network_interface {
    device_index         = 0
    network_interface_id = aws_network_interface.web-server-nic.id
  }

  user_data = <<-EOF
#!/bin/bash
sudo apt update -y
sudo apt install apache2 -y
sudo systemctl enable apache2
sudo systemctl start apache2
sudo bash -c 'echo "This web server is running in AWS!" > /var/www/html/index.html'
EOF

  tags = {
    Name = "web-server"
  }
}

# Create RDS database instance
resource "aws_db_instance" "pgsql" {
  allocated_storage            = 20
  db_name                      = "minerva"
  engine                       = "postgres"
  engine_version               = "13.7"
  instance_class               = "db.t3.micro"
  username                     = var.db_user
  password                     = var.db_password
  skip_final_snapshot          = true
  performance_insights_enabled = true
  publicly_accessible          = true
  db_subnet_group_name         = aws_db_subnet_group.subnet-group-1.name
  vpc_security_group_ids       = [aws_security_group.allow_web.id]
  availability_zone            = var.availability_zone_1
}