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

