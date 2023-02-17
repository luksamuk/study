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
    from_port   = 30000
    to_port     = 30000
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"] # Anyone can access
  }

  tags = {
    Name        = "${var.environment}-sg-3"
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
