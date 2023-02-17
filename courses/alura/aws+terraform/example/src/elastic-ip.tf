# Create a network interface with an IP in the subnet
# This is so that the web server has a private IP within the subnet
resource "aws_network_interface" "web-server-nic" {
  subnet_id   = aws_subnet.subnet-1.id
  private_ips = ["10.0.1.50"]
  security_groups = [
    aws_security_group.security-group-1.id,
    aws_security_group.security-group-2.id,
    aws_security_group.security-group-3.id,
    aws_security_group.security-group-4.id
  ]

  tags = {
    Name        = "${var.environment}-nic-1"
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
    Name        = "${var.environment}-eip-1"
    environment = var.environment
  }
}