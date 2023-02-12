variable "region" {
  description = "Region of deployment. Defaults to América do Sul (São Paulo)"
  type        = string
  default     = "sa-east-1"
}

variable "availability_zone" {
  description = "Availability zone for subnet and instances. Defaults to AZ 1 of América do Sul (São Paulo)"
  type        = string
  default     = "sa-east-1a" # América do Sul (São Paulo) Zone 1
}

variable "key_name" {
  description = "Key name for allowing SSH connections on EC2 instances"
  type        = string
}

variable "ami" {
  description = "AMI for EC2 instance. Defaults to Ubuntu 22.04 LTS"
  type        = string
  default     = "ami-04473c3d3be6a927f"
}

