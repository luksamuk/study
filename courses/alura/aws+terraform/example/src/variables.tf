variable "environment" {
  description = "Deployment environment"
  type = string
}

variable "region" {
  description = "Region of deployment. Defaults to América do Sul (São Paulo)"
  type        = string
  default     = "sa-east-1"
}

variable "availability_zone_1" {
  description = "Availability zone 1 for subnet, instances and RDS. Defaults to AZ 1 of América do Sul (São Paulo)"
  type        = string
  default     = "sa-east-1a" # América do Sul (São Paulo) Zone 1
}

variable "availability_zone_2" {
  description = "Availability zone 2 for RDS. Defaults to AZ 2 of América do Sul (São Paulo)"
  type        = string
  default     = "sa-east-1b" # América do Sul (São Paulo) Zone 2
}

variable "key_name" {
  description = "Key name for allowing SSH connections on EC2 instances"
  type        = string
  default     = "lucasvieira"
}

variable "ami" {
  description = "AMI for EC2 instance. Defaults to Ubuntu 22.04 LTS"
  type        = string
  default     = "ami-04473c3d3be6a927f"
}

variable "db_user" {
  description = "RDS database username"
  type        = string
  default     = "postgres"
}

variable "db_password" {
  description = "RDS database password"
  type        = string
  default     = "postgres"
}

variable "account_id" {
  description = "ID for account where ECR repositories are stored under"
  type = string
}

variable "default_db" {
  description = "Name of the default database"
  type = string
  nullable = true
}