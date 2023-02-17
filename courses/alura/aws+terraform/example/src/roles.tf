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
        Sid    = "GrantECRAuthAccess",
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

