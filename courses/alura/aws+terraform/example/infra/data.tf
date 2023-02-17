data "template_file" "ec2-user-data" {
    template = "${file("${path.module}/user-data.sh")}"
    vars = {
        AWS_ACCOUNT_ID = var.account_id
        AWS_REGION = var.region
    }
}
