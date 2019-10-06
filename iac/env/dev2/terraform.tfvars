# app/env to scaffold
app = "manger"
environment = "dev2"

internal = true
container_port = "3000"
replicas = "1"
health_check = "/health"
region = "eu-west-1"
aws_profile = "default"

vpc = "vpc-051531ba6b968c4f4"

# It’s good practice to create one subnet per availability zone.

private_subnets = "subnet-027c0eefeb186acc8,subnet-040e35bf93670a66e"
public_subnets  = "subnet-01d2568890f648013,subnet-048821643d9f6a445"

tags = {
  application   = "manger"
  environment   = "dev2"
  team          = "avengers"
  customer      = "internal"
  contact-email = "michael.nisi@gmail.com"
}
