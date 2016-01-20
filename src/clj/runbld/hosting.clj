(ns runbld.hosting)

(defprotocol HostingProvider
  (datacenter       [_] "us-west-2c")
  (image-id         [_] "ami-546546546")
  (instance-id      [_] "i-65465465")
  (instance-type    [_] "m4.xlarge")
  (hosting-provider [_] "aws-ec2")
  (region           [_] "us-west-2"))
