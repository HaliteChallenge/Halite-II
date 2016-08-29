import boto.ec2
import boto.manage.cmdshell
import time
import os.path
import configparser

parser = configparser.ConfigParser()
parser.read("../../halite.ini")
AWS_CONFIG = parser["aws"]
print(AWS_CONFIG)

conn = boto.ec2.connect_to_region("us-east-1", aws_access_key_id=AWS_CONFIG["accessKey"], aws_secret_access_key=AWS_CONFIG["secretAccessKey"])

securityGroup = conn.get_all_security_groups(filters={'group-name': AWS_CONFIG["securityGroupName"]})[0]

reservation = conn.run_instances(AWS_CONFIG["amiID"], key_name=AWS_CONFIG["keyName"], instance_type=AWS_CONFIG["instanceType"], security_groups=[securityGroup])
instance = reservation.instances[0]

status = instance.update()
while status == "pending":
    time.sleep(1)
    status = instance.update()
    print("Waiting for instance to start...")

ssh_client = boto.manage.cmdshell.sshclient_from_instance(instance, os.path.join("../../", AWS_CONFIG["keyFilePath"]))
status, stdout, stderr = ssh_client.run('ls -al')
