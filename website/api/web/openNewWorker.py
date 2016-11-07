import boto.ec2
import boto.manage.cmdshell
import time
import random
import pymysql
import os.path
import configparser

parser = configparser.ConfigParser()
parser.read("../../../halite.ini")
AWS_CONFIG = parser["aws"]
DB_CONFIG = parser["database"]

# Create instance
conn = boto.ec2.connect_to_region("us-east-1", aws_access_key_id=AWS_CONFIG["accessKey"], aws_secret_access_key=AWS_CONFIG["secretAccessKey"])
securityGroup = conn.get_all_security_groups(filters={'group-name': AWS_CONFIG["securityGroupName"]})[0]
reservation = conn.run_instances(AWS_CONFIG["amiID"], key_name=AWS_CONFIG["keyName"], instance_type=AWS_CONFIG["instanceType"], security_groups=[securityGroup])
instance = reservation.instances[0]

# Wait to start
status = instance.update()
while status == "pending":
    time.sleep(1)
    status = instance.update()
    print("Waiting for instance to start...")

# Add worker to db
db = pymysql.connect(host=DB_CONFIG["hostname"], user=DB_CONFIG['username'], passwd=DB_CONFIG['password'], db=DB_CONFIG['name'])
cursor = db.cursor()

ipAddress = instance.ip_address
print(ipAddress)

numRows = 9999
while numRows != 0:
    apiKey = random.randrange(0, 10000000)
    cursor.execute("SELECT * FROM Worker WHERE apiKey="+str(apiKey))
    numRows = int(cursor.rowcount)
cursor.execute("INSERT INTO Worker (apiKey, ipAddress) VALUES ("+str(apiKey)+", '"+str(ipAddress)+"')")
db.commit()

def runCommandOnInstance(instance, command):
    # Connect to ssh
    ssh_client = None
    for a in range(1000):
        try:
            ssh_client = boto.manage.cmdshell.sshclient_from_instance(instance, os.path.join("../../../", AWS_CONFIG["keyFilePath"]), user_name="ubuntu")
            print("Connected to server")
            break
        except Exception as e:
            print("except")
            print(str(e))

    # Run install script
    status, stdout, stderr = ssh_client.run(command)
    print(stdout)
    print(stderr)

configFileContents = open("../../../halite.ini").read()
runCommandOnInstance(instance, "sudo apt-get update; sudo apt-get upgrade -y; sudo apt-get install -f; sudo rm -rf /var/lib/apt/lists/*; sudo apt-get update")
runCommandOnInstance(instance, "sudo apt-get install -y --force-yes git; git clone https://github.com/HaliteChallenge/Halite.git; cd Halite; echo '"+configFileContents+"' > halite.ini; cd worker; sudo ./install.sh "+str(apiKey)+"; sudo reboot")

time.sleep(10)
runCommandOnInstance(instance, 'cd ~/Halite/worker; ./startWorkerScreen.sh')
