#!/usr/bin/env bash

# We are running as root, switch to a less privileged account
sudo -iu worker bash <<"EOF"
cd

# Fetch the coordinator
# TODO: This is in GCloud right now, but we should use Git when we go public
while [ ! -f ./Halite.tgz ]; do
    sleep 5
    gsutil cp gs://halite-2-deployed-artifacts/Halite.tgz .
done

tar -xzf Halite.tgz
cd Halite/
sudo cp -rf website/_site/* /var/www/html/
echo $(datetime) > ~/_SUCCESS
EOF
