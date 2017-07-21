#!/usr/bin/env bash

NUM_BOTS=4

## Create a user to be used by the worker exclusively.
sudo groupadd bots
sudo useradd -m worker -U -G bots -s /bin/bash

# Don't just grant random people sudo access.
sudo rm /etc/sudoers.d/google_sudoers

## Add necessary repositories for Node.js and Mono.
# https://nodejs.org/en/download/package-manager/#debian-and-ubuntu-based-linux-distributions
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
# http://www.mono-project.com/download/#download-lin
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb http://download.mono-project.com/repo/ubuntu xenial main" | sudo tee /etc/apt/sources.list.d/mono-official.list
sudo apt-get update
sudo apt-get -y upgrade

## List the packages to install for running bots.
PACKAGES="build-essential gcc g++ python3 python3.6 python3-pip git golang julia ocaml openjdk-8-jdk php ruby scala nodejs mono-complete"
## List the packages to install for the worker itself.
WORKER_PACKAGES="virtualenv cgroup-tools unzip"

## List Python packages to preinstall.
PYTHON_PACKAGES="numpy scipy scikit-learn pillow h5py tensorflow keras theano"
## List Ruby gems to preinstall.
RUBY_PACKAGES="bundler"

## Install everything
sudo apt-get -y install ${PACKAGES} ${WORKER_PACKAGES}

sudo pip3 install ${PYTHON_PACKAGES}
sudo python3.6 -m pip install ${PYTHON_PACKAGES}

sudo gem install ${RUBY_PACKAGES}

## Install Rustup, making sure the worker user can access it.
sudo -iu worker sh -c 'curl https://sh.rustup.rs -sSf > rustup.sh; sh rustup.sh -y'

## Install Leiningen/Clojure, also for the worker user.
sudo sh -c 'echo "$(curl -fsSL https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein)" > /usr/bin/lein'
sudo chmod a+x /usr/bin/lein
sudo -iu worker lein

## Print out packages installed.
echo "Packages"
for package in ${PACKAGES}; do
    dpkg-query -W ${package}
done

echo "Worker Packages"
for package in ${WORKER_PACKAGES}; do
    dpkg-query -W ${package}
done

echo "Python 3.5 Packages"
for package in ${PYTHON_PACKAGES}; do
    echo ${package} $(pip3 show ${package} | grep Version | awk '{print $2}')
done

echo "Python 3.6 Packages"
for package in ${PYTHON_PACKAGES}; do
    echo ${package} $(python3.6 -m pip show ${package} | grep Version | awk '{print $2}')
done

echo "Ruby Gems"
for package in ${RUBY_PACKAGES}; do
    gem list | grep ${package}
done

echo "Rust Packages"
sudo -iu worker bash << EOF
rustc -V
cargo -V
rustup -V
EOF

echo "Clojure Packages"
sudo -iu worker bash << EOF
lein version
EOF

## Create four cgroups to isolate bots.
for i in $(seq 0 $((NUM_BOTS-1))); do
    CGROUP="bot_${i}"
    # Grant control over the cgroup to the worker user
    sudo cgcreate -g cpu,memory:/${CGROUP} -t worker:worker
    sudo -u worker cgset -r cpu.shares=1024 memory.limit_in_bytes=$((350*1024*1024)) ${CGROUP}
done

## Create a user to be used by compilation. This user will have limited Internet access.
sudo useradd -m bot_compilation
## No access to 10. addresses (which are our own servers)
## We are giving them general network access (to download dependencies)
sudo iptables -A OUTPUT -d 10.0.0.0/8 -m owner --uid-owner bot_compilation -j DROP
## Grant sudo access to the worker as this user.
sudo sh -c "echo \"worker ALL=(bot_compilation) NOPASSWD: /bin/bash\" > /etc/sudoers.d/worker_bot_compilation"
sudo chmod 0400 /etc/sudoers.d/worker_bot_compilation

## Create four users to isolate bots.
for i in $(seq 0 $((NUM_BOTS-1))); do
    USERNAME="bot_${i}"
    sudo useradd -m -g bots ${USERNAME}
    ## Deny all network access to this user.
    sudo iptables -A OUTPUT -m owner --uid-owner ${USERNAME} -j DROP
    ## Grant sudo access to the worker.
    sudo sh -c "echo \"worker ALL=(${USERNAME}) NOPASSWD: /bin/bash\" > /etc/sudoers.d/worker_${USERNAME}"
    sudo chmod 0400 /etc/sudoers.d/worker_${USERNAME}
done