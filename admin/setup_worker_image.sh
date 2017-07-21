#!/usr/bin/env bash

# https://nodejs.org/en/download/package-manager/#debian-and-ubuntu-based-linux-distributions
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
# http://www.mono-project.com/download/#download-lin
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb http://download.mono-project.com/repo/ubuntu xenial main" | sudo tee /etc/apt/sources.list.d/mono-official.list
sudo apt-get update
sudo apt-get -y upgrade

sudo useradd -m worker
sudo sh -c 'echo "worker ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/worker'
sudo chmod 0400 /etc/sudoers.d/worker

PACKAGES="build-essential gcc g++ python3 python3.6 python3-pip git golang julia ocaml openjdk-8-jdk php ruby scala nodejs mono-complete libgeos-dev"
WORKER_PACKAGES="virtualenv cgroup-tools"

PYTHON_PACKAGES="numpy scipy scikit-learn pillow h5py tensorflow keras theano shapely"
RUBY_PACKAGES="bundler"

sudo apt-get -y install ${PACKAGES} ${WORKER_PACKAGES}

sudo pip3 install ${PYTHON_PACKAGES}
sudo python3.6 -m pip install ${PYTHON_PACKAGES}

sudo gem install ${RUBY_PACKAGES}

# https://www.rustup.rs/
# Install for the worker user
sudo -iu worker sh -c 'curl https://sh.rustup.rs -sSf > rustup.sh; sh rustup.sh -y'

sudo sh -c 'echo "$(curl -fsSL https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein)" > /usr/bin/lein'
sudo chmod a+x /usr/bin/lein
sudo -iu worker lein

# Miniconda
sudo -iu worker sh -c 'curl https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -sSf > miniconda.sh; bash miniconda.sh -b -p $HOME/miniconda'

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