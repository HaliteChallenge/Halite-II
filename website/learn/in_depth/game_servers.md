---
layout: doc_page
title: Game Servers
toc: true
---

## Hardware

All compilation and games are run in Google Cloud Compute Engine instances with 1 vCPU and 2.5 GB memory.

This is the script used to set up the instance:

```bash
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

PACKAGES="build-essential gcc g++ python3 python3.6 python3-pip git golang julia ocaml openjdk-8-jdk php ruby scala nodejs mono-complete"
WORKER_PACKAGES="virtualenv cgroup-tools"

PYTHON_PACKAGES="numpy scipy scikit-learn pillow h5py tensorflow keras theano"
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
```

## Compilation

Bot compilation is done using this [autocompile script][autocompile-script].

To facilitate the installation of custom software, we allow users to include an install script. If a file named `install.sh` exists in your submission, it is run as a bash script under the root user in a sandbox with internet access and 10 minutes of runtime. Bots may only read and write to their current directory, so all files that you want to be available at runtime must be installed locally.

Your main file must be called MyBot. Your language is recognized using the file extension of your MyBot file. The appropriate file extensions for each language are:

- Java - .java
- Python - .py
- C++ - .cpp and .h(pp)
- C# - .cs
- Rust - .toml (for your Cargo.toml) and .rs (for your Rust source)
- Scala - .scala
- Ruby - .rb
- Go - .go
- PHP - .php
- JavaScript - .js
- OCaml - .ml
- Clojure - .clj
- C - .c
- Julia - .jl

The following compilers/interpreters are used:

- build-essential	`12.1ubuntu2`
- gcc	`4:6.3.0-2ubuntu1`
- g++	`4:6.3.0-2ubuntu1`
- python3	`3.5.3-1`
- python3.6	`3.6.1-1`
- python3-pip	`9.0.1-2`
- git	`1:2.11.0-2ubuntu0.1`
- golang	`2:1.7~1ubuntu1`
- julia	`0.4.7-6ubuntu1`
- ocaml	`4.02.3-6ubuntu2`
- openjdk-8-jdk:amd64	`8u131-b11-0ubuntu1.17.04.1`
- php	`1:7.0+49`
- ruby	`1:2.3.3`
- scala	`2.11.8-1`
- nodejs	`8.1.2-1nodesource1~zesty1`
- mono-complete	`5.0.1.1-0xamarin5+ubuntu1604b1`
- virtualenv	`15.1.0+ds-1`
- rustc `1.18.0 (03fc9d622 2017-06-06)`
- cargo `0.19.0 (28d1d60d4 2017-05-16)`
- rustup `1.5.0 (92d0d1e9e 2017-06-24)`
- Leiningen `2.7.1`

For Python, the following packages are available:

- numpy `1.13.0`
- scipy `0.19.1`
- scikit-learn `0.18.2`
- pillow `4.1.1`
- h5py `2.7.0`
- tensorflow `1.2.0`
- keras `2.0.5`
- theano `0.9.0`

Both Python 3.5 and 3.6 are installed. To use 3.6, you will have to explicitly use a `run.sh` file to invoke `python3.6`.

## Games

Bots are given 350 MB of RAM and equal amounts of CPU.

Games are always run using the most recent environment build.

[autocompile-script]: https://github.com/HaliteChallenge/Halite/blob/02b8a4a8c14498ddc471039c9a453137379420c1/worker/compiler.py
