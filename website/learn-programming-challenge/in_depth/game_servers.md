---
layout: doc_page
title: Game Servers
toc: true
---

## Hardware

All compilation and games are run in Google Cloud Compute Engine instances with 1 vCPU and 2.5 GB memory.

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

Packages
build-essential	12.1ubuntu2
gcc	4:6.3.0-2ubuntu1
g++	4:6.3.0-2ubuntu1
python3	3.5.3-1
python3.6	3.6.1-1
	python3-pip	9.0.1-2
git	1:2.11.0-2ubuntu0.1
golang	2:1.7~1ubuntu1
julia	0.4.7-6ubuntu1
ocaml	4.02.3-6ubuntu2
openjdk-8-jdk:amd64	8u131-b11-2ubuntu1.17.04.3
php	1:7.0+49
ruby	1:2.3.3
scala	2.11.8-1
nodejs	8.2.1-2nodesource1~zesty1
mono-complete	5.0.1.1-0xamarin5+ubuntu1604b1
dotnet-dev-1.1.0	1.1.0-1
libgeos-dev	3.5.1-3
tcl8.5	8.5.19-2
mit-scheme:amd64	9.1.1-5build1
racket	6.7-1
octave	4.0.3-3ubuntu1
luajit	2.0.4+dfsg-1
lua5.2	5.2.4-1.1build1
ghc	8.0.2-1~build1
erlang-base-hipe	1:19.2.1+dfsg-2ubuntu1
coffeescript	1.10.0~dfsg-1
dart	1.24.2-1
fp-compiler:amd64	3.0.2+dfsg-2
sbcl	2:1.3.3-1ubuntu2
dmd-bin	2.075.0-0
mono-vbnc	4.6-0xamarin3+ubuntu1604b1
gnat-6	6.3.0-12ubuntu2
Worker Packages
virtualenv	15.1.0+ds-1
cgroup-tools	0.41-8ubuntu1
unzip	6.0-20ubuntu1
iptables-persistent	1.0.4+nmu1
Python 3.5 Packages
numpy 1.13.1
scipy 0.19.1
scikit-learn 0.18.2
pillow 4.2.1
h5py 2.7.0
tensorflow 1.2.1
keras 2.0.6
theano 0.9.0
shapely 1.5.17.post1
Python 3.6 Packages
numpy 1.13.1
scipy 0.19.1
scikit-learn 0.18.2
pillow 4.2.1
h5py 2.7.0
tensorflow 1.2.1
keras 2.0.6
theano 0.9.0
shapely 1.5.17
Ruby Gems
bundler (1.15.3)
Rust Packages
rustc 1.19.0 (0ade33941 2017-07-17)
cargo 0.20.0 (a60d185c8 2017-07-13)
rustup 1.5.0 (92d0d1e9e 2017-06-24)
Clojure Packages
Leiningen 2.7.1 on Java 1.8.0_131 OpenJDK 64-Bit Server VM
Groovy Packages
groovy: 2.4.12
Miniconda
conda 4.3.21

- build-essential	12.1ubuntu2
- gcc	4:6.3.0-2ubuntu1
- g++	4:6.3.0-2ubuntu1
- python3	3.5.3-1
- python3.6	3.6.1-1
- python3-pip	9.0.1-2
- git	1:2.11.0-2ubuntu0.1
- golang	2:1.7~1ubuntu1
- julia	0.4.7-6ubuntu1
- ocaml	4.02.3-6ubuntu2
- openjdk-8-jdk:amd64	8u131-b11-2ubuntu1.17.04.3
- php	1:7.0+49
- ruby	1:2.3.3
- scala	2.11.8-1
- nodejs	8.2.1-2nodesource1~zesty1
- mono-complete	5.0.1.1-0xamarin5+ubuntu1604b1
- dotnet-dev-1.1.0	1.1.0-1
- libgeos-dev	3.5.1-3
- virtualenv	15.1.0+ds-1
- cgroup-tools	0.41-8ubuntu1

Python 3.5 Packages:

- numpy 1.13.1
- scipy 0.19.1
- scikit-learn 0.18.2
- pillow 4.2.1
- h5py 2.7.0
- tensorflow 1.2.1
- keras 2.0.6
- theano 0.9.0
- shapely 1.5.17.post1

Python 3.6 Packages

- numpy 1.13.1
- scipy 0.19.1
- scikit-learn 0.18.2
- pillow 4.2.1
- h5py 2.7.0
- tensorflow 1.2.1
- keras 2.0.6
- theano 0.9.0
- shapely 1.5.17

Ruby Gems:

- bundler (1.15.3)

Rust Packages:

- rustc 1.19.0 (0ade33941 2017-07-17)
- cargo 0.20.0 (a60d185c8 2017-07-13)
- rustup 1.5.0 (92d0d1e9e 2017-06-24)

Clojure Packages:

- Leiningen 2.7.1 on Java 1.8.0_131 OpenJDK 64-Bit Server VM

Both Python 3.5 and 3.6 are installed. To use 3.6, you will have to explicitly use a `run.sh` file to invoke `python3.6`.

## Games

Bots are given 350 MB of RAM and equal amounts of CPU.

Games are always run using the most recent environment build.

### GPU-Enabled Games

The following packages are available during __runtime__ (__not__ compile time) for GPU-enabled bots:

- cuda	8.0.61-1
- libcupti-dev:amd64	8.0.44-3

Python 3.5 Packages:
- tensorflow-gpu 1.2.1

Python 3.6 Packages:
- tensorflow-gpu 1.2.1

[autocompile-script]: https://github.com/HaliteChallenge/Halite/blob/02b8a4a8c14498ddc471039c9a453137379420c1/worker/compiler.py
