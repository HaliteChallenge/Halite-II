FROM ubuntu:latest

RUN apt-get update

RUN apt-get install -y sudo
RUN apt-get install -y curl

RUN apt-get install -y libstdc++6
RUN apt-get install -y gcc
RUN apt-get install -y g++
RUN apt-get install -y python3 python3-pip python3-dev python3-numpy
RUN curl -sSf https://static.rust-lang.org/rustup.sh | sh 
RUN apt-get install -y python-software-properties
RUN apt-get install -y software-properties-common
RUN apt-get install -y openjdk-8-jdk
RUN apt-get install -y scala

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/mono-xamarin.list
RUN apt-get update
RUN apt-get install -y mono-devel
