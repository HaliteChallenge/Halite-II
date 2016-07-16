############################################################
# Dockerfile to build sandbox for executing user code
# Based on Ubuntu
############################################################

FROM chug/ubuntu14.04x64
MAINTAINER ASAD MEMON, OSMAN ALI

# Update the repository sources list
RUN echo "deb http://archive.ubuntu.com/ubuntu trusty main universe" > /etc/apt/sources.list
RUN apt-get update
#RUN apt-get upgrade
#Install all the languages/compilers we are supporting.
RUN apt-get install -y libstdc++6
RUN apt-get install -y gcc
RUN apt-get install -y g++
RUN apt-get install -y python
RUN apt-get install -y python3-pip
RUN curl -sSf https://static.rust-lang.org/rustup.sh | sh 

#prepare for Java download
RUN apt-get install -y python-software-properties
RUN apt-get install -y software-properties-common

#grab oracle java (auto accept licence)
RUN add-apt-repository -y ppa:webupd8team/java
RUN apt-get update
RUN echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections
RUN apt-get install -y oracle-java7-installer

RUN apt-get install -y gobjc
RUN apt-get install -y gnustep-devel &&  sed -i 's/#define BASE_NATIVE_OBJC_EXCEPTIONS     1/#define BASE_NATIVE_OBJC_EXCEPTIONS     0/g' /usr/include/GNUstep/GNUstepBase/GSConfig.h

RUN apt-get install -y sudo
RUN apt-get install -y bc

RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN apt-get -y update
RUN apt-get -y upgrade
RUN apt-get -y dist-upgrade

# Libraries
RUN pip3 install --upgrade https://storage.googleapis.com/tensorflow/linux/cpu/tensorflow-0.9.0-cp34-cp34m-linux_x86_64.whl
RUN apt-get install -y python3-numpy python3-scipy python3-dev python3-pip python3-nose g++ libblas-dev git
RUN pip3 install Theano
RUN git clone https://github.com/fchollet/keras.git
WORKDIR keras/
RUN python3 setup.py install
WORKDIR ../
RUN apt-get install -y python3-h5py
