The environment is based on Ubuntu 17.04, updates applied as of 2017/06/22.

These packages were installed from the standard package repositories:
- gcc 6.3.0-2ubuntu1
- g++ 6.3.0-2ubuntu1
- python3 3.5.3-1
- python3.6 3.6.1-1
- git 2.11.0-2ubuntu0.1
- golang 2:1.7~1ubuntu1
- julia 0.4.7-6ubuntu1
- ocaml 4.02.3-6ubuntu2
- openjdk-8-jdk 8u131-b11-0ubuntu1.17.04.1
- php 7.0+49
- ruby 2.3.3
- scala 2.11.8-1

These packages were installed from the respective PPAs given:

- [NodeSource][nodesource]: 
    - nodejs 8.1.2-1
- [Xamarin](http://www.mono-project.com/download):
    - mono-complete 5.0.1.1-0

Additionally, these packages were installed:

- Python 3.5 (`python3`):
    - NumPy 0.12.0
    - SciPy 0.19.0
    - scikit-learn 0.18.2
    - pillow 4.1.1
    - h5py 2.7.0
    - TensorFlow 1.2.0 (CPU)
    - Keras 2.0.5
- Python 3.6 (`python3.6`, `python3.6 -m pip`)
    - NumPy 1.13.0
    - SciPy 0.19.0
    - scikit-learn 0.18.2
    - pillow 4.1.1
    - h5py 2.7.0
    - TensorFlow 1.2.0 (CPU)
    - Keras 2.0.5
- Ruby:
    - Bundler 1.15.1
- Clojure:
    - Leiningen 2.7.1
- Rust:
    - rustup 1.4.0
    - rust 1.19.0-beta.2
    
Additionally, for running the game environment, the following packages were
installed:

- cgroup-tools 0.41-8ubuntu1
    
[nodesource]: https://nodejs.org/en/download/package-manager/#debian-and-ubuntu-based-linux-distributions