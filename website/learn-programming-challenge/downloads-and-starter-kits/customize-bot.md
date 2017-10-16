---
layout: doc_page
title: Customize your bot
toc: false
sort_key: 3
---

There are several ways to extend and customize your bot submission, including installing dependencies, changing the language of your bot and exercising package managers pre-installeD on our game servers. 

## Customizing your language name

If you are using a language that is generic or that does not have first class support on the server, you can include a file named LANGUAGE containing the name of the language you are using. This will be used only for display on the rankings and in your profile.

This is especially useful when you are uploading a Jar file for JVM languages or writing ML bots, and you want to specifically call out a language like `Kotlin` or in the case of ML use `ML` in lieu of language. 

## Installing Dependencies

Instead of packaging up your dependencies inside your zip file, you may include a bash file named `install.sh` that will be run before your bot is compiled.

It will be run with internet access and write access to only its current directory. It may run for a `maximum of 10 minutes` and will not be able to make global installation changes (i.e. `apt-get` will not work).

## Package Managers

The following package managers are already installed on the server and can be used to install dependencies locally:

- `pip3` (Python 3.5)
- `python3.6 -m pip` (Python 3.6)
- `virtualenv`
- `npm`
- `cargo`

`curl` and `wget` are also available and can be used to download additional runtimes, tools, and environments.

If your library isn't on a package manager that supports local installation and you can’t download it with `curl`, you are going to have to compile it on our game servers. Include the source of you library in your bot's zip file and put compilation instructions in the `install.sh` file.

**Note:**

If you are using `pip`, then make sure to use the `--system` flag with the `--target` flag, due to how Debian/Ubuntu patch the `pip` packages (reference: [pip#3826](https://github.com/pypa/pip/issues/3826)).

## Preinstalled Libraries

For convenience's sake, we include `tensorflow`, `keras (using a tensorflow backend)`, `numpy`, `scipy`, `scikit-learn`, `pillow`, `pandas` and `h5py` on our game servers. Just import these libraries from your python files like normal.

## Compilation

Bot compilation is done using [this autocompile script](https://github.com/HaliteChallenge/Halite-II/blob/master/apiserver/worker/compiler.py). Many languages will be properly auto-detected and compiled if needed without the need for an install.sh script.

Your main file must be called `MyBot.*` Your language is recognized using the file extension of your MyBot file. The appropriate file extensions for each language are:

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

See the [Game Server Reference](/other-resources/game-servers) for details about compiler and runtime versions.

## JVM Languages

For JVM languages, you can submit a jar file inside of your zip file instead of source files. The jar will be executed java -jar MyBot.jar so you need to define a Main-Class header in the manifest.

## Custom run script

You may supply a run.sh script to control how your bot is run. Many languages will be properly auto detected and run without the need for an install.sh script. You should only include a custom run.sh script if you have a real need for one.

## Custom Runtime

You could use a run.sh file to use a custom runtime such as PyPy instead of the default Python 3.


