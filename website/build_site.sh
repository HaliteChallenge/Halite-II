#!/usr/bin/env bash
sudo gem install bundler
bundle install --path=vendor/bundle

# Set up builds folder with Windows.exe, Linux-x64, MacOS halite environment executables
# you need make sure that website/assets/downloads and website/_data needs to be created for this script to work
python3 make_starter_kits.py 0.9.0-Beta ../../builds/

# building the website
npm install
cd ../libhaliteviz
npm install
cd ../website
npm run build
bundle exec jekyll build


# post processing steps
cd _site/assets/css

# keeping it separate for now pending decision on code editor workflows
postcss main_new.css > main_new_min.css
cp main_new_min.css main_new.css
rm -f main_new_min.css
