#!/usr/bin/env bash
sudo gem install bundler
bundle install --path=vendor/bundle

# # Set up builds folder with Windows.exe, Linux-x64, MacOS halite environment executables
# # you need make sure that website/assets/downloads and website/_data needs to be created for this script to work
python3 make_starter_kits.py 0.9.0-Beta ../../builds/

# # building the website
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

# js
cd ../js
uglifyjs --compress --mangle -- bundle.js > bundle.min.js
cp bundle.min.js bundle.js
rm -f bundle.min.js

uglifyjs --compress --mangle -- f49921291dbd28462a61.worker.js > f49921291dbd28462a61.worker.min.js
cp f49921291dbd28462a61.worker.min.js f49921291dbd28462a61.worker.js
rm -f f49921291dbd28462a61.worker.min.js

uglifyjs --compress --mangle -- 276d7a58ef9843d54ee0.worker.js > 276d7a58ef9843d54ee0.worker.min.js
cp 276d7a58ef9843d54ee0.worker.min.js 276d7a58ef9843d54ee0.worker.js
rm -f 276d7a58ef9843d54ee0.worker.min.js
