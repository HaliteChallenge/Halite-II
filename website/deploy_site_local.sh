#!/usr/bin/env bash
sudo gem install bundler
bundle install --path=vendor/bundle
npm install
cd ../libhaliteviz
npm install
cd ../website
npm run build
bundle exec jekyll serve