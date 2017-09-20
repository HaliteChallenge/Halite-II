#!/usr/bin/env bash
sudo gem install bundler
bundle install --path=vendor/bundle

npm install

npm run build
bundle exec jekyll build
