#!/usr/bin/env bash
# python3 make_starter_kits.py path/to/folder/with/compiled/halite/environments
npm install
cd ../libhaliteviz
npm install
cd ../website
npm run build
bundle exec jekyll build
gcloud compute scp --recurse _site/* ubuntu@halite-web-server-staging:/var/www/html