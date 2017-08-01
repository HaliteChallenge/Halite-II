# Halite Website

## Setup

Assuming you have RubyGems and NPM installed:

    $ sudo gem install bundler
    $ bundle install --path=vendor/bundle
    $ npm install
    $ cd ../libhaliteviz
    $ npm install
    $ cd ../website
    $ npm run build
    $ bundle exec jekyll serve
    
Then visit <http://localhost:4000>.
    
If you modify the JavaScript, you might want to run the Webpack watcher, which will rebuild the JavaScript bundle on the fly:

    $ npm run watch
    
To deploy, you will need the [Google Cloud SDK][gcloud-sdk] installed and configured with an API key, the project, region, and zone. You will also want a folder with compiled Halite environment binaries. The binaries should be named after the platform they are for, e.g. `MacOS`, `Linux-x64`, and `Windows.exe`. Run `make_starter_kits.py` to generate all the starter kits and source downloads.

Currently, there is no automation for building the offline visualizer.

    $ # Make sure to kill Jekyll and Webpack beforehand, if they're running
    $ python3 make_starter_kits.py path/to/folder/with/compiled/halite/environments
    $ npm run build
    $ bundle exec jekyll build
    $ gcloud compute scp --recurse _site/* ubuntu@GOOGLE_CLOUD_INSTANCE_NAME:/var/www/html

    E.g gcloud compute scp --recurse _site/* ubuntu@halite-web-server:/var/www/html

    
[gcloud-sdk]: https://cloud.google.com/sdk/gcloud/