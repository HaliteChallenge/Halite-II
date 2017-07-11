# Halite Website

## Setup

    $ sudo gem install bundler
    $ bundle install --path=vendor/bundle
    $ npm install
    $ npm run build
    $ bundle exec jekyll serve
    
Then visit <http://localhost:4000>.
    
If you modify the JavaScript, you might want to run the Webpack watcher:

    $ npm run watch
    
To deploy, you will need the [Google Cloud SDK][gcloud-sdk] installed and configured with an API key, the project, region, and zone. You will also want builds of the Halite environment provided if possible (in assets/environments/PLATFORM.zip).

    $ ./make_starter_kits.sh
    $ ./make_environments.sh
    $ npm run build
    $ bundle exec jekyll build
    $ gcloud compute scp --recurse _site/* ubuntu@GOOGLE_CLOUD_INSTANCE_NAME:/var/www/html

    E.g gcloud compute scp --recurse _site/* ubuntu@halite-web-server:/var/www/html

    
[gcloud-sdk]: https://cloud.google.com/sdk/gcloud/