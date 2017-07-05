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