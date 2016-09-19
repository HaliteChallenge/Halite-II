# Installation

If you have not already:

```
git clone https://github.com/HaliteChallenge/Halite.git
```

### Website/Manager Server Setup

* Execute: `cd website && ./install.sh`
* Check that you are on php >= 5.6 and mysql >= 5.6.5: `php -v; mysql -V`
* Symlink the repo to /var/www: `ln -s ~/Halite /var/www`
* Create and write a halite.ini file in the root directory of the project
* Apache setup
 * [Allow the following of symlinks by apache](http://superuser.com/questions/244245/how-do-i-get-apache-to-follow-symlinks)
 * [Allow .htaccess override](http://stackoverflow.com/a/22526144)
 * [Redirect root directory to website directory](http://serverfault.com/questions/9992/how-to-get-apache2-to-redirect-to-a-subdirectory)
 * Tell apache to forbid access to the storage/errors and storage/bots folders and to the halite.ini file

### Database server setup

* Execute: `cd website/sql && ./install.sh`
* Add a superuser by wildcarding its host and allowing remote login
 * `CREATE USER 'superuser'@'%' IDENTIFIED BY 'SOME_RANDOM_PASSWORD';`
 * `GRANT ALL PRIVILEGES ON *.* TO 'superuser'@'%' IDENTIFIED BY 'SOME_RANDOM_PASSWORD' WITH GRANT OPTION;`
 * `FLUSH PRIVILEGES;`
 * Comment out `bind-address = 127.0.0.1` in `/etc/mysql/my.cnf`
 * `sudo service mysql restart`

### Worker server setup

Run: `cd website/php && python3 openNewWorker.py`. Make sure that you have the proper AWS key pair.
