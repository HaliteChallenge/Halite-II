# Installation

If you have not already:

```git clone https://github.com/HaliteChallenge/Halite.git```

## Website/Manager server setup

* ```sudo apt-get install lamp-server^```
* Symlink the repo to /var/www: ```ln -s ~/Halite /var/www```
* ```./setupWebsite.sh```
* Create and write a halite.ini file in the root directory of the project
* Apache setup
 * Allow the following of symlinks by apache
 * Allow .htaccess override
 * Enable apache modules named mod_rewrite and php5_module, if not already enabled
 * [Redirect root directory to website directory](http://serverfault.com/questions/9992/how-to-get-apache2-to-redirect-to-a-subdirectory)
 * Dont forget to restart apache: ```sudo service apache2 restart```

## Database server setup

* Execute: ```./setupDatabase.sh```
* Add a superuser by wildcarding its host and allowing remote login
 * ```CREATE USER 'superuser'@'%' IDENTIFIED BY 'SOME_RANDOM_PASSWORD';```
 * ```GRANT ALL PRIVILEGES ON *.* TO 'superuser'@'%' IDENTIFIED BY 'SOME_RANDOM_PASSWORD' WITH GRANT OPTION;```
 * ```FLUSH PRIVILEGES;```
 * Comment out `bind-address = 127.0.0.1` in `/etc/mysql/my.cnf`
 * ```sudo service mysql restart```

## Worker server setup

* Execute: ```./install.sh```
* Create a `halite.ini` file in the root directory of the project
 * Create a `[worker]` section
 * Fill in `apiKey`, `managerURL`, and `emailPassword`
* Add the `apiKey` and the ip address of the worker to the `Worker` table
* Execute ```sudo python3 worker.py```

## Enable automatic pulling

Give apache pulling permission

```chown -R ./ .git && ```

Create a `git_pull.php` script

```<?php echo shell_exec("/usr/bin/git pull"); ?>```

Tell github to call `git_pull.php` by adding it as a Halite repo webhook
