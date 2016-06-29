# Installation

## Website/Manager server setup

* Execute: ```$ cd HALITE_FOLDER && ./setupWebsite.sh```
* Allow .htaccess override
* Enable apache modules named mod_rewrite and php5_module
* Create and write a halite.ini file in the root directory of the project

## Database server setup

* Execute: ```$ cd HALITE_FOLDER && ./setupDatabase.sh```
* Add a superuser by wildcarding its host and allowing remote login
 * ```CREATE USER 'superuser'@'%' IDENTIFIED BY 'SOME_RANDOM_PASSWORD';```
 * ```GRANT ALL PRIVILEGES ON *.* TO 'superuser'@'%' IDENTIFIED BY 'SOME_RANDOM_PASSWORD' WITH GRANT OPTION;```
 * ```FLUSH PRIVILEGES;```
 * Comment out `bind-address = 127.0.0.1` in `/etc/mysql/my.cnf`
 * ```sudo service mysql restart```

## Worker server setup

* Execute: ```cd HALITE_FOLDER/worker && ./install.sh```
* Create the worker's `halite.ini` file
 * Create a `[worker]` section
 * Fill in `apiKey`, `managerURL`, and `emailPassword`
* Add the `apiKey` and the worker's ip address to the `Worker` table
* Execute ```sudo python3 worker.py```
