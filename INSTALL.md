# Installation

## Website/Manager server setup

* Execute: ```$ cd HALITE_FOLDER && ./setupWebsite.sh```
* Allow .htaccess override
* Enable apache modules named mod_rewrite and php5_module
* Create and write a halite.ini file in the root directory of the project

## Database server setup

* Execute: ```$ cd HALITE_FOLDER && ./setupDatabase.sh```
* Add a superuser by wildcarding its host and allowing remote login

## Worker server setup

Execute:

```cd HALITE_FOLDER/worker && ./install.sh && sudo python3 worker.py```
