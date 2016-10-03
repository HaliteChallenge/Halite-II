# Installation

If you have not already:

    git clone https://github.com/HaliteChallenge/Halite.git

### Website/Manager Server Setup

    $ cd website && ./install.sh

Check that you are on php >= 5.6 and mysql >= 5.6.5: 

    $ `php -v; mysql -V
    
Symlink the repo to /var/www:

    ln -s ~/Halite /var/www

Create and write a halite.ini file in the root directory of the project

Finish Apache setup:

 * [Allow the following of symlinks by apache](http://superuser.com/questions/244245/how-do-i-get-apache-to-follow-symlinks)
 * [Allow .htaccess override](http://stackoverflow.com/a/22526144)
 * [Redirect root directory to website directory](http://serverfault.com/questions/9992/how-to-get-apache2-to-redirect-to-a-subdirectory)
 * Tell apache to forbid access to the storage/errors and storage/bots folders and to the halite.ini file
 * [Increase your max file upload size (worker's posting large replays, users posting big bot archives)](http://stackoverflow.com/questions/2184513/php-change-the-maximum-upload-file-size)

To setup automatic backups on the website server, copy the `backupWebsite` file in the `Halite/website/cron` folder into the server's `/etc/cron.hourly` folder, change the IP address in the file to that of the backup server, and make sure that the ssh key of the website server is in the `~/.ssh/authorized_keys` file on the backup server. Once copied, mark the `backupWebsite` file as executable and give cron permission to execute it like so:

    cd /etc/cron.hourly
    chmod +x backupWebsite
    chmod 755 backupWebsite

### Database server setup

    $ cd website/sql && ./install.sh
    
Add a superuser by wildcarding its host and allowing remote login. In the MySQL shell:

    CREATE USER 'superuser'@'%' IDENTIFIED BY 'SOME_RANDOM_PASSWORD';
    GRANT ALL PRIVILEGES ON *.* TO 'superuser'@'%' IDENTIFIED BY 'SOME_RANDOM_PASSWORD' WITH GRANT OPTION;
    FLUSH PRIVILEGES;

To finish superuser setup, comment out `bind-address = 127.0.0.1` in `/etc/mysql/my.cnf`, and:
 
    $ sudo service mysql restart

For ease of use of the mysql on the command line and for install scripts to be guranteed to run smoothly, please edit your `~/.my.cnf` to include these lines:

    [mysqldump]
    user=MYSQL_USERNAME
    password=MYSQL_PASSWORD

    [client]
    user=MYSQL_USERNAME
    password=MYSQL_PASSWORD

To setup automatic backups on the db server, copy the `backupDatabase` file in the `Halite/website/cron` folder into the server's `/etc/cron.hourly` folder, change the IP address in the file to the one of the backup server, and make sure that the ssh key of the db server is in the `~/.ssh/authorized_keys` file on the backup server. Once copied, mark the `backupDatabase` file as executable and give anyone permission to execute it like so:

    cd /etc/cron.hourly
    chmod +x backupDatabase
    chmod 755 backupDatabase

### Worker server setup
***Note:*** Make sure that your `halite.ini` file points to the proper AWS key pair.

On your local machine:

    $ cd website/php && python3 openNewWorker.py

