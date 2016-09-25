add-apt-repository -y ppa:ubuntu-toolchain-r/test
apt-get update

# Python
apt-get install -y python3 

# Java
apt-get install -y openjdk-7-jdk libjansi-java

# Rust
curl -sSf https://static.rust-lang.org/rustup.sh | sh 

# C++
apt-get install -y g++-4.9
export CXX="g++-4.9"

# Scala
wget www.scala-lang.org/files/archive/scala-2.10.4.deb
dpkg -i scala-2.10.4.deb
apt-get update -y
apt-get install -y scala
wget https://bintray.com/artifact/download/sbt/debian/sbt-0.13.6.deb
dpkg -i sbt-0.13.6.deb
apt-get update -y
apt-get install -y sbt

# Php unit
wget https://phar.phpunit.de/phpunit.phar
chmod +x phpunit.phar
mv phpunit.phar /usr/local/bin/phpunit


php -v
mysql -V
phpunit --version
