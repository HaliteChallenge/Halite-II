add-apt-repository -y ppa:ubuntu-toolchain-r/test
apt-get update

# Python
apt-get install -y python3

# Java
apt-get install -y openjdk-8-jdk libjansi-java

# Rust
curl -sSf https://static.rust-lang.org/rustup.sh | sh

# C++
apt-get install -y g++-4.9

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
wget https://phar.phpunit.de/phpunit-5.7.phar
chmod +x phpunit-5.7.phar
mv phpunit-5.7.phar /usr/local/bin/phpunit


php -v
mysql -V
phpunit --version

update-alternatives --set java $(update-alternatives --list java | grep java-8-openjdk)
update-alternatives --set javac $(update-alternatives --list javac | grep java-8-openjdk)
update-alternatives --display java
update-alternatives --display javac
java -version
javac -version
