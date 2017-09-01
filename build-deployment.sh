mkdir Halite
mkdir "Halite/website"
mkdir "Halite/website/_site"
cp -rf apiserver Halite/
cp -rf website/_site/* Halite/website/_site
cp -f ../admin-scripts/config.py Halite/apiserver/apiserver/
tar -cvzf ../Halite.tgz Halite
rm -r Halite
