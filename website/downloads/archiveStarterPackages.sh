rm *-Starter-Package.zip

cd ../../airesources/

rm -r *-Starter-Package

mkdir Halite-Python-Starter-Package Halite-Java-Starter-Package Halite-C++-Starter-Package

cp Python/* Halite-Python-Starter-Package/
cp Java/* Halite-Java-Starter-Package/
cp C++/* Halite-C++-Starter-Package/

zip -r Halite-Python-Starter-Package.zip Halite-Python-Starter-Package/ 
zip -r Halite-C++-Starter-Package.zip Halite-C++-Starter-Package/
zip -r Halite-Java-Starter-Package.zip Halite-Java-Starter-Package/ 

mv *.zip ../website/downloads

rm -r *-Starter-Package
