rm *-Starter-Package.zip

cd ../../airesources/

rm -r *-Starter-Package

mkdir Halite-Python-Starter-Package Halite-Java-Starter-Package Halite-C++-Starter-Package Halite-Rust-Starter-Package

cp -r Python/* Halite-Python-Starter-Package/
cp -r Java/* Halite-Java-Starter-Package/
cp -r C++/* Halite-C++-Starter-Package/
cp -r Rust/* Halite-Rust-Starter-Package/

zip -r Halite-Python-Starter-Package.zip Halite-Python-Starter-Package/ 
zip -r Halite-C++-Starter-Package.zip Halite-C++-Starter-Package/
zip -r Halite-Java-Starter-Package.zip Halite-Java-Starter-Package/ 
zip -r Halite-Rust-Starter-Package.zip Halite-Rust-Starter-Package/ 

mv *.zip ../website/downloads

rm -r *-Starter-Package
