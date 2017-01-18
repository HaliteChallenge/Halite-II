rm *-Starter-Package.zip

cd ../airesources/

rm -r *-Starter-Package

mkdir Halite-Python-Starter-Package \
      Halite-Java-Starter-Package \
      Halite-C++-Starter-Package \
      Halite-Rust-Starter-Package \
      Halite-C#-Starter-Package \
      Halite-Scala-Starter-Package \
      Halite-Ruby-Starter-Package \
      Halite-Go-Starter-Package \
      Halite-PHP-Starter-Package \
      Halite-JavaScript-Starter-Package \
      Halite-OCaml-Starter-Package \
      Halite-Clojure-Starter-Package \
      Halite-C-Starter-Package \
      Halite-Julia-Starter-Package

cp -r Python/* Halite-Python-Starter-Package/
cp -r Java/* Halite-Java-Starter-Package/
cp -r C++/* Halite-C++-Starter-Package/
cp -r Rust/* Halite-Rust-Starter-Package/
cp -r Java/* Halite-Scala-Starter-Package/
cp -r CSharp/* Halite-C#-Starter-Package/
cp -r Ruby/* Halite-Ruby-Starter-Package/
cp -r Go/* Halite-Go-Starter-Package/
cp -r PHP/* Halite-PHP-Starter-Package/
cp -r JavaScript/* Halite-JavaScript-Starter-Package/
cp -r OCaml/* Halite-OCaml-Starter-Package/
cp -r Clojure/* Halite-Clojure-Starter-Package/
cp -r C/* Halite-C-Starter-Package/
cp -r Julia/* Halite-Julia-Starter-Package/

cp -r Scala/* Halite-Scala-Starter-Package/
rm Halite-Scala-Starter-Package/MyBot.java

zip -r Halite-Python-Starter-Package.zip Halite-Python-Starter-Package/
zip -r Halite-C++-Starter-Package.zip Halite-C++-Starter-Package/
zip -r Halite-Java-Starter-Package.zip Halite-Java-Starter-Package/ 
zip -r Halite-Rust-Starter-Package.zip Halite-Rust-Starter-Package/ 
zip -r Halite-C#-Starter-Package.zip Halite-C#-Starter-Package/ 
zip -r Halite-Scala-Starter-Package.zip Halite-Scala-Starter-Package/ 
zip -r Halite-Ruby-Starter-Package.zip Halite-Ruby-Starter-Package/ 
zip -r Halite-Go-Starter-Package.zip Halite-Go-Starter-Package/ 
zip -r Halite-PHP-Starter-Package.zip Halite-PHP-Starter-Package/ 
zip -r Halite-JavaScript-Starter-Package.zip Halite-JavaScript-Starter-Package/
zip -r Halite-OCaml-Starter-Package.zip Halite-OCaml-Starter-Package/
zip -r Halite-Clojure-Starter-Package.zip Halite-Clojure-Starter-Package/
zip -r Halite-C-Starter-Package.zip Halite-C-Starter-Package/
zip -r Halite-Julia-Starter-Package.zip Halite-Julia-Starter-Package/

mkdir -p ../website/downloads/starterpackages
mv *.zip ../website/downloads/starterpackages

rm -r *-Starter-Package
