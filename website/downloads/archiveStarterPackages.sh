cd ../../airesources/

cd Python
zip Python.zip ./*

cd ../C++
zip C++.zip ./*

cd ../Java
zip Java.zip ./*

cd ..

mv Java/Java.zip ../website/downloads
mv Python/Python.zip ../website/downloads
mv C++/C++.zip ../website/downloads

