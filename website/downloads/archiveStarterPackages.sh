rm Python.zip Java.zip C++.zip

cd ../../airesources/

rm __pycache__
zip -r Python.zip Python

zip -r C++.zip C++

zip -r Java.zip Java

mv Java.zip ../website/downloads
mv Python.zip ../website/downloads
mv C++.zip ../website/downloads

