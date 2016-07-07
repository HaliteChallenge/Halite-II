if [ $# -eq 0 ]; then
	echo "You need to supply the name of your zip as an arguement to this script."
else
	ZIP="$1.zip"

	cd ../../halite/environment
	make clean
	make
	zip $ZIP environment
	mv $ZIP ../

	cd ../visualizer
	make clean
	make
	zip -ur ../$ZIP fonts shaders visualizer

	cd ../../website/downloads
	rm $ZIP 
	mv ../../halite/$ZIP ./

fi
