if [ $# -eq 0 ]; then
	echo "You need to supply the name of your zip as an arguement to this script."
else
	ZIP="Halite-$1.zip"

	cd ../../halite
	mkdir Halite-$1

	cd environment
	make clean
	make
	cp environment ../Halite-$1

	cd ../visualizer
	make clean
	make
	cp -r fonts shaders visualizer ../Halite-$1

	cd ../
	zip -r $ZIP Halite-$1
	rm -r Halite-$1	

	cd ../website/downloads
	rm $ZIP 
	mv ../../halite/$ZIP ./

fi
