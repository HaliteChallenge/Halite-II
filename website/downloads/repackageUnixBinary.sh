if [ $# -eq 0 ]; then
	echo "You need to supply the name of your zip as an arguement to this script."
else
	cd ../../environment
	make clean
	make
    cp environment "../website/downloads/HaliteEnvironment-$1"
fi
