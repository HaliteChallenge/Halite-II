ENVIRONMENT="environment"
RUNFILE="run.sh"
WORKINGPATH="workingPath"

if [ ! -f $ENVIRONMENT ]; then
	echo "NO ENVIRONMENT!!"
	cd ../halite/environment
	make clean
	make
	mv environment ../../worker
	cd ../../worker
fi

WIDTH=$1
HEIGHT=$2

mkdir $WORKINGPATH
cp $ENVIRONMENT $WORKINGPATH
for BOT in $(eval echo {$3..$END});
	do mv $BOT $WORKINGPATH;
done

cd $WORKINGPATH
for BOT in $(eval echo {$3..$END});
	do chmod +x "$BOT/$RUNFILE";
done

BOTSTARTCOMMANDS = ""
for BOT in $(eval echo {$3..$END});
	do BOTSTARTCOMMANDS="$BOTSTARTCOMMANDS 'cd $PWD/$BOT && ./$RUNFILE'";
done

docker run -v $PWD:$PWD virtual_machine sh -c "cd $PWD && chmod +x $ENVIRONMENT && ./$ENVIRONMENT -q $WIDTH $HEIGHT $BOTSTARTCOMMANDS"

docker rm -v $(docker ps -aq) >/dev/null

rm /run/network/ifstate.veth*

mv *.hlt ../
cd ..
rm -r $WORKINGPATH
