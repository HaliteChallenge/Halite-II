#!/bin/bash

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
NUMBOTS=$3
BOTSTART=4

mkdir $WORKINGPATH
cp $ENVIRONMENT $WORKINGPATH
for BOT in ${@:$BOTSTART:$NUMBOTS};
	do mv $BOT $WORKINGPATH;
done

cd $WORKINGPATH
for BOT in ${@:$BOTSTART:$NUMBOTS};
	do chmod +x "$BOT/$RUNFILE";
done;

BOTSTARTCOMMANDS=""
for i in `seq $BOTSTART $((4+$NUMBOTS-1))`;
do
	BOT=${!i}
	
	BOTNAMEINDEX=$(($i+$NUMBOTS))
	BOTNAME=${!BOTNAMEINDEX}

	BOTSTARTCOMMANDS="$BOTSTARTCOMMANDS 'cd $PWD/$BOT && ./$RUNFILE' '$BOTNAME'";
done

docker run -v $PWD:$PWD virtual_machine sh -c "cd $PWD && chmod +x $ENVIRONMENT && ./$ENVIRONMENT -q -s $WIDTH $HEIGHT $BOTSTARTCOMMANDS"

docker rm -v $(docker ps -aq) >/dev/null

rm /run/network/ifstate.veth*

mv *.hlt ../
cd ..
rm -r $WORKINGPATH
