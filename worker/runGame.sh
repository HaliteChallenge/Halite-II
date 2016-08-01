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

	BOTSTARTCOMMANDS="$BOTSTARTCOMMANDS '/usr/bin/docker run --net=none --memory=\"512m\" --cpu-shares=1024 -i -v $PWD/$BOT:$PWD/$BOT virtual_machine sh -c \"cd $PWD/$BOT && ./$RUNFILE\"' '$BOTNAME'";
done

eval "chmod +x $ENVIRONMENT"

eval "./$ENVIRONMENT -q -o -d $WIDTH $HEIGHT $BOTSTARTCOMMANDS"

docker stop  $(docker ps -aq) >/dev/null
docker rm -v $(docker ps -aq) >/dev/null

rm /run/network/ifstate.veth*

mv *.hlt ../
mv *.log ../
cd ..
rm -r $WORKINGPATH
