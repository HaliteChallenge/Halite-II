#!/bin/bash

ENVIRONMENT="halite"
RUNFILE="run.sh"
WORKINGPATH="workingPath"

if [ ! -f $ENVIRONMENT ]; then
    echo "NO ENVIRONMENT!!"
    cd ../../environment
    make clean
    make 
    mv halite ../apiserver/worker
    cd ../apiserver/worker
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
for BOT in ${@:$BOTSTART:$NUMBOTS}; do
    chmod +x "$BOT/$RUNFILE";
    chmod 755 "$BOT/$RUNFILE";
done;

BOTSTARTCOMMANDS=""
for i in `seq $BOTSTART $((4+$NUMBOTS-1))`;
do
    BOT=${!i};
    
    BOTNAMEINDEX=$(($i+$NUMBOTS));
    BOTNAME=${!BOTNAMEINDEX};
    CGROUP=halitebot_$i

    # Grant control over the group to our user
    sudo cgcreate -g cpu,memory:/${CGROUP} -t worker:worker
    cgset -r cpu.shares=1024 memory.limit_in_bytes=$((350*1024*1024)) ${CGROUP}

    BOTSTARTCOMMANDS+="\"cgexec -g cpu,memory:$CGROUP sh -c 'cd $PWD/$BOT && ./$RUNFILE'\" "
    BOTSTARTCOMMANDS+="\"$BOTNAME\" ";
done

eval "chmod +x $ENVIRONMENT"

RUN_GAME_COMMAND="./$ENVIRONMENT -q -o -d \"$WIDTH $HEIGHT\" $BOTSTARTCOMMANDS"
>&2 echo $RUN_GAME_COMMAND;
eval $RUN_GAME_COMMAND;

mv *.hlt ../
mv *.log ../
cd ..
rm -r $WORKINGPATH
