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
# TODO: figure out what exactly this does
# TODO: just port this all to Python?
for i in `seq 4 $((4+$NUMBOTS-1))`;
do
    BOT=${!i};

    BOTNAMEINDEX=$(($i+$NUMBOTS));
    BOTNAME=${!BOTNAMEINDEX};
    CGROUP=halitebot_$i

    BOTSTARTCOMMANDS+="\"cgexec -g cpu,memory:$CGROUP sh -c 'cd $PWD/$BOT && sudo -u bot_${i} -s ./$RUNFILE'\" "
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
