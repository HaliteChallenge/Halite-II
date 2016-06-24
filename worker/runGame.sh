ENVIRONMENT="Environment"
RUNFILE="run.sh"
WORKINGPATH="workingPath"

WIDTH=$1
HEIGHT=$2
BOT1=$3
BOT2=$4

mkdir $WORKINGPATH
cp $ENVIRONMENT $WORKINGPATH
mv $BOT1 $WORKINGPATH
mv $BOT2 $WORKINGPATH

cd $WORKINGPATH

chmod +x "$BOT1/$RUNFILE"
chmod +x "$BOT2/$RUNFILE"

docker run -v $PWD:$PWD virtual_machine sh -c "cd $PWD && chmod +x $ENVIRONMENT && ./$ENVIRONMENT $WIDTH $HEIGHT 'cd $PWD/$BOT1 && ./$RUNFILE' 'cd $PWD/$BOT2 && ./$RUNFILE'"

mv *.hlt ../
cd ..
rm -r $WORKINGPATH
