#!/bin/bash
#
# script to download games from the cloud for a given day
# (something like this is done to create the replay.zip files)
#
if [ $# -ne 1 ]
then
    echo usage $0 yyyymmdd
    exit 1
fi

if [ -e $1 ]
then
    echo "File or directory named $1 already exists. Aborting."
    exit 1
fi

mkdir $1
cd $1
#can use replay-$1-0000* to debug this script
gsutil -m cp gs://halite-2-gold-replays/replay-$1-* ./

for i in replay-$1-*
do
    echo "decompressing $i"
    mv "$i" "$i.hlt"
    zstdless "$i.hlt" > "$i"
    rm "$i.hlt"
done

echo "downloaded games to $1/"

