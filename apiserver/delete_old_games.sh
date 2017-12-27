#!/bin/bash

cd $(dirname $0)
. venv/bin/activate

export PYTHONPATH=$(pwd)

python -m apiserver.scripts.delete_old_games "$(pwd)/delete_old_games.log"
