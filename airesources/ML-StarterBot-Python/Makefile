.PHONY: model_short_training model_long_training clean_model clean_data default data

# Name of the data file
FILE=replays-20170930-sample.zip

# Halite binary address - please change here if you're not using MacOS
HALITE_BINARY_ADDRESS=https://halite.io/assets/downloads/Halite2_MacOS.zip

# There is nothing special about this seed (other that it's the 1 milionth prime) .
SEED=15485863

SOURCES_FOR_TRAINING= tsmlstarterbot/common.py \
	tsmlstarterbot/neural_net.py tsmlstarterbot/parsing.py \
	tsmlstarterbot/train.py

default: model_long_training

model_long_training: models/model_long_training.ckpt.meta

model_short_training: models/model_short_training.ckpt.meta

models/model_long_training.ckpt.meta: data/${FILE} ${SOURCES_FOR_TRAINING}
	mkdir -p models/
	python3 -m tsmlstarterbot.train --model_name model_long_training --data data/${FILE} --games_limit 1000 --steps 5000 --seed ${SEED}

models/model_short_training.ckpt.meta: data/${FILE} ${SOURCES_FOR_TRAINING}
	mkdir -p models/
	python3 -m tsmlstarterbot.train --model_name model_short_training --data data/${FILE} --games_limit 100 --steps 500 --seed ${SEED}

clean_model:
	rm -rf models

clean_data:
	rm -rf data

real_clean: clean_model clean_data
	rm -f submission.zip
	rm -f bin/halite

data: data/${FILE}

data/${FILE}:
	mkdir -p data
	curl https://storage.googleapis.com/ml-bot-data/${FILE} -o data/${FILE}

bin/halite:
	curl $(HALITE_BINARY_ADDRESS) -o halite_binary.zip
	unzip halite_binary.zip -d bin
	rm -rf halite_binary.zip

compare: bin/compare.sh MyBotShortTraining.py MyBot.py model_short_training model_long_training bin/halite
	bin/compare.sh MyBotShortTraining.py MyBot.py

submission: 
	zip -r submission.zip MyBot.py LANGUAGE hlt/ tsmlstarterbot/ models/

