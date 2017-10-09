import tsmlstarterbot

# Load the model from the models directory. Models directory is created during training.
# Run "make" to download data and train.
tsmlstarterbot.Bot(location="model_long_training.ckpt", name="MyBot").play()
