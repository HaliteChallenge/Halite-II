from tsmlstarterbot.bot import Bot

# The purpose of this bot is to see how much the training helps. To compare this bot with the bot trained with default
# settings run "make compare".
Bot(location="model_short_training.ckpt", name="MyBotShortTraining").play()
