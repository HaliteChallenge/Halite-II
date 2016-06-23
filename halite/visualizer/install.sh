if [ "$(uname)" == "Darwin" ]; then
  	echo "Mac detected"
  	brew install homebrew/versions/glfw3 glew freetype pkg-config
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
	echo "Linux detected"
	sudo apt-get install -qq g++-4.8 libstdc++6-4.7-dev libglfw-dev libfreetype6-dev pkg-config
else
  echo "You are on an unsupported platform. Only Mac and Linux are supported by this install script."
fi
