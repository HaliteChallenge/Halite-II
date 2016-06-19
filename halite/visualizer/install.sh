if [ "$(uname)" == "Darwin" ]; then
  echo "Mac detected"
  brew install homebrew/versions/glfw3 glew freetype pkg-config
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
  echo "Linux detected"
else
  echo "You are on an unsupported platform. Only Mac and Linux are supported by this install script."
fi
