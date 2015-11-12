#pragma once

#include "OpenGL.h"
#include "../Core/Halite.h"

void setup(unsigned short width, unsigned short height, unsigned char numplayers);

void render(std::vector<hlt::Map * > & full_game, short & turnNumber, float zoom);