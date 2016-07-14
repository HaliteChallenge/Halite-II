CXXFLAGS += -g -I/opt/X11/include/freetype2/freetype -I/opt/X11/include/freetype2 -I/usr/local/include/freetype2 -I/usr/include/freetype2 -std=c++11 `pkg-config --cflags glfw3`
SOURCES=$(shell find . -name "*.cpp")
OBJECTS=$(SOURCES:%.cpp=%.o)
TARGET=visualizer
LDFLAGS= -lpthread -lz -lbz2 libGLEW.a libpng16.a libpng.a libglfw3.a libfreetype.a -framework Cocoa -framework OpenGL -framework IOKit -framework CoreVideo
.PHONY: alls
all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ $(LDFLAGS)

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJECTS)