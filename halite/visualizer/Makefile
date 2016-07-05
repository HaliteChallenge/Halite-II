CXXFLAGS += -g -I/opt/X11/include/freetype2 -I/usr/local/include/freetype2 -I/usr/include/freetype2 -std=c++11 `pkg-config --cflags glfw3`
SOURCES=$(shell find . -name "*.cpp")
OBJECTS=$(SOURCES:%.cpp=%.o)
TARGET=visualizer
LDFLAGS= -lGLEW `pkg-config --static --libs glfw3` -lGL -lpthread -lfreetype

.PHONY: alls
all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ $(LDFLAGS)

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJECTS)
