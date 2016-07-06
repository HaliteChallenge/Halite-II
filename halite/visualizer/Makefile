<<<<<<< HEAD
CXXFLAGS += -g -DGLEW_STATIC -I/opt/X11/include/freetype2 -I/usr/local/include/freetype2 -I/usr/include/freetype2 -std=c++11 `pkg-config --cflags glfw3`
=======
CXXFLAGS += -I/opt/X11/include/freetype2 -I/usr/local/include/freetype2 -I/usr/include/freetype2 -std=c++11 `pkg-config --cflags glfw3`
>>>>>>> 53ffe42cf718cfedaa3ec329b0688c093513683c
SOURCES=$(shell find . -name "*.cpp")
OBJECTS=$(SOURCES:%.cpp=%.o)
TARGET=visualizer
LDFLAGS= ../../../glew/lib/libGLEW.a `pkg-config --static --libs glfw3` -lGL -lpthread -lfreetype

.PHONY: alls
all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ $(LDFLAGS)

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJECTS)
