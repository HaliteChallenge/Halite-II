CXXFLAGS += -std=c++11 `pkg-config --cflags glfw3`
SOURCES=$(shell find . -name "*.cpp")
OBJECTS=$(SOURCES:%.cpp=%.o)
TARGET=visualizer.o
LDFLAGS=`pkg-config --static --libs glfw3` -lglfw3 -lX11 -lXrandr -lXinerama -lGLEW -lXi -lXxf86vm -lXcursor -lGL -lpthread -lfreetype

.PHONY: all
all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@ $(LDFLAGS)

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJECTS)
