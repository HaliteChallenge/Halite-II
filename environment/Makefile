CXXFLAGS += -std=c++11 -pthread -I ./
INSTALL_PATH?=/usr/local
SOURCES=$(shell find . -name "*.cpp")
OBJECTS=$(SOURCES:%.cpp=%.o)
TARGET=halite

.PHONY: all
all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJECTS)

.PHONY: install
install:
	install -m 0755 halite $(INSTALL_PATH)/bin
