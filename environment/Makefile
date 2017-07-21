CXXFLAGS += -std=c++11 -pthread -I ./ -g -fsanitize=address,undefined -O3
INSTALL_PATH?=/usr/local
SOURCES=$(shell find . -name "*.cpp")
CSOURCES=$(shell find . -name "*.c")
COBJECTS=$(CSOURCES:%.c=%.o)
OBJECTS=$(SOURCES:%.cpp=%.o) $(COBJECTS)
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
