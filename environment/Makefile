PLATFORM=$(shell uname)

CXXFLAGS += -std=c++11 -pthread -I ./ -g -O2 -Wall -pedantic -fsanitize=address,undefined
INSTALL_PATH?=/usr/local

ZSTD_PATH=./zstd-1.3.0
ifeq ($(PLATFORM),Darwin)
ZSTD_STATIC_OBJECT=libzstd.dylib
else
ZSTD_STATIC_OBJECT=libzstd.a
endif

LDFLAGS=-L .
LDLIBS=-lzstd

SOURCES=$(shell find . -type f -name "*.cpp" ! -path $(ZSTD_PATH)/\*)
CSOURCES=$(shell find . -type f -name "*.c" ! -path $(ZSTD_PATH)/\*)

COBJECTS=$(CSOURCES:%.c=%.o)
OBJECTS=$(SOURCES:%.cpp=%.o) $(COBJECTS)

TARGET=halite

.PHONY: all
all: $(TARGET)

$(TARGET): $(OBJECTS) $(ZSTD_STATIC_OBJECT)
	$(LINK.cpp) $(OBJECTS) $(LDFLAGS) $(LDLIBS) -o $@

$(ZSTD_STATIC_OBJECT):
	cd $(ZSTD_PATH) && $(MAKE) lib-release
	cp $(ZSTD_PATH)/lib/$(ZSTD_STATIC_OBJECT) .

.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJECTS) libzstd.a libzstd.dylib

.PHONY: install
install:
	install -m 0755 halite $(INSTALL_PATH)/bin
