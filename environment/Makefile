PLATFORM=$(shell uname)

CXXFLAGS += -std=c++11 -pthread -I ./ -g -O2 -Wall -pedantic
INSTALL_PATH?=/usr/local

ZSTD_PATH=./zstd-1.3.0
ZSTD_STATIC_OBJECT=libzstd.a

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
	rm -f $(TARGET) $(OBJECTS) libzstd.a libzstd.dylib *.bc libzstd.js $(ZSTD_PATH)/lib/*.a
	cd $(ZSTD_PATH)/lib && make clean

.PHONY: install
install:
	install -m 0755 halite $(INSTALL_PATH)/bin

libzstd.bc:
	cd $(ZSTD_PATH)/lib && emmake $(MAKE) SHARED_EXT='bc' lib-release
	mv $(ZSTD_PATH)/lib/*.bc .

libzstd.js: libzstd.bc
	emcc libzstd.bc -s 'EXPORT_NAME="libzstd"' -s 'EXPORTED_FUNCTIONS=["_ZSTD_versionNumber", "_ZSTD_decompress", "_ZSTD_getFrameContentSize", "_ZSTD_isError"]' -s 'MODULARIZE=1' -o libzstd.js