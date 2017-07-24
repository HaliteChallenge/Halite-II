CXXFLAGS += -std=c++11 -I ./ -g -fsanitize=address,undefined
SOURCES=$(shell find ../../C++/hlt -name "*.cpp")
OBJECTS=$(SOURCES:%.cpp=%.o)
TARGETS=dogfighter fastsettler settlermk2
TARGET_OBJECTS=$(TARGETS:%=%.o)

.PHONY: all
all: $(TARGETS)

dogfighter: dogfighter.o $(OBJECTS)
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@

fastsettler: fastsettler.o $(OBJECTS)
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@

settlermk2: settlermk2.o $(OBJECTS)
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@

.PHONY: clean
clean:
	rm -f $(TARGETS) $(OBJECTS) $(TARGET_OBJECTS)
