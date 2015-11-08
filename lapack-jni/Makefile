MAKEFLAGS += --no-builtin-rules
.suffixes:
.PHONY: clean

JAVA_HOME ?= $(shell readlink -e "$$(dirname "$$(readlink -e "$$(which javac)")")"/..)
JAVAC = javac
JAVAH = javah

CFLAGS  += -Iinclude -I"$(JAVA_HOME)/include" -I"$(JAVA_HOME)/include/linux" \
           -fPIC
LDFLAGS += -shared -lblas -llapacke

OBJS = $(patsubst src/%.c,build/%.o,$(wildcard src/*.c))

all: build/LapackJNI.jar

build/LapackJNI.jar: src/LapackJNI.c build/LapackJNI.class build/libLapackJNI.so
	jar cf $@ -C build scala

build/LapackJNI.class: src/LapackJNI.java
	mkdir -p build
	$(JAVAC) -d $(dir $@) $^

build/LapackJNI.o: src/LapackJNI.c
	mkdir -p build
	$(CC) -c $(CFLAGS) -o $@ $^

# Link the library used by JNI
build/libLapackJNI.so: build/LapackJNI.o
	$(CC) $(LDFLAGS) -o $@ $^

# Generate the binding code
src/LapackJNI.c:
	(cd src; ../bindgen/bindgen include.h -- -o /dev/null)

src/LapackJNI.java:
	(cd src; ../bindgen/bindgen include.h -- -o /dev/null)

clean:
	-rm -rf build
	-rm -f src/LapackJNI.c src/LapackJNI.java
