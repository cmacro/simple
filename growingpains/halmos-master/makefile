DEBUGGER=valgrind
CFLAGS=-g -std=c99 -Wextra -Wall -pedantic -Werror -Wshadow -Wpointer-arith \
-Isrc $(OPTFLAGS) 
LIBS=$(OPTLIBS)

SOURCES:=$(wildcard src/*.c)
OBJECTS:=$(patsubst %.c,%.o,$(SOURCES))
DEPENDENCIES:=$(patsubst %.c,%.d,$(SOURCES))

TESTSOURCE:=$(wildcard tests/*_tests.c)
TESTOBJECT:=$(patsubst %.c,%.o,$(TESTSOURCE))
TESTS:=$(patsubst %.c,%,$(TESTSOURCE))
DEPENDENCIES+=$(patsubst %.c,%.d,$(TESTSOURCE))

TARGET=bin/halmos
TARGETMAIN=src/main.o
TESTSCRIPT=tests/runtests.sh

all: $(DEPENDENCIES) $(TARGET) tests tags

.PHONY: dev release build tests trace clean

dev: CFLAGS=-g -Wextra -Wall -pedantic -Werror -Isrc $(OPTFLAGS)
dev: all

release: CFLAGS=-g -O2 -Wextra -Wall -pedantic -Isrc -DNDEBUG $(OPTFLAGS)
release: all

$(TARGET): build $(OBJECTS)
	$(CC) $(LIBS) -o $@ $(OBJECTS)

build:
	@mkdir -p bin

src/%.d: src/%.c
	$(CC) $(CFLAGS) -MM $< -MF $@
	sed -e 's:$*.o:src/$*.o:g' $@ > tmp
	mv -f tmp $@


$(TESTS): % : %.o $(subst $(TARGETMAIN),,$(OBJECTS))
	$(CC) $(LIBS) -o $@ $< $(subst $(TARGETMAIN),,$(OBJECTS))

tests: $(TESTS)
	sh ./$(TESTSCRIPT)

tests/%.d: tests/%.c
	$(CC) $(CFLAGS) -MM $< -MF $@
	sed -e 's:$*.o:tests/$*.o:g' $@ > tmp
	mv -f tmp $@

trace/trace.o: trace/trace.c
	$(CC) -c $< -o $@

trace: CFLAGS+=-O2 -finstrument-functions
trace: $(OBJECTS) trace/trace.o
	gcc -g $(OBJECTS) trace/trace.o -o trace/halmos_trace

valgrind:
	VALGRIND="valgrind --log-file=/tmp/valgrind-%p.log" $(MAKE)

clean:
	rm -rf $(OBJECTS) $(TESTOBJECT) $(DEPENDENCIES)
	rm -f tests/tests.log

tags:
	ctags -R

ifeq (,$(filter $(MAKECMDGOALS),clean))
-include $(DEPENDENCIES)
endif
