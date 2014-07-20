CC=gcc
CFLAGS=-fpic -O3  # for shared libraries
CFLAGS2=-O3

all: libdyc.so

inputformat.lex.c: inputformat.l
	flex -oinputformat.lex.c inputformat.l

inputformat.tab.c inputformat.tab.h: inputformat.y
	bison -d inputformat.y

inputformat.tab.o: inputformat.tab.c inputformat.tab.h inputformat.lex.c
	${CC} ${CFLAGS} -c inputformat.tab.c

randomvalues.o: randomvalues.c dycmain.h
	${CC} ${CFLAGS} -c randomvalues.c

# this is the main for .foo.c, no need to make it shared
dycmain.o: dycmain.c dycmain.h
	${CC} ${CFLAGS2} -c dycmain.c

libdyc.so: inputformat.tab.o randomvalues.o dycmain.o
	${CC} -shared -o libdyc.so inputformat.tab.o randomvalues.o

clean:
	rm *.o libdyc.so

