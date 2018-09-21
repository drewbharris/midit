CC=gcc
ALSA_OPTS=`pkg-config --libs --cflags alsa`
CFLAGS=-g3 -c ${ALSA_OPTS}

all: src/libmidit.a listports

src/libmidit.a: src/midit.o
	ar rc src/libmidit.a src/*.o
	ranlib src/libmidit.a

src/midit.o: src/midit.c
	$(CC) $(CFLAGS) src/midit.c -o src/midit.o

listports: src/libmidit.a
	$(CC) -g3 src/listports.c -o bin/listports -L./src -lmidit ${ALSA_OPTS}

clean:
	rm src/libmidit.a src/*.o bin/*