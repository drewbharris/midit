LDLIBS := -lasound
CFLAGS ?= -g -O2
CFLAGS += -W -Wall
DESTDIR ?= /usr/local
bindir ?= /bin
mandir ?= /share/man

all: midit

midit: midit.c

clean:
	rm -f midit

install: all midit.1
	mkdir -p $(DESTDIR)$(bindir)
	mkdir -p $(DESTDIR)$(mandir)/1
	cp midit $(DESTDIR)$(bindir)/
	chmod 755 $(DESTDIR)$(bindir)/midit
	cp midit.1 $(DESTDIR)$(mandir)/man1/
	chmod 744 $(DESTDIR)$(mandir)/man1/midit.1

uninstall:
	rm -f $(DESTDIR)$(bindir)/midit
	rm -f $(DESTDIR)$(mandir)/man1/midit.1

.PHONY: all clean install uninstall
