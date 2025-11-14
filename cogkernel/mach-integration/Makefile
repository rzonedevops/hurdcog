# Makefile for HurdCog-Cognumach Integration Layer

GUILE = guile
GUILD = guild

SOURCES = machspace-bridge.scm
OBJECTS = $(SOURCES:.scm=.go)

all: $(OBJECTS)

%.go: %.scm
	$(GUILD) compile -o $@ $<

clean:
	rm -f $(OBJECTS)

test:
	$(GUILE) -c "(use-modules (machspace-bridge)) (machspace-init)"

.PHONY: all clean test
