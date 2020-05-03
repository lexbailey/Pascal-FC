
all: pfccomp pint

pfccomp: pfccomp.pas
	fpc -Miso -g $<

pint: pint.pas
	fpc -Miso -g $<

install: pfccomp pint
	install pint /usr/bin/
	install pfc /usr/bin/
	install pfccomp /usr/bin/

.PHONY: all install
