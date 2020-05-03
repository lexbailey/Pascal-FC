
all: pfccomp pint copy

pfccomp: pfccomp.pas
	fpc -Miso -g $<

pint: pint.pas
	fpc -Miso -g $<

copy:
	cp pint /usr/bin/
	cp pfc /usr/bin/
	cp pfccomp /usr/bin/
	
.PHONY: all
