
all: pfccomp pint

pfccomp: pfccomp.pas
	fpc -Miso -g $<

pint: pint.pas
	fpc -Miso -g $<

.PHONY: all
