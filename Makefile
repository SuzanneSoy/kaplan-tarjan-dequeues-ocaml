.SECONDEXPANSION:

.PHONY: all
all: deques deques.mli Makefile

deques: deques.ml Makefile
	ocamlc $< -o $@

deques.mli: deques.ml Makefile
	ocamlc -i $< > $@

.PHONY: print-tool-versions
print-tool-versions: Makefile
	ocamlc -version

jacm-final.pdf: Makefile
	wget http://www.math.tau.ac.il/~haimk/adv-ds-2000/jacm-final.pdf

#sudo apt-get install libpodofo-utils to get podofobox
jacm-final-page%.pdf: jacm-final.pdf Makefile
	pdftk P=$< cat P$* output $@

# jacm-final-crop-LEFT-TOP-WIDTH-HEIGHT-pagePAGE.pdf
# Original page width: 488, original page height: 721.
# podofobox expects LEFT BOTTOM WIDTH HEIGHT, multiplied by 100.
jacm-final-crop-%.pdf: $$(shell echo '%.pdf' | sed -e 's/^[-0-9]*-page/jacm-final-page/')
	f () { \
	  podofobox $< $@ \
	    media $$(( ($$1) * 100 )) \
	          $$(( ( 720 - ($$2) - ($$4) ) * 100 )) \
	          $$(( ($$3) * 100 )) \
	          $$(( ($$4) * 100 )); \
        }; \
        f $$(echo '$*' | sed -e 's/page[0-9]*$$//' -e 's/-/ /g')

.PHONY: clean
clean: Makefile
	rm jacm-final-page*.pdf jacm-final-crop-*-page*.pdf
