.SECONDEXPANSION:

.PHONY: all
all: deques deques.mli doc deques.html Makefile

deques: deques.ml deques.cmi Makefile
	ocamlc $< -o $@

deques.mli: deques.ml Makefile
	ocamlc -i $< > $@

deques.cmi: deques.mli Makefile
	ocamlc $< -o $@

re=\( *\)(\*\* jacm-final.pdf p.\([0-9]\+\) \(([0-9]\+)\) \(§[.0-9]\+\) media \([0-9]\+\) \([0-9]\+\) \([0-9]\+\) \([0-9]\+\) \*)
deques.d: deques.ml Makefile
	(printf 'deques.html: '; sed -ne 's~^$(re)$$~jacm-final-crop-\5-\6-\7-\8-page\2.png~p' $< | tr '\n'  ' ') > $@

-include deques.d

deques.color.html: deques.ml Makefile
	caml2html -charset utf-8 $< -t -o $@

# TODO: put this in a <style>…</style> tag
style=style="width:63em; margin: 1.5em 0em; display: block; padding: 0.5em 0em 0.5em 1em; border-left: thick solid \#ccc;"

deques.html: deques.color.html Makefile
#	(echo '<!DOCTYPE html><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><title>$(<)</title><pre>'; \
#	 sed -e 's~[&]~\&amp;~' -e 's~<~\&gt;~' -e 's~>~\&lt;~' -e 's~"~\&quot;~' -e "s~'~\&#039;~" deques.ml \
#	 | sed -e 's~^$(re)$$~<img style="width:63em;" src="jacm-final-crop-\5-\6-\7-\8-page\2.png" alt="jacm-final.pdf page \2 \3 \4" />~'; \
#	 echo '</pre>') > $@
	sed -e 's~$(re)~<img $(style) src="jacm-final-crop-\5-\6-\7-\8-page\2.png" alt="jacm-final.pdf page \2 \3 \4" />~g'  $< > $@

.PHONY: print-tool-versions
print-tool-versions: Makefile
	ocamlc -version

jacm-final.pdf: Makefile
	wget http://www.math.tau.ac.il/~haimk/adv-ds-2000/jacm-final.pdf -O$@ && touch $@

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

jacm-final-crop-%.png: jacm-final-crop-%.pdf Makefile
	convert -density 300 $< $@

doc: deques.ml Makefile
	git clean -dfx doc
	mkdir doc
	ocamlfind ocamldoc -html -all-params -colorize-code -charset utf-8 $< -d $@
	touch doc

.PHONY: clean
clean: Makefile
	rm -- jacm-final-page*.pdf jacm-final-crop-*-page*.pdf jacm-final-crop-*-page*.png deques.html
	rm -r -- doc
