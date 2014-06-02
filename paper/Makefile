.PHONY: quick clean

TEXFILES=sigplanconf-template.tex grammars.tex macros.tex semantics.tex splitting.tex

sigplanconf-template.pdf : $(TEXFILES)
	pdflatex $< && bibtex $(patsubst %.tex,%,$<) && pdflatex $< && pdflatex $<

quick : $(TEXFILES)
	pdflatex $<

clean:
	-rm *.log *.out *.aux *.bbl *.blg

