.PHONY: quick clean

TEXFILES=sigplanconf-template.tex grammars.tex macros.tex semantics.tex splitting.tex

quick : $(TEXFILES)
	pdflatex $<

sigplanconf-template.pdf : $(TEXFILES)
	pdflatex $< && bibtex $(patsubst %.tex,%,$<) && pdflatex $< && pdflatex $<

clean:
	-rm *.log *.out *.aux *.bbl *.blg

