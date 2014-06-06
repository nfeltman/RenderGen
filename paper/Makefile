.PHONY: quick clean

TEXFILES=paper.tex grammars.tex macros.tex semantics.tex splitting.tex

paper.pdf : $(TEXFILES)
	pdflatex $< && bibtex $(patsubst %.tex,%,$<) && pdflatex $< && pdflatex $<

quick : $(TEXFILES)
	pdflatex $<

clean:
	-rm *.log *.out *.aux *.bbl *.blg

