
# common.mk : Common makefile for building tex documents.
#
# This file should be included at the end of a top-level makefile that
# defines:
#
#   NAME_TEXFILE    - the name of the main tex file (becomes name of output)
#   OTHER_TEXFILES  - list of tex files constituting document
#   BIBFILE         - bib file for document


ifndef MAIN_TEXFILE
$(error No main tex file specified)
endif

ifndef BIBFILE
$(error No bib file specified)
endif

NAME = $(basename $(MAIN_TEXFILE))

TEXFILES = $(MAIN_TEXFILE) $(OTHER_TEXFILES)

PDFLATEX_FLAGS = "--shell-escape"


all: $(NAME).pdf

$(NAME).pdf: $(GNUPLOT2EPS) $(TEXFILES) $(TEXMACRO_FILE) $(BIBFILE)
	pdflatex ${PDFLATEX_FLAGS} $(NAME)
	bibtex $(basename $(BIBFILE))
	pdflatex ${PDFLATEX_FLAGS} $(NAME)
	pdflatex ${PDFLATEX_FLAGS} $(NAME)

clean:
	@rm -rf *.aux *.bbl *.log *.dvi *.blg *.ps *.bak *~ *.toc *.lot *.lof *.out $(NAME).pdf 

spell: $(SPELL)

%.spell:
	aspell check $*
