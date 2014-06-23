TEX =openout_any=a rubber --shell-escape --pdf
.PHONY: clean all

all: clean spellcheck presentation.pdf

spellcheck:
	ispell -t *.tex

presentation.pdf: presentation.aux 
	$(TEX) presentation.tex

presentation.aux:  *.tex


clean: 
	$(TEX) --clean presentation.tex

