TEX = rubber --pdf
.PHONY: clean all

all: clean spellcheck report.pdf

spellcheck:
	ispell -t *.tex

report.pdf: report.aux 
	$(TEX) report.tex

report.aux: introduction.tex background.tex plan.tex evaluation.tex bibliography.bib

clean: 
	$(TEX) --clean report.tex

