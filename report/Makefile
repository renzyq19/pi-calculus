TEX = rubber --pdf
.PHONY: clean all

all: report.pdf

report.pdf: report.aux 
	$(TEX) report.tex

report.aux: introduction.tex background.tex plan.tex evaluation.tex bibliography.bib

clean: 
	$(TEX) --clean report.tex

