TEX = rubber --pdf
.PHONY: clean all

all: clean spellcheck report.pdf

spellcheck:
	ispell -t *.tex

report.pdf: report.aux 
	$(TEX) report.tex

report.aux: introduction.tex csAndPs.tex primitives.tex patterns.tex tsAndParser.tex background.tex plan.tex evaluation.tex bibliography.bib pi-calc.lhs prims.lhs


clean: 
	$(TEX) --clean report.tex
	rm -rf pi-calc.tex

