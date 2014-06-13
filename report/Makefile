TEX = rubber --pdf
.PHONY: clean all

all: clean spellcheck report.pdf

spellcheck:
	ispell -t *.tex

report.pdf: report.aux 
	$(TEX) report.tex

report.aux: introduction.tex csAndPs.tex primitives.tex patterns.tex tsAndParser.tex background.tex phi.tex plan.tex evaluation.tex extension.tex bibliography.bib pi-calc.tex prims.tex

pi-calc.tex: pi-calc.lhs
prims.tex: prims.lhs

clean: 
	$(TEX) --clean report.tex
	rm -rf pi-calc.tex
	rm -rf prims.tex

