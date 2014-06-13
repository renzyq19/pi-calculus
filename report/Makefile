TEX = rubber --pdf
.PHONY: clean all

all: clean spellcheck report.pdf

spellcheck:
	ispell -t *.tex

report.pdf: report.aux 
	$(TEX) report.tex

report.aux:  *.tex

background.tex: sockets.lhs
pi-calc.tex: pi-calc.lhs
prims.tex: prims.lhs

clean: 
	$(TEX) --clean report.tex
	rm -rf pi-calc.tex
	rm -rf prims.tex

