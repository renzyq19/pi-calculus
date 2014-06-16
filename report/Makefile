TEX =openout_any=a rubber --shell-escape --pdf
.PHONY: clean all

all: clean spellcheck report.pdf

spellcheck:
	ispell -t *.tex

report.pdf: report.aux 
	$(TEX) report.tex

report.aux:  *.tex

background.tex: sockets.hs pi-calc.hs
primitives.tex: prims.lhs
typedefs.tex: typedefs.lhs

clean: 
	$(TEX) --clean report.tex
	rm -rf pi-calc.tex
	rm -rf prims.tex

