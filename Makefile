MAIN=main

all:
	pdflatex $(MAIN)
	pdflatex $(MAIN)
	pdflatex $(MAIN)

bib:
	pdflatex $(MAIN)
	bibtex $(MAIN)
	pdflatex $(MAIN)
	bibtex $(MAIN)
	pdflatex $(MAIN)

clean: 
	rm -f *.aux *.log *.out $(MAIN).bbl main.blg main.pdf

