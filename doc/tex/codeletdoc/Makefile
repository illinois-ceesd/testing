REPORT = codeletdoc
REFS = references
PDFLATEX = pdflatex -shell-escape

SOURCEFILES = $(REPORT).tex $(REFS).bib $(shell ls Figures/*.pdf Figures/*.png)

$(REPORT).pdf: $(SOURCEFILES) $(REPORT).bbl  
	$(PDFLATEX) $(REPORT)
	$(PDFLATEX) $(REPORT)
	bibtex $(REPORT)
	$(PDFLATEX) $(REPORT)


$(REPORT).bbl: $(REFS).bib 
	$(PDFLATEX) $(REPORT)
	$(PDFLATEX) $(REPORT)
	bibtex $(REPORT)
	$(PDFLATEX) $(REPORT)

clean:
	rm -f *.log *.flc *.glo *.gls *.ilg *.nlo *.nls *.glg *.xdy *.aux *.log *.bbl *.blg *.out *.dvi *~ $(REPORT).pdf
