SUFFIXES = .tex .pdf

doc_DATA = physics.pdf

EXTRA_DIST = physics.tex

.tex.pdf:
	$(PDFLATEX) $<
	$(PDFLATEX) $<

CLEANFILES = *.pdf *.aux *.log *.out
