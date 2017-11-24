SUFFIXES = .tex .pdf

doc_DATA = physics.pdf

EXTRA_DIST = physics.tex

.tex.pdf:
	$(XELATEX) $<

CLEANFILES = *.pdf *.aux *.log
