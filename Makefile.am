ACLOCAL_ANFLAGS = -I m4

SUBDIRS = ssim tests doc

EXTRA_DIST = AUTHORS bootstrap configure.ac ChangeLog COPYING README.md TODO.md

run:
	$(GUILE) --no-auto-compile -L $(top_builddir) $(top_srcdir)/ssim/ssim.scm

repl:
	$(GUILE) --no-auto-compile -L $(top_builddir)
