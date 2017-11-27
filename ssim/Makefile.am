SUFFIXES = .scm

bin_SCRIPTS = ssim

EXTRA_DIST =  linear-algebra.scm physics.scm quaternion.scm ssim.scm util.scm

pkgdata_DATA = linear-algebra.scm physics.scm quaternion.scm ssim.scm util.scm

cachedir = $(GUILE_CACHE)/ssim

cache_DATA = $(pkgdata_DATA:.scm=.go)

install-data-hook:
	$(TOUCH) $(addprefix $(DESTDIR)$(cachedir)/,$(cache_DATA))

.scm.go:
	LD_LIBRARY_PATH=$(srcdir)/.libs:$(LD_LIBRARY_PATH) GUILE_AUTO_COMPILE=0 $(GUILE_TOOLS) compile -L$(top_builddir) -o $@ $<
