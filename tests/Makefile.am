SUFFIXES = .scm .log .trs

TEST_EXTENSIONS = .scm
TESTS_ENVIRONMENT = GUILE_AUTO_COMPILE=0
SCM_LOG_DRIVER = $(GUILE) -L $(top_builddir) -L $(srcdir) $(top_srcdir)/build-aux/test-driver.scm

TESTS = test_physics.scm test_quaternion.scm

EXTRA_DIST = $(TESTS)
