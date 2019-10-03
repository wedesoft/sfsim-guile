AC_PREREQ([2.69])
AC_INIT([sfsim], [0.1.1], [jan@wedesoft.de])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([--warnings=no-portability foreign])
AC_CONFIG_MACRO_DIR([m4])
AC_REQUIRE_AUX_FILE([test-driver.scm])

GUILE_PROGS
GUILE_CACHE_DIR

GUILE_MODULE_REQUIRED(gl)
GUILE_MODULE_REQUIRED(glut)

AC_CHECK_PROG(TOUCH,touch,touch,touch)

AC_CHECK_PROG(PDFLATEX,pdflatex,pdflatex,no)
if test "x$PDFLATEX" = xno; then
  AC_MSG_ERROR([Could not find 'pdflatex' in path.])
fi

AC_OUTPUT(Makefile
          sfsim/Makefile
          sfsim/sfsim
          tests/Makefile
          doc/Makefile)
