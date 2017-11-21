AC_PREREQ([2.69])
AC_INIT([ssim], [0.1.1], [jan@wedesoft.de])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([--warnings=no-portability foreign])
AC_CONFIG_MACRO_DIR([m4])
AC_REQUIRE_AUX_FILE([test-driver.scm])
GUILE_PKG([2.2 2.0])
GUILE_MODULE_REQUIRED(gl)
GUILE_MODULE_REQUIRED(glut)
AC_OUTPUT(Makefile
          ssim/Makefile
          ssim/ssim
          tests/Makefile)
