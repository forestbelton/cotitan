AC_INIT(cotitan, 0.1)
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_FILES([Makefile])
AC_PROG_CC
AC_LANG(C)

AC_CHECK_HEADER(curses.h)
AC_CHECK_HEADER(ncurses.h)

if test "$ac_cv_header_curses_h" == no && test "$ac_cv_header_ncurses_h" == no
then
  AC_MSG_ERROR([error: no compatible curses library found])
fi

AC_OUTPUT
