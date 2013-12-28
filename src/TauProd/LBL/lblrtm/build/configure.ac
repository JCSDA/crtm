## Put version control id tag in output configure script
AC_REVISION($Id: $)


## Initialise autoconf
AC_INIT([LBLRTM I/O], [v0.1.0], [ncep.list.emc.jcsda_crtm.support@noaa.gov], [lblrtm_io])


# Output info for user
AC_MSG_NOTICE([AC_PACKAGE_NAME AC_PACKAGE_VERSION])


# Check for existence of unique file before proceeding
AC_CONFIG_SRCDIR([libsrc/LBLRTM_Parameters.f90])


# Define the configuration files
#AC_CONFIG_FILES([Makefile libsrc/Makefile libsrc/test/Makefile])
AC_CONFIG_FILES([Makefile libsrc/Makefile])


# Check for programs
AC_PROG_FC(ifort gfortran xlf2003 pgf95 g95)
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_CHECK_TOOL([AR], ar)
# ...Set defaults for ARFLAGS since there is no autoconf macro
test -n "$ARFLAGS" || ARFLAGS="crvs"
AC_SUBST(ARFLAGS)


# Set the programming language
AC_LANG(Fortran)
AC_FC_FREEFORM
AC_FC_SRCEXT(f90)


# Establish the default type of LBLRTM files
AC_ARG_ENABLE([double],
  [AS_HELP_STRING([--disable-double],[Build for single-precision files])],
  [double=no],
  [double=yes])


# Set the default type promotion compiler flags for the double case
file_type="single"
if test "x$enable_double" = "xyes"; then
  file_type="double"
  case $FC in
    gfortran)    FCFLAGS="-fdefault-real-8 -fdefault-integer-8 $FCFLAGS";;
    ifort|pgf95) FCFLAGS="-r8 -i8 $FCFLAGS";;
    xlf2003)     FCFLAGS="-qrealsize=8 -qintsize=8 $FCFLAGS";;
    *) AC_MSG_WARN([Double-precision promotion flags for $FC unknown and not set])
       file_type="single";;
  esac
fi
AC_MSG_NOTICE([Building library for ${file_type} precision file I/O.])
AC_SUBST(FILE_TYPE,[${file_type}])


# Special case the xlf2003 compiler to:
#   a) set the AIX macro
#   b) not insert "#line" directives when it encounters multiple empty lines
if test "x${FC}" = xxlf2003; then
  FPPFLAGS="-WF,-DAIX -WF,-P $FPPFLAGS"
fi
AC_SUBST(FPPFLAGS)


# Create the configure script
AC_OUTPUT
