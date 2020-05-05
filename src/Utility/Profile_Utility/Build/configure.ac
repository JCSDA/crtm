## Put version control id tag in output configure script
AC_REVISION($Id$)


## Initialise autoconf
AC_INIT([Profile Utility], m4_esyscmd([tr -d "\n'" < libsrc/Profile_Utility_Version.inc]), [crtm-support@googlegroups.com], [profile_utility])


# Output info for user
AC_MSG_NOTICE([AC_PACKAGE_NAME AC_PACKAGE_VERSION])


# Check for existence of unique file before proceeding
AC_CONFIG_SRCDIR([libsrc/Profile_Utility.fpp])


# Define the configuration files
AC_CONFIG_FILES([Makefile libsrc/Makefile libsrc/test/Makefile])


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


# Section for special cases for supported compilers
# ...Special case the xlf2003 compiler to:
#      a) recognise the .fpp (Fortran PreProcessor) suffix
#      b) not insert "#line" directives when it encounters multiple empty lines
if test "${FC}" = xlf2003; then
  FPPFLAGS="-qsuffix=cpp=fpp -WF,-P $FPPFLAGS"
fi


# Generate variable substitutions
AC_SUBST(FPPFLAGS)


# Create the configure script
AC_OUTPUT
