## Put version control id tag in output configure script
AC_REVISION($Id: configure.ac 42447 2014-07-02 13:53:17Z paul.vandelst@noaa.gov $)


## Initialise autoconf
AC_INIT([LBLRTM I/O], [v1.0.0], [crtm-support@googlegroups.com], [lblrtmio])


# Output info for user
AC_MSG_NOTICE([AC_PACKAGE_NAME AC_PACKAGE_VERSION])


# Check for existence of unique file before proceeding
AC_CONFIG_SRCDIR([libsrc/LBLRTM_Parameters.f90])


# Define the configuration files
AC_CONFIG_FILES([Makefile libsrc/Makefile libsrc/test/Makefile])


# Check for programs
AC_PROG_FC(ifort gfortran xlf2003 pgf95)
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


# Allow user to enable a single-precision build
AC_ARG_ENABLE([single],
  [AS_HELP_STRING([--enable-single],[Build for single-precision files])],
  [enable_single=yes],
  [enable_single=no])


# Set the default type promotion compiler flags for the double case
if test "x$enable_single" = "xno"; then
  file_type="double"
#JR REAL_SIZE=8, INT_SIZE=4 required in order for apps under CRTM_coef to build w/o error
#JR REAL_SIZE=8, INT_SIZE=8 required in order for "make check" to succeed
#JR Mods to FCFLAGS below MUST match FPPFLAGS settings
  FPPFLAGS="-DREAL_SIZE=8 -DINT_SIZE=4"
  case $FC in
    gfortran)    FCFLAGS="-fdefault-real-8 $FCFLAGS";;
    ifort|pgf95) FCFLAGS="-r8 -i4 $FCFLAGS";;
    xlf2003)     FCFLAGS="-qrealsize=8 -qintsize=4 $FCFLAGS";;
    *) AC_MSG_ERROR([float and integer promotion flags for $FC unknown and not set]);;
  esac
else
  file_type="single"
  FPPFLAGS="-DREAL_SIZE=4 -DINT_SIZE=4"
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


# Append netCDF library information to standard flags
AC_MSG_NOTICE([Using NETCDF_DIR location: "${NETCDF_DIR}"])
FCFLAGS="${FCFLAGS} -I${NETCDF_DIR}/include"
LDFLAGS="${LDFLAGS} -L${NETCDF_DIR}/lib"
LIBS="${LIBS} -lnetcdff"
# ...and now check for Fortran netCDF library
AC_MSG_CHECKING([for library containing nf90_inq_libvers])
AC_LINK_IFELSE(
  [AC_LANG_SOURCE([program main; use netcdf; character(80)::ver; ver=nf90_inq_libvers(); end])],
  [AC_MSG_RESULT([yes])],
  [AC_MSG_FAILURE([Unable to find netCDF Fortran library.],[1])])


# Create the configure script
AC_OUTPUT
