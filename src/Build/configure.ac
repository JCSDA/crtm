## Put version control id tag in output configure script
AC_REVISION($Id: $)


## Initialise autoconf
AC_INIT([CRTM], [v2.2.0-alpha], [ncep.list.emc.jcsda_crtm.support@noaa.gov], [crtm])


# Output info for user
AC_MSG_NOTICE([AC_PACKAGE_NAME AC_PACKAGE_VERSION])


# Check for existence of unique file before proceeding
AC_CONFIG_SRCDIR([libsrc/CRTM_Module.f90])


# Define the configuration files
AC_CONFIG_FILES([Makefile libsrc/Makefile libsrc/test/Makefile])


# Check for programs
AC_PROG_FC(ifort gfortran xlf2003 pgf95 g95)
AC_PROG_INSTALL
AC_CHECK_TOOL([AR], ar)
AC_CHECK_TOOL([RANLIB], ranlib)
# ...Set defaults for ARFLAGS since there is no autoconf macro
test -n "$ARFLAGS" || ARFLAGS="crvs"
AC_SUBST(ARFLAGS)


# Set the programming language
AC_LANG(Fortran)
AC_FC_FREEFORM
AC_FC_SRCEXT(f90)


# Check platform endian-ness
AC_MSG_CHECKING([if platform is little-endian])
if test 1 -eq `printf '\1' | od -dAn`; then
  AC_MSG_RESULT(yes)
  ENDIAN_PLATFORM="little"
else
  AC_MSG_RESULT(no)
  ENDIAN_PLATFORM="big"
fi


# Determine the I/O endian-type to use.
AC_ARG_ENABLE([big-endian],
  [AS_HELP_STRING([--disable-big-endian],[Native I/O on little-endian machines. @<:@default: big-endian@:>@])],
  [endian_io=little],[endian_io=big])

# ...Modify the compiler switches accordingly
if test "${endian_io}" = big -a "${ENDIAN_PLATFORM}" = little; then
  case ${FC} in
    ifort)    FCFLAGS="-convert big_endian $FCFLAGS";;
    gfortran) FCFLAGS="-fconvert=big-endian $FCFLAGS";;
    pgf95)    FCFLAGS="-byteswapio $FCFLAGS";;
    g95)      FCFLAGS="-fendian=big $FCFLAGS";;
    xlf2003)  ;;
    *)        AC_MSG_WARN([Endian swap flags for $FC unknown and not set]);;
  esac
else
  case ${FC} in
    xlf2003) FPPFLAGS="-WF,-DLITTLE_ENDIAN $FPPFLAGS" ;;
    *)       FPPFLAGS="-DLITTLE_ENDIAN $FPPFLAGS" ;;
  esac
fi
AC_MSG_NOTICE([Building ${endian_io}-endian version])


# Section for special cases for supported compilers

# ...Special case the ifort compiler to force byte-unit record lengths
if test "${FC}" = ifort; then
  FCFLAGS="-assume byterecl $FCFLAGS"
fi

# ...Special case the xlf2003 compiler to recognise the .fpp (Fortran PreProcessor) suffix
if test "${FC}" = xlf2003; then
  FPPFLAGS="-qsuffix=cpp=fpp $FPPFLAGS"
fi


# Generate variable substitutions
AC_SUBST(ENDIAN_PLATFORM)
AC_SUBST(ENDIAN_IO,[${endian_io}])
AC_SUBST(FPPFLAGS)


# Create the configure script
AC_OUTPUT
