## Put version control id tag in output configure script
AC_REVISION($Id: $)

## Initialise autoconf
AC_INIT([crtm], [2.1.2], [ncep.list.emc.jcsda_crtm.support@noaa.gov])

# Output info for user
AC_MSG_NOTICE([crtm AC_PACKAGE_VERSION])

# Check for existence of unique file before proceeding
AC_CONFIG_SRCDIR([libsrc/CRTM_Module.f90])

# Define the configuration files
AC_CONFIG_FILES([Makefile
                 libsrc/Makefile
                 test/Makefile])

# Check for programs
AC_PROG_FC(ifort gfortran xlf2003 pgf95 g95)
AC_PROG_INSTALL

# Set the programming language
AC_LANG(Fortran)
AC_FC_FREEFORM
AC_FC_SRCEXT(f90)


# Determine the I/O endian-type to use.
AC_ARG_ENABLE([big-endian],
  [AS_HELP_STRING([--disable-big-endian],[Native I/O on little-endian machines. @<:@default: big-endian@:>@])],
  [endian_type=little],[endian_type=big])

# Modify the compiler switches accordingly
if test "${endian_type}" = big; then
  case ${FC} in
    ifort)    FCFLAGS="-convert big_endian $FCFLAGS";;
    gfortran) FCFLAGS="-fconvert=big-endian $FCFLAGS";;
    pgf95)    FCFLAGS="-byteswapio $FCFLAGS";;
    g95)      FCFLAGS="-fendian=big $FCFLAGS";;
    xlf2003)  ;;
    *)        AC_MSG_WARN([Endian swap flags for $FC unknown and not set]);;
  esac
  AC_SUBST(FPPFLAGS,[])
else
  AC_SUBST(FPPFLAGS,[-DLITTLE_ENDIAN])
fi
AC_MSG_NOTICE([Building ${endian_type}-endian version])


# Create the configure script
AC_OUTPUT
