## Put version control id tag in output configure script
AC_REVISION($Id: $)

## Initialise autoconf
AC_INIT([CRTM], m4_esyscmd([tr -d "\n'" < libsrc/CRTM_Version.inc |tr -d "\n'" < libsrc/CRTM_Version.inc | tr -d "#define CRTM_VERSION "]), [ncep.list.emc.jcsda_crtm.support@noaa.gov], [crtm])

# Output info for user
AC_MSG_NOTICE([AC_PACKAGE_NAME AC_PACKAGE_VERSION])

# Check for existence of unique file before proceeding
AC_CONFIG_SRCDIR([libsrc/CRTM_Module.fpp])

# Define the configuration files
AC_CONFIG_FILES([Makefile libsrc/Makefile libsrc/test/Makefile])

# Check for Fortran compilers
AC_PROG_FC(gfortran ifort xlf2003 xlf2008 pgfortran)
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_CHECK_TOOL([AR], ar)

# ...Set defaults for ARFLAGS since there is no autoconf macro
test -n "$ARFLAGS" || ARFLAGS="crvs"
AC_SUBST(ARFLAGS)

# Set the programming language
AC_LANG(Fortran)
AC_FC_SRCEXT(f90)

# Check platform endian-ness
AC_MSG_CHECKING([endian-ness of host])
if test 1 -eq `printf '\1' | od -dAn`; then
  ENDIAN_PLATFORM="little"
else
  ENDIAN_PLATFORM="big"
fi
AC_MSG_RESULT([$ENDIAN_PLATFORM])

# Determine the I/O endian-type to use.
AC_MSG_CHECKING([endian-ness of I/O to be done])
AC_ARG_ENABLE([big-endian],
  [AS_HELP_STRING([--disable-big-endian],[Native I/O on little-endian machines. @<:@default: big-endian@:>@])],
  [ENDIAN_IO=little],[ENDIAN_IO=big])
AC_MSG_RESULT([$ENDIAN_IO])

# ...All little-endian I/O requires the following for the check program
# ...Special case the xlf2* compiler to:
#      a) recognise the .fpp (Fortran PreProcessor) suffix
#      b) not insert "#line" directives when it encounters multiple empty lines
if test "${ENDIAN_IO}" = little; then
  case ${FC} in
    xlf2*)   FPPFLAGS="-qsuffix=cpp=fpp -WF,-P -WF,-DLITTLE_ENDIAN $FPPFLAGS" ;;
    *)       FPPFLAGS="-DLITTLE_ENDIAN $FPPFLAGS" ;;
  esac
fi

# ...Big-endian I/O on little-endian platforms
if test "${ENDIAN_IO}" = big -a "${ENDIAN_PLATFORM}" = little; then
  case ${FC} in
    ifort)        FCFLAGS="-convert big_endian $FCFLAGS";;
    gfortran)     FCFLAGS="-fconvert=big-endian $FCFLAGS";;
    pgfortran)    FCFLAGS="-Mbyteswapio $FCFLAGS";;
    *)            AC_MSG_WARN([Endian swap flags for $FC unknown and not set]);;
  esac
fi

# ...Little-endian I/O on big-endian platforms
if test "${ENDIAN_IO}" = little -a "${ENDIAN_PLATFORM}" = big; then
  AC_MSG_WARN([Little-endian I/O on big-endian platforms not supported via compiler switches])
fi

# Set compiler-specific flags. Normally in autoconf this is done OUTSIDE of configure. One example
# problem with the approach utilized here is e.g. what if FC=mpif90? It won't pick up the
# appropriate flags. But this approach makes running configure very simple.
# case $FC in
#  gfortran*)
#    FCFLAGS="$FCFLAGS -cpp -ffree-form -fimplicit-none -frecord-marker=4 -ggdb -Wall -Wconversion -std=f2008" ;;
#  ifort)
#    FCFLAGS="$FCFLAGS -e08 -free -assume byterecl,realloc_lhs" ;;
#  pgfortran)
#    FCFLAGS="$FCFLAGS -Mfree -Mdaz -Minform,inform -Mnosave -Mref_externals -Kieee" ;;
#  xlf20*)
#    FCFLAGS="$FCFLAGS -qdbg -qhalt=W -qlanglvl=2008pure -qsuffix=f=f90:cpp=fpp:cpp=F90" ;;
#  *) AC_MSG_WARN([Compiler $FC not found: no default flags will be set]);;
#esac

# Add a canned list of debugging flags if --debug was specified. Note -O setting will overried
# any user-specified settings at configure time
AC_ARG_ENABLE([debug], AS_HELP_STRING([--enable-debug], [set compiler flags for debugging]))
if test "x$enable_debug" = xyes; then
  AC_MSG_NOTICE([debug flags requested: any -O settings from the environment will be overridden])
# Debug flags
  case ${FC} in
    gfortran*)
      FCFLAGS="$FCFLAGS -g -O0 -fbounds-check -ffpe-trap=overflow,zero,invalid" ;;
    ifort)
      FCFLAGS="$FCFLAGS -g -O0 -check bounds -fp-stack-check -mieee-fp" ;;
    pgfortran)
      FCFLAGS="$FCFLAGS -g -O0 -Ktrap=ovf,divz -Mbounds -Mchkstk -Mdclchk" ;;
    xlf2*)
      FCFLAGS="$FCFLAGS -qcheck -qextcheck -qfloat=nomaf:rndsngl -qflttrap=ov:zero:en -qinitauto -qmaxmem=-1" ;;
  esac
fi

# Add a canned list of optimization flags if --optimize was specified. Note -O setting will override
# any user-specified settings at configure time
AC_ARG_ENABLE([optimize], [AS_HELP_STRING([--enable-optimized],
	         	   [set compiler flags for optimized performance])])
test "x$enable_optimize" = xyes || enable_optimize=no
if test "x$enable_optimize" = xyes; then
  if test "x$enable_debug" = xyes; then
    AC_MSG_ERROR([--enable-debug and --enable-optimize are mutually exclusive!])
  fi
  AC_MSG_NOTICE([optimize flags requested: any -O settings from the environment will be overridden])
# Optimized flags
  case ${FC} in
    gfortran*)
      FCFLAGS="$FCFLAGS -O3 -funroll-loops" ;;
    ifort)
      FCFLAGS="$FCFLAGS -O3 -fp-model source" ;;
    pgfortran)
      FCFLAGS="$FCFLAGS -gopt -O3" ;;
# The following has not been tested due to no access to xlf compilers
    xlf20*)
      FCFLAGS="$FCFLAGS -qarch=auto -qstrict -NS32768 -O3" ;;
  esac
fi

# Check for enabling OpenMP (default disabled)
AC_ARG_ENABLE([openmp], AS_HELP_STRING([--enable-openmp], [Enable OpenMP]))
if test "x$enable_openmp" = xyes; then
  AC_OPENMP()
  if test "x$ac_cv_prog_fc_openmp" = xunsupported; then
    AC_MSG_NOTICE([OpenMP Fortran support not found])
  else
    have_fort_omp=yes
    FCFLAGS="$FCFLAGS $OPENMP_FCFLAGS"
    LDFLAGS="$LDFLAGS $OPENMP_FCFLAGS"
  fi
fi

## Optional auto-profiling with GPTL. Default not enabled
## Check for enabling GPTL timing library
#AC_ARG_WITH([gptl],
#              [AS_HELP_STRING([--with-gptl=<path>],
#                              [GPTL timing library is to be included])],
#              [gptl_path=$with_gptl],[gptl_path=none])
#
#if test "$gptl_path" != none; then
#  PKG_CONFIG_PATH="${gptl_path}/lib/pkgconfig"
#  PKG_CHECK_MODULES(GPTL, gptl, found_gptl=yes, found_gptl=no)
#  if test "$found_gptl" = yes; then
## PKG_CHECK_MODULES only works with C, but GPTL_CFLAGS will work here
#    case "${FC}" in
#       xlf2*) FPPFLAGS="$FPPFLAGS -WF,-DENABLE_GPTL $GPTL_CFLAGS";;
#       *)     FPPFLAGS="$FPPFLAGS -DENABLE_GPTL $GPTL_CFLAGS";;
#    esac
#
#    FCFLAGS="$FCFLAGS $GPTL_CFLAGS"
#    LDFLAGS="$LDFLAGS $GPTL_LIBS"
#
## Check for enabling auto-profiling (default enabled when GPTL support enabled)
#    AC_ARG_ENABLE([autoprofiling],
#      AS_HELP_STRING([--disable-autoprofiling], [Disable autoprofiing even when GPTL enabled]))
#
#    AS_IF([test "x$enable_autoprofiling" != "xno"], [
## Adjust flags is auto-profiling also desired
## See if auto-instrumentation flag is available. If so, set INSTRFLAG for testing
#      AX_CHECK_COMPILE_FLAG([-finstrument-functions], [finstrf=yes], [finstrf=no])
#      AX_CHECK_COMPILE_FLAG([-Minstrument:functions], [minstrf=yes], [minstrf=no])
#    ])
## Auto-instrumentation often requires a special link flag
#    rdynamic=yes
#    bdynamic=yes
#    AX_CHECK_LINK_FLAG(-rdynamic, [AC_SUBST([INSTR_LINK], [-rdynamic])], [rdynamic=no])
#    if test "$rdynamic" = yes; then
#      INSTR_LINK="-rdynamic"
#      AC_MSG_NOTICE([-rdynamic link flag may be needed for auto-instrumentation])
#    else
#      AX_CHECK_LINK_FLAG(-Bdynamic, [AC_SUBST([INSTR_LINK], [-Bdynamic])], [bdynamic=no])
#      if test "$bdynamic" = yes; then
#        INSTR_LINK="-Bdynamic"
#        AC_MSG_NOTICE([-Bdynamic link flag may be needed for auto-instrumentation])
#      else
#        AC_MSG_WARN([Dynamic linking flag not found: Auto-instrumentation may not work properly])
#      fi
#    fi
#
#    if test "x$finstrf" = xyes; then
#      FCFLAGS="$FCFLAGS -finstrument-functions"
#    elif test "x$minstrf" = xyes; then
#      FCFLAGS="$FCFLAGS -Minstrument:functions"
#    fi
#  fi
#fi

# Generate variable substitutions
AC_SUBST(ENDIAN_PLATFORM)
AC_SUBST(ENDIAN_IO)
AC_SUBST(FPPFLAGS)

# Create the configure script and Makefiles
AC_OUTPUT
