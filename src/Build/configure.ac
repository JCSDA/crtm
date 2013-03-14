## Put version control id tag in output configure script
AC_REVISION($Id: $)

## Initialise autoconf
AC_INIT([CRTM], [2.1.2], [ncep.list.emc.jcsda_crtm.support@noaa.gov])

# Output info for user
AC_MSG_NOTICE([CRTM AC_PACKAGE_VERSION])

# Check for existence of unique file before proceeding
AC_CONFIG_SRCDIR([libsrc/CRTM_Module.f90])

# Define the configuration files
AC_CONFIG_FILES([Makefile
                 libsrc/Makefile
                 test/Makefile])

# Check for programs
AC_PROG_FC(ifort gfortran xlf2003 pgf95)
AC_PROG_INSTALL

# Set the programming language
AC_LANG(Fortran)
AC_FC_FREEFORM
AC_FC_SRCEXT(f90)

# Create the configure script
AC_OUTPUT
