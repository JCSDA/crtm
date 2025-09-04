####################################################################
# FLAGS COMMON TO ALL BUILD TYPES
####################################################################

set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")

####################################################################
# RELEASE FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS_RELEASE "-O2 -gopt -mp -Munroll -Minline" )

####################################################################
# DEBUG FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -Mbounds -Mchkstk -traceback -mp" )

####################################################################
# BIT REPRODUCIBLE FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS_BIT     "-O2 -gopt -Munroll -Minline" )

####################################################################
# LINK FLAGS
####################################################################

set( CMAKE_Fortran_LINK_FLAGS    "-mp" )

####################################################################

# Meaning of flags
# ----------------
# -mp           : Enable OpenMP parallelization
# -gopt         : Generate optimization information
# -Munroll      : Unroll loops for better performance
# -Minline      : Enable function inlining
# -Mbounds      : Array bounds checking
# -Mchkstk      : Check for stack overflow
# -traceback    : Enable traceback on runtime errors