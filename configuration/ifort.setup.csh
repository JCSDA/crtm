#!/bin/csh
#-------------------------------------------------------------------------------#
# PRODUCTION build settings for Linux ifort compiler
#-------------------------------------------------------------------------------#

setenv FC "ifort"
setenv FCFLAGS "-O2 -free -assume byterecl"
setenv LDFLAGS ""
setenv LIBS ""
