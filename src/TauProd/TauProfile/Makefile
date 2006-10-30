#==============================================================================
#
# Makefile for creating a tarball of the TauProfile code
#
# $Id: Makefile,v 1.1 2004/06/14 14:36:51 paulv Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

F90_DIR = $(HOME)/f90

include $(F90_DIR)/make.macros


# -------------
# This makefile
# -------------

MAKEFILE = Makefile


# ------------
# Object files
# ------------

OBJ_FILES = TauProfile_Define.o \
            TauProfile_netCDF_IO.o



#-----------------------------------------------------------------------------
#                    -- Create tarball for distribution --
#-----------------------------------------------------------------------------

all: tar


# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = TauProfile_software.tar

tar:
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	compress $(TAR_FILE)

