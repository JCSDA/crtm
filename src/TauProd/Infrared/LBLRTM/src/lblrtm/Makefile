#------------------------------------------------------------------------------
#
# NAME:
#       Makefile
#
# PURPOSE:
#       Unix make file for LBLRTM compilation and link
#
# LANGUAGE:
#       Unix make
#
# CALLING SEQUENCE:
#       make
#         or
#       make -f Makefile
#
# CREATION HISTORY:
#       Written by:     Paul van Delst, CIMSS/SSEC 13-Mar-2002
#                       paul.vandelst@ssec.wisc.edu
#
#  Copyright (C) 2002 Paul van Delst
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
#------------------------------------------------------------------------------

# Define macros
# -------------
include ../make.macros

MAKE_FILE = Makefile
LBLRTM_VERSION = v9.4
LBLRTM_OBJ_FILES = lblrtm.o \
                   oprop.o \
                   contnm.o \
                   xmerge.o \
                   testmm.o \
                   lblatm.o \
                   lbllow.o \
                   postsub.o \
                   fftscn.o \
                   pltlbl.o \
                   lbldum.o \
                   solar.o \
                   nonlte.o
UTIL_DIR = ../$(SHARED_DIR)


# Define targets
# --------------
# Default make is double precision.
all: dbl

# Single and double precision targets. The make is based on OS type.
# Platform specific definitions are contained in make.macros file
sgl:
	@$(LINK) fftscn_sgl.f fftscn.f
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKE_FILE) lblrtm $(SUNOS_SGL_FLAGS) ;; \
	  "AIX")     make -f $(MAKE_FILE) lblrtm $(AIX_SGL_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKE_FILE) lblrtm $(IRIX64_SGL_FLAGS) ;; \
	  "Linux" )  make -f $(MAKE_FILE) lblrtm $(Linux_SGL_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
	esac

dbl:
	@$(LINK) fftscn_dbl.f fftscn.f
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKE_FILE) lblrtm $(SUNOS_DBL_FLAGS) ;; \
	  "AIX")     make -f $(MAKE_FILE) lblrtm $(AIX_DBL_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKE_FILE) lblrtm $(IRIX64_DBL_FLAGS) ;; \
	  "Linux" )  make -f $(MAKE_FILE) lblrtm $(Linux_DBL_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
	esac

# Link stage with object file dependency
lblrtm: util_obj lblrtm_obj
	$(FL) $(UTIL_DIR)/$(UTIL_OBJ_FILE) $(LBLRTM_OBJ_FILES) $(FL_FLAGS) lblrtm_$(LBLRTM_VERSION)_$(TAG)

# Make of utility module. Flags for make are the
# platform specific definitions
util_obj:
	cd $(UTIL_DIR); make all "FC=$(FC)" \
	                         "FL=$(FL)" \
	                         "FC_FLAGS=$(FC_FLAGS)" \
	                         "FL_FLAGS=$(FL_FLAGS)" \
	                         "UTIL_OBJ_FILE=$(UTIL_OBJ_FILE)"
	                           
# Make of LBLRTM source. Uses default compilation
# rules defined in make.rules file
lblrtm_obj: $(LBLRTM_OBJ_FILES)

# Cleanup
clean:
	rm -f *.o fftscn.f; cd $(UTIL_DIR); make clean


# Default rules
# -------------
include ../make.rules
