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

#-----------------------------------------------------------------------------
#                              -- Define macros --
#-----------------------------------------------------------------------------

include ../make.macros

MAKEFILE = Makefile

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



#-----------------------------------------------------------------------------
#                              -- Define targets --
#-----------------------------------------------------------------------------

# -- Default make is double precision.
all: dbl


# -- Single and double precision targets. The make is based on OS type.
# -- Platform specific definitions are contained in make.macros file
sgl:
	@$(LINK) fftscn_sgl.f fftscn.f
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKEFILE) lblrtm $(SUNOS_SGL_FLAGS) ;; \
	  "AIX")     make -f $(MAKEFILE) lblrtm $(AIX_SGL_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKEFILE) lblrtm $(IRIX64_SGL_FLAGS) ;; \
	  "Linux" )  make -f $(MAKEFILE) lblrtm $(Linux_SGL_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
	esac

dbl:
	@$(LINK) fftscn_dbl.f fftscn.f
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKEFILE) lblrtm $(SUNOS_DBL_FLAGS) ;; \
	  "AIX")     make -f $(MAKEFILE) lblrtm $(AIX_DBL_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKEFILE) lblrtm $(IRIX64_DBL_FLAGS) ;; \
	  "Linux" )  make -f $(MAKEFILE) lblrtm $(Linux_DBL_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
	esac

# -- Link stage with object file dependency
lblrtm: util_obj lblrtm_obj
	$(FL) $(UTIL_DIR)/$(UTIL_OBJ_FILE) $(LBLRTM_OBJ_FILES) $(FL_FLAGS) lblrtm_$(LBLRTM_VERSION)_$(TAG)


# -- Make of utility module. Flags for make are the
# -- platform specific definitions
util_obj:
	cd $(UTIL_DIR); make all "FC=$(FC)" \
	                         "FL=$(FL)" \
	                         "FC_FLAGS=$(FC_FLAGS)" \
	                         "FL_FLAGS=$(FL_FLAGS)" \
	                         "UTIL_OBJ_FILE=$(UTIL_OBJ_FILE)"
	                           
# -- Make of LBLRTM source. Uses default compilation
# -- rules defined in make.rules file
lblrtm_obj: $(LBLRTM_OBJ_FILES)


# -- Cleanup
clean:
	rm -f *.o fftscn.f; cd $(UTIL_DIR); make clean




#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include ../make.rules


#-------------------------------------------------------------------------------
#                          -- MODIFICATION HISTORY --
#-------------------------------------------------------------------------------
#
# $Id: Makefile,v 2.12 2005/09/13 14:13:25 paulv Exp $
#
# $Date: 2005/09/13 14:13:25 $
#
# $Revision: 2.12 $
#
# $Name:  $
#
# $State: Exp $
#
# $Log: Makefile,v $
# Revision 2.12  2005/09/13 14:13:25  paulv
# - Updated executable name to current 9.4 release.
#
# Revision 2.11  2004/07/19 16:52:25  paulv
# Updated for beta LBLRTM v8.6 distribution
#
# Revision 2.10  2004/07/19 16:35:54  paulv
# - Updated the "clean" target to remove the generic fftscn source file link.
#
# Revision 2.9  2004/04/16 18:55:19  paulv
# Updated for LBLRTM v8.4 distribution
#
# Revision 2.8  2003/12/02 16:54:56  paulv
# Updated for LBLRTM v8.3 distribution
#
# Revision 2.7  2003/07/15 19:06:56  paulv
# - New version to allow for single or double precision compilation from
#   the command line.
#
# Revision 2.6  2003/06/18 20:43:24  paulv
# - Updated for v7.04 release.
#
# Revision 2.5  2002/10/17 17:48:15  paulv
# - Updated for v6.12 release.
#
# Revision 2.4  2002/10/17 17:35:31  paulv
# - Added realclean target.
#
# Revision 2.3  2002/05/07 20:09:51  paulv
# - Default is to use the double precision code.
#
# Revision 2.2  2002/03/15 19:10:40  paulv
# - Added obj file definitions for LBLRTM v6.01 source.
# - Changed executable output name to lblrtm_v6.01
#
# Revision 2.1  2002/03/14 20:58:12  paulv
# - New makefile version.
#
#
#
