#------------------------------------------------------------------------------
#
# NAME:
#       Makefile
#
# PURPOSE:
#       Unix make file for LNFL compilation and link
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
EXE_FILE = lnfl_v2.1
LNFL_OBJ_FILES = lnfl.o
UTIL_DIR = ../$(SHARED_DIR)



#-----------------------------------------------------------------------------
#                              -- Define targets --
#-----------------------------------------------------------------------------

# -- Make is single precision and based on OS type.
# -- Platform specific definitions are contained 
# -- in make.macros file
all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKEFILE) lnfl $(SUNOS_SGL_FLAGS) ;; \
	  "AIX")     make -f $(MAKEFILE) lnfl $(AIX_SGL_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKEFILE) lnfl $(IRIX64_SGL_FLAGS) ;; \
	  "Linux" )  make -f $(MAKEFILE) lnfl $(Linux_SGL_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
	esac


# -- Link stage with object file dependency
lnfl: util_obj lnfl_obj
	$(FL) $(UTIL_DIR)/$(UTIL_OBJ_FILE) $(LNFL_OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)


# -- Make of utility module. Flags for make are the
# -- platform specific definitions
util_obj:
	cd $(UTIL_DIR); make all "FC=$(FC)" \
	                         "FL=$(FL)" \
	                         "FC_FLAGS=$(FC_FLAGS)" \
	                         "FL_FLAGS=$(FL_FLAGS)" \
	                         "UTIL_OBJ_FILE=$(UTIL_OBJ_FILE)"
	                           
# -- Make of LNFL source. Uses default compilation
# -- rules defined in make.rules file
lnfl_obj: $(LNFL_OBJ_FILES)


# -- Cleanup
clean:
	rm -f *.o; cd $(UTIL_DIR); make clean



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include ../make.rules


#-------------------------------------------------------------------------------
#                          -- MODIFICATION HISTORY --
#-------------------------------------------------------------------------------
#
# $Id: Makefile,v 2.6 2005/09/13 15:31:38 paulv Exp $
#
# $Date: 2005/09/13 15:31:38 $
#
# $Revision: 2.6 $
#
# $Name:  $
#
# $State: Exp $
#
# $Log: Makefile,v $
# Revision 2.6  2005/09/13 15:31:38  paulv
# - Updated for version 2.1 of lnfl code for Release 9.4 of LBLRTM.
#
# Revision 2.5  2003/12/02 16:54:09  paulv
# Updated for LNFL v1.15 distribution
#
# Revision 2.4  2003/07/15 19:08:58  paulv
# - Changed compilation flag macros to use single precision compilation flags.
#
# Revision 2.3  2003/06/20 13:54:12  paulv
# Updated for LNFL v1.8 distribution
#
# Revision 2.2  2002/03/15 18:59:25  paulv
# - Changed output executable name to latest version.
#
# Revision 2.1  2002/03/15 17:51:18  paulv
# - New makefile version
#
#
#
