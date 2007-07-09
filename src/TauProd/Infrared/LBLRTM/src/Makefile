#------------------------------------------------------------------------------
#
# NAME:
#       Makefile
#
# PURPOSE:
#       Unix make file for LBLRTM/LNFL compilation and link
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
#                          -- Define default macros --
#-----------------------------------------------------------------------------

include make.macros



#-----------------------------------------------------------------------------
#                              -- Define targets --
#-----------------------------------------------------------------------------

all: lblrtm lnfl

lblrtm:
	cd $(LBLRTM_DIR); make all

lnfl:
	cd $(LNFL_DIR); make all


clean: clean_lblrtm clean_lnfl clean_shared

clean_lblrtm:
	cd $(LBLRTM_DIR); make clean

clean_lnfl:
	cd $(LNFL_DIR); make clean

clean_shared:
	cd $(SHARED_DIR); make clean


#-------------------------------------------------------------------------------
#                          -- MODIFICATION HISTORY --
#-------------------------------------------------------------------------------
#
# $Id: Makefile,v 1.1 2005/09/13 19:50:27 paulv Exp $
#
# $Date: 2005/09/13 19:50:27 $
#
# $Revision: 1.1 $
#
# $Name:  $
#
# $State: Exp $
#
# $Log: Makefile,v $
# Revision 1.1  2005/09/13 19:50:27  paulv
# Initial checkin.
#
#
#
