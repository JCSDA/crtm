#==============================================================================
#
# Makefile driver for creating tarballs and html of all the pCRTM code
# and documentation
#
# $Id: Makefile,v 1.1 2004/12/29 20:33:29 paulv Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                             -- MAIN TARGET --
#-----------------------------------------------------------------------------

all: source example test

source:
	cd Source_Code; make all; cd ..

example:
	cd Example_Code; make all; cd ..

test:
	cd Test_Code; make all; cd ..

