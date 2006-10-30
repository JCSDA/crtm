#==============================================================================
#
# Makefile for creating a tarball of the pCRTM Example code
#
# $Id: Makefile,v 1.1 2004/12/29 20:33:29 paulv Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                            -- DEFINE MACROS --
#-----------------------------------------------------------------------------

F90_DIR = $(HOME)/f90

include $(F90_DIR)/make.macros



#-----------------------------------------------------------------------------
#                             -- MAIN TARGET --
#-----------------------------------------------------------------------------

all: web_update



#-----------------------------------------------------------------------------
#                            -- CREATE TARBALLS --
#-----------------------------------------------------------------------------

all_tar: forward_tar tangent_linear_tar adjoint_tar k_matrix_tar

forward_tar:
	cd Forward_Model_Example; make tar; make realclean; cd ..

tangent_linear_tar:
	cd Tangent_Linear_Model_Example; make tar; make realclean; cd ..

adjoint_tar:
	cd Adjoint_Model_Example; make tar; make realclean; cd ..

k_matrix_tar:
	cd K_Matrix_Model_Example; make tar; make realclean; cd ..



#-----------------------------------------------------------------------------
#                        -- Update web directories --
#-----------------------------------------------------------------------------

# -------------
# Web page root
# -------------

WEBROOT = $(HOME)/MyWebStuff/Fortran90/CRTM/Prototype/Example_Code


# --------------------------------
# Move tarballs to web directories
# --------------------------------

web_update: all_tar
	@$(MOVE) ./Forward_Model_Example/*.Z $(WEBROOT)
	@$(MOVE) ./Tangent_Linear_Model_Example/*.Z $(WEBROOT)
	@$(MOVE) ./Adjoint_Model_Example/*.Z $(WEBROOT)
	@$(MOVE) ./K_Matrix_Model_Example/*.Z $(WEBROOT)
