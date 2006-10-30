#==============================================================================
#
# Makefile for creating a tarball of the pCRTM Test code
#
# $Id: Makefile,v 1.1 2004/12/29 20:33:29 paulv Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- DEFINE MACROS --
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

all_tar: tangent_linear_tar adjoint_tar k_matrix_tar

tangent_linear_tar:
	cd Tangent_Linear_Model_Test; make tar; make realclean; cd ..

adjoint_tar:
	cd Adjoint_Model_Test; make tar; make realclean; cd ..

k_matrix_tar:
	cd K_Matrix_Model_Test; make tar; make realclean; cd ..



#-----------------------------------------------------------------------------
#                        -- Update web directories --
#-----------------------------------------------------------------------------

# -------------
# Web page root
# -------------

WEBROOT = $(HOME)/MyWebStuff/Fortran90/CRTM/Prototype/Test_Code


# --------------------------------
# Move tarballs to web directories
# --------------------------------

web_update: all_tar
	@$(MOVE) ./Tangent_Linear_Model_Test/*.Z $(WEBROOT)
	@$(MOVE) ./Adjoint_Model_Test/*.Z $(WEBROOT)
	@$(MOVE) ./K_Matrix_Model_Test/*.Z $(WEBROOT)
