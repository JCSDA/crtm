#==============================================================================
#
# Makefile for creating a tarball of the pCRTM source code and documentation
#
# $Id: Makefile,v 1.3 2006/05/02 14:58:35 dgroff Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- DEFINE MACROS --
#-----------------------------------------------------------------------------

F90_DIR = $(HOME)/f90

include $(F90_DIR)/make.macros



#-----------------------------------------------------------------------------
#                        -- DEFINE SOURCE CODE FILES --
#-----------------------------------------------------------------------------

# -- Dependent source files
UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = Type_Kinds.f90 \
                       File_Utility.f90 \
                       Message_Handler.f90 \
                       Fundamental_Constants.f90

# -- The SpcCoeff source files
SPCCOEFF_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SpcCoeff
SPCCOEFF_SOURCE_FILES = SpcCoeff_Define.f90 \
                        SpcCoeff_Binary_IO.f90

# -- The TauCoeff source files
TAUCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/TauCoeff
TAUCOEFF_SOURCE_FILES = TauCoeff_Define.f90 \
                        TauCoeff_Binary_IO.f90

# -- All the above dependents
ALL_DEPENDENT_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                             $(SPCCOEFF_SOURCE_FILES) \
                             $(TAUCOEFF_SOURCE_FILES)


# -- The pCRTM source files
pCRTM_SOURCE_FILES = parameters.f90 \
                     initialize.f90 \
                     spectral_coefficients.f90 \
                     transmittance_coefficients.f90 \
                     sensor_planck_routines.f90 \
                     absorber_profile.f90 \
                     predictors.f90 \
                     transmittance.f90 \
                     radiance.f90 \
                     forward_model.f90 \
                     tangent_linear_model.f90 \
                     adjoint_model.f90 \
                     k_matrix_model.f90

# -- All the source code
ALL_SOURCE_FILES = $(ALL_DEPENDENT_SOURCE_FILES) \
                   $(pCRTM_SOURCE_FILES)




#-----------------------------------------------------------------------------
#                  -- CREATE DEPENDENT SOURCE FILE LINKS --
#-----------------------------------------------------------------------------

create_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SPCCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SPCCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${TAUCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${TAUCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done


remove_links:
	@for source_file in ${ALL_DEPENDENT_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done



#-----------------------------------------------------------------------------
#                             -- MAIN TARGET --
#-----------------------------------------------------------------------------

all: web_update



#-----------------------------------------------------------------------------
#                            -- Create tarballs --
#-----------------------------------------------------------------------------

TAR_FILE = pCRTM_software.tar

tar: create_links
	$(TARBALL_CREATE) $(TAR_FILE) $(ALL_SOURCE_FILES)
	compress -f $(TAR_FILE)
	@make remove_links



#-----------------------------------------------------------------------------
#                           -- CREATE HTML FILES --
#-----------------------------------------------------------------------------

PRO_FILE = main.pro

html:
	@n=0; \
	for source_file in ${pCRTM_SOURCE_FILES}; do \
	  n=`expr $$n + 1`; \
	done; \
	i=0; \
	for source_file in ${pCRTM_SOURCE_FILES}; do \
	  i=`expr $$i + 1`; \
	  case $$i in \
	    1) FILES="['$$source_file'" ;; \
	    $$n) FILES="$$FILES,'$$source_file']" ;; \
	    *) FILES="$$FILES,'$$source_file'" ;; \
	  esac; \
	done; \
	echo "create_f90_html_doc,$$FILES" > ${PRO_FILE}; \
	echo "exit" >> ${PRO_FILE}; \
	idl ${PRO_FILE}; \
	$(REMOVE) $(PRO_FILE)



#-----------------------------------------------------------------------------
#                        -- Update web directories --
#-----------------------------------------------------------------------------

# -------------
# Web page root
# -------------

WEBROOT = $(HOME)/MyWebStuff/Fortran90/CRTM/Prototype/Source_Code


# --------------------------------
# Move tarballs to web directories
# --------------------------------

web_update: tar html
	@$(MOVE) $(TAR_FILE).Z $(WEBROOT)
	@$(MOVE) *.html $(WEBROOT)
