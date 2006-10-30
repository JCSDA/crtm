#==============================================================================
#
# Makefile for creating a tarball of the SpcCoeff code
#
# $Id: Makefile,v 1.5 2006/05/02 16:58:02 dgroff Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

F90_DIR = $(HOME)/f90

include $(F90_DIR)/make.macros



#-----------------------------------------------------------------------------
#                        -- Define source code files --
#-----------------------------------------------------------------------------

# -- Dependent source files
UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = Type_Kinds.f90 \
                       File_Utility.f90 \
                       Message_Handler.f90 \
                       Compare_Float_Numbers.f90 \
                       Binary_File_Utility.f90 \
                       Endian_Utility.f90
 
NETCDF_SOURCE_DIR = $(F90_DIR)/NETCDF
NETCDF_SOURCE_FILES = netCDF_Utility.f90 \
                      netCDF_Attribute_Utility.f90 \
                      netCDF_Dimension_Utility.f90 \
                      netCDF_Variable_Utility.f90

# -- The SpcCoeff code
SPCCOEFF_SOURCE_FILES = SpcCoeff_Define.f90 \
                        SpcCoeff_Binary_IO.f90 \
                        SpcCoeff_netCDF_IO.f90

# -- Everything
ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES) \
                   $(SPCCOEFF_SOURCE_FILES)



#-----------------------------------------------------------------------------
#                  -- Create dependent source file links --
#-----------------------------------------------------------------------------

create_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${NETCDF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${NETCDF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done


remove_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done
	@for source_file in ${NETCDF_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done



#-----------------------------------------------------------------------------
#                             -- Main target --
#-----------------------------------------------------------------------------

all: web_update



#-----------------------------------------------------------------------------
#                            -- Create tarballs --
#-----------------------------------------------------------------------------

all_tar: tar create_ir_spccoeff_tar create_mw_spccoeff_tar spccoeff_nc2bin_tar


TAR_FILE = SpcCoeff_software.tar

tar: create_links
	$(TARBALL_CREATE) $(TAR_FILE) $(ALL_SOURCE_FILES)
	compress -f $(TAR_FILE)
	@make remove_links


create_ir_spccoeff_tar:
	cd Create_IR_SpcCoeff; make tar; make realclean; cd ..

	
create_mw_spccoeff_tar:
	cd Create_MW_SpcCoeff; make tar; make realclean; cd ..


spccoeff_nc2bin_tar:
	cd SpcCoeff_NC2BIN; make tar; make realclean; cd ..




#-----------------------------------------------------------------------------
#                           -- Create HTML files --
#-----------------------------------------------------------------------------

all_html: html create_ir_spccoeff_html create_mw_spccoeff_html spccoeff_nc2bin_html

PRO_FILE = main.pro

html:
	@n=0; \
	for source_file in ${SPCCOEFF_SOURCE_FILES}; do \
	  n=`expr $$n + 1`; \
	done; \
	i=0; \
	for source_file in ${SPCCOEFF_SOURCE_FILES}; do \
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


create_ir_spccoeff_html:
	cd Create_IR_SpcCoeff; make html; make realclean; cd ..


create_mw_spccoeff_html:
	cd Create_MW_SpcCoeff; make html; make realclean; cd ..


spccoeff_nc2bin_html:
	cd SpcCoeff_NC2BIN; make html; make realclean; cd ..



#-----------------------------------------------------------------------------
#                        -- Update web directories --
#-----------------------------------------------------------------------------

# -------------
# Web page root
# -------------

WEBROOT = $(HOME)/MyWebStuff/Fortran90/Instrument_Information/SpcCoeff


# --------------------------------
# Move tarballs to web directories
# --------------------------------

web_update: all_tar all_html
	@$(MOVE) $(TAR_FILE).Z $(WEBROOT)
	@$(MOVE) *.html $(WEBROOT)
	@$(MOVE) ./Create_IR_SpcCoeff/*.Z $(WEBROOT)/Create_IR_SpcCoeff
	@$(MOVE) ./Create_IR_SpcCoeff/*.html $(WEBROOT)/Create_IR_SpcCoeff
	@$(MOVE) ./Create_MW_SpcCoeff/*.Z $(WEBROOT)/Create_MW_SpcCoeff
	@$(MOVE) ./Create_MW_SpcCoeff/*.html $(WEBROOT)/Create_MW_SpcCoeff
	@$(MOVE) ./SpcCoeff_NC2BIN/*.Z $(WEBROOT)/SpcCoeff_NC2BIN
	@$(MOVE) ./SpcCoeff_NC2BIN/*.html $(WEBROOT)/SpcCoeff_NC2BIN

