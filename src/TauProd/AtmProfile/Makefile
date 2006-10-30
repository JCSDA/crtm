#==============================================================================
#
# Makefile for creating the AtmProfile source code tarballs and
# html documentation
#
# $Id: Makefile,v 1.1 2004/12/30 18:52:01 paulv Exp $
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

# -- Utility source files
UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = Type_Kinds.f90 \
                       File_Utility.f90 \
                       Error_Handler.f90 \
                       Compare_Float_Numbers.f90 \
                       Binary_File_Utility.f90

# -- The netCDF source files
NETCDF_SOURCE_DIR = $(F90_DIR)/NETCDF
NETCDF_SOURCE_FILES = netCDF_Dimension_Utility.f90 \
                      netCDF_Attribute_Utility.f90 \
                      netCDF_Variable_Utility.f90 \
                      netCDF_Utility.f90

# -- The AtmProfile source files
ATMPROFILE_SOURCE_FILES = AtmProfile_Define.f90 \
                          AtmProfile_Parameters.f90 \
                          AtmProfile_netCDF_IO.f90 \
                          AtmProfile_Binary_IO.f90

# -- All the source code
ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES) \
                   $(ATMPROFILE_SOURCE_FILES)




#-----------------------------------------------------------------------------
#                  -- CREATE DEPENDENT SOURCE FILE LINKS --
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
\


#-----------------------------------------------------------------------------
#                             -- MAIN TARGET --
#-----------------------------------------------------------------------------

all: web_update



#-----------------------------------------------------------------------------
#                            -- CREATE TARBALLS --
#-----------------------------------------------------------------------------

all_tar: tar atmprofile_nc2bin_tar test_atmprofile_tar

TAR_FILE = AtmProfile_software.tar

tar: create_links
	$(TARBALL_CREATE) $(TAR_FILE) $(ALL_SOURCE_FILES)
	compress -f $(TAR_FILE)
	@make remove_links

atmprofile_nc2bin_tar:
	cd AtmProfile_NC2BIN; make tar; make realclean; cd ..

test_atmprofile_tar:
	cd Test_AtmProfile; make tar; make realclean; cd ..



#-----------------------------------------------------------------------------
#                           -- CREATE HTML FILES --
#-----------------------------------------------------------------------------

all_html: html atmprofile_nc2bin_html

PRO_FILE = main.pro

html:
	@n=0; \
	for source_file in ${ATMPROFILE_SOURCE_FILES}; do \
	  n=`expr $$n + 1`; \
	done; \
	i=0; \
	for source_file in ${ATMPROFILE_SOURCE_FILES}; do \
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

atmprofile_nc2bin_html:
	cd AtmProfile_NC2BIN; make html; make realclean; cd ..



#-----------------------------------------------------------------------------
#                        -- UPDATE WEB DIRECTORIES --
#-----------------------------------------------------------------------------

# -------------
# Web page root
# -------------

WEBROOT = $(HOME)/MyWebStuff/Fortran90/AtmProfile


# --------------------------------
# Move tarballs to web directories
# --------------------------------

web_update: all_tar all_html
	@$(MOVE) $(TAR_FILE).Z $(WEBROOT)
	@$(MOVE) *.html $(WEBROOT)
	@$(MOVE) ./AtmProfile_NC2BIN/*.Z $(WEBROOT)/AtmProfile_NC2BIN
	@$(MOVE) ./AtmProfile_NC2BIN/*.html $(WEBROOT)/AtmProfile_NC2BIN
	@$(MOVE) ./Test_AtmProfile/*.Z $(WEBROOT)/Test_AtmProfile
