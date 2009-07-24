#==============================================================================
#
# Makefile for creating a tarball of the SRF code
#
# $Id: Makefile,v 1.2 2004/09/03 20:32:59 paulv Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

F90_DIR = $(HOME)/f90

include $(F90_DIR)/make.macros


# -------------
# This makefile
# -------------

MAKEFILE = Makefile


# ------------
# Object files
# ------------

OBJ_FILES = SRF_Define.o \
            SRF_ASCII_IO.o \
            SRF_netCDF_IO.o \
            SRF_Utility.o



#-----------------------------------------------------------------------------
#                            -- Create tarballs --
#-----------------------------------------------------------------------------

all_tar: tar average_srfs_tar srf_ascii2nc_tar srf_ascii_io_test_tar srf_netcdf_io_test_tar


TAR_FILE = SRF_software.tar

tar:
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	compress $(TAR_FILE)


average_srfs_tar:
	cd Average_SRFs; make tar; make realclean; cd ..


srf_ascii2nc_tar:
	cd SRF_ASCII2NC; make tar; make realclean; cd ..


srf_ascii_io_test_tar:
	cd SRF_ASCII_IO_Test; make tar; make realclean; cd ..


srf_netcdf_io_test_tar:
	cd SRF_netCDF_IO_Test; make tar; make realclean; cd ..



#-----------------------------------------------------------------------------
#                           -- Create HTML files --
#-----------------------------------------------------------------------------

all_html: html average_srfs_html srf_ascii2nc_html srf_ascii_io_test_html srf_netcdf_io_test_html

PRO_FILE = main.pro
SOURCE_FILES = ${OBJ_FILES:.o=.f90}

html:
	@n=0; \
	for source_file in ${SOURCE_FILES}; do \
	  n=`expr $$n + 1`; \
	done; \
	i=0; \
	for source_file in ${SOURCE_FILES}; do \
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


average_srfs_html:
	cd Average_SRFs; make html; make realclean; cd ..


srf_ascii2nc_html:
	cd SRF_ASCII2NC; make html; make realclean; cd ..


srf_ascii_io_test_html:
	cd SRF_ASCII_IO_Test; make html; make realclean; cd ..


srf_netcdf_io_test_html:
	cd SRF_netCDF_IO_Test; make html; make realclean; cd ..




#-----------------------------------------------------------------------------
#                        -- Update web directories --
#-----------------------------------------------------------------------------

all: web_update


# -------------
# Web page root
# -------------

WEBROOT = $(HOME)/MyWebStuff/Fortran90/Instrument_Information/SRF


# --------------------------------
# Move tarballs to web directories
# --------------------------------

web_update: all_tar all_html
	@$(MOVE) $(TAR_FILE).Z $(WEBROOT)
	@$(MOVE) *.html $(WEBROOT)
	@$(MOVE) ./Average_SRFs/*.Z    $(WEBROOT)/Average_SRFs
	@$(MOVE) ./Average_SRFs/*.html $(WEBROOT)/Average_SRFs
	@$(MOVE) ./SRF_ASCII2NC/*.Z    $(WEBROOT)/SRF_ASCII2NC
	@$(MOVE) ./SRF_ASCII2NC/*.html $(WEBROOT)/SRF_ASCII2NC
	@$(MOVE) ./SRF_ASCII_IO_Test/*.Z    $(WEBROOT)/SRF_ASCII_IO_Test
	@$(MOVE) ./SRF_ASCII_IO_Test/*.html $(WEBROOT)/SRF_ASCII_IO_Test
	@$(MOVE) ./SRF_netCDF_IO_Test/*.Z    $(WEBROOT)/SRF_netCDF_IO_Test
	@$(MOVE) ./SRF_netCDF_IO_Test/*.html $(WEBROOT)/SRF_netCDF_IO_Test

