#==============================================================================
#
# Makefile to install Transmittance Production scripts
#
# $Id: Makefile,v 1.4 2006/01/05 19:23:30 paulv Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

F90_DIR = $(HOME)/f90

include $(F90_DIR)/make.macros


# ------------------------
# Shell scripts to install
# ------------------------

SHELL_SOURCE_DIR = $(F90_DIR)/Transmittance_Production/shell_scripts

SHELL_SCRIPT_FILES = Check_LBL_Run_Completion \
                     Check_TauSpc_Run_Completion \
                     Define_Molecule_Tag \
                     Install_LBLRTM \
                     Install_TauProd \
                     Remove_Batch_Log_Files \
                     Remove_SRF_Files \
                     Start_Processing \
                     TauProd_Cleanup \
                     process_TauProfile_files \
                     process_TauSpc_files \
                     process_tape5_files



# ----------
# Install it
# ----------

install:
	@if [ -d $(HOME)/bin ]; then \
	  cd $(HOME)/bin; \
	  for script_file in ${SHELL_SCRIPT_FILES}; do \
	    $(LINK) ${SHELL_SOURCE_DIR}/$$script_file .; \
	  done; \
	fi

