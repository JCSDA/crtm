#==============================================================================
#
# Makefile to install Transmittance Production scripts
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# Shell scripts to install
SHELL_SOURCE_DIR = $(CRTM_SOURCE_ROOT)/TauProd/Infrared/shell_scripts

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



# --------------
# Define targets
# --------------
install:
	@if [ -d $(HOME)/bin ]; then \
	  cd $(HOME)/bin; \
	  for script_file in ${SHELL_SCRIPT_FILES}; do \
	    $(LINK) ${SHELL_SOURCE_DIR}/$$script_file .; \
	  done; \
	fi

