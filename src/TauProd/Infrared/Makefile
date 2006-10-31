#==============================================================================
#
# Makefile for compiling/packaging all the required Transmittance
# Production software
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# Source code directories
CREATE_LBLRTM_INPUT_FILES_DIR = ./Create_LBLRTM_Input_Files
LBLRTM_TO_NETCDF_DIR = ./LBLRTM_to_netCDF
CREATE_PROCESS_DEFAULTS_FILE_DIR = ./Create_Process_Defaults_File
CREATE_PROCESSCONTROL_FILE_DIR = ./Create_ProcessControl_File
CHECK_PROCESSCONTROL_FILE_DIR = ./Check_ProcessControl_File
CONVOLVE_TAUSPC_WITH_SRF_DIR = ./Convolve_TauSpc_with_SRF
CHECK_TAUPROFILE_DATA_DIR = ./Check_TauProfile_Data
ASSEMBLE_TAUPROFILE_DATA_DIR = ./Assemble_TauProfile_Data
COMPUTE_EFFECTIVE_TAUPROFILE_DIR = ./Compute_Effective_TauProfile

ALL_SOURCE_DIRS = $(CREATE_LBLRTM_INPUT_FILES_DIR) \
                  $(LBLRTM_TO_NETCDF_DIR) \
                  $(CREATE_PROCESS_DEFAULTS_FILE_DIR) \
                  $(CREATE_PROCESSCONTROL_FILE_DIR) \
                  $(CHECK_PROCESSCONTROL_FILE_DIR) \
                  $(CONVOLVE_TAUSPC_WITH_SRF_DIR) \
                  $(CHECK_TAUPROFILE_DATA_DIR) \
                  $(ASSEMBLE_TAUPROFILE_DATA_DIR) \
                  $(COMPUTE_EFFECTIVE_TAUPROFILE_DIR)

# Shell script directory
SHELL_SCRIPTS_DIR = ./shell_scripts


# --------------
# Define targets
# --------------
# Compilation targets
all: create_lblrtm_input_files \
     lblrtm_to_netcdf \
     create_process_defaults_file \
     create_processcontrol_file \
     check_processcontrol_file \
     convolve_tauspc_with_srf \
     check_tauprofile_data \
     assemble_tauprofile_data \
     compute_effective_tauprofile


create_lblrtm_input_files:
	cd $(CREATE_LBLRTM_INPUT_FILES_DIR); make


lblrtm_to_netcdf:
	cd $(LBLRTM_TO_NETCDF_DIR); make


create_process_defaults_file:
	cd $(CREATE_PROCESS_DEFAULTS_FILE_DIR); make


create_processcontrol_file:
	cd $(CREATE_PROCESSCONTROL_FILE_DIR); make


check_processcontrol_file:
	cd $(CHECK_PROCESSCONTROL_FILE_DIR); make


convolve_tauspc_with_srf:
	cd $(CONVOLVE_TAUSPC_WITH_SRF_DIR); make


check_tauprofile_data:
	cd $(CHECK_TAUPROFILE_DATA_DIR); make


assemble_tauprofile_data:
	cd $(ASSEMBLE_TAUPROFILE_DATA_DIR); make


compute_effective_tauprofile:
	cd $(COMPUTE_EFFECTIVE_TAUPROFILE_DIR); make


# Installation targets
install: install_create_lblrtm_input_files \
         install_lblrtm_to_netcdf \
         install_create_process_defaults_file \
         install_create_processcontrol_file \
         install_check_processcontrol_file \
         install_convolve_tauspc_with_srf \
         install_check_tauprofile_data \
         install_assemble_tauprofile_data \
         install_compute_effective_tauprofile \
         install_shell_scripts


install_create_lblrtm_input_files:
	cd $(CREATE_LBLRTM_INPUT_FILES_DIR); make install


install_lblrtm_to_netcdf:
	cd $(LBLRTM_TO_NETCDF_DIR); make install


install_create_process_defaults_file:
	cd $(CREATE_PROCESS_DEFAULTS_FILE_DIR); make install


install_create_processcontrol_file:
	cd $(CREATE_PROCESSCONTROL_FILE_DIR); make install


install_check_processcontrol_file:
	cd $(CHECK_PROCESSCONTROL_FILE_DIR); make install


install_convolve_tauspc_with_srf:
	cd $(CONVOLVE_TAUSPC_WITH_SRF_DIR); make install


install_check_tauprofile_data:
	cd $(CHECK_TAUPROFILE_DATA_DIR); make install


install_assemble_tauprofile_data:
	cd $(ASSEMBLE_TAUPROFILE_DATA_DIR); make install


install_compute_effective_tauprofile:
	cd $(COMPUTE_EFFECTIVE_TAUPROFILE_DIR); make install


install_shell_scripts:
	cd $(SHELL_SCRIPTS_DIR); make install


# Cleanup targets
clean:
	@for source_dir in ${ALL_SOURCE_DIRS}; do \
	  if [ -d $$source_dir ]; then \
	    echo ; echo ; \
	    echo " Cleaning up directory: $$source_dir"; \
	    echo " ----------------------"; \
	    cd $$source_dir; \
	    make clean; \
	    cd ..; \
	  fi ; \
	done

realclean:
	@for source_dir in ${ALL_SOURCE_DIRS}; do \
	  if [ -d $$source_dir ]; then \
	    echo ; echo ; \
	    echo " REALLY cleaning up directory: $$source_dir"; \
	    echo " -----------------------------"; \
	    cd $$source_dir; \
	    make realclean; \
	    cd ..; \
	  fi ; \
	done


# Packaging targets
#ATMPROFILE_FILES = UMBC_101LVL_48ATM.AtmProfile.nc \
#                   CIMSS_101LVL_32ATM.AtmProfile.nc \
#                   ECMWF_101LVL_52ATM.AtmProfile.nc
#TAR_FILE = IR_TauProd_software.tar
#
#tar:
#	cd ..; \
#        $(TARBALL_CREATE) $(TAR_FILE) AIRS; \
#        $(TARBALL_APPEND) $(TAR_FILE) AtmProfile; \
#        $(TARBALL_APPEND) $(TAR_FILE) Constants; \
#        $(TARBALL_APPEND) $(TAR_FILE) Instrument_Information; \
#        $(TARBALL_APPEND) $(TAR_FILE) Integration; \
#        $(TARBALL_APPEND) $(TAR_FILE) Interpolation; \
#        $(TARBALL_APPEND) $(TAR_FILE) LBLRTM; \
#        $(TARBALL_APPEND) $(TAR_FILE) NETCDF; \
#        $(TARBALL_APPEND) $(TAR_FILE) Profile_Utility; \
#        $(TARBALL_APPEND) $(TAR_FILE) TauProfile; \
#        $(TARBALL_APPEND) $(TAR_FILE) Transmittance_Production; \
#        $(TARBALL_APPEND) $(TAR_FILE) Utility; \
#        $(TARBALL_APPEND) $(TAR_FILE) make.macros make.rules; \
#        compress $(TAR_FILE)
