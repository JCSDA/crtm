#==============================================================================
#
# Makefile for builidng and installing all the required TauProd softweare
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


# -----------
# Main target
# -----------
# Compilation targets
all: link build install

# Link targets
link: link_message \
      link_create_lblrtm_input_files \
      link_lblrtm_to_netcdf \
      link_create_process_defaults_file \
      link_create_processcontrol_file \
      link_check_processcontrol_file \
      link_convolve_tauspc_with_srf \
      link_check_tauprofile_data \
      link_assemble_tauprofile_data \
      link_compute_effective_tauprofile

link_message:
	@echo ; \
	echo "Linking in source files (this may take a while)...."; \
	echo "---------------------------------------------------"


link_create_lblrtm_input_files:
	cd $(CREATE_LBLRTM_INPUT_FILES_DIR); make create_links


link_lblrtm_to_netcdf:
	cd $(LBLRTM_TO_NETCDF_DIR); make create_links


link_create_process_defaults_file:
	cd $(CREATE_PROCESS_DEFAULTS_FILE_DIR); make create_links


link_create_processcontrol_file:
	cd $(CREATE_PROCESSCONTROL_FILE_DIR); make create_links


link_check_processcontrol_file:
	cd $(CHECK_PROCESSCONTROL_FILE_DIR); make create_links


link_convolve_tauspc_with_srf:
	cd $(CONVOLVE_TAUSPC_WITH_SRF_DIR); make create_links


link_check_tauprofile_data:
	cd $(CHECK_TAUPROFILE_DATA_DIR); make create_links


link_assemble_tauprofile_data:
	cd $(ASSEMBLE_TAUPROFILE_DATA_DIR); make create_links


link_compute_effective_tauprofile:
	cd $(COMPUTE_EFFECTIVE_TAUPROFILE_DIR); make create_links


# Build targets
build: build_message \
       build_create_lblrtm_input_files \
       build_lblrtm_to_netcdf \
       build_create_process_defaults_file \
       build_create_processcontrol_file \
       build_check_processcontrol_file \
       build_convolve_tauspc_with_srf \
       build_check_tauprofile_data \
       build_assemble_tauprofile_data \
       build_compute_effective_tauprofile


build_message:
	@echo ; \
	echo "Building TauProd software (this may take a while)...."; \
	echo "-----------------------------------------------------"

build_create_lblrtm_input_files:
	cd $(CREATE_LBLRTM_INPUT_FILES_DIR); make


build_lblrtm_to_netcdf:
	cd $(LBLRTM_TO_NETCDF_DIR); make


build_create_process_defaults_file:
	cd $(CREATE_PROCESS_DEFAULTS_FILE_DIR); make


build_create_processcontrol_file:
	cd $(CREATE_PROCESSCONTROL_FILE_DIR); make


build_check_processcontrol_file:
	cd $(CHECK_PROCESSCONTROL_FILE_DIR); make


build_convolve_tauspc_with_srf:
	cd $(CONVOLVE_TAUSPC_WITH_SRF_DIR); make


build_check_tauprofile_data:
	cd $(CHECK_TAUPROFILE_DATA_DIR); make


build_assemble_tauprofile_data:
	cd $(ASSEMBLE_TAUPROFILE_DATA_DIR); make


build_compute_effective_tauprofile:
	cd $(COMPUTE_EFFECTIVE_TAUPROFILE_DIR); make


# Installation targets
install: install_message \
         install_create_lblrtm_input_files \
         install_lblrtm_to_netcdf \
         install_create_process_defaults_file \
         install_create_processcontrol_file \
         install_check_processcontrol_file \
         install_convolve_tauspc_with_srf \
         install_check_tauprofile_data \
         install_assemble_tauprofile_data \
         install_compute_effective_tauprofile \
         install_shell_scripts


install_message:
	@echo ; \
	echo "Installing TauProd software...."; \
	echo "-------------------------------"


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
