#==============================================================================
#
# Makefile for local JCSDA CRTM Build package
#
# $Id: Makefile,v 1.11 2006/08/31 17:06:30 frpv Exp $
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# The library directories
SOURCE_DIR = src
LIBRARY_DIR = lib
INCLUDE_DIR = include

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            Sort_Utility.f90 \
            Fundamental_Constants.f90 \
            Spectral_Units_Conversion.f90 \
            SpcCoeff_Define.f90 \
            SpcCoeff_Binary_IO.f90 \
            TauCoeff_Define.f90 \
            TauCoeff_Binary_IO.f90 \
            CloudCoeff_Define.f90 \
            CloudCoeff_Binary_IO.f90 \
            AerosolCoeff_Define.f90 \
            AerosolCoeff_Binary_IO.f90 \
            EmisCoeff_Define.f90 \
            EmisCoeff_Binary_IO.f90 \
            NESDIS_AMSRE_SICEEM_Module.f90 \
            NESDIS_AMSRE_SNOWEM_Module.f90 \
            NESDIS_AMSU_SICEEM_Module.f90 \
            NESDIS_AMSU_SnowEM_Module.f90 \
            NESDIS_LandEM_Module.f90 \
            NESDIS_OCEANEM_Module.f90 \
            NESDIS_SEAICE_PHYEM_MODULE.f90 \
            NESDIS_SnowEM_Parameters.f90 \
            NESDIS_SSMI_Module.f90 \
            NESDIS_SSMI_SIceEM_Module.f90 \
            NESDIS_SSMI_SnowEM_Module.f90 \
            CRTM_Utility.f90 \
            CRTM_Interpolation.f90 \
            CRTM_SpcCoeff.f90 \
            CRTM_TauCoeff.f90 \
            CRTM_AerosolCoeff.f90 \
            CRTM_CloudCoeff.f90 \
            CRTM_EmisCoeff.f90 \
            CRTM_Atmosphere_Define.f90 \
            CRTM_Cloud_Define.f90 \
            CRTM_Aerosol_Define.f90 \
            CRTM_Atmosphere_Binary_IO.f90 \
            CRTM_Cloud_Binary_IO.f90 \
            CRTM_Aerosol_Binary_IO.f90 \
            CRTM_Surface_Define.f90 \
            CRTM_SensorData_Define.f90 \
            CRTM_SensorInfo.f90 \
            CRTM_Surface_Binary_IO.f90 \
            CRTM_ChannelInfo_Define.f90 \
            CRTM_ChannelInfo.f90 \
            CRTM_GeometryInfo_Define.f90 \
            CRTM_Predictor_Define.f90 \
            CRTM_Predictor.f90 \
            CRTM_AtmAbsorption_Define.f90 \
            CRTM_AtmAbsorption.f90 \
            CRTM_AtmScatter_Define.f90 \
            CRTM_AerosolScatter.f90 \
            CRTM_CloudScatter.f90 \
            CRTM_SfcOptics_Define.f90 \
            CRTM_SfcOptics.f90 \
            CRTM_IR_Ice_SfcOptics.f90 \
            CRTM_IR_Land_SfcOptics.f90 \
            CRTM_IR_Snow_SfcOptics.f90 \
            CRTM_IR_Water_SfcOptics.f90 \
            CRTM_MW_Ice_SfcOptics.f90 \
            CRTM_MW_Land_SfcOptics.f90 \
            CRTM_MW_Snow_SfcOptics.f90 \
            CRTM_MW_Water_SfcOptics.f90 \
            CRTM_IRSSEM.f90 \
            CRTM_surface_ir_emissivity.f90 \
            CRTM_Fastem1.f90 \
            CRTM_AtmOptics.f90 \
            CRTM_RTSolution_Define.f90 \
            CRTM_RTSolution.f90 \
            CRTM_Planck_Functions.f90 \
            CRTM_Options_Define.f90 \
            CRTM_Parameters.f90 \
            CRTM_LifeCycle.f90 \
            CRTM_Forward_Module.f90 \
            CRTM_Tangent_Linear_Module.f90 \
            CRTM_Adjoint_Module.f90 \
            CRTM_K_Matrix_Module.f90 \
            CRTM_Module.f90


# ------------------
# Define dependecies
# ------------------
# Make based on OS type
all: create_links
	@make -f makefile.dist all

# Install the library and include files in their repective directories
install:
	@make -f makefile.dist install

# Clean up
clean: remove_links
	@make -f makefile.dist clean

distclean: remove_links
	@make -f makefile.dist distclean


# File link creation and removal
macro_links:
	@linkfiles $(CRTM_SOURCE_ROOT) make.macros ; \
	cd $(SOURCE_DIR); \
	linkfiles $(CRTM_SOURCE_ROOT) make.rules; \
	cd ..

create_links: macro_links
	@cd $(SOURCE_DIR); \
	linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES); \
	cd ..

remove_links: macro_links
	@cd $(SOURCE_DIR); \
	unlinkfiles $(SRC_FILES) ; \
	cd ..


# -------------------------------
# Create tarball for distribution
# -------------------------------
TAR_FILE = JCSDA_CRTM.tar
tar: clean create_links
	$(TARBALL_CREATE) $(TAR_FILE) --exclude CVS $(SOURCE_DIR)
	@$(COPY) makefile.dist makefile
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	@$(REMOVE) makefile
	$(TARBALL_APPEND) $(TAR_FILE) make.macros
	compress $(TAR_FILE)

