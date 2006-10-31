#==============================================================================
#
# Makefile for JCSDA CRTM Build
#
# $Id: makefile,v 1.10 2006/08/31 17:06:47 frpv Exp $
#
#==============================================================================

# -------------
# Define macros
# -------------
include ../make.macros

# This makefile
MAKE_FILE = makefile

# The library name
LIBRARY_TAG = CRTM
LIBRARY_NAME = lib$(LIBRARY_TAG).a

# JCSDA CRTM Object files
OBJ_FILES = Type_Kinds.o \
            File_Utility.o \
            Message_Handler.o \
            Compare_Float_Numbers.o \
            Endian_Utility.o \
            Binary_File_Utility.o \
            Sort_Utility.o \
            Fundamental_Constants.o \
            Spectral_Units_Conversion.o \
            SpcCoeff_Define.o \
            SpcCoeff_Binary_IO.o \
            TauCoeff_Define.o \
            TauCoeff_Binary_IO.o \
            CloudCoeff_Define.o \
            CloudCoeff_Binary_IO.o \
            AerosolCoeff_Define.o \
            AerosolCoeff_Binary_IO.o \
            EmisCoeff_Define.o \
            EmisCoeff_Binary_IO.o \
            NESDIS_AMSRE_SICEEM_Module.o \
            NESDIS_AMSRE_SNOWEM_Module.o \
            NESDIS_AMSU_SICEEM_Module.o \
            NESDIS_AMSU_SnowEM_Module.o \
            NESDIS_LandEM_Module.o \
            NESDIS_OCEANEM_Module.o \
            NESDIS_SEAICE_PHYEM_MODULE.o \
            NESDIS_SnowEM_Parameters.o \
            NESDIS_SSMI_Module.o \
            NESDIS_SSMI_SIceEM_Module.o \
            NESDIS_SSMI_SnowEM_Module.o \
            CRTM_Utility.o \
            CRTM_Interpolation.o \
            CRTM_SpcCoeff.o \
            CRTM_TauCoeff.o \
            CRTM_AerosolCoeff.o \
            CRTM_CloudCoeff.o \
            CRTM_EmisCoeff.o \
            CRTM_Atmosphere_Define.o \
            CRTM_Cloud_Define.o \
            CRTM_Aerosol_Define.o \
            CRTM_Atmosphere_Binary_IO.o \
            CRTM_Cloud_Binary_IO.o \
            CRTM_Aerosol_Binary_IO.o \
            CRTM_Surface_Define.o \
            CRTM_SensorData_Define.o \
            CRTM_SensorInfo.o \
            CRTM_Surface_Binary_IO.o \
            CRTM_ChannelInfo_Define.o \
            CRTM_ChannelInfo.o \
            CRTM_GeometryInfo_Define.o \
            CRTM_Predictor_Define.o \
            CRTM_Predictor.o \
            CRTM_AtmAbsorption_Define.o \
            CRTM_AtmAbsorption.o \
            CRTM_AtmScatter_Define.o \
            CRTM_AerosolScatter.o \
            CRTM_CloudScatter.o \
            CRTM_SfcOptics_Define.o \
            CRTM_SfcOptics.o \
            CRTM_IR_Ice_SfcOptics.o \
            CRTM_IR_Land_SfcOptics.o \
            CRTM_IR_Snow_SfcOptics.o \
            CRTM_IR_Water_SfcOptics.o \
            CRTM_MW_Ice_SfcOptics.o \
            CRTM_MW_Land_SfcOptics.o \
            CRTM_MW_Snow_SfcOptics.o \
            CRTM_MW_Water_SfcOptics.o \
            CRTM_IRSSEM.o \
            CRTM_surface_ir_emissivity.o \
            CRTM_Fastem1.o \
            CRTM_AtmOptics.o \
            CRTM_RTSolution_Define.o \
            CRTM_RTSolution.o \
            CRTM_Planck_Functions.o \
            CRTM_Options_Define.o \
            CRTM_Parameters.o \
            CRTM_LifeCycle.o \
            CRTM_Forward_Module.o \
            CRTM_Tangent_Linear_Module.o \
            CRTM_Adjoint_Module.o \
            CRTM_K_Matrix_Module.o \
            CRTM_Module.o


# ------------------
# Define dependecies
# ------------------
# Default build based on OS type
all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKE_FILE) library $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKE_FILE) library $(AIX_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKE_FILE) library $(IRIX64_FLAGS) ;; \
	  "Linux" )  make -f $(MAKE_FILE) library $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac

# IBM AIX Compiler
ibm_debug:
	make -f $(MAKE_FILE) library $(AIX_FLAGS_DEBUG)

ibm:
	make -f $(MAKE_FILE) library $(AIX_FLAGS_PROD)


# SGI MIPSpro Compiler
sgi_debug:
	make -f $(MAKE_FILE) library $(IRIX64_FLAGS_DEBUG)

sgi:
	make -f $(MAKE_FILE) library $(IRIX64_FLAGS_PROD)


# SunOS Compiler
sun_debug:
	make -f $(MAKE_FILE) library $(SUNOS_FLAGS_DEBUG)

sun:
	make -f $(MAKE_FILE) library $(SUNOS_FLAGS_PROD)


# Linux compilers.
intel_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_INTEL_DEBUG)

intel:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_INTEL_PROD)

lahey_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_LAHEY_DEBUG)

lahey:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_LAHEY_PROD)

pgi_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_PGI_DEBUG)

pgi:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_PGI_PROD)

g95_debug:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_G95_DEBUG)

g95:
	make -f $(MAKE_FILE) library $(LINUX_FLAGS_G95_PROD)


# Build the library and include files
library: $(OBJ_FILES)
	$(ARCHIVER) $(ARCHIVER_FLAGS) $(LIBRARY_NAME) $(OBJ_FILES)

# Clean up the chaff
clean:
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.stb *.a

# Define build dependencies and rules
include make.dependencies
include make.rules
