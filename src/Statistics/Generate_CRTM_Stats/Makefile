#==============================================================================
#
# Makefile for generating regression fit statistics
#
# $Id: Makefile,v 1.3 2006/11/21 23:42:54 dgroff Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

include $(CRTM_SOURCE_ROOT)/make.macros


# -------------
# This makefile
# -------------

MAKE_FILE = Makefile


# ---------------
# Executable file
# ---------------

EXE_FILE = Generate_CRTM_Stats


# ------------
# Object files
# ------------

SRC_FILES =         CRTM_AerosolCoeff.f90 \
                    Type_Kinds.f90 \
                    Sort_Utility.f90 \
                    File_Utility.f90 \
                    CRTM_Parameters.f90 \
                    Message_Handler.f90 \
                    Endian_Utility.f90 \
                    Binary_File_Utility.f90 \
                    CRTM_GeometryInfo_Define.f90 \
                    Compare_Float_Numbers.f90 \
                    EmisCoeff_Define.f90 \
                    EmisCoeff_Binary_IO.f90 \
                    CloudCoeff_Define.f90 \
                    CloudCoeff_Binary_IO.f90 \
                    CRTM_CloudCoeff.f90 \
                    CRTM_EmisCoeff.f90 \
                    CRTM_RTSolution_Define.f90 \
                    CRTM_RTSolution.f90 \
                    AntCorr_Define.f90 \
                    AntCorr_Binary_IO.f90 \
                    CRTM_AntCorr.f90 \
                    AerosolCoeff_Define.f90 \
                    AerosolCoeff_Binary_IO.f90 \
                    CRTM_Aerosol_Define.f90 \
                    CRTM_Cloud_Define.f90 \
                    CRTM_Cloud_Binary_IO.f90 \
                    CRTM_Aerosol_Binary_IO.f90 \
                    SpcCoeff_Define.f90 \
                    SpcCoeff_Binary_IO.f90 \
                    CRTM_SpcCoeff.f90 \
                    CRTM_ChannelInfo_Define.f90 \
                    netCDF_Dimension_Utility.f90 \
                    netCDF_Variable_Utility.f90 \
                    netCDF_Attribute_Utility.f90 \
                    netCDF_Utility.f90 \
                    CRTM_SensorData_Define.f90 \
                    TauCoeff_Define.f90 \
                    TauCoeff_Binary_IO.f90 \
                    CRTM_TauCoeff.f90 \
                    CRTM_LifeCycle.f90 \
                    List_File_Utility.f90 \
                    Endian_Utility.f90 \
                    SpcCoeff_Binary_IO.f90 \
 		    CRTM_Atmosphere_Define.f90 \
                    CRTM_Atmosphere_Binary_IO.f90 \
		    CRTM_AtmAbsorption_Define.f90 \
	            CRTM_Predictor_Define.f90 \
                    CRTM_Predictor.f90 \
                    CRTM_AtmAbsorption.f90 \
 	            CRTM_AtmScatter_Define.f90 \
	            CRTM_AerosolScatter.f90 \
                    CRTM_Interpolation.f90 \
	            CRTM_CloudScatter.f90 \
 		    CRTM_AtmOptics.f90 \
                    CRTM_LifeCycle.f90 \
                    ComponentTest_Define.f90 \
                    ComponentTest_netCDF_IO.f90 \
                    CRTM_Surface_Define.f90 \
		    TauProfile_Define.f90 \
		    CRTM_SfcOptics_Define.f90 \
		    NESDIS_SnowEM_Parameters.f90 \
		    NESDIS_LandEM_Module.f90 \
		    NESDIS_OCEANEM_Module.f90 \
		    NESDIS_AMSU_SnowEM_Module.f90 \
		    NESDIS_SSMI_SnowEM_Module.f90 \
		    NESDIS_AMSRE_SNOWEM_Module.f90 \
		    NESDIS_AMSRE_SICEEM_Module.f90 \
		    NESDIS_AMSU_SICEEM_Module.f90 \
		    NESDIS_SSMI_SIceEM_Module.f90 \
		    NESDIS_SEAICE_PHYEM_MODULE.f90 \
		    Fundamental_Constants.f90 \
		    Spectral_Units_Conversion.f90 \
		    CRTM_surface_ir_emissivity.f90 \
		    CRTM_IRSSEM.f90 \
		    CRTM_Fastem1.f90 \
		    CRTM_SensorInfo.f90 \
		    CRTM_MW_Land_SfcOptics.f90 \
		    CRTM_MW_Water_SfcOptics.f90 \
		    CRTM_MW_Snow_SfcOptics.f90 \
		    CRTM_MW_Ice_SfcOptics.f90 \
		    CRTM_IR_Land_SfcOptics.f90 \
		    CRTM_IR_Water_SfcOptics.f90 \
		    CRTM_IR_Snow_SfcOptics.f90 \
		    CRTM_IR_Ice_SfcOptics.f90 \
		    CRTM_SfcOptics.f90 \
		    CRTM_Options_Define.f90 \
		    CRTM_Planck_Functions.f90 \
		    CRTM_Utility.f90 \
		    CRTM_Forward_Module.f90 \
		    TauProfile_Define.f90 \
		    TauProfile_netCDF_IO.f90 \
		    CRTMstats_Define.f90 \
		    CRTMstats_netCDF_IO.f90 \
		    SensorInfo_Define.f90
		   
		    
		    

OBJ_FILES = ${SRC_FILES:.f90=.o} $(EXE_FILE).o


# -------------------------------
# Include and library definitions
# -------------------------------

INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf



#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------

# ---------------------
# Make based on OS type
# ---------------------

all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")   make -f $(MAKE_FILE) test_program $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKE_FILE) test_program $(AIX_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKE_FILE) test_program $(IRIX64_FLAGS) ;; \
	  "Linux" )  make -f $(MAKE_FILE) test_program $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac



# ----------------
# Make the program
# ----------------

test_program: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(EXTRA_FL_FLAGS) $(FL_FLAGS) $(EXE_FILE)



# --------
# Clean up
# --------

clean:
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.stb *.bin

realclean: remove_links clean


# Source and (big-endian) data file link creation and removal
SENSORID_LIST = amsua_n17 \
	        hirs3_n17 \
		ssmis_f16 \
		airs281SUBSET_aqua

create_links: 
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)
	@linkcoeffdata -b $(SENSORID_LIST)
	@linktestdata -b

remove_links:
	@unlinkfiles $(SRC_FILES)
	@unlinkcoeffdata
	@unlinktestdata

# ---------------
# Dependency list
# ---------------

include make.dependencies

# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = $(EXE_FILE)_software.tar

tar: create_links
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	@$(COPY) makefile.dist makefile
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	@$(REMOVE) makefile
	@$(COPY) $(REPOSITORY_DIR)/make.macros .
	@$(COPY) $(REPOSITORY_DIR)/make.rules .
	$(TARBALL_APPEND) $(TAR_FILE) make.macros make.rules
	@$(REMOVE) make.macros make.rules
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  $(TARBALL_APPEND) $(TAR_FILE) $$file_tag.*.bin.*Endian ; \
	done
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(CRTM_SOURCE_ROOT)/make.rules

