#==============================================================================
#
# Makefile for CRTM Forward model test program
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

include make.macros


# -------------
# This makefile
# -------------

MAKE_FILE = makefile


# ---------------
# Executable file
# ---------------

EXE_FILE = Test_Forward


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o \
                    Compare_Float_Numbers.o \
                    Endian_Utility.o \
                    Binary_File_Utility.o
SPCCOEFF_OBJ_FILES = SpcCoeff_Define.o \
                     SpcCoeff_Binary_IO.o
TAUCOEFF_OBJ_FILES = TauCoeff_Define.o \
                     TauCoeff_Binary_IO.o
AEROSOLCOEFF_OBJ_FILES = AerosolCoeff_Define.o \
                         AerosolCoeff_Binary_IO.o
SCATTERCOEFF_OBJ_FILES = ScatterCoeff_Define.o \
                         ScatterCoeff_Binary_IO.o
CRTM_COEFF_OBJ_FILES = CRTM_SpcCoeff.o \
                       CRTM_TauCoeff.o \
                       CRTM_AerosolCoeff.o \
                       CRTM_ScatterCoeff.o
CRTM_ATMOSPHERE_OBJ_FILES = CRTM_Atmosphere_Define.o \
                            CRTM_Cloud_Define.o \
                            CRTM_Aerosol_Define.o \
                            CRTM_Atmosphere_Binary_IO.o \
                            CRTM_Cloud_Binary_IO.o \
                            CRTM_Aerosol_Binary_IO.o
CRTM_SURFACE_OBJ_FILES = CRTM_Surface_Define.o \
                         CRTM_SensorData_Define.o \
                         CRTM_Surface_Binary_IO.o
CRTM_CHANNELINFO_OBJ_FILES = CRTM_ChannelInfo_Define.o \
                             CRTM_ChannelInfo.o
CRTM_GEOMETRYINFO_OBJ_FILES = CRTM_GeometryInfo_Define.o

CRTM_ATMABSORPTION_OBJ_FILES = CRTM_AtmAbsorption_IntAbsorber.o \
                               CRTM_AtmAbsorption_Predictor.o \
                               CRTM_AtmAbsorption_Define.o \
                               CRTM_AtmAbsorption.o
CRTM_ATMSCATTER_OBJ_FILES = CRTM_AtmScatter_Define.o \
                            CRTM_AerosolScatter.o \
                            CRTM_CloudScatter.o
CRTM_SFCOPTICS_OBJ_FILES = CRTM_SfcOptics_Define.o \
                           CRTM_SfcOptics.o
CRTM_RTSOLUTION_OBJ_FILES = CRTM_RTSolution_Define.o \
                            CRTM_Planck_Functions.o \
                            CRTM_RTSolution.o

CRTM_OBJ_FILES = CRTM_Parameters.o \
                 CRTM_LifeCycle.o \
                 CRTM_Forward_Module.o \
                 CRTM_Tangent_Linear_Module.o \
                 CRTM_Adjoint_Module.o \
                 CRTM_K_Matrix_Module.o \
                 CRTM_Module.o

SOI_OBJ_FILES = SOI_delta_scaling.o \
                SOI_doubling.o \
                SOI_emissivity.o \
                SOI_hg_phase_function.o \
                SOI_thermal_source.o \
                SOI_rt_model.o 
                

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(SPCCOEFF_OBJ_FILES) \
            $(TAUCOEFF_OBJ_FILES) \
            $(AEROSOLCOEFF_OBJ_FILES) \
            $(SCATTERCOEFF_OBJ_FILES) \
            $(CRTM_COEFF_OBJ_FILES) \
            $(CRTM_ATMOSPHERE_OBJ_FILES) \
            $(CRTM_SURFACE_OBJ_FILES) \
            $(CRTM_CHANNELINFO_OBJ_FILES) \
            $(CRTM_GEOMETRYINFO_OBJ_FILES) \
            $(CRTM_ATMABSORPTION_OBJ_FILES) \
            $(CRTM_ATMSCATTER_OBJ_FILES) \
            $(CRTM_SFCOPTICS_OBJ_FILES) \
            $(CRTM_RTSOLUTION_OBJ_FILES) \
            $(CRTM_OBJ_FILES) \
            $(SOI_OBJ_FILES) \
            $(EXE_FILE).o



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
	  "Linux" )  make -f $(MAKE_FILE) test_program $(Linux_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac


# -- Targets for specific Linux compilers. For some there are "regular"
# -- and "optimised" targets. Note that the PGI compiler cannot compile
# -- the code due to compiler bugs (as of v5.2-4).
intel:
	make -f $(MAKE_FILE) test_program $(Linux_FLAGS_Intel_Regular)

intel_opt:
	make -f $(MAKE_FILE) test_program $(Linux_FLAGS_Intel_Optimised)

lahey:
	make -f $(MAKE_FILE) test_program $(Linux_FLAGS_Lahey_Regular)

lahey_opt:
	make -f $(MAKE_FILE) test_program $(Linux_FLAGS_Lahey_Optimised)

pgi:
	make -f $(MAKE_FILE) test_program $(Linux_FLAGS_PGI_Regular)

pgi_opt:
	make -f $(MAKE_FILE) test_program $(Linux_FLAGS_PGI_Optimised)

g95:
	make -f $(MAKE_FILE) test_program $(Linux_FLAGS_g95)



# ----------------
# Make the program
# ----------------

test_program: $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(EXTRA_FL_FLAGS) $(FL_FLAGS) $(EXE_FILE)



# --------
# Clean up
# --------

clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb


# ---------------
# Dependency list
# ---------------

Type_Kinds.o : Type_Kinds.f90 
File_Utility.o : File_Utility.f90 
Message_Handler.o : Message_Handler.f90 File_Utility.o 
Binary_File_Utility.o : Binary_File_Utility.f90 Type_Kinds.o File_Utility.o Message_Handler.o Endian_Utility.o 
Endian_Utility.o : Endian_Utility.f90 Type_Kinds.o 
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o 

SpcCoeff_Define.o : SpcCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 
SpcCoeff_Binary_IO.o : SpcCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o SpcCoeff_Define.o 
TauCoeff_Define.o : TauCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 
TauCoeff_Binary_IO.o : TauCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o TauCoeff_Define.o 
ScatterCoeff_Define.o : ScatterCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 
ScatterCoeff_Binary_IO.o : ScatterCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o ScatterCoeff_Define.o 
AerosolCoeff_Define.o : AerosolCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 
AerosolCoeff_Binary_IO.o : AerosolCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o AerosolCoeff_Define.o 

CRTM_Parameters.o : CRTM_Parameters.f90 Type_Kinds.o 

CRTM_ChannelInfo_Define.o : CRTM_ChannelInfo_Define.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o 
CRTM_ChannelInfo.o : CRTM_ChannelInfo.f90 Type_Kinds.o Message_Handler.o CRTM_ChannelInfo_Define.o 
CRTM_SpcCoeff.o : CRTM_SpcCoeff.f90 Type_Kinds.o Message_Handler.o SpcCoeff_Define.o SpcCoeff_Binary_IO.o CRTM_Parameters.o 
CRTM_TauCoeff.o : CRTM_TauCoeff.f90 Type_Kinds.o Message_Handler.o TauCoeff_Define.o TauCoeff_Binary_IO.o CRTM_Parameters.o 
CRTM_ScatterCoeff.o : CRTM_ScatterCoeff.f90 Type_Kinds.o Message_Handler.o ScatterCoeff_Define.o ScatterCoeff_Binary_IO.o CRTM_Parameters.o 
CRTM_AerosolCoeff.o : CRTM_AerosolCoeff.f90 Type_Kinds.o Message_Handler.o AerosolCoeff_Define.o AerosolCoeff_Binary_IO.o CRTM_Parameters.o 
CRTM_LifeCycle.o : CRTM_LifeCycle.f90 Message_Handler.o CRTM_SpcCoeff.o CRTM_TauCoeff.o CRTM_AerosolCoeff.o CRTM_ScatterCoeff.o CRTM_ChannelInfo.o 

CRTM_Cloud_Define.o : CRTM_Cloud_Define.f90 Type_Kinds.o Message_Handler.o 
CRTM_Aerosol_Define.o : CRTM_Aerosol_Define.f90 Type_Kinds.o Message_Handler.o 
CRTM_Atmosphere_Define.o : CRTM_Atmosphere_Define.f90 Type_Kinds.o Message_Handler.o CRTM_Cloud_Define.o CRTM_Aerosol_Define.o 
CRTM_Cloud_Binary_IO.o : CRTM_Cloud_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o CRTM_Cloud_Define.o 
CRTM_Aerosol_Binary_IO.o : CRTM_Aerosol_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o CRTM_Aerosol_Define.o 
CRTM_Atmosphere_Binary_IO.o : CRTM_Atmosphere_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o CRTM_Atmosphere_Define.o CRTM_Cloud_Binary_IO.o CRTM_Aerosol_Binary_IO.o 

CRTM_SensorData_Define.o : CRTM_SensorData_Define.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o 
CRTM_Surface_Define.o : CRTM_Surface_Define.f90 Type_Kinds.o Message_Handler.o CRTM_SensorData_Define.o 
CRTM_Surface_Binary_IO.o : CRTM_Surface_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o CRTM_Surface_Define.o 

CRTM_GeometryInfo_Define.o : CRTM_GeometryInfo_Define.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o 

SOI_delta_scaling.o : SOI_delta_scaling.f90 Type_Kinds.o
SOI_doubling.o : SOI_doubling.f90 Type_Kinds.o
SOI_emissivity.o : SOI_emissivity.f90 Type_Kinds.o
SOI_hg_phase_function.o : SOI_hg_phase_function.f90 Type_Kinds.o
SOI_thermal_source.o : SOI_thermal_source.f90 Type_Kinds.o
SOI_rt_model.o : SOI_rt_model.f90 Type_Kinds.o SOI_delta_scaling.o SOI_doubling.o SOI_emissivity.o SOI_hg_phase_function.o SOI_thermal_source.o

CRTM_RTSolution_Define.o : CRTM_RTSolution_Define.f90 Type_Kinds.o Message_Handler.o 
CRTM_Planck_Functions.o : CRTM_Planck_Functions.f90 CRTM_Parameters.o CRTM_SpcCoeff.o Type_Kinds.o
CRTM_RTSolution.o : CRTM_RTSolution.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_AtmAbsorption_Define.o CRTM_AtmScatter_Define.o CRTM_SfcOptics.o CRTM_RTSolution_Define.o  CRTM_Planck_Functions.o SOI_rt_model.o

CRTM_AtmAbsorption_Predictor.o : CRTM_AtmAbsorption_Predictor.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o CRTM_AtmAbsorption_Define.o 
CRTM_AtmAbsorption_IntAbsorber.o : CRTM_AtmAbsorption_IntAbsorber.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o 
CRTM_AtmAbsorption_Define.o : CRTM_AtmAbsorption_Define.f90 Type_Kinds.o Message_Handler.o 
CRTM_AtmAbsorption.o : CRTM_AtmAbsorption.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_TauCoeff.o CRTM_Atmosphere_Define.o CRTM_GeometryInfo_Define.o CRTM_AtmAbsorption_Define.o CRTM_AtmAbsorption_IntAbsorber.o CRTM_AtmAbsorption_Predictor.o 

CRTM_AtmScatter_Define.o : CRTM_AtmScatter_Define.f90 Type_Kinds.o Message_Handler.o 
CRTM_CloudScatter.o : CRTM_CloudScatter.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_ScatterCoeff.o CRTM_Atmosphere_Define.o CRTM_GeometryInfo_Define.o CRTM_AtmScatter_Define.o 
CRTM_AerosolScatter.o : CRTM_AerosolScatter.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_AerosolCoeff.o CRTM_Atmosphere_Define.o CRTM_GeometryInfo_Define.o CRTM_AtmScatter_Define.o 

CRTM_SfcOptics_Define.o : CRTM_SfcOptics_Define.f90 Type_Kinds.o Message_Handler.o 
CRTM_SfcOptics.o : CRTM_SfcOptics.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_SfcOptics_Define.o 

CRTM_Forward_Module.o : CRTM_Forward_Module.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_ChannelInfo_Define.o CRTM_AtmAbsorption.o CRTM_AerosolScatter.o CRTM_CloudScatter.o CRTM_SfcOptics.o CRTM_RTSolution.o 
CRTM_Tangent_Linear_Module.o : CRTM_Tangent_Linear_Module.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_ChannelInfo_Define.o CRTM_AtmAbsorption.o CRTM_AerosolScatter.o CRTM_CloudScatter.o CRTM_SfcOptics.o CRTM_RTSolution.o 
CRTM_Adjoint_Module.o : CRTM_Adjoint_Module.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_ChannelInfo_Define.o CRTM_AtmAbsorption.o CRTM_AerosolScatter.o CRTM_CloudScatter.o CRTM_SfcOptics.o CRTM_RTSolution.o 
CRTM_K_Matrix_Module.o : CRTM_K_Matrix_Module.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_ChannelInfo_Define.o CRTM_AtmAbsorption.o CRTM_AerosolScatter.o CRTM_CloudScatter.o CRTM_SfcOptics.o CRTM_RTSolution.o 

CRTM_Module.o : CRTM_Module.f90 CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_ChannelInfo_Define.o CRTM_RTSolution_Define.o CRTM_LifeCycle.o CRTM_Forward_Module.o CRTM_Tangent_Linear_Module.o CRTM_Adjoint_Module.o CRTM_K_Matrix_Module.o 

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Message_Handler.o CRTM_Module.o CRTM_Atmosphere_Binary_IO.o CRTM_Surface_Binary_IO.o



# -------------------------------
# Create tarball for distribution
# -------------------------------

ATMOSPHERE_DATA_FILE = ECMWF.Atmosphere.bin.Big_Endian
SURFACE_DATA_FILE = Surface.bin.Big_Endian
DATA_FILE_TAGS = amsua_n17 \
                 hirs3_n17 \
                 ssmis_f16

TAR_FILE = $(EXE_FILE)_software.tar

tar:
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	$(TARBALL_APPEND) $(TAR_FILE) make.macros make.rules
	$(TARBALL_APPEND) $(TAR_FILE) $(ATMOSPHERE_DATA_FILE)
	$(TARBALL_APPEND) $(TAR_FILE) $(SURFACE_DATA_FILE)
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  $(TARBALL_APPEND) $(TAR_FILE) $$file_tag.*.bin* ; \
	done
	@$(TARBALL_APPEND) $(TAR_FILE) *.ScatterCoeff.bin*
	@$(TARBALL_APPEND) $(TAR_FILE) *.AerosolCoeff.bin*
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include make.rules

