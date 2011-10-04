#==============================================================================
#
# Makefile for SSMIS TauProd program
#
#==============================================================================
ROOT=$(CRTM_SOURCE_ROOT)
RT_FILES_DIR=$(CRTM_SOURCE_ROOT)/Zeeman/src_lbl
RT_F77_FILES_DIR=$(RT_FILES_DIR)
  
# -------------
# Define macros
# -------------
include $(ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = compute_channel_trans 

# Source files 
SSMIS_FILES = \
     radiance_ssmis.f90 \
     ssmis_passband.f90 \
     compute_channel_trans.f90

RT_FILES = \
     O2_zeeman_lbl.f90 \
     MW_SensorFrequencies.f90 \
     transmittance_microwave_lbl.f90 \
     Model_Profile_Set.f90 \
     Read_AtmProfile.f90 \
     Tran_matrix_linearBase.f90 \
     interpolate.f90 \
     profile_conversion.f90


RT_F77_FILES = \
     ZEEMAN.f \
     TBMX.f \
     o2abs.f \
     ABSN2.f \
     ABH2O.f \
     CERROR.f \
     TRANX2.f \
     o2n2_l93_xxx.f

CRTM_FILES = TmpAtmProfile_Define.f90 \
             TmpAtmProfile_netCDF_IO.f90 \
             Type_Kinds.f90 \
             File_Utility.f90 \
             Message_Handler.f90 \
             Compare_Float_Numbers.f90 \
             netCDF_Utility.f90 \
             netCDF_Dimension_Utility.f90 \
             netCDF_Variable_Utility.f90 \
             netCDF_Attribute_Utility.f90 \
             Units_Conversion.f90 \
             Profile_Utility_Parameters.f90 \
             Atmospheric_Properties.f90 \
             SRF_Define.f90 \
             SRF_netCDF_IO.f90 \
             Integrate_Utility.f90 \
             Interpolate_Utility.f90 \
             Linear_Interpolation.f90 \
             Polynomial_Interpolation.f90 \
             Spline_Interpolation.f90 \
             Search_Utility.f90 \
             Fundamental_Constants.f90 \
             String_Utility.f90
             
  
# Obj files used in link phase
OBJ_FILES = ${SSMIS_FILES:.f90=.o} \
            ${RT_FILES:.f90=.o} \
            ${RT_F77_FILES:.f=.o} \
            ${CRTM_FILES:.f90=.o}
               

# Include and library definitions
#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -lessl -L$(HOME)/local/netcdf/lib -lnetcdf 


# ------------------
# Define dependecies
# ------------------
include $(ROOT)/make.common_targets

# Create and remove source links
create_links:
	@linkfiles $(ROOT) $(CRTM_FILES)
	@linkfiles $(RT_FILES_DIR) $(RT_FILES)
	@linkfiles $(RT_F77_FILES_DIR) $(RT_F77_FILES)
                     
remove_links:
	@unlinkfiles $(CRTM_FILES)
	@unlinkfiles $(RT_FILES)
	@unlinkfiles $(RT_F77_FILES)

# Squeaky clean target
realclean:: clean remove_links
  
# Source dependencies
include make.dependencies

# --------------------
# Define default rules
# --------------------
include $(ROOT)/make.rules
