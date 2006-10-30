#==============================================================================
#
# Makefile for CRTM Forward model test program
#
# $Id: Makefile,v 1.2 2006/05/02 14:58:35 dgroff Exp $
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

MAKE_FILE = Makefile


# ---------------
# Executable file
# ---------------

EXE_FILE = CRTM_Consistency_Check


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o \
                    Compare_Float_Numbers.o \
                    Endian_Utility.o \
                    Binary_File_Utility.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Attribute_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Variable_Utility.o
SPCCOEFF_OBJ_FILES = SpcCoeff_Define.o \
                     SpcCoeff_Binary_IO.o
TAUCOEFF_OBJ_FILES = TauCoeff_Define.o \
                     TauCoeff_Binary_IO.o
AEROSOLCOEFF_OBJ_FILES = AerosolCoeff_Define.o \
                         AerosolCoeff_Binary_IO.o
SCATTERCOEFF_OBJ_FILES = CloudCoeff_Define.o \
                         CloudCoeff_Binary_IO.o
CRTM_UTILITY_OBJ_FILES = CRTM_Utility.o \
                         CRTM_Interpolation.o
CRTM_COEFF_OBJ_FILES = CRTM_SpcCoeff.o \
                       CRTM_TauCoeff.o \
                       CRTM_AerosolCoeff.o \
                       CRTM_CloudCoeff.o \
                        CRTM_EmisCoeff.o
CRTM_ATMOSPHERE_OBJ_FILES = CRTM_Atmosphere_Define.o \
                            CRTM_Cloud_Define.o \
                            CRTM_Aerosol_Define.o \
                            CRTM_Atmosphere_Binary_IO.o \
                            CRTM_Cloud_Binary_IO.o \
                            CRTM_Aerosol_Binary_IO.o
CRTM_SURFACE_OBJ_FILES = CRTM_Surface_Define.o \
                         CRTM_SensorData_Define.o \
                           CRTM_SensorInfo.o \
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
                           CRTM_SfcOptics.o \
                           CRTM_IR_Ice_SfcOptics.o \
                           CRTM_IR_Land_SfcOptics.o \
                           CRTM_IR_Snow_SfcOptics.o \
                           CRTM_IR_Water_SfcOptics.o \
                           CRTM_MW_Ice_SfcOptics.o \
                           CRTM_MW_Land_SfcOptics.o \
                           CRTM_MW_Snow_SfcOptics.o \
                           CRTM_MW_Water_SfcOptics.o
CRTM_IRSSEM_OBJ_FILES = CRTM_IRSSEM.o 
IRSSEM_EMISCOEFF_OBJ_FILES = EmisCoeff_Define.o \
                           EmisCoeff_Binary_IO.o
IRSSEM_EMISSIVITY_OBJ_FILES = Emissivity_Define.o 
IRLSEM_EMISSIVITY_OBJ_FILES = CRTM_surface_ir_emissivity.o 
NESDIS_Emissivity_OBJ_FILES = NESDIS_AMSRE_SICEEM_Module.o \
                           NESDIS_AMSRE_SNOWEM_Module.o \
                           NESDIS_AMSU_SICEEM_Module.o \
                           NESDIS_AMSU_SnowEM_Module.o \
                           NESDIS_LandEM_Module.o \
                           NESDIS_OCEANEM_Module.o \
                           NESDIS_SEAICE_PHYEM_MODULE.o \
                           NESDIS_SnowEM_Parameters.o \
                           NESDIS_SSMI_Module.o \
                           NESDIS_SSMI_SIceEM_Module.o \
                           NESDIS_SSMI_SnowEM_Module.o 
CRTM_AtmOptics_OBJ_FILES = CRTM_AtmOptics.o
CRTM_RTSOLUTION_OBJ_FILES = CRTM_RTSolution_Define.o \
                            CRTM_RTSolution.o \
                            CRTM_Compute_Phase.o \
                            CRTM_Planck_Functions.o
CRTM_OBJ_FILES = CRTM_Parameters.o \
                 CRTM_LifeCycle.o \
                 CRTM_Forward_Module.o \
                 CRTM_Tangent_Linear_Module.o \
                 CRTM_Adjoint_Module.o \
                 CRTM_K_Matrix_Module.o \
                 CRTM_Module.o
FWDTLMTEST_OBJ_FILES = FWDTLMtest_Define.o \
                       FWDTLMtest_netCDF_IO.o
TLADMTEST_OBJ_FILES = TLADMtest_Define.o \
                       TLADMtest_netCDF_IO.o
ADKMTEST_OBJ_FILES = ADKMtest_Define.o \
                       ADKMtest_netCDF_IO.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(NETCDF_OBJ_FILES) \
            $(SPCCOEFF_OBJ_FILES) \
            $(TAUCOEFF_OBJ_FILES) \
            $(AEROSOLCOEFF_OBJ_FILES) \
            $(SCATTERCOEFF_OBJ_FILES) \
            $(CRTM_UTILITY_OBJ_FILES) \
            $(CRTM_COEFF_OBJ_FILES) \
            $(CRTM_ATMOSPHERE_OBJ_FILES) \
            $(CRTM_SURFACE_OBJ_FILES) \
            $(CRTM_CHANNELINFO_OBJ_FILES) \
            $(CRTM_GEOMETRYINFO_OBJ_FILES) \
            $(CRTM_ATMABSORPTION_OBJ_FILES) \
            $(CRTM_ATMSCATTER_OBJ_FILES) \
            $(CRTM_SFCOPTICS_OBJ_FILES)  \
            $(CRTM_IRSSEM_OBJ_FILES) \
            $(IRSSEM_EMISCOEFF_OBJ_FILES)  \
            $(IRSSEM_EMISSIVITY_OBJ_FILES) \
            $(IRLSEM_EMISSIVITY_OBJ_FILES) \
            $(NESDIS_Emissivity_OBJ_FILES) \
            $(CRTM_AtmOptics_OBJ_FILES) \
            $(CRTM_RTSOLUTION_OBJ_FILES) \
            $(CRTM_OBJ_FILES) \
            $(FWDTLMTEST_OBJ_FILES) \
            $(TLADMTEST_OBJ_FILES) \
            $(ADKMTEST_OBJ_FILES) \
            $(EXE_FILE).o


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

test_program: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(EXTRA_FL_FLAGS) $(FL_FLAGS) $(EXE_FILE)



# --------
# Clean up
# --------

clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb

realclean: remove_links clean


# --------------------------------
# Create the links for compilation
# --------------------------------

create_links: create_source_links create_data_links

# -- The source code links
UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

NETCDF_SOURCE_DIR = $(F90_DIR)/NETCDF
NETCDF_SOURCE_FILES = ${NETCDF_OBJ_FILES:.o=.f90}

SPCCOEFF_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SpcCoeff
SPCCOEFF_SOURCE_FILES = ${SPCCOEFF_OBJ_FILES:.o=.f90}

TAUCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/TauCoeff
TAUCOEFF_SOURCE_FILES = ${TAUCOEFF_OBJ_FILES:.o=.f90}

AEROSOLCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/AerosolCoeff
AEROSOLCOEFF_SOURCE_FILES = ${AEROSOLCOEFF_OBJ_FILES:.o=.f90}

SCATTERCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/ScatterCoeff
SCATTERCOEFF_SOURCE_FILES = ${SCATTERCOEFF_OBJ_FILES:.o=.f90}

CRTM_UTILITY_SOURCE_DIR = $(F90_DIR)/CRTM/Utility
CRTM_UTILITY_SOURCE_FILES = ${CRTM_UTILITY_OBJ_FILES:.o=.f90}

CRTM_COEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients
CRTM_COEFF_SOURCE_FILES = ${CRTM_COEFF_OBJ_FILES:.o=.f90}

CRTM_ATMOSPHERE_SOURCE_DIR = $(F90_DIR)/CRTM/Atmosphere
CRTM_ATMOSPHERE_SOURCE_FILES = ${CRTM_ATMOSPHERE_OBJ_FILES:.o=.f90}

CRTM_SURFACE_SOURCE_DIR = $(F90_DIR)/CRTM/Surface
CRTM_SURFACE_SOURCE_FILES = ${CRTM_SURFACE_OBJ_FILES:.o=.f90}

CRTM_CHANNELINFO_SOURCE_DIR = $(F90_DIR)/CRTM/ChannelInfo
CRTM_CHANNELINFO_SOURCE_FILES = ${CRTM_CHANNELINFO_OBJ_FILES:.o=.f90}

CRTM_GEOMETRYINFO_SOURCE_DIR = $(F90_DIR)/CRTM/GeometryInfo
CRTM_GEOMETRYINFO_SOURCE_FILES = ${CRTM_GEOMETRYINFO_OBJ_FILES:.o=.f90}

CRTM_ATMABSORPTION_SOURCE_DIR = $(F90_DIR)/CRTM/AtmAbsorption
CRTM_ATMABSORPTION_SOURCE_FILES = ${CRTM_ATMABSORPTION_OBJ_FILES:.o=.f90}

CRTM_ATMSCATTER_SOURCE_DIR = $(F90_DIR)/CRTM/AtmScatter
CRTM_ATMSCATTER_SOURCE_FILES = ${CRTM_ATMSCATTER_OBJ_FILES:.o=.f90}

CRTM_SFCOPTICS_SOURCE_DIR = $(F90_DIR)/CRTM/SfcOptics
CRTM_SFCOPTICS_SOURCE_FILES = ${CRTM_SFCOPTICS_OBJ_FILES:.o=.f90}

CRTM_IRSSEM_SOURCE_DIR = $(F90_DIR)/CRTM/SfcOptics/IRSSEM/Spectral
CRTM_IRSSEM_SOURCE_FILES = ${CRTM_IRSSEM_OBJ_FILES:.o=.f90}

IRSSEM_EMISCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/EmisCoeff/Spectral
IRSSEM_EMISCOEFF_SOURCE_FILES = ${IRSSEM_EMISCOEFF_OBJ_FILES:.o=.f90}

IRSSEM_EMISSIVITY_SOURCE_DIR = $(F90_DIR)/Emissivity
IRSSEM_EMISSIVITY_SOURCE_FILES = ${IRSSEM_EMISSIVITY_OBJ_FILES:.o=.f90}

IRLSEM_EMISSIVITY_SOURCE_DIR = $(F90_DIR)/CRTM/SfcOptics/IRLSEM
IRLSEM_EMISSIVITY_SOURCE_FILES = ${IRLSEM_EMISSIVITY_OBJ_FILES:.o=.f90}

NESDIS_Emissivity_SOURCE_DIR = $(F90_DIR)/CRTM/SfcOptics/NESDIS_Emissivity
NESDIS_Emissivity_SOURCE_FILES = ${NESDIS_Emissivity_OBJ_FILES:.o=.f90}

CRTM_AtmOptics_SOURCE_DIR = $(F90_DIR)/CRTM/AtmOptics
CRTM_AtmOptics_SOURCE_FILES =${CRTM_AtmOptics_OBJ_FILES:.o=.f90}

CRTM_RTSOLUTION_SOURCE_DIR = $(F90_DIR)/CRTM/RTSolution
CRTM_RTSOLUTION_SOURCE_FILES = ${CRTM_RTSOLUTION_OBJ_FILES:.o=.f90}

CRTM_SOURCE_DIR = $(F90_DIR)/CRTM
CRTM_SOURCE_FILES = ${CRTM_OBJ_FILES:.o=.f90}

FWDTLMTEST_SOURCE_DIR = $(F90_DIR)/CRTM/Test_Code/Utility/FWDTLMtest
FWDTLMTEST_SOURCE_FILES = ${FWDTLMTEST_OBJ_FILES:.o=.f90}

TLADMTEST_SOURCE_DIR = $(F90_DIR)/CRTM/Test_Code/Utility/TLADMtest
TLADMTEST_SOURCE_FILES = ${TLADMTEST_OBJ_FILES:.o=.f90}

ADKMTEST_SOURCE_DIR = $(F90_DIR)/CRTM/Test_Code/Utility/ADKMtest
ADKMTEST_SOURCE_FILES = ${ADKMTEST_OBJ_FILES:.o=.f90}

create_source_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${NETCDF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${NETCDF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SPCCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SPCCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${TAUCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${TAUCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${AEROSOLCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${AEROSOLCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SCATTERCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SCATTERCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_COEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_COEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_ATMOSPHERE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_ATMOSPHERE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_SURFACE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_SURFACE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_CHANNELINFO_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_CHANNELINFO_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_GEOMETRYINFO_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_GEOMETRYINFO_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_ATMABSORPTION_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_ATMABSORPTION_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_ATMSCATTER_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_ATMSCATTER_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_SFCOPTICS_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_SFCOPTICS_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${IRSSEM_EMISCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${IRSSEM_EMISCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_IRSSEM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_IRSSEM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${IRSSEM_EMISSIVITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${IRSSEM_EMISSIVITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${IRLSEM_EMISSIVITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${IRLSEM_EMISSIVITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${NESDIS_Emissivity_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${NESDIS_Emissivity_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_AtmOptics_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_AtmOptics_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_RTSOLUTION_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_RTSOLUTION_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${FWDTLMTEST_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${FWDTLMTEST_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${TLADMTEST_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${TLADMTEST_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${ADKMTEST_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${ADKMTEST_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

# -- The data file links
ATMOSPHERE_DATA_DIR = ../Test_DataFiles
ATMOSPHERE_DATA_FILE = ECMWF.Atmosphere.bin

SURFACE_DATA_DIR = ../Test_DataFiles
SURFACE_DATA_FILE = Surface.bin

SPCCOEFF_DATA_DIR = $(HOME)/CRTM/SpcCoeff/Current
TAUCOEFF_DATA_DIR = $(HOME)/CRTM/TauCoeff
SCATTERCOEFF_DATA_DIR = $(HOME)/f90/CRTM/Coefficients/ScatterCoeff/ScatterCoeff_DummyWrite
AEROSOLCOEFF_DATA_DIR = $(HOME)/f90/CRTM/Coefficients/AerosolCoeff/AerosolCoeff_DummyWrite

DATA_FILE_TAGS = amsua_n17 \
                 hirs3_n17 \
                 ssmis_f16

DATA_ENDIAN_TAGS = Big_Endian

create_data_links:
	@if [ ! -f ${ATMOSPHERE_DATA_FILE} ]; then \
	      $(LINK) ${ATMOSPHERE_DATA_DIR}/${ATMOSPHERE_DATA_FILE} ${ATMOSPHERE_DATA_FILE}.${DATA_ENDIAN_TAGS} ; \
	fi
	@if [ ! -f ${SURFACE_DATA_FILE} ]; then \
	      $(LINK) ${SURFACE_DATA_DIR}/${SURFACE_DATA_FILE} ${SURFACE_DATA_FILE}.${DATA_ENDIAN_TAGS} ; \
	fi
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  spccoeff_file=$$file_tag.SpcCoeff.bin ; \
	  for endian_tag in ${DATA_ENDIAN_TAGS}; do \
	    link_file=$$spccoeff_file.$$endian_tag ; \
	    if [ ! -f $$link_file ]; then \
	      $(LINK) ${SPCCOEFF_DATA_DIR}/$$endian_tag/$$spccoeff_file $$link_file ; \
	    fi ; \
	  done; \
	done
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  taucoeff_file=$$file_tag.TauCoeff.bin ; \
	  for endian_tag in ${DATA_ENDIAN_TAGS}; do \
	    link_file=$$taucoeff_file.$$endian_tag ; \
	    if [ ! -f $$link_file ]; then \
	      $(LINK) ${TAUCOEFF_DATA_DIR}/$$endian_tag/Current/$$taucoeff_file $$link_file ; \
	    fi ; \
	  done; \
	done
	@for endian_tag in ${DATA_ENDIAN_TAGS}; do \
	  scattercoeff_file=dummy.ScatterCoeff.bin.$$endian_tag ; \
	  link_file=$$scattercoeff_file ; \
	  if [ ! -f $$link_file ]; then \
	    $(LINK) ${SCATTERCOEFF_DATA_DIR}/$$scattercoeff_file $$link_file ; \
	  fi ; \
	done
	@for endian_tag in ${DATA_ENDIAN_TAGS}; do \
	  aerosolcoeff_file=dummy.AerosolCoeff.bin.$$endian_tag ; \
	  link_file=$$aerosolcoeff_file ; \
	  if [ ! -f $$link_file ]; then \
	    $(LINK) ${AEROSOLCOEFF_DATA_DIR}/$$aerosolcoeff_file $$link_file ; \
	  fi ; \
	done

# ----------------
# Remove the links
# ----------------

remove_links: remove_source_links remove_data_links

# -- The source code links
ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES) \
                   $(SPCCOEFF_SOURCE_FILES) \
                   $(TAUCOEFF_SOURCE_FILES) \
                   $(AEROSOLCOEFF_SOURCE_FILES) \
                   $(SCATTERCOEFF_SOURCE_FILES) \
                   $(CRTM_UTILITY_SOURCE_FILES) \
                   $(CRTM_COEFF_SOURCE_FILES) \
                   $(CRTM_ATMOSPHERE_SOURCE_FILES) \
                   $(CRTM_SURFACE_SOURCE_FILES) \
                   $(CRTM_CHANNELINFO_SOURCE_FILES) \
                   $(CRTM_GEOMETRYINFO_SOURCE_FILES) \
                   $(CRTM_ATMABSORPTION_SOURCE_FILES) \
                   $(CRTM_ATMSCATTER_SOURCE_FILES) \
                   $(CRTM_SFCOPTICS_SOURCE_FILES) \
                   $(CRTM_IRSSEM_SOURCE_FILES)  \
                   $(IRSSEM_EMISCOEFF_SOURCE_FILES)  \
                   $(IRSSEM_EMISSIVITY_SOURCE_FILES) \
                   $(IRLSEM_EMISSIVITY_SOURCE_FILES) \
                   $(NESDIS_Emissivity_SOURCE_FILES) \
                   $(CRTM_AtmOptics_SOURCE_FILES) \
                   $(CRTM_RTSOLUTION_SOURCE_FILES) \
                   $(FWDTLMTEST_SOURCE_FILES) \
                   $(TLADMTEST_SOURCE_FILES) \
                   $(ADKMTEST_SOURCE_FILES) \
                   $(CRTM_SOURCE_FILES)

                   
remove_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done

remove_source_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done

# -- The data file links
remove_data_links:
	@$(REMOVE) ${ATMOSPHERE_DATA_FILE}.${DATA_ENDIAN_TAGS}
	@$(REMOVE) ${SURFACE_DATA_FILE}.${DATA_ENDIAN_TAGS}
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  $(REMOVE) $$file_tag.*.bin* ; \
	done
	@$(REMOVE) dummy.ScatterCoeff.bin*
	@$(REMOVE) dummy.AerosolCoeff.bin*


# ---------------
# Dependency list
# ---------------

Type_Kinds.o : Type_Kinds.f90 
File_Utility.o : File_Utility.f90 
Message_Handler.o : Message_Handler.f90 File_Utility.o 
Binary_File_Utility.o : Binary_File_Utility.f90 Type_Kinds.o File_Utility.o Message_Handler.o Endian_Utility.o 
Endian_Utility.o : Endian_Utility.f90 Type_Kinds.o 
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o 
CRTM_Utility.o : CRTM_Utility.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o
CRTM_Interpolation.o : CRTM_Interpolation.f90 Type_Kinds.o
CRTM_Planck_Functions.o : CRTM_Planck_Functions.f90 Type_Kinds.o CRTM_Parameters.o CRTM_SpcCoeff.o SpcCoeff_Define.o

netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 Message_Handler.o Type_Kinds.o 
netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 Message_Handler.o Type_Kinds.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 Message_Handler.o Type_Kinds.o 
netCDF_Utility.o : netCDF_Utility.f90 netCDF_Attribute_Utility.o netCDF_Variable_Utility.o netCDF_Dimension_Utility.o Message_Handler.o Type_Kinds.o 

SpcCoeff_Define.o : SpcCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 
CRTM_AtmOptics.o : CRTM_AtmOptics.f90 Type_Kinds.o CRTM_Parameters.o 
SpcCoeff_Binary_IO.o : SpcCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o SpcCoeff_Define.o 
TauCoeff_Define.o : TauCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 
TauCoeff_Binary_IO.o : TauCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o TauCoeff_Define.o 
CloudCoeff_Define.o : CloudCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 
CloudCoeff_Binary_IO.o : CloudCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o CloudCoeff_Define.o 
AerosolCoeff_Define.o : AerosolCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 
AerosolCoeff_Binary_IO.o : AerosolCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o Binary_File_Utility.o AerosolCoeff_Define.o 

CRTM_Parameters.o : CRTM_Parameters.f90 Type_Kinds.o 

CRTM_ChannelInfo_Define.o : CRTM_ChannelInfo_Define.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o 
CRTM_ChannelInfo.o : CRTM_ChannelInfo.f90 Type_Kinds.o Message_Handler.o CRTM_ChannelInfo_Define.o CRTM_SpcCoeff.o SpcCoeff_Define.o
CRTM_SpcCoeff.o : CRTM_SpcCoeff.f90 Type_Kinds.o Message_Handler.o SpcCoeff_Define.o SpcCoeff_Binary_IO.o CRTM_Parameters.o 
CRTM_TauCoeff.o : CRTM_TauCoeff.f90 Type_Kinds.o Message_Handler.o TauCoeff_Define.o TauCoeff_Binary_IO.o CRTM_Parameters.o 
CRTM_CloudCoeff.o : CRTM_CloudCoeff.f90 Type_Kinds.o Message_Handler.o CloudCoeff_Define.o CloudCoeff_Binary_IO.o CRTM_Parameters.o 
CRTM_AerosolCoeff.o : CRTM_AerosolCoeff.f90 Type_Kinds.o Message_Handler.o AerosolCoeff_Define.o AerosolCoeff_Binary_IO.o CRTM_Parameters.o 
CRTM_LifeCycle.o : CRTM_LifeCycle.f90 Message_Handler.o CRTM_SpcCoeff.o CRTM_TauCoeff.o CRTM_AerosolCoeff.o CRTM_CloudCoeff.o CRTM_ChannelInfo.o 

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

CRTM_Compute_Phase.o : CRTM_Compute_Phase.f90 Type_Kinds.o CRTM_Parameters.o CRTM_AtmScatter_Define.o CRTM_Utility.o

CRTM_RTSolution_Define.o : CRTM_RTSolution_Define.f90 Type_Kinds.o Message_Handler.o 
CRTM_RTSolution.o : CRTM_RTSolution.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_AtmAbsorption_Define.o CRTM_AtmScatter_Define.o CRTM_SfcOptics.o CRTM_RTSolution_Define.o CRTM_Planck_Functions.o CRTM_Utility.o CRTM_CloudScatter.o CRTM_Compute_Phase.o

CRTM_AtmAbsorption_Predictor.o : CRTM_AtmAbsorption_Predictor.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o CRTM_AtmAbsorption_Define.o 
CRTM_AtmAbsorption_IntAbsorber.o : CRTM_AtmAbsorption_IntAbsorber.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o 
CRTM_AtmAbsorption_Define.o : CRTM_AtmAbsorption_Define.f90 Type_Kinds.o Message_Handler.o 
CRTM_AtmAbsorption.o : CRTM_AtmAbsorption.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_TauCoeff.o CRTM_Atmosphere_Define.o CRTM_GeometryInfo_Define.o CRTM_AtmAbsorption_Define.o CRTM_AtmAbsorption_IntAbsorber.o CRTM_AtmAbsorption_Predictor.o 

CRTM_AtmScatter_Define.o : CRTM_AtmScatter_Define.f90 Type_Kinds.o Message_Handler.o 
CRTM_CloudScatter.o : CRTM_CloudScatter.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_CloudCoeff.o CRTM_Atmosphere_Define.o CRTM_GeometryInfo_Define.o CRTM_AtmScatter_Define.o
CRTM_AerosolScatter.o : CRTM_AerosolScatter.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_AerosolCoeff.o CRTM_Atmosphere_Define.o CRTM_GeometryInfo_Define.o CRTM_AtmScatter_Define.o 

CRTM_SensorInfo.o : CRTM_SensorInfo.f90
CRTM_surface_ir_emissivity.o : CRTM_surface_ir_emissivity.f90 Type_Kinds.o
CRTM_IRSSEM.o : CRTM_IRSSEM.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o CRTM_EmisCoeff.o
NESDIS_AMSU_SICEEM_Module.o : NESDIS_AMSU_SICEEM_Module.f90 Type_Kinds.o NESDIS_LandEM_Module.o
NESDIS_AMSU_SnowEM_Module.o : NESDIS_AMSU_SnowEM_Module.f90 Type_Kinds.o NESDIS_LandEM_Module.o NESDIS_SnowEM_Parameters.o
NESDIS_LandEM_Module.o : NESDIS_LandEM_Module.f90 Type_Kinds.o
NESDIS_OCEANEM_Module.o : NESDIS_OCEANEM_Module.f90 Type_Kinds.o
NESDIS_SEAICE_PHYEM_MODULE.o : NESDIS_SEAICE_PHYEM_MODULE.f90 Type_Kinds.o
NESDIS_SnowEM_Parameters.o : NESDIS_SnowEM_Parameters.f90 Type_Kinds.o
NESDIS_SSMI_SIceEM_Module.o : NESDIS_SSMI_SIceEM_Module.f90 Type_Kinds.o NESDIS_LandEM_Module.o
NESDIS_SSMI_SnowEM_Module.o : NESDIS_SSMI_SnowEM_Module.f90 Type_Kinds.o NESDIS_LandEM_Module.o
NESDIS_AMSRE_SICEEM_Module.o : NESDIS_AMSRE_SICEEM_Module.f90 Type_Kinds.o NESDIS_LandEM_Module.o
NESDIS_AMSRE_SNOWEM_Module.o : NESDIS_AMSRE_SNOWEM_Module.f90 Type_Kinds.o NESDIS_LandEM_Module.o NESDIS_SnowEM_Parameters.o

CRTM_IR_Ice_SfcOptics.o : CRTM_IR_Ice_SfcOptics.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_GeometryInfo_Define.o CRTM_SfcOptics_Define.o CRTM_SpcCoeff.o CRTM_surface_ir_emissivity.o

CRTM_IR_Land_SfcOptics.o : CRTM_IR_Land_SfcOptics.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_GeometryInfo_Define.o CRTM_SfcOptics_Define.o CRTM_SpcCoeff.o CRTM_surface_ir_emissivity.o

CRTM_IR_Snow_SfcOptics.o : CRTM_IR_Snow_SfcOptics.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_GeometryInfo_Define.o CRTM_SfcOptics_Define.o CRTM_SpcCoeff.o CRTM_surface_ir_emissivity.o

CRTM_IR_Water_SfcOptics.o : CRTM_IR_Water_SfcOptics.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_GeometryInfo_Define.o CRTM_SfcOptics_Define.o CRTM_SpcCoeff.o CRTM_IRSSEM.o

CRTM_MW_Ice_SfcOptics.o : CRTM_MW_Ice_SfcOptics.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_GeometryInfo_Define.o CRTM_SfcOptics_Define.o CRTM_SpcCoeff.o CRTM_SensorInfo.o NESDIS_AMSU_SICEEM_Module.o NESDIS_SSMI_SIceEM_Module.o NESDIS_AMSRE_SICEEM_Module.o NESDIS_SEAICE_PHYEM_MODULE.o

CRTM_MW_Land_SfcOptics.o : CRTM_MW_Land_SfcOptics.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_GeometryInfo_Define.o CRTM_SfcOptics_Define.o CRTM_SpcCoeff.o NESDIS_OCEANEM_Module.o NESDIS_LandEM_Module.o

CRTM_MW_Snow_SfcOptics.o : CRTM_MW_Snow_SfcOptics.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_GeometryInfo_Define.o CRTM_SfcOptics_Define.o CRTM_SpcCoeff.o CRTM_SensorInfo.o NESDIS_AMSU_SnowEM_Module.o NESDIS_SSMI_SnowEM_Module.o NESDIS_AMSRE_SNOWEM_Module.o NESDIS_LandEM_Module.o

CRTM_MW_Water_SfcOptics.o : CRTM_MW_Water_SfcOptics.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_GeometryInfo_Define.o CRTM_SfcOptics_Define.o CRTM_SpcCoeff.o NESDIS_OCEANEM_Module.o

CRTM_SfcOptics_Define.o : CRTM_SfcOptics_Define.f90 Type_Kinds.o Message_Handler.o 
CRTM_SfcOptics.o : CRTM_SfcOptics.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_SpcCoeff.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_SfcOptics_Define.o CRTM_SpcCoeff.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_SfcOptics_Define.o CRTM_MW_Ice_SfcOptics.o CRTM_MW_Land_SfcOptics.o CRTM_MW_Snow_SfcOptics.o CRTM_MW_Water_SfcOptics.o CRTM_IR_Ice_SfcOptics.o CRTM_IR_Land_SfcOptics.o CRTM_IR_Snow_SfcOptics.o CRTM_IR_Water_SfcOptics.o 

EmisCoeff_Define.o : EmisCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
EmisCoeff_Binary_IO.o : EmisCoeff_Binary_IO.f90 Type_Kinds.o Message_Handler.o File_Utility.o Binary_File_Utility.o EmisCoeff_Define.o
CRTM_EmisCoeff.o : CRTM_EmisCoeff.f90 Type_Kinds.o Message_Handler.o EmisCoeff_Define.o EmisCoeff_Binary_IO.o

CRTM_Forward_Module.o : CRTM_Forward_Module.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_ChannelInfo_Define.o CRTM_AtmAbsorption.o CRTM_AerosolScatter.o CRTM_CloudScatter.o CRTM_SfcOptics.o CRTM_RTSolution.o CRTM_AtmOptics.o
CRTM_Tangent_Linear_Module.o : CRTM_Tangent_Linear_Module.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_ChannelInfo_Define.o CRTM_AtmAbsorption.o CRTM_AerosolScatter.o CRTM_CloudScatter.o CRTM_SfcOptics.o CRTM_RTSolution.o CRTM_AtmOptics.o 
CRTM_Adjoint_Module.o : CRTM_Adjoint_Module.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_ChannelInfo_Define.o CRTM_AtmAbsorption.o CRTM_AerosolScatter.o CRTM_CloudScatter.o CRTM_SfcOptics.o CRTM_RTSolution.o CRTM_AtmOptics.o

CRTM_K_Matrix_Module.o : CRTM_K_Matrix_Module.f90 Type_Kinds.o Message_Handler.o CRTM_Parameters.o CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_ChannelInfo_Define.o CRTM_AtmAbsorption.o CRTM_AerosolScatter.o CRTM_CloudScatter.o CRTM_SfcOptics.o CRTM_RTSolution.o CRTM_AtmOptics.o

CRTM_Module.o : CRTM_Module.f90 CRTM_Atmosphere_Define.o CRTM_Surface_Define.o CRTM_GeometryInfo_Define.o CRTM_ChannelInfo_Define.o CRTM_RTSolution_Define.o CRTM_LifeCycle.o CRTM_ChannelInfo.o CRTM_Forward_Module.o CRTM_Tangent_Linear_Module.o CRTM_Adjoint_Module.o

FWDTLMtest_Define.o : FWDTLMtest_Define.f90 Type_Kinds.o Message_Handler.o
FWDTLMtest_netCDF_IO.o : FWDTLMtest_netCDF_IO.f90 Type_Kinds.o Message_Handler.o FWDTLMtest_Define.o netCDF_Utility.o

TLADMtest_Define.o : TLADMtest_Define.f90 Type_Kinds.o Message_Handler.o
TLADMtest_netCDF_IO.o : TLADMtest_netCDF_IO.f90 Type_Kinds.o Message_Handler.o TLADMtest_Define.o netCDF_Utility.o

ADKMtest_Define.o : ADKMtest_Define.f90 Type_Kinds.o Message_Handler.o
ADKMtest_netCDF_IO.o : ADKMtest_netCDF_IO.f90 Type_Kinds.o Message_Handler.o ADKMtest_Define.o netCDF_Utility.o

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Message_Handler.o File_Utility.o CRTM_Module.o CRTM_Atmosphere_Binary_IO.o CRTM_Surface_Binary_IO.o



# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = $(EXE_FILE)_software.tar

tar: create_links
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	@$(COPY) makefile.dist makefile
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	@$(REMOVE) makefile
	@$(COPY) $(F90_DIR)/make.macros .
	@$(COPY) $(F90_DIR)/make.rules .
	$(TARBALL_APPEND) $(TAR_FILE) make.macros make.rules
	@$(REMOVE) make.macros make.rules
	$(TARBALL_APPEND) $(TAR_FILE) $(ATMOSPHERE_DATA_FILE).$(DATA_ENDIAN_TAGS)
	$(TARBALL_APPEND) $(TAR_FILE) $(SURFACE_DATA_FILE).$(DATA_ENDIAN_TAGS)
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  $(TARBALL_APPEND) $(TAR_FILE) $$file_tag.*.bin* ; \
	done
	@$(TARBALL_APPEND) $(TAR_FILE) *.ScatterCoeff.bin*
	@$(TARBALL_APPEND) $(TAR_FILE) *.AerosolCoeff.bin*
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(F90_DIR)/make.rules

