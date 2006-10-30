#==============================================================================
#
# Makefile for pCRTM Adjoint Model Example
#
# $Id: Makefile,v 1.7 2006/05/02 14:58:35 dgroff Exp $
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

EXE_FILE = Adjoint_Model_Example


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
pCRTM_OBJ_FILES = parameters.o \
                  sensor_planck_routines.o \
                  spectral_coefficients.o \
                  transmittance_coefficients.o \
                  initialize.o \
                  absorber_profile.o \
                  predictors.o \
                  transmittance.o \
                  radiance.o \
                  forward_model.o \
                  adjoint_model.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(SPCCOEFF_OBJ_FILES) \
            $(TAUCOEFF_OBJ_FILES) \
            $(pCRTM_OBJ_FILES) \
            $(EXE_FILE).o


# -----------------------------
# The profile data include file
# -----------------------------

INCLUDE_FILE = Profile.inc

INCLUDES = -I.



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

SPCCOEFF_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SpcCoeff
SPCCOEFF_SOURCE_FILES = ${SPCCOEFF_OBJ_FILES:.o=.f90}

TAUCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/TauCoeff
TAUCOEFF_SOURCE_FILES = ${TAUCOEFF_OBJ_FILES:.o=.f90}

pCRTM_SOURCE_DIR = $(F90_DIR)/CRTM/pCRTM/Source_Code
pCRTM_SOURCE_FILES = ${pCRTM_OBJ_FILES:.o=.f90}

create_source_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
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
	@for source_file in ${pCRTM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${pCRTM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done


# -- The data file links
SPCCOEFF_DATA_DIR = $(HOME)/CRTM/SpcCoeff/Current
TAUCOEFF_DATA_DIR = $(HOME)/CRTM/TauCoeff

DATA_FILE_TAGS = amsua_n17 \
                 hirs3_n17 \
                 ssmis_f16

DATA_ENDIAN_TAGS = Little_Endian Big_Endian

create_data_links:
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

# ----------------
# Remove the links
# ----------------

remove_links: remove_source_links remove_data_links

# -- The source code links
ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(SPCCOEFF_SOURCE_FILES) \
                   $(TAUCOEFF_SOURCE_FILES) \
                   $(pCRTM_SOURCE_FILES)

remove_source_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done

# -- The data file links
remove_data_links:
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  $(REMOVE) $$file_tag.*.bin* ; \
	done


# ---------------
# Dependency list
# ---------------

Type_Kinds.o : Type_Kinds.f90
File_Utility.o : File_Utility.f90
Message_Handler.o : Message_Handler.f90 File_Utility.o
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o
Endian_Utility.o : Endian_Utility.f90 Type_Kinds.o
Binary_File_Utility.o : Binary_File_Utility.f90 Type_Kinds.o File_Utility.o Message_Handler.o Endian_Utility.o

parameters.o : parameters.f90 Type_Kinds.o
spectral_coefficients.o : spectral_coefficients.f90 Type_Kinds.o Message_Handler.o parameters.o SpcCoeff_Define.o SpcCoeff_Binary_IO.o
transmittance_coefficients.o : transmittance_coefficients.f90 Type_Kinds.o Message_Handler.o parameters.o TauCoeff_Define.o TauCoeff_Binary_IO.o
initialize.o : initialize.f90 Message_Handler.o transmittance_coefficients.o spectral_coefficients.o
absorber_profile.o : absorber_profile.f90 Type_Kinds.o parameters.o
predictors.o : predictors.f90 Type_Kinds.o Message_Handler.o parameters.o
transmittance.o : transmittance.f90 Type_Kinds.o parameters.o transmittance_coefficients.o
sensor_planck_routines.o : sensor_planck_routines.f90 Type_Kinds.o parameters.o spectral_coefficients.o
radiance.o : radiance.f90 Type_Kinds.o parameters.o spectral_coefficients.o sensor_planck_routines.o
forward_model.o : forward_model.f90 Type_Kinds.o Message_Handler.o parameters.o spectral_coefficients.o absorber_profile.o predictors.o transmittance.o radiance.o
adjoint_model.o : adjoint_model.f90 Type_Kinds.o Message_Handler.o parameters.o spectral_coefficients.o absorber_profile.o predictors.o transmittance.o radiance.o forward_model.o

SpcCoeff_Define.o : SpcCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
SpcCoeff_Binary_IO.o : SpcCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o SpcCoeff_Define.o Binary_File_Utility.o
TauCoeff_Define.o : TauCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
TauCoeff_Binary_IO.o : TauCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o TauCoeff_Define.o Binary_File_Utility.o

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Message_Handler.o initialize.o parameters.o adjoint_model.o

$(EXE_FILE) : $(INCLUDE_FILE)


# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = pCRTM_$(EXE_FILE)_software.tar

tar: create_links
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	@$(COPY) makefile.dist makefile
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	@$(REMOVE) makefile
	@$(COPY) $(F90_DIR)/make.macros .
	@$(COPY) $(F90_DIR)/make.rules .
	$(TARBALL_APPEND) $(TAR_FILE) make.macros make.rules
	@$(REMOVE) make.macros make.rules
	$(TARBALL_APPEND) $(TAR_FILE) $(INCLUDE_FILE)
	@for file_tag in ${DATA_FILE_TAGS}; do \
	  $(TARBALL_APPEND) $(TAR_FILE) $$file_tag.results ; \
	  $(TARBALL_APPEND) $(TAR_FILE) $$file_tag.*.bin.*Endian ; \
	done
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(F90_DIR)/make.rules

