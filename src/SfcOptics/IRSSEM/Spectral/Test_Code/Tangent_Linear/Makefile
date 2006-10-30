#==============================================================================
#
# Makefile for CRTM IRSSEM Test_Tangent_Linear program
#
# $Id: Makefile,v 1.2 2006/05/02 14:58:35 dgroff Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

F90_DIR = $(HOME)/f90

include $(F90_DIR)/make.macros


# ----------------
# Executable files
# ----------------

EXE_FILE = Test_Tangent_Linear


# -------------
# This makefile
# -------------

MAKEFILE = Makefile


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o \
                    Endian_Utility.o \
                    Binary_File_Utility.o \
                    Compare_Float_Numbers.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Variable_Utility.o \
                   netCDF_Attribute_Utility.o
EMISCOEFF_OBJ_FILES = EmisCoeff_Define.o \
                      EmisCoeff_Binary_IO.o
CRTM_EMISCOEFF_OBJ_FILES = CRTM_EmisCoeff.o
CRTM_IRSSEM_OBJ_FILES = CRTM_IRSSEM.o
FWDTLMTEST_OBJ_FILES = FWDTLMtest_Define.o \
                       FWDTLMtest_netCDF_IO.o
OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(NETCDF_OBJ_FILES) \
            $(EMISCOEFF_OBJ_FILES) \
            $(CRTM_EMISCOEFF_OBJ_FILES) \
            $(CRTM_IRSSEM_OBJ_FILES) \
            $(FWDTLMTEST_OBJ_FILES) \
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
	  "SunOS")   make -f $(MAKEFILE) the_program $(SUNOS_FLAGS) ;; \
	  "AIX")     make -f $(MAKEFILE) the_program $(AIX_FLAGS) ;; \
	  "IRIX64" ) make -f $(MAKEFILE) the_program $(IRIX64_FLAGS) ;; \
	  "Linux" )  make -f $(MAKEFILE) the_program $(Linux_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
	esac


# ---------------------
# Make the test program
# ---------------------

the_program: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)


# --------
# Clean up
# --------

clean:
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.stb

realclean: remove_links clean


# ----------------------------
# Create links for compilation
# ----------------------------

UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

NETCDF_SOURCE_DIR = $(F90_DIR)/NETCDF
NETCDF_SOURCE_FILES = ${NETCDF_OBJ_FILES:.o=.f90}

EMISCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients/EmisCoeff/Spectral
EMISCOEFF_SOURCE_FILES = ${EMISCOEFF_OBJ_FILES:.o=.f90}

CRTM_EMISCOEFF_SOURCE_DIR = $(F90_DIR)/CRTM/Coefficients
CRTM_EMISCOEFF_SOURCE_FILES = ${CRTM_EMISCOEFF_OBJ_FILES:.o=.f90}

CRTM_IRSSEM_SOURCE_DIR = $(F90_DIR)/CRTM/SfcOptics/IRSSEM/Spectral
CRTM_IRSSEM_SOURCE_FILES = ${CRTM_IRSSEM_OBJ_FILES:.o=.f90}

FWDTLMTEST_SOURCE_DIR = $(F90_DIR)/CRTM/Test_Code/Utility/FWDTLMtest
FWDTLMTEST_SOURCE_FILES = ${FWDTLMTEST_OBJ_FILES:.o=.f90}


create_links:
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
	@for source_file in ${EMISCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${EMISCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_EMISCOEFF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_EMISCOEFF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${CRTM_IRSSEM_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${CRTM_IRSSEM_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${FWDTLMTEST_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${FWDTLMTEST_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES) \
                   $(EMISCOEFF_SOURCE_FILES) \
                   $(CRTM_EMISCOEFF_SOURCE_FILES) \
                   $(CRTM_IRSSEM_SOURCE_FILES) \
                   $(FWDTLMTEST_SOURCE_FILES)
                   
remove_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done


# ---------------
# Dependency list
# ---------------

Type_Kinds.o : Type_Kinds.f90 
File_Utility.o : File_Utility.f90 
Message_Handler.o : Message_Handler.f90 File_Utility.o 
Endian_Utility.o : Endian_Utility.f90 Type_Kinds.o
Binary_File_Utility.o : Binary_File_Utility.f90 Type_Kinds.o File_Utility.o Message_Handler.o Endian_Utility.o
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o

netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 Message_Handler.o Type_Kinds.o 
netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 Message_Handler.o Type_Kinds.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 Message_Handler.o Type_Kinds.o 
netCDF_Utility.o : netCDF_Utility.f90 netCDF_Attribute_Utility.o netCDF_Variable_Utility.o netCDF_Dimension_Utility.o Message_Handler.o Type_Kinds.o 

EmisCoeff_Define.o : EmisCoeff_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
EmisCoeff_Binary_IO.o : EmisCoeff_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o EmisCoeff_Define.o Binary_File_Utility.o 

CRTM_EmisCoeff.o : CRTM_EmisCoeff.f90 Type_Kinds.o Message_Handler.o EmisCoeff_Define.o EmisCoeff_Binary_IO.o 
CRTM_IRSSEM.o : CRTM_IRSSEM.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o CRTM_EmisCoeff.o 

FWDTLMtest_Define.o : FWDTLMtest_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o
FWDTLMtest_netCDF_IO.o : FWDTLMtest_netCDF_IO.f90 Type_Kinds.o Message_Handler.o FWDTLMtest_Define.o netCDF_Utility.o 

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o File_Utility.o Message_Handler.o Compare_Float_Numbers.o FWDTLMtest_Define.o FWDTLMtest_netCDF_IO.o CRTM_IRSSEM.o


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
	compress $(TAR_FILE)



#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(F90_DIR)/make.rules

