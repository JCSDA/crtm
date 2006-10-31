#==============================================================================
#
# Makefile for Combine_TauProfile program
#
# $Id: Makefile,v 1.3 2006/06/30 16:47:16 dgroff Exp $
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

MAKEFILE = Makefile


# -----------------
# Main program name
# -----------------

EXE_FILE = Combine_TauProfile


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o \
                    Compare_Float_Numbers.o
SENSORINFO_OBJ_FILES = SensorInfo_Define.o \
                       SensorInfo_LinkedList.o \
                       SensorInfo_IO.o
TAUPROFILE_OBJ_FILES = TauProfile_Define.o \
                       TauProfile_netCDF_IO.o
NETCDF_OBJ_FILES = netCDF_Utility.o \
                   netCDF_Dimension_Utility.o \
                   netCDF_Variable_Utility.o \
                   netCDF_Attribute_Utility.o

MODULE_OBJ_FILES = $(UTILITY_OBJ_FILES) \
                   $(SENSORINFO_OBJ_FILES) \
                   $(TAUPROFILE_OBJ_FILES) \
                   $(NETCDF_OBJ_FILES)

OBJ_FILES = $(MODULE_OBJ_FILES) \
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


# ----------
# Install it
# ----------

install:
	@if [ -d $(HOME)/bin ]; then \
	    $(MOVE) $(EXE_FILE) $(HOME)/bin; \
	fi


# ----------------------------
# Create links for compilation
# ----------------------------

UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

SENSORINFO_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SensorInfo
SENSORINFO_SOURCE_FILES = ${SENSORINFO_OBJ_FILES:.o=.f90}

TAUPROFILE_SOURCE_DIR = $(F90_DIR)/TauProfile
TAUPROFILE_SOURCE_FILES = ${TAUPROFILE_OBJ_FILES:.o=.f90}

NETCDF_SOURCE_DIR = $(F90_DIR)/NETCDF
NETCDF_SOURCE_FILES = ${NETCDF_OBJ_FILES:.o=.f90}

create_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SENSORINFO_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SENSORINFO_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${TAUPROFILE_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${TAUPROFILE_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${NETCDF_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${NETCDF_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(SENSORINFO_SOURCE_FILES) \
                   $(TAUPROFILE_SOURCE_FILES) \
                   $(NETCDF_SOURCE_FILES)
                   
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
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 Type_Kinds.o 

netCDF_Utility.o : netCDF_Utility.f90 Type_Kinds.o Message_Handler.o netCDF_Dimension_Utility.o netCDF_Variable_Utility.o netCDF_Attribute_Utility.o 
netCDF_Variable_Utility.o : netCDF_Variable_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Dimension_Utility.o : netCDF_Dimension_Utility.f90 Type_Kinds.o Message_Handler.o 
netCDF_Attribute_Utility.o : netCDF_Attribute_Utility.f90 Type_Kinds.o Message_Handler.o 

SensorInfo_Define.o : SensorInfo_Define.f90 Type_Kinds.o Message_Handler.o
SensorInfo_LinkedList.o : SensorInfo_LinkedList.f90 Type_Kinds.o Message_Handler.o SensorInfo_Define.o 
SensorInfo_IO.o : SensorInfo_IO.f90 Type_Kinds.o Message_Handler.o File_Utility.o SensorInfo_Define.o

TauProfile_Define.o : TauProfile_Define.f90 Type_Kinds.o Message_Handler.o Compare_Float_Numbers.o 
TauProfile_netCDF_IO.o : TauProfile_netCDF_IO.f90 Type_Kinds.o Message_Handler.o TauProfile_Define.o netCDF_Utility.o 

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o File_Utility.o Message_Handler.o SensorInfo_Define.o SensorInfo_LinkedList.o SensorInfo_IO.o TauProfile_Define.o TauProfile_netCDF_IO.o 


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
	@make -f $(MAKEFILE) realclean


#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(F90_DIR)/make.rules

