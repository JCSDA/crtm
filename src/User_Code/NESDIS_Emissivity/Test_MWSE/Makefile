#==============================================================================
#
# Makefile for Test_MWSE program
#
# $Id: Makefile,v 1.4 2006/05/02 14:58:35 dgroff Exp $
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


# ----------------
# Executable files
# ----------------

EXE_FILE = Test_MWSE


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o
EMISSIVITY_OBJ_FILES = kinds.o \
                       constants.o \
                       landem.o \
                       MWSE_Snow_Parameters.o \
                       MWSE_Snow.o
EMISTESTDATA_OBJ_FILES = EmisTestData_Define.o \
                         EmisTestData_Binary_IO.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(EMISSIVITY_OBJ_FILES) \
            $(EMISTESTDATA_OBJ_FILES) \
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

UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

EMISSIVITY_SOURCE_DIR = ..
EMISSIVITY_SOURCE_FILES = ${EMISSIVITY_OBJ_FILES:.o=.f90}

EMISTESTDATA_SOURCE_DIR = ../Test_DataSets
EMISTESTDATA_SOURCE_FILES = ${EMISTESTDATA_OBJ_FILES:.o=.f90}


create_links:
	@for source_file in ${UTILITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${UTILITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${EMISSIVITY_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${EMISSIVITY_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${EMISTESTDATA_SOURCE_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${EMISTESTDATA_SOURCE_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(EMISSIVITY_SOURCE_FILES) \
                   $(EMISTESTDATA_SOURCE_FILES)

remove_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done


# --------------------------
# Create the data file links
# --------------------------

DATA_DIR = ../Test_DataSets

DATA_FILES = emin_amsua15.dat.DirectBinary \
             emin_amsua16.dat.DirectBinary \
             emin_amsub15.dat.DirectBinary \
             emin_amsub16.dat.DirectBinary \
             emin_amsub17.dat.DirectBinary \
             emin_ssmi13.dat.DirectBinary \
             emin_ssmi14.dat.DirectBinary \
             emin_ssmi15.dat.DirectBinary

data_links_big:
	@for data_file in ${DATA_FILES}; do \
	  $(LINK) ${DATA_DIR}/$$data_file.Big_Endian $$data_file ; \
	done

data_links_little:
	@for data_file in ${DATA_FILES}; do \
	  $(LINK) ${DATA_DIR}/$$data_file.Little_Endian $$data_file ; \
	done


# ---------------
# Dependency list
# ---------------

kinds.o : kinds.f90 
contants.o : constants.f90 kinds.o
landem.o : landem.f90 constants.o kinds.o

MWSE_Snow_Parameters.o : MWSE_Snow_Parameters.f90 kinds.o
MWSE_Snow.o : MWSE_Snow.f90 landem.o MWSE_Snow_Parameters.o constants.o kinds.o

Type_Kinds.o : Type_Kinds.f90
File_Utility.o : File_Utility.f90
Message_Handler.o : Message_Handler.f90 File_Utility.o

EmisTestData_Define.o : EmisTestData_Define.f90 Type_Kinds.o Message_Handler.o
EmisTestData_Binary_IO.o : EmisTestData_Binary_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o EmisTestData_Define.o

$(EXE_FILE).o : $(EXE_FILE).f90 kinds.o Message_Handler.o EmisTestData_Define.o EmisTestData_Binary_IO.o MWSE_Snow.o


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

