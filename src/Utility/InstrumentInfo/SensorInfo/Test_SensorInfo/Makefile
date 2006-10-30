#==============================================================================
#
# Makefile for Test_SensorInfo program
#
# $Id: Makefile,v 1.12 2006/05/02 16:58:02 dgroff Exp $
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

EXE_FILE = Test_SensorInfo


# ------------
# Object files
# ------------

UTILITY_OBJ_FILES = Type_Kinds.o \
                    File_Utility.o \
                    Message_Handler.o
SENSORINFO_OBJ_FILES = SensorInfo_Define.o \
                       SensorInfo_LinkedList.o \
                       SensorInfo_IO.o

OBJ_FILES = $(UTILITY_OBJ_FILES) \
            $(SENSORINFO_OBJ_FILES) \
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

create_links: create_source_links create_data_links

remove_links: remove_source_links remove_data_links


UTILITY_SOURCE_DIR = $(F90_DIR)/Utility
UTILITY_SOURCE_FILES = ${UTILITY_OBJ_FILES:.o=.f90}

SENSORINFO_SOURCE_DIR = $(F90_DIR)/Instrument_Information/SensorInfo
SENSORINFO_SOURCE_FILES = ${SENSORINFO_OBJ_FILES:.o=.f90}


create_source_links:
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

ALL_SOURCE_FILES = $(UTILITY_SOURCE_FILES) \
                   $(SENSORINFO_SOURCE_FILES)
                   
remove_source_links:
	@for source_file in ${ALL_SOURCE_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done


SENSORINFO_FILE = SensorInfo.input_test

create_data_links:
	@if [ ! -f $(SENSORINFO_FILE) ]; then \
	  $(LINK) ../SensorInfo $(SENSORINFO_FILE) ; \
	fi

remove_data_links:
	$(REMOVE) $(SENSORINFO_FILE)



# ---------------
# Dependency list
# ---------------

Type_Kinds.o : Type_Kinds.f90
File_Utility.o : File_Utility.f90
Message_Handler.o : Message_Handler.f90 File_Utility.o

SensorInfo_Define.o : SensorInfo_Define.f90 Type_Kinds.o Message_Handler.o
SensorInfo_LinkedList.o : SensorInfo_LinkedList.f90 Type_Kinds.o Message_Handler.o SensorInfo_Define.o 
SensorInfo_IO.o : SensorInfo_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o SensorInfo_Define.o 

$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o Message_Handler.o SensorInfo_Define.o SensorInfo_LinkedList.o SensorInfo_IO.o


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
	$(TARBALL_APPEND) $(TAR_FILE) SensorInfo.input_test
	$(TARBALL_APPEND) $(TAR_FILE) AAA.README
	compress $(TAR_FILE)


# -------------------------
# Create html documentation
# -------------------------

PRO_FILE = main.pro

html:
	@echo "create_f90_html_doc,'${EXE_FILE}.f90'" > ${PRO_FILE}; \
	echo "exit" >> ${PRO_FILE}; \
	idl ${PRO_FILE}; \
	$(REMOVE) $(PRO_FILE)


#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include $(F90_DIR)/make.rules

