#==============================================================================
#
# Makefile for Info_SAT2SENSOR program
#
#==============================================================================

# -------------
# Define macros
# -------------

include $(REPOSITORY_SOURCE_ROOT)/make.macros

# This makefile
MAKEFILE = Makefile

# Executable files
EXE_FILE = Info_SAT2SENSOR

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            SatInfo_Define.f90 \
            SatInfo_IO.f90 \
            SensorInfo_Define.f90 \
            SensorInfo_LinkedList.f90 \
            SensorInfo_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o


# ------------------
# Define dependecies
# ------------------
# Make based on OS type
all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")  make -f $(MAKEFILE) build $(SUNOS_FLAGS) ;; \
	  "AIX")    make -f $(MAKEFILE) build $(AIX_FLAGS) ;; \
	  "IRIX64") make -f $(MAKEFILE) build $(IRIX64_FLAGS) ;; \
	  "Linux")  make -f $(MAKEFILE) build $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
	esac

# Build the program
build: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)

# Clean up
clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb

realclean: remove_links clean

# Install it
install:
	@if [ -d $(HOME)/bin ]; then \
	  $(MOVE) $(EXE_FILE) $(HOME)/bin; \
	fi


# Source file link creation and removal
create_links:
	@linkfiles $(REPOSITORY_SOURCE_ROOT) $(SRC_FILES)
	@linkfiles $(REPOSITORY_SOURCE_ROOT) global_satinfo.gsi SensorInfo

remove_links:
	@unlinkfiles $(SRC_FILES)
	@unlinkfiles global_satinfo.gsi SensorInfo


# Dependency list
Type_Kinds.o : Type_Kinds.f90
File_Utility.o : File_Utility.f90
Message_Handler.o : Message_Handler.f90 File_Utility.o
SatInfo_Define.o : SatInfo_Define.f90 Type_Kinds.o File_Utility.o Message_Handler.o 
SatInfo_IO.o : SatInfo_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o SatInfo_Define.o 
SensorInfo_Define.o : SensorInfo_Define.f90 Type_Kinds.o Message_Handler.o
SensorInfo_LinkedList.o : SensorInfo_LinkedList.f90 Type_Kinds.o Message_Handler.o SensorInfo_Define.o 
SensorInfo_IO.o : SensorInfo_IO.f90 Type_Kinds.o File_Utility.o Message_Handler.o SensorInfo_Define.o 
$(EXE_FILE).o : $(EXE_FILE).f90 Type_Kinds.o File_Utility.o Message_Handler.o SatInfo_IO.o SensorInfo_Define.o SensorInfo_LinkedList.o SensorInfo_IO.o


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
	@compress $(TAR_FILE)


# --------------------
# Define default rules
# --------------------
include $(REPOSITORY_SOURCE_ROOT)/make.rules
