#==============================================================================
#
# Makefile for stand-alone Sensor Planck Utility example program
#
# $Id$
#
#==============================================================================

include ../make.macros

# The executable and object files
EXE_FILE = Sensor_Planck_Utility_Example
SRC_FILE = $(EXE_FILE).f90
OBJ_FILE = ${SRC_FILE:.f90=.o}

EXTRA_FC_FLAGS = -I../$(INC_DIR)
LIBRARIES = -L../$(LIB_DIR) -l$(PACKAGE)

all:: $(OBJ_FILE)
	@echo; echo; \
	echo "=============================================="; \
	echo "Building $(EXE_FILE) using:"; \
	echo "  Compiler: $(FC)"; \
	echo "  Switches: $(FC_FLAGS)"; \
	echo "=============================================="; \
	$(FL) $(OBJ_FILE) $(FL_FLAGS) $(LIBRARIES) -o $(EXE_FILE)
	@echo; echo; \
	echo "=============================================="; \
	echo "Running $(EXE_FILE)...."; \
	echo "=============================================="; \
	./$(EXE_FILE)

clean::
	@echo; echo; \
	echo "=============================================="; \
	echo "Cleaning up $(EXE_FILE)"; \
	echo "=============================================="; \
	$(REMOVE) $(OBJ_FILE) $(EXE_FILE)


# Local dependencies
$(OBJ_FILE): $(SRC_FILE)

include ../make.rules
