#==============================================================================
#
# Makefile for Sensor Planck Utility library Build
#
# $Id$
#
#==============================================================================

include ../make.macros
include make.filelist

# Library build
all library: $(LIBRARY)

$(LIBRARY):: $(OBJ_FILES)
	$(ARCHIVER) $(ARCHIVER_FLAGS) $@ $(OBJ_FILES)

# Library cleanup
clean::
	$(REMOVE) $(OBJ_FILES) *.$(EXT_MOD) $(LIBRARY)

include make.dependencies
include ../make.rules
