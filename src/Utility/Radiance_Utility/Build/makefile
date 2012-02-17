#================================================================================
#
# Makefile for Sensor Planck library build
#
# Specify your build conditions by sourcing a configuration setup file prior
# to invoking make. For example, to use the gfortran debug compilation settings,
#
#   $ . configure/gfortran_debug.setup
#
# and then
#
#   $ make
#
# $Id$
#
#================================================================================

# Define macros
include make.macros


# Main targets
all $(LIBRARY): libsrc/all

example: $(LIBRARY) install example/all


# Cleanup targets
clean: libsrc/clean example/clean

distclean: clean
	$(REMOVE) -r $(LIB_DIR)
	$(REMOVE) -r $(INC_DIR)


# Installation targets
install: install_lib install_inc

install_lib: $(LIB_DIR) $(LIBRARY)
	@echo; echo "Installing $(LIBRARY) in `pwd`/$(LIB_DIR)"; \
	$(COPY) $(LIBSRC_DIR)/$(LIBRARY) $(LIB_DIR); \
	echo

install_inc: $(INC_DIR) $(LIBRARY)
	@echo; echo "Installing *.$(EXT_MOD) in `pwd`/$(INC_DIR)"; \
	$(COPY) $(LIBSRC_DIR)/*.$(EXT_MOD) $(INC_DIR); \
	echo

# ...Create directories if necessary
$(LIB_DIR) \
$(INC_DIR) :
	- test -d $@ || mkdir $@


# Subtargets
libsrc/all    \
libsrc/clean  \
example/all   \
example/clean \
:
	@subdir=`echo $@ | sed 's,/.*,,'`; \
	target=`echo $@ | sed 's,.*/,,'`; \
	$(MAKE) SUBDIR=$$subdir TARGET=$$target subdir_target

subdir_target:
	@echo ""
	@cd $(SUBDIR) && \
	    echo "Making \`$(TARGET)' in directory `pwd`" && \
	    echo "" && \
	    $(MAKE) $(TARGET) || exit 1
	@echo ""
	@echo "Returning to directory `pwd`"
	@echo ""
