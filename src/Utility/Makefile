#==============================================================================
#
# Makefile for creating a tarball and html code of the utility code
#
# $Id: Makefile,v 1.5 2006/05/02 16:58:03 dgroff Exp $
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

F90_DIR = $(HOME)/f90

include $(F90_DIR)/make.macros



#-----------------------------------------------------------------------------
#                        -- Define source code files --
#-----------------------------------------------------------------------------

SOURCE_FILES = Binary_File_Utility.f90 \
               Compare_Float_Numbers.f90 \
               Date_Utility.f90 \
               Endian_Utility.f90 \
               Message_Handler.f90 \
               File_Utility.f90 \
               Fundamental_Constants.f90 \
               List_File_Utility.f90 \
               String_Utility.f90 \
               Type_Kinds.f90



#-----------------------------------------------------------------------------
#                             -- Main target --
#-----------------------------------------------------------------------------

all: web_update



#-----------------------------------------------------------------------------
#                            -- Create tarballs --
#-----------------------------------------------------------------------------

all_tar: tar \
         numeric_inquiry \
         test_compare_float \
         test_date_utility \
         test_endian_utility \
         test_list_file_utility \
         test_string_utility \
         test_type_kinds


TAR_FILE = Utility_software.tar

tar:
	$(TARBALL_CREATE) $(TAR_FILE) $(SOURCE_FILES)
	compress -f $(TAR_FILE)

numeric_inquiry:
	cd Numeric_Inquiry; make tar; make realclean; cd ..

test_compare_float:
	cd Test_Compare_Float; make tar; make realclean; cd ..

test_date_utility:
	cd Test_Date_Utility; make tar; make realclean; cd ..

test_endian_utility:
	cd Test_Endian_Utility; make tar; make realclean; cd ..

test_list_file_utility:
	cd Test_List_File_Utility; make tar; make realclean; cd ..

test_string_utility:
	cd Test_String_Utility; make tar; make realclean; cd ..

test_type_kinds:
	cd Test_Type_Kinds; make tar; make realclean; cd ..



#-----------------------------------------------------------------------------
#                           -- Create HTML files --
#-----------------------------------------------------------------------------

all_html: html

PRO_FILE = main.pro

html:
	@n=0; \
	for source_file in ${SOURCE_FILES}; do \
	  n=`expr $$n + 1`; \
	done; \
	i=0; \
	for source_file in ${SOURCE_FILES}; do \
	  i=`expr $$i + 1`; \
	  case $$i in \
	    1) FILES="['$$source_file'" ;; \
	    $$n) FILES="$$FILES,'$$source_file']" ;; \
	    *) FILES="$$FILES,'$$source_file'" ;; \
	  esac; \
	done; \
	if [ $$n -eq 1 ]; then \
	  FILES="$$FILES]"; \
	fi; \
	echo "create_f90_html_doc,$$FILES" > ${PRO_FILE}; \
	echo "exit" >> ${PRO_FILE}; \
	idl ${PRO_FILE}; \
	$(REMOVE) $(PRO_FILE)



#-----------------------------------------------------------------------------
#                        -- Update web directories --
#-----------------------------------------------------------------------------

# -------------
# Web page root
# -------------

WEBROOT = $(HOME)/MyWebStuff/Fortran90/Utility


# --------------------------------
# Move tarballs to web directories
# --------------------------------

web_update: all_tar all_html
	@$(MOVE) $(TAR_FILE).Z $(WEBROOT)
	@$(MOVE) *.html $(WEBROOT)
	@$(MOVE) ./Numeric_Inquiry/*.Z $(WEBROOT)/Numeric_Inquiry
	@$(MOVE) ./Test_Compare_Float/*.Z $(WEBROOT)/Test_Compare_Float
	@$(MOVE) ./Test_Date_Utility/*.Z $(WEBROOT)/Test_Date_Utility
	@$(MOVE) ./Test_Endian_Utility/*.Z $(WEBROOT)/Test_Endian_Utility
	@$(MOVE) ./Test_List_File_Utility/*.Z $(WEBROOT)/Test_List_File_Utility
	@$(MOVE) ./Test_String_Utility/*.Z $(WEBROOT)/Test_String_Utility
	@$(MOVE) ./Test_Type_Kinds/*.Z $(WEBROOT)/Test_Type_Kinds
