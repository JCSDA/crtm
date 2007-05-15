#==============================================================================
#
# Master makefile for building, running, and tarballing the CRTM
# example test codes
#
#==============================================================================

# -------------
# Define macros
# -------------
include make.macros


# -----------
# Main target
# -----------
all: link build run

# Link targets
link: link_test_forward \
      link_test_tangent_linear \
      link_test_adjoint \
      link_test_k_matrix

link_test_forward:
	cd Forward; make create_links; cd ..
        
link_test_tangent_linear:
	cd Tangent_Linear; make create_links; cd ..

link_test_adjoint:
	cd Adjoint; make create_links; cd ..

link_test_k_matrix:
	cd K_Matrix; make create_links; cd ..


# Build targets
build: build_test_forward \
       build_test_tangent_linear \
       build_test_adjoint \
       build_test_k_matrix

build_test_forward: link_test_forward
	cd Forward; make; cd ..

build_test_tangent_linear: link_test_tangent_linear
	cd Tangent_Linear; make; cd ..

build_test_adjoint: link_test_adjoint
	cd Adjoint; make; cd ..

build_test_k_matrix: link_test_k_matrix
	cd K_Matrix; make; cd ..


# run targets
run: run_test_forward \
     run_test_tangent_linear \
     run_test_adjoint \
     run_test_k_matrix

run_test_forward: build_test_forward
	cd Forward; make run; cd ..

run_test_tangent_linear: build_test_tangent_linear
	cd Tangent_Linear; make run; cd ..

run_test_adjoint: build_test_adjoint
	cd Adjoint; make run; cd ..

run_test_k_matrix: build_test_k_matrix
	cd K_Matrix; make run; cd ..


# Clean targets
clean: clean_test_forward \
       clean_test_tangent_linear \
       clean_test_adjoint \
       clean_test_k_matrix

clean_test_forward:
	cd Forward; make clean; cd ..

clean_test_tangent_linear:
	cd Tangent_Linear; make clean; cd ..

clean_test_adjoint:
	cd Adjoint; make clean; cd ..

clean_test_k_matrix:
	cd K_Matrix; make clean; cd ..


# realclean targets
realclean: realclean_test_forward \
           realclean_test_tangent_linear \
           realclean_test_adjoint \
           realclean_test_k_matrix

realclean_test_forward:
	cd Forward; make realclean; cd ..

realclean_test_tangent_linear:
	cd Tangent_Linear; make realclean; cd ..

realclean_test_adjoint:
	cd Adjoint; make realclean; cd ..

realclean_test_k_matrix:
	cd K_Matrix; make realclean; cd ..


# Tarball target
TAR_FILE = CRTM_Test_software.tar

tar: link
	$(TARBALL_CREATE) $(TAR_FILE) make.macros make.rules make.common
	$(TARBALL_APPEND) $(TAR_FILE) Makefile
	$(TARBALL_APPEND) $(TAR_FILE) --exclude .svn Forward Tangent_Linear Adjoint K_Matrix
	compress $(TAR_FILE)

