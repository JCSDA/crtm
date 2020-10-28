B. Johnson JCSDA 10/2020

Synopsis:
Application, Unit, and Regression tests, largely culled from Paul van Delst's and Dave Groff's CRTM tests, modified to work with CRTM v2.4.0 in a CMake environment.
Not a complete or comprehensive suite of tests, please add a test each time you add a new code element or substantially change code.

In theory, these tests should work with CRTM v2.3.0, and possibly CRTM v2.2.3 (untested)

Layout:

  cmake/                 (cmake Modules directory for storing the required cmake modules for the testing framework, do not delete!)
  CMakeLists.txt         (primary CMakeLists.txt file, tests, parameters, etc.  are defined here)
  mains/                 (top level of source files)
    application/            (Application tests go here, larger, end-to-end, more punishing tests [large scope ])
    regression/             (Regression tests go here to check various functionalities of CRTM   [medium scope])
    unit/                   (Unit tests go here to test small units of code, convergence, etc.   [small scope ])
  readme_crtm_tests.txt  (this file)
  test_build/            (directory where you cmake and make and ctest)




Instructions:

  Tests do not have to have a success metric, but it's nice to have in order to test for failure.
  STOP 0 and STOP 1 signal ctest for success or failure, respectively.  Any "failure" messages or "error" messages should have a STOP 1 following it.
	Any existing STOP should be converted to a STOP 1 as well.   


Prerequisites:

	Make and _install_ the CRTM library following the directions provided by the CRTM packate.
  These tests use both the generated modules and the libcrtm.a, so it needs to see both of these.


  The cmake will throw an error if the CRTM `lib/` and `include/` directories are not found.
  If this is the case, please edit the `CMakeLists.txt` file to point to your current installation folder of the CRTM.  

	Currently, it is looking for the CRTM in the following folder:

         ${CRTM_SOURCE_ROOT}/Build/crtm_v2.4.0-alpha/   
	
			 In CMakeLists.txt it's looking for it in this line: 

         HINTS "$ENV{CRTM_SOURCE_ROOT}/Build/crtm_v2.4.0-alpha/lib"


Running Tests:
  cd ./test_build   (directory where things are built and run) 
  cmake ..
  make -j12
  ctest

Slightly more verbose Mode (-VV):

  ls -l  (find your test name)
  ctest -VV -R testname (run an individual test)

Cleanup:

  In the test_build directory, you may need to "rm -rf *" to clean things up, particularly if you're adding new tests to the CMakeLists.txt file and testing, etc.


Troubleshooting:
    "error #7002: Error in opening the compiled module file.  Check INCLUDE paths.   [CRTM_MODULE]"
		This means that the CRTM module files are not being seen.

    "CMake Error at CMakeLists.txt:101 (message):     CRTM library not found!"
    This means that CRTM library file (libcrtm.a or libcrtm.so) are not found.

		When in doubt, clear it out (see Cleanup).

Support:
  Please feel free to contact us at:
    https://forums.jcsda.org/
    crtm-support@groups.google.com
  
