B. Johnson JCSDA 10/2020
Note: reduced version of the v2.4.1 tests for use in the v2.4.0 EMC repository.  (BTJ, 5/2022)

Synopsis:
Application, Unit, and Regression tests, largely culled from Paul van Delst's and Dave Groff's CRTM tests, modified to work with CRTM v2.4.0/lib"

Instructions:
 cd <crtm_root>  (e.g., REL-2.4.0_emc/)

 mkdir build
 cd build
 cmake ..
 make -j12
 ctest

Support:
  Please feel free to contact us at:
    https://forums.jcsda.org/
    crtm-support@groups.google.com
  
