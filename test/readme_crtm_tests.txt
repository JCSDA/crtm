B. Johnson JCSDA 6/2019

Synopsis:
CRTM test codes go in "./mains"

Instructions:
use the instructions at http://academy.jcsda.org/june2019/slides/20190613-Testing-JEDI.pptx or
https://jointcenterforsatellitedataassimilation-jedi-docs.readthedocs-hosted.com/en/latest/developer/building_and_testing/adding_a_test.html

Tests do not have to have a success metric, but it's nice to have in order to test for failure.
exit(0) and exit(1) signal ctest for success or failure, respectively.    exit() is a language extension, but seems to be supported
by both gfortran and ifortran.


For example: check_crtm currently passess even if it cannot find the binary files.   

Rules:
Binary files go in "./testinput".



