The CRTM graduate student test
Version v2.4.0, revision 1.0

Basic requirements:
(1) A Fortran 2003 compatible compiler:  Preferred: GNU Fortran > v5.0 (tested), intel fortran > 15.0 (tested).  Untested: LLVM Flang, Flang, LFortran, NAG, PGI, CCE, IBM, AOCC, Lahey, FTN95, NEC -- please report issues.

(2) A netCDF4 / HDF5 library (search for `libnetcdff.a` or `libnetcdff.so`) 

(3) A linux, macOS, or unix-style environment.  This has not been tested under any Windows Fortran
environments.

(4) Bash shell is preferred.

--
GST Instructions:

(1) Start a timer to keep track of how long the whole process takes. 

(2) Store your git credentials locally to make downloading easier (otherwise it will ask for your login for each item):
`git config --global credential.helper store`

(3) Download and compile the code following the instructions provided in the package. The code is available from github.com (<15 minutes to download):
`git-lfs clone https://github.com/JCSDA/crtm.git --branch release/REL-2.4.0-alpha`  #potentially much faster, but requires newer version of git-lfs.  
or  

`git clone https://github.com/JCSDA/crtm.git --branch release/REL-2.4.0-alpha`  #potentially slower, but compatible.  

`cd REL-2.4.0-alpha`

(4) Then follow the build instructions in the supplied README.md file. (~5 minutes to build).  Ignore any/all warnings, but report any errors to me.

(5) Run the included test utility that exercises several elements of the code, and also verifies that the test completes successfully.  (< 1 minute to make and run to completion): 

make check 
(6) Modification of the test utility to include additional input information (e.g., add new sensors, more clouds, additional absorbers, etc.), and modify the code to provide additional output information (i.e., the “Jacobian” output) (~ 15 minutes to modify, rebuild, verify output)

   Task: Add a GOES-R visible sensor to the check_crtm.fpp file by modifying the appropriate lines.

   Action #1: find the big_endian files v.abi_gr.SpcCoeff.bin and v.abi_gr.TauCoeff.bin within the ./REL-2.4.0-alpha/fix/ directory 

   Action #2: copy those files to the ./REL-2.4.0-alpha/libsrc/test/coefficients/big_endian/ directory

   Action #3: modify check_crtm.fpp (located in the ./REL-2.4.0-alpha/libsrc/test/ directory) to add one more sensor: change n_sensors = 3, and append "v.abi_gr" to the sensor list, taking care to maintain the same number of characters for each item in the list.

   Action #4: use the "make check" command to see if it runs successfully.  

Please provide any feedback you have (easy to use?  any problems?  Was the test successful?).
