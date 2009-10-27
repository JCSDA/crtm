This is the NSIDC DataViewer program, version 1.0, written by David Fanning of the National 
Snow and Ice Data Center (NSIDC) and released on 11 November 2008. The program is written
in the Interactive Data Language (IDL). For additional information about IDL, see the
ITTVIS web page (http://www.ittvis.com).

DATA FILES

   Currently the program reads the following NSIDC image data files.

   nsidc-0001: DMSP-SSM Daily Polar Gridded Brightness Temperatures
               http://www.nsidc.org/data/nsidc-0001.html
   nsidc-0032: DMSP SSM/I Pathfinder Daily EASE-Grid Brightness Temperatures
               http://www.nsidc.org/data/nsidc-0032.html
   nsidc-0071: Nimbus-7 SMMR Pathfinder Daily EASE-Grid Brightness Temperatures
               http://www.nsidc.org/data/nsidc-0071.html
   nsidc-0079: Bootstrap Sea Ice Concentrations from Nimbus-7 SMMR and DMST SSM/I
               http://www.nsidc.org/data/nsidc-0079.html
   nsidc-0080: Near Real-Time DMSP SSM/I Daily Polar Gridded Brightness Temperatures
               http://www.nsidc.org/data/nsidc-0080.html
   nsidc-0301: AMSR-E/Aqua Daily EASE-Grid Brightness Temperatures
               http://www.nsidc.org/data/nsidc-0301.html
   nsidc-0302: AMSR-E/Aqua Daily Global Quarter-Degree Gridded Brightness Temperatures
               http://www.nsidc.org/data/nsidc-0302.html
   Sea Ice:    AMSR-E/Aqua Daily L3 25Km Brightness Temperature & Sea Ice Concentration Polar Grids
               http://nsidc.org/data/ae_si25.html

   Additionally, the program reads BMP, GIF, JPEG, PNG, PPM, SRF, TIFF, DICOM, or 
   JPEG2000 image files .

SET-UP 

    The DataViewer set-up depends upon whether you downloaded the DataViewer files
    as a zip file from an NSIDC web page, or whether you received the DataViewer files
    on a CD-ROM. The zip file contains IDL source code only, and requires a full IDL license
    to run the program. The CD distribution contains the IDL source code, but can
    also be run without an IDL license, using the IDL Virtual Machine.

    DATAVIEWER AS ZIP FILE

    If you downloaded the DataViewer zip file, then you have DataViewer source code
    files only. You must have a full IDL license to run the DataViewer program.
    When you extract the zip file to your local directory, it will unzip itself into a 
    "dataviewer" directory. You must add this dataviewer directory to your IDL path. See
    the section below, entitled ADDING DATAVIEWER TO THE IDL PATH, for additional 
    directions. (Note that the sample "data" directory is not included in the DataViewer
    zip file and must be downloaded separately.)

    DATAVIWER ON CD-ROM

    If you received the DataViewer files on a CD, then you can run the DataViewer
    program directly from the CD using the IDL Virtual Machine. The CD supports LINUX
    and WINDOWS machines only. In some cases, simply inserting the CD into your CD-ROM 
    drive is all that is required to run the program. However, if the program doesn't 
    run automatically, simply browse to the root directory of the CD. There you will
    find a shell script, named dataviewer_linux, that you can run to start the
    DataViewer program if you are running from a LINUX machine. Or, you will find a 
    program named dataviewer_windows.exe that you can double-click to start the
    DataViewer program on a WINDOWS machine.

    Program performance will be better if the DataViewer program runs from a local
    disk, rather than from the CD-ROM. Simply drag the files on the CD to any
    location on your hard drive, and follow the directions below. The IDL Virtual
    Machine files are included in the dataviewer/IDL70 directory, so everything needed
    to run the DataViewer program is on the CD, including sample NSIDC image files.

DATAVIEWER DIRECTORIES

    FILES ON THE CD

    The following folders or directories are included on the DataViewer CD.

       root-- this is the main directory of the CD and contains this README file, and
              the start-up scripts and programs for starting the DataViewer program on
              various machine architectures. The dataviewer.sav file is also here. This
              is an IDL save file that contains all the IDL programs files necessary to
              run the IDL program.
          config       -- this folder contains configuration text files
          data         -- this folder contains sample data files and is only available
                          on the DataViewer CD or by separate download
          IDL70        -- this folder contains the IDL Virtual Machine files
          resources    -- this folder contains resources for the DataViewer program
          dataviewer_source -- this folder contains the IDL source code for the DataViewer program,
                          which you will only need if you have a full IDL license and wish to
                          modify the source code itself.
           	config       -- this folder contains configuration text files
          	resources    -- this folder contains resources for the DataViewer program
         	library      -- this folder contains the Catalyst and Coyote Library files
                                used by DataViewer
                source       -- this folder contains the DataViewer IDL source code files.


    The dataviewer.sav file is the file that can be run on the IDL Virtual Machine. Do not remove 
    this file from its current locaton relative to the other files on the distribution, if you 
    want the DataViewer program to run correctly. (However, you can make shortcuts to this file 
    and install the shortcuts elsewhere, if you like.)

    FILES IN THE ZIP FILE

    If you received the DataViewer distribution via a zip file, you must have a full
    IDL license to run the IDL source code. You must add the dataviewer folder and all
    of its sub-folders to your IDL path (!PATH). See below for directions. The folders
    in the zip file are these:

       dataviewer   -- this folder contains the IDL source code for the DataViewer program,
                       which you will only need if you have a full IDL license and wish to
                       modify the source code itself
          config    -- this folder contains configuration text files
          data      -- this folder contains sample image files and must be downloaded separately
          resources -- this folder contains resources for the program
          library   -- this folder contains the Catalyst and Coyote Library files
          source    -- this folder contains the DataViewer IDL source code files

   In addition, you might have downloaded a separate zip file of NSIDC sample data files.
   These files will extract into a data directory that should also be installed in the main
   dataviewer directory.

CONFIGURING THE PROGRAM 

   You can configure the DataViewer program ONLY if you are not running the program 
   directly from the CD.

   Inside the config folder you will find the file "dataviewer_default.txt". This is 
   the configuration file for the DataViewer application. The only parameter you are 
   likely to want to change initially is the location of the data directory containing
   your NSIDC image files. Open this configuration file in any text editor, and find 
   this line in the file:

      DATAVIEWER_DATA_DIRECTORY, 'DEFAULT'

   Remove the word "DEFAULT" between the single quotes and substitute the location
   of your data directory. Be absolutely sure the directory name is enclosed in one
   pair of single quotes. If you fail to make a change, the program will point
   to the "dataviewer/data" sub-directory. This folder, if it is available,
   or if you downloaded it, will contain sample NSIDC data files for you to use 
   in the DataViewer.

   Configuration files are most easily changed from inside the DataViewer program 
   itself. Make modifications to the program from the File menu button. When you 
   are satisfied with the way the program looks and works, choose the "Save Current 
   Configuration..." button. You can save your configuration file as the default, or
   you can name it something else. You can have as many configuration files as you 
   like. Opening a configuration file from inside the DataViewer program immediately 
   implements that configuration for the program. You can have a different configuration 
   file for each type of image you wish to view, if you like. Note that you CANNOT
   save a configuration file on the read-only CD-ROM.

RUNNING THE PROGRAM

    How you run the program depends on whether or not you have the file
    "dataviewer.sav" in your program distribution. If you have this file, and you 
    do not have a full IDL license, then you can run the DataViewer program on the 
    IDL Virtual Machine, which is provided in the CD-ROM distribution. 

    If you have the "dataviewer.sav" file, but you also have a full IDL license, 
    you can choose to run the DataViewer program from source. Simply drag the
    folder dataviewer_source to any location on your hard drive and add the 
    folder to your IDL path (!PATH). See the directions below for doing so.

    RUNNING ON THE IDL VIRTUAL MACHINE 

    If you received the DataViewer distibution on CD, you need only to double 
    click the dataviewer_linux shell script, or the dataviewer_windows.ext file 
    in the dataviewer folder to start the program, depending on your operating
    system. (Some LINUX machines will not allow you to run a shell script by
    double clicking. On those machines, you will need to run the shell script 
    from a terminal window in the usual way, e.g., by typing ./dataviewer_linux.)
    On some machines, the DataViewer program will start automatically when you 
    insert the CD-ROM into your machine. The program might run faster if you
    first copy the contents of the CD to a local hard drive, and run the 
    DataViewer program from there.

    RUNNING THE DATAVIEWER SOURCE CODE

    To run the DataViewer source code from a CD distribution, drag the 
    dataviewer_source directory from your DataViewer distribution to a
    location on your hard drive. Then, you must add the dataviewer_source folder 
    to your IDL path (!PATH). Be sure to add the folder in such a way that all 
    sub-folders are also included in the path. Usually, this means adding a +  
    mark to the directory name or checking the box in front of the directory name, 
    depending upon how you add the directory to your IDL path.

    ADDING DATAVIEWER TO THE IDL PATH

    There are many ways to add a folder and its sub-folders to the IDL path
    (!PATH). See the IDL on-line help for additional information. But one way 
    to make sure all the DataViewer programs are on your IDL path is to enter IDL 
    and type one or the other of the following two commands, depending upon your 
    machine operating system. 

        UNIX OPERATING SYSTEM

        Here is what you do if you are running IDL on a UNIX operating system.
        Assume you have installed the dataviewer_source folder in your home directory
        inside the folder IDL, like this:

            ~/IDL/dataviewer_source/

       From within IDL, you can type this command to add the dataviewer directory
       and all of its sub-directories to the IDL path:

           IDL> !PATH = Expand_Path('+~/IDL/dataviewer_source/') + ':' + !PATH

       WINDOWS OPERATING SYSTEM

       Here is what you do if you are running IDL on a WINDOWS operating system.
       Assume you have installed the dataviewer folder on your C drive, inside the 
       folder IDL, like this:

           C:\IDL\dataviewer_source

       From within IDL, you can type this command to add the dataviewer directory
       and all of its sub-directories to the IDL path:

          IDL> !PATH = Expand_Path('+C:\IDL\dataviewer_source\') + ';' + !PATH

    Notice the plus sign (+) in front of the directory name for both operating systems. 
    This is an indication to IDL that this directory should be expanded and all the 
    sub-directories in this directory should also be added to the path. Note that in UNIX, 
    the files in the path are separated by colons, whereas in WINDOWS, the files are 
    separated by semi-colons. If you know how to do it, you can also include the appropriate
    command above in your IDL start-up file, so the DataViewer program is added to your
    IDL path whenever IDL is started.

    STARTING THE DATAVIEWER PROGRAM
    
    You can run the program from within IDL by typing "dataviewer" at the IDL command line.

       IDL> dataviewer
       
    If you like, you can pass the program the name of an image file to open:
   
       IDL> dataviewer, 'C:\data\EASE-F13-NL2006364D.85H.gz'
      
    Or, you can pass it the name of a folder. The program will read and open all
    the image files in the folder.
   
       IDL> dataviewer, 'C:\data\'

USING DATAVIEWER MENU OPTIONS
      
   Selecting Files
   
       You can open files in the DataViewer by choosing either the Open Image Files... 
       or the Open Image Directory of Files... buttons under the File menu. The latter 
       button allows you to choose an image directory and the program attempts to open 
       all the image files in the directory. If you use the  Open Image Files... button, 
       you have the opportunity to filter files before selection. Multiple files can be 
       selected by holding down the SHIFT or CONTROL keys while you make a selection. 
       Note that if a single file is selected, the window layout is changed to a 1-by-1 
       layout grid, regardless of the current window layout grid configuration.
   
   Changing the Window Layout
   
       The DataViewer program appears in a completely resizeable window. Feel free to 
       adjust the size of the window for optimal viewing of images. Simple grab any
       side or corner of the DataViewer window with your mouse and size accordingly.

       At any time you can change the layout of the images in the DataViewer window
       by selecting Change Window Layout Grid button from the File menu. Up to 64 image 
       files in an 8-by-8 grid layout is allowed.

   Saving the DataViewer Display Window

       The DataViewer display window can be saved at any time to a variety of file
       formats. Choose the Save Window As... button from the File menu to save the
       window in JPEG, PNG, PostScript, and other file formats.
   
   Changing Colors
   
       Colors for the image, and for different properties in the image
       (e.g., missing values, out-of-bounds values, etc.) can be changed
       from the Colors menu.
       
   Animating Images
   
        All selected images can be animated by selecting the Animate All Images...
        button from the Operations menu.

   Stretching or Scaling Images

        All selected images can be stretched (with Histogram stretching) or rescaled
        by selecting the Histogram Stretch All Images... button from the Operations
        menu. Various stretches, included LINEAR, LOG, and GAMMA stretches can be
        applied.

   Refreshing Images

        All images can be refreshed and returned to their default properties by choosing
        the Refresh All Images button from the Operations menu.
        
   Displaying Image Names
   
       Typically, images are displayed with their names below the images. This
       functionality can be turned on or off by selecting the Image Names button
       from the Operations menu.
       
   Displaying Image Color Bars
   
       Typically, images are displayed with a color bar above the images. This
       functionality can be turned on or off by selecting the Colorbars button
       from the Operations menu.

   Rearranging Images
   
        Images, when they are read into the program, are read in alphabetical order.
        You can rearrange the images in the current display window by left clicking
        inside the image and dragging it on top of the image location where you would
        like the image to be. All other images in the window will move to accommodate
        this rearrangement. You might have to rearrange the images, for example, prior
        to animating them.
        
   Image Information
   
         As you move your mouse over the images in the DataViewer display, you
         see the name of the image and its value and pixel location displayed in
         the status bar widget of the program interface. In this way, you can see
         the value of the image under your cursor.
        
   Other Image Properties
   
          You can change individual properties of images by right-clicking
          on individual images. Properties will include changing image colors,
          scaling the image, displaying the image in its natural size, and
          annotating the image (see below), among various other properties.
                    
  Annotating the Image

          You can annotate any image you like by right-clicking on the image
          you wish to annotate and choosing the Annotate Image selection from
          the pop-up menu. If you choose to add text to the annotation, be sure
          to hit the carriage return when you are finished typing text, for the
          text to be "set" in the annotation window. You can select and drag
          annotations in the window to position them where you want them. If you
          select an annotation and then right click it, you will be able to change
          that annotation's properties.

HELP

   If you need help with this program or its set-up, please contact NSIDC User Services:
   
       National Snow and Ice Data Center (NSIDC) User Services
       NSIDC/CIRES University of Colorado
       Boulder, CO 80309
       Phone: 303-492-6199
       E-Mail: nsidc@nsidc.org

