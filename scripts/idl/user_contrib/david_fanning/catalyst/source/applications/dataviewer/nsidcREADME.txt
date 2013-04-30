Welcome to the NSIDC DataViewer program, version 1.1, written by David
Fanning for the National Snow and Ice Data Center (NSIDC). The DataViewer
was released 11 November 2008 and updated 4 August 2010.  The program is
written in the Interactive Data Language (IDL) and works with IDL 6.4
and above. For information about IDL, visit the ITT Visual Information
Solutions site at http://www.ittvis.com.

For assistance with the DataViewer, please contact the NSIDC User
Services Office at:

	Phone: +1 303.492.6199
	Email: nsidc@nsidc.org

Please also note the following known issues:
	* DataViewer produces a scaling error with time values in DMSP-F17 SSMIS time (.tim) files. When using SSMIS .tim files, please be advised that data points will display as 1/10th of their actual value.
	* DataViewer is currently incompatible with data obtained using the Polaris search and subsetter tool.


The DataViewer works with the following NSIDC data sets, listed by data set title and URL:

   DMSP SSM/I-SSMIS Daily Polar Gridded Brightness Temperatures
	http://nsidc.org/data/nsidc-0001.html

   DMSP SSM/I-SSMIS Pathfinder Daily EASE-Grid Brightness Temperatures
	http://nsidc.org/data/nsidc-0032.html

   Northern Hemisphere EASE-Grid Weekly Snow Cover and Sea Ice Extent Version 3
	http://nsidc.org/data/nsidc-0046.html

   Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS Passive Microwave Data
	http://nsidc.org/data/nsidc-0051.html

   Nimbus-7 SMMR Pathfinder Daily EASE-Grid Brightness Temperatures
	http://nsidc.org/data/nsidc-0071.html

   Bootstrap Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS
	http://nsidc.org/data/nsidc-0079.html

   Near-Real-Time DMSP SSM/I-SSMIS Gridded Brightness Temperatures
	http://nsidc.org/data/nsidc-0080.html

   Near-Real-Time DMSP SSM/I-SSMIS Daily Polar Gridded Sea Ice Concentrations
	http://nsidc.org/data/nsidc-0081.html

   AMSR-E/Aqua Daily EASE-Grid Brightness Temperatures
	http://nsidc.org/data/nsidc-0301.html

   AMSR-E/Aqua Daily Global Quarter-Degree Gridded Brightness Temperatures
	http://nsidc.org/data/nsidc-0302.html

   Near-Real-Time SSM/I-SSMIS Pathfinder Daily Gridded Brightness Temperatures
	http://nsidc.org/data/nsidc-0342.html

   AMSR-E/Aqua Daily L3 25 km Brightness Temperature & Sea Ice Concentration Polar Grids
	http://nsidc.org/data/ae_si25.html

Additionally, the program reads BMP, GIF, JPEG, PNG, PPM, SRF, TIFF, DICOM, or
JPEG2000 image files.

The DataViewer no longer supports the following NSIDC data set:

   Polar Pathfinder Daily 25km EASE-Grid Sea Ice Motion Vectors*
	http://nsidc.org/data/nsidc-0116.html
	* Please note: The ice motion vectors only display the mean gridded fields.

SET-UP

IDL is required for the use of the DataViewer. If you do not have IDL, you may
download the free IDL Virtual Machine (VM) from ITTVIS at http://www.ittvis.com.
NSIDC distributes the DataViewer as an IDL save file named dataviewer.sav, which
contains all the IDL programs and libraries needed to run the DataViewer program.
The dataviewer.sav file can be used in either a normal IDL session, as an IDL
run-time application, or it can be run on the IDL Virtual Machine.


DOWNLOADING IDL VIRTUAL MACHINE (VM)

If you do not have an IDL license, you may download the IDL VM from ITTVIS
at http://www.ittvis.com.  If you have an IDL license, these steps can be
skipped.

Step 1: Register to become a user of the IDL VM.  The ITTVIS Web Administrator
will send you an automated email with your verification code, and a link to
submit your final approval.

Step 2: It may take a full day to hear back with your approval, but there
should be no problem with being approved. Once you are approved, you can
download the VM to your workstation. If you do not hear from ITTVIS within
one day, please contact them by phone. The number is listed on their Web
site at http://www.ittvis.com.


CONTENTS OF THE DATAVIEWER FILE

Two folders or directories and one IDL "save" file are included in a
"dataviewer" top directory in the DataViewer zip or tar file you
downloaded from NSIDC. Extract the dataviewer directory to a location
on your computer. Inside the dataviewer directory, you will find the
following directories and files:

\config\
This directory contains the configuration text file, dataviewer_default.txt.

\resources\
This directory contains resources required to run the DataViewer program.

\dataviewer.sav\
This is an IDL "save" file. This file may be restored in a normal IDL session,
it may be run with an IDL run-time license, or it may be opened by the IDL
Virtual Machine to run the DataViewer program. It is important, however,
not to move the dataviewer.sav file with respect to the other files and
folders in the dataviewer directory. If the dataviewer.sav file is moved
in relation to the other files or folders, the DataViewer program will
not function correctly.


RUNNING DATAVIEWER ON AN IDL VIRTUAL MACHINE

If you downloaded the IDL VM, you can run the dataviewer.sav file
as an application. Using your mouse, drag the dataviewer.sav icon
onto the VM icon, or just double click the VM icon and select the
DataViewer save file in the resulting file dialog box.


RUNNING DATAVIEWER AS AN IDL RUN-TIME APPLICATION

If you have an IDL run-time license installed (and you will if you
have installed a full version of IDL), you may run the DataViewer
program as a run-time application. On a Windows computer, just
double-click the dataviewer.sav program icon to start the DataViewer
as a run-time application. On a UNIX system, indicate that IDL
should be started in run-time mode, like this, where the text
"<path>" should be replaced by the actual path to the save file:

	% idl -rt=<path>dataviewer.sav


RUNNING DATAVIEWER WITH A FULL IDL LICENSE

If you have not added the DataViewer directory to your IDL path
(see ADDING DATAVIEWER TO YOUR IDL PATH, below), you can run
the program from within IDL by typing commands like this, where
the text "<path>" should be replaced by the actual path to the
DataViewer save file:

   IDL> Restore, <path>dataviewer.sav
   IDL> dataviewer

If you have added the DataViewer to your IDL path (see ADDING DATAVIEWER
TO THE IDL PATH, below), you can run the program from within IDL by
typing "dataviewer" at the IDL command line.

   IDL> dataviewer

If you like, you can pass the program the name of an image file to open:

   IDL> dataviewer, 'C:\data\EASE-F13-NL2006364D.85H.gz'

Or, you can pass it the name of a folder. The program will read and
open all the image files in the folder.

   IDL> dataviewer, 'C:\data\'


ADDING DATAVIEWER TO THE IDL PATH

It is only possible to add the DataViewer to the IDL path if you have a
fully-licensed version of IDL. There are many ways to add a folder and its
sub-folders to the IDL path (!PATH). See the IDL online help for additional
information. One way to make sure all the DataViewer programs are on your
IDL path is to enter IDL and type one or the other of the following two
commands, depending upon your machine operating system.

UNIX OPERATING SYSTEM
Assuming you have installed the dataviewer_source folder in your home
directory inside the folder IDL, for example ~/IDL/dataviewer/, you can
type the following command within IDL to add the dataviewer directory
and all of its sub-directories to the IDL path:

   IDL> !PATH = Expand_Path('+~/IDL/dataviewer/') + ':' + !PATH

WINDOWS OPERATING SYSTEM
Assuming you have installed the dataviewer folder on your C drive, inside
the folder IDL, for example C:\IDL\dataviewer, you can type this command
within IDL to add the dataviewer directory and all of its sub-directories
to the IDL path:

   IDL> !PATH = Expand_Path('+C:\IDL\dataviewer\') + ';' + !PATH.

You may put this command in your IDL start-up file to add the DataViewer
program to your IDL path whenever IDL is started.


CONFIGURING THE DATAVIEWER PROGRAM

The first time you open the DataViewer application it will assume your
image files are located in a "data" directory in the dataviewer folder.
This data directory may not exist, and if it does not you will be prompted
to select a different data directory. If you choose not to select a data
directory at this time, the dataviewer folder itself will be the temporary
data directory. Otherwise, select or create a directory of your choosing
and this will become the data directory for this session. This directory
you select or create will then automatically be saved as the new default
location in the configuration file (dataviewer_default.txt).

You may change the default data directory at any time from within the
DataViewer. To do so, select “Change Default Data Directory…” from the
"File" menu. The directory you select will automatically be saved as the
new default directory in the configuration file.

Note that as you open files in the DataViewer, the DataViewer will always
return to the last location in which you selected an image file, regardless
of the default data directory location.

Along with the data directory, you may also change other default settings,
such as image layout, from within the DataViewer at any time. To do so,
select “Edit Current Configuration File…” from the "File" menu. Make your
changes, then select "Apply Configuration" to view your new settings. When
you are satisfied with your changes, select "Save Configuration."


USING DATAVIEWER OPTIONS

The DataViewer provides different mechanisms for displaying data images.
Tab options in the navigation bar allow you to control the image operation
you would like to run, including image display, image animate, color change,
etc.  When images are displayed in the DataViewer, you can scroll over a
particular image and make further changes just to that image.  Use your
right-click mouse button to bring up a pop up window with operations for
that specific image.


SELECTING FILES

You can open files in the DataViewer by choosing either the Open Image Files
or the Open Image Directory of Files under the File tab. The latter button
allows you to choose an image directory and the program attempts to open
all the image files in the directory. If you use the Open Image Files button,
you have the opportunity to filter files before selection. Multiple files
can be selected by holding down the SHIFT or CONTROL keys while you make
a selection.  Note that if a single file is selected, the window layout
is changed to a 1-by-1 layout grid, regardless of the current window layout
grid configuration.


CHANGING THE WINDOW LAYOUT

The DataViewer program appears in a completely resizeable window. Feel
free to adjust the size of the window for optimal viewing of images. Simply
grab any side or corner of the DataViewer window with your mouse and size
accordingly.  You can change the layout of the images at any time in the
DataViewer window by selecting the Change Window Layout Grid button from
the File tab. Up to 64 image files in an 8-by-8 grid layout is allowed.


SAVING THE DATAVIEWER DISPLAY WINDOW

The DataViewer display window can be saved at any time to a variety of
file formats. Choose the Save Window As button from the File tab to
save the window in JPEG, PNG, PostScript, and other file formats.


CHANGING COLORS

Colors for the image, and for different properties in the image (missing
values, out-of-bounds values, etc.). can be changed from the Colors tab.


ANIMATING IMAGES

All selected images can be animated by selecting the Animate All Images
button from the Operations tab.


STRETCHING OR SCALING IMAGES

All selected images can be stretched with Histogram stretching or rescaled
by selecting the Histogram Stretch All Images button from the Operations
tab. Various stretches, including LINEAR, LOG, and GAMMA stretches, can
be applied.


REFRESHING IMAGES

All images can be refreshed and returned to their default properties by
choosing the Refresh All Images button from the Operations tab.


DISPLAYING IMAGE NAMES

Typically, images are displayed with their names below the images. This
functionality can be turned on or off by selecting the Image Names button
from the Operations tab.


DISPLAYING IMAGE COLOR BARS

Typically, images are displayed with a color bar above the images. This
functionality can be turned on or off by selecting the Colorbars button
from the Operations tab.


REARRANGING IMAGES

Images are read into the program in alphabetical order.  You can rearrange
the images in the current display window by left-clicking inside the image
and dragging it on top of the image location where you would like the image
to be. All other images in the window will move to accommodate this rearrangement.


IMAGE INFORMATION

As you move your mouse over the images in the DataViewer display, you see
the name of the image, its value, and pixel location (longitude, latitude)
displayed in the status bar widget of the program interface. In this way,
you can see the value of the image under your cursor.


OTHER IMAGE PROPERTIES

You can change individual properties of images by right-clicking on
individual images. Properties will include changing image colors,
scaling the image, displaying the image in its natural size, and
annotating the image (see below), among various other properties.


ANNOTATING THE IMAGE

You can annotate any image you like by right-clicking on the image you
wish to annotate and choosing the Annotate Image selection from the pop-up
tab. If you choose to add text to the annotation, be sure to hit the
carriage return when you are finished typing text; this will ensure the
text is "set" in the annotation window. You can select and drag annotations
in the window to position them where you want them. If you select an
annotation and then right-click it, you will be able to change the
properties of that annotation.


ASSISTANCE WITH THE DATAVIEWER

If you need help with this program or its set-up, please contact NSIDC
User Services:

   National Snow and Ice Data Center (NSIDC) User Services
   NSIDC/CIRES, 449 UCB
   University of Colorado
   Boulder, Colorado 80309-0449 USA
   Phone: +1 303.492.6199
   Email: nsidc@nsidc.org
