;*****************************************************************************************************
;+
; NAME:
;       PARSE_NSIDC_FILENAME_0046_new
;
; PURPOSE:
;
;       The purpose of this routine is to read and obtain information about
;       NSIDC images nsidc-0046 (for Version 4 data), northern hemisphere weekly snow cover and
;       snow extent.
;
; AUTHOR:
;
;       David W. Fanning, Ph.D
;       National Snow and Ice Data Center (NSIDC)
;       NSIDC/CIRES University of Colorado
;       Boulder, CO 80309
;       E-Mail: fanning@nsidc.org
;
; CATEGORY:
;
;       File Reading.
;
; SYNTAX:
;
;       image = parse_nsidc_filename_0046_new(filename)
;
; RETURN_VALUE:
;
;       image:       The image data in the file.
;
; ARGUMENTS:
;
;       filename:    The name of an NSIDC-0046 image file.
;
; OUTPUT_KEYWORDS:
;
;        INFO:       An output structure containing information about the image. See the
;                    documentation for PARSE_NSIDC_FILENAME for an example.
;
;        SUCCESS:    Set to 1 if the file was successfully read, otherwise to 0.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 9 June 2010.
;       Updated: Matthew Savoie, 11 Jan 2013.
;-
;******************************************************************************************;
;  Copyright (c) 2010, Regents of the University of Colorado. All rights reserved.         ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      1. Redistributions of source code must retain the above copyright                   ;
;         notice, this list of conditions and the following disclaimer.                    ;
;      2. Redistributions in binary form must reproduce the above copyright                ;
;         notice, this list of conditions and the following disclaimer in the              ;
;         documentation and/or other materials provided with the distribution.             ;
;      3. Neither the name of the Regents of the University of Colorado nor the names      ;
;         of its contributors may be used to endorse or promote products derived from      ;
;          this software without specific prior written permission.                        ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY THE REGENTS OF THE UNIVERISTY OF COLORADO ''AS IS'' AND    ;
;  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED           ;
;  WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.     ;
;  IN NO EVENT SHALL THE REGENTS OF THE UNIVERSITY OF COLORADO BE LIABLE FOR ANY DIRECT,   ;
;  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT      ;
;  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR      ;
;  PROFITS OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,        ;
;  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      ;
;  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE              ;
;  POSSIBILITY OF SUCH DAMAGE.                                                             ;
;******************************************************************************************;
FUNCTION parse_nsidc_filename_0046_new, filename, INFO=info, SUCCESS=success

   COMPILE_OPT idl2

   ; Simple error handling. Return to caller.
   ON_ERROR, 2

   ; Check parameters.
   success = 0 ; Assume no success
   IF N_Elements(filename) EQ 0 THEN Message, "A filename is a required input parameter."

   ; Set up colors.
   landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
   grid_color = CatGetDefault('DATAVIEWER_GRID_COLOR')
   vector_color = CatGetDefault('DATAVIEWER_VECTOR_COLOR')
   outline_color = CatGetDefault('DATAVIEWER_OUTLINE_COLOR')

   ; Parse the root file name to determine the parameters that need to be set appropriately.
   ; If this is not a compressed file, with extension .gz, then we will have to add the extension
   ; back to the filename and set the extension to a null string.
   root_name = cgRootName(filename, DIRECTORY=theDirectory, EXTENSION=theExtension)
   IF StrUpCase(theExtension) NE 'GZ' THEN BEGIN
        root_name = root_name + '.' + theExtension
        theExtension = ''
   ENDIF


   ; Pull out information from the filename for info structure and for further processing.
   parts =  stregex( root_name,  '.*([[:digit:]]{4})([[:digit:]]{2})([[:digit:]]{2})-', /extract, /subexpr)
   year = parts[ 1 ]
   month = parts[ 2 ]
   day = parts[ 3 ]
   colorChangeAllowed = 0
   colorChangeNColors = 256 ; This is required to be gt 1 for subsequent operation.

   ; Set up the image variable.
   image = BytArr(720,720)

    ; Create map projection information.  Polar stereographic projections
   mapCoord = Obj_New('MAPCOORD', 111, $
                      SEMIMAJOR_AXIS = 6378137,  $
                      semiminor_axis = 6356752.3142, $
                      CENTER_LONGITUDE = 0.0, $
                      CENTER_LATITUDE = 90.0, LIMIT = [ 0.12, -180., 90.0, 180.0 ] )

    lats = Indgen(9)*10
    lons = Indgen(11)*36
    lonlab = 55

   ; Read the data file.
   OpenR, lun, filename, /GET_LUN, SWAP_IF_BIG_ENDIAN=1
   ReadU, lun, image
   Free_Lun, lun
   image = Reverse(image, 2)
   s = Size(image, /Dimensions)


   ; Pixels values in the image.
   ; Snow free land: 0  ->
   ; Snow-covered land: 1
   ; Sea ice: 2
   ; QC sea ice: 3
   ; QC ocean: 4
   ; QC snow: 5
   ; Unclassifiable water: 253
   ; Outside of map: 254
   ; Open ocean: 255


   ; Define a color object with appropriate colors.
   TVLCT, rr, gg, bb, /Get
   LoadCT, 0, /SILENT
   TVLCT, cgColor('tan', /TRIPLE), 0
   TVLCT, cgColor('cornsilk', /TRIPLE), 1
   TVLCT, cgColor('BLU2', /TRIPLE), 2
   TVLCT, cgColor('BLU3', /TRIPLE), 3
   TVLCT, cgColor('GRN2', /TRIPLE), 4
   TVLCT, cgColor('ivory', /TRIPLE), 5
   TVLCT, cgColor('gray', /TRIPLE), 253
   TVLCT, cgColor('linen', /TRIPLE), 254
   TVLCT, cgColor('YGB3', /TRIPLE), 255
   TVLCT, r, g, b, /Get
   colors = Obj_New('CatColors', COLORPALETTE=[[r], [g], [b]])
   TVLCT, rr, gg, bb


   ; Set up outline and grid for the image.
   outline = Obj_New('Map_Outline', MAP_OBJECT=mapCoord, COLOR=outline_color)
   grid = Obj_New('Map_Grid', MAP_OBJECT=mapCoord, COLOR=grid_color, $
        LATS=lats, LONS=lons, LATLAB=latlab, LONLAB=lonlab)
   mapCoord -> SetProperty, OUTLINE_OBJECT=outline, GRID_OBJECT=grid

   ; Create an info structure containing information for this image.
   info = {directory :   theDirectory, $
           filename:     filename, $
           extension:    theExtension, $
           year:         Fix(year), $
           month:        Fix(month), $
           nsidc_tag:   'nsidc_0046', $
           colorChangeAllowed: colorChangeAllowed, $
           colorChangeNColors: colorChangeNColors, $
           xsize:        s[0], $
           ysize:        s[1], $
           colors:       colors, $
           mapcoord:     mapcoord}

     success = 1
     RETURN, image

 END
