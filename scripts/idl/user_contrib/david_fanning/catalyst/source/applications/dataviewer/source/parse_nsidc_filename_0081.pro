;*****************************************************************************************************
;+
; NAME:
;       PARSE_NSIDC_FILENAME_0081
;
; PURPOSE:
;
;       The purpose of this routine is to read and obtain information about
;       NSIDC images nsidc-0081, Near Real-Time DMSP SSM/I Daily Polar Gridded 
;       Brightness Temperatures images.
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
;       image = Parse_NSIDC_Filename_0081(filename)
;       
; RETURN_VALUE:
; 
;       image:       The image data in the file.
;       
; ARGUMENTS:
; 
;       filename:    The name of an NSIDC-0081 image file. 
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
;       Written by: David W. Fanning, 17 February 2009.
;-
;******************************************************************************************;
;  Copyright (c) 2009, Regents of the University of Colorado. All rights reserved.         ;
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
FUNCTION Parse_NSIDC_Filename_0081, filename, INFO=info, SUCCESS=success

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
   sensor = StrMid(root_name, 12, 3)
   year = StrMid(root_name, 3, 4)
   month = StrMid(root_name, 7, 2)
   day = StrMid(root_name, 9, 2)
   region = StrMid(root_name, 20, 1) ; "S" or "N"
   colorChangeAllowed = 1
   colorChangeNColors = 250

   ; Set up the image variable. Size depends on region and frequency.
   CASE StrUpCase(region) OF

     'N': image = BytArr(304,448)

     'S': image = BytArr(316,332)

   ENDCASE

   ; Create map projection information.  Polar stereographic projections
   CASE StrUpCase(region) OF
   
        'N': BEGIN
               mapCoord = Obj_New('MapCoord', 106, $
                    SEMIMAJOR_AXIS=6378273L, $
                    SEMIMINOR_AXIS=6356889L, $
                    CENTER_LONGITUDE=-45.0, $
                    CENTER_LATITUDE=90.0)
                    
               uv = Map_Proj_Forward(Map_Structure=mapCoord->GetMapStructure(), $
                  [168.35D, 102.34D, 350.03D, 279.26], $
                  [30.98D, 31.37d, 34.35, 33.92])
               xrange = [uv[0,0], uv[0,1]]
               yrange = [uv[1,3], uv[1,0]]
               mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
               lats = Indgen(9)*10
               lons = Indgen(11)*36
            END
        'S': BEGIN
               mapCoord = Obj_New('MapCoord', 106, $
                    SEMIMAJOR_AXIS=6378273L, $
                    SEMIMINOR_AXIS=6356889L, $
                    CENTER_LONGITUDE=0.0, $
                    CENTER_LATITUDE=-90.0)
               uv = Map_Proj_Forward(Map_Structure=mapCoord->GetMapStructure(), $
                  [317.76d, 42.24d, 135.00d, 225.00d], $
                  [-39.23d, -39.23d, -41.45d, -41.45d])
               xrange = [uv[0,0], uv[0,1]]
               yrange = [uv[1,3], uv[1,0]]
               mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
               lats = -Reverse(Indgen(9)*10)
               lons = Indgen(11)*36
            END
   ENDCASE
   
   ; Read the data file.
   OpenR, lun, filename, /GET_LUN, SWAP_IF_BIG_ENDIAN=1
   header = bytarr(300)
   ReadU, lun, header, image
   Free_Lun, lun

   ; Pixels in the image.
   ; Missing pixels = 255
   ; Land pixels = 254
   ; Land boundary pixels = 253
   ; Pole hole = 251

   ; Locate missing values, if any.
   s = Size(image, /Dimensions)
   missing = 255
   missingIndices = Where(image EQ missing, missingCnt)
   temp = Float(image)
   IF missingCnt GT 0 THEN temp[missingIndices] = 0
   
   poleHoleIndices = Where(image EQ 251, poleCnt)
   IF poleCnt GT 0 THEN temp[poleHoleIndices] = 0
   
   landIndices = Where(image EQ 254, landCnt)
   IF landCnt GT 0 THEN temp[landIndices] = 0
   
   boundaryIndices = Where(image EQ 253, boundaryCnt)
   IF boundaryCnt GT 0 THEN temp[boundaryCnt] = 0
   
   image = image / 2.5 ; Scaled properly
   IF missingCnt GT 0 THEN image[missingIndices] = 255
   IF poleCnt GT 0 THEN image[poleHoleIndices] = 255
   IF landCnt GT 0 THEN image[landIndices] = 252
   IF boundaryCnt GT 0 THEN  image[boundaryIndices] = 252
   
   ; Reverse the image.
   image = Reverse(image, 2)
   
   ; It is possible that these data files have no data in them!
   IF missingCnt EQ (s[0] * s[1]) THEN BEGIN
        baseName = File_Basename(filename)
        ok = Dialog_Message(['File ' + baseName, ' has no valid image data.'])
        success = 0
        RETURN, -1
   ENDIF
   
   ; Data concentraions from 0  to 100, or from 0 to 250 in the file.
   sclmin = 0
   sclmax = 100

   ; Set up outline and grid for the image.
   outline = Obj_New('Map_Outline', MAP_OBJECT=mapCoord, COLOR=outline_color)
   grid = Obj_New('Map_Grid', MAP_OBJECT=mapCoord, COLOR=grid_color, $
        LATS=lats, LONS=lons)
   mapCoord -> SetProperty, OUTLINE_OBJECT=outline, GRID_OBJECT=grid
   
   ; Create an info structure containing information for this image.
   info = {directory :   theDirectory, $
           filename:     filename, $
           extension:    theExtension, $
           sensor:       StrUpCase(sensor), $
           year:         Fix(year), $
           month:        Fix(month), $
           missingValue:      255, $
           landmaskValue:     252, $
           nsidc_tag:   'nsidc_0081', $
           colorChangeAllowed: colorChangeAllowed, $
           colorChangeNColors: colorChangeNColors, $
           xsize:        s[0], $
           ysize:        s[1], $
           sclmin:       sclmin, $
           sclmax:       sclmax, $
           mapcoord:     mapcoord}

     success = 1
     RETURN, image

 END


