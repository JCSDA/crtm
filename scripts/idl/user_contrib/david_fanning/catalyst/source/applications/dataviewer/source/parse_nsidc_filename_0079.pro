;*****************************************************************************************************
;+
; NAME:
;       PARSE_NSIDC_FILENAME_0079
;
; PURPOSE:
;
;       The purpose of this routine is to read and obtain information about
;       NSIDC images nsidc-0079, Bootstrap Sea Ice Concentrations from Nimbus-7 
;       SMMR and DMST SSM/I images.
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
;       image = Parse_NSIDC_Filename_0079(filename)
;       
; RETURN_VALUE:
; 
;       image:       The image data in the file.
;       
; ARGUMENTS:
; 
;       filename:    The name of an NSIDC-0079 image file. 
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
;       Written by: David W. Fanning, 23 June 2008.
;-
;******************************************************************************************;
;  Copyright (c) 2008, Regents of the University of Colorado. All rights reserved.         ;
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
FUNCTION Parse_NSIDC_Filename_0079, filename, INFO=info, SUCCESS=success

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
   ; Example: bt_200707_f13_v02_n.bin. The data can be either daily or monthly.
   IF StrMid(root_name, 9, 1) EQ '_' THEN monthlyData = 1 ELSE monthlyData = 0
   sensor = StrMid(root_name, 10, 3)
   year = StrMid(root_name, 3, 4)
   month = StrMid(root_name, 7, 2)
   colorChangeAllowed = 1
   colorChangeNColors = 250
   landmask = -999
   
   ; If the data is monthly, the rest of the information will be in a different
   ; location than if it is daily data.
   IF monthlyData THEN BEGIN
        day = ""
        version = StrMid(root_name, 14, 3)
        region = StrMid(root_name, 18, 1)
   ENDIF ELSE BEGIN
        day = StrMid(root_name, 9, 2)
        version = StrMid(root_name, 16, 3)
        region = StrMid(root_name, 20, 1)
   ENDELSE
   
;   Print, 'Sensor: ', sensor
;   Print, 'Year: ', year
;   Print, 'Month: ', month
;   Print, 'Day: ', day
;   Print, 'Version: ', version
;   Print, 'Region: ', region
   
   ; Set up the image variable. Size depends on region.
   CASE StrUpCase(region) OF

     'N': image = IntArr(304, 448)
     'S': image = IntArr(316, 332)
     ELSE: BEGIN
        ok = Dialog_Message('Unknown region: ' + StrUpCase(region))
        RETURN, -1
        END

   ENDCASE

   ; Create map projection information.  Polar stereographic projections
   CASE StrUpCase(region) OF
   
        'N': BEGIN
               mapCoord = Obj_New('MAPCOORD', 106, $
                    SEMIMAJOR_AXIS=6378273L, $
                    SEMIMINOR_AXIS=6356889L, $
                    CENTER_LONGITUDE=-45.0, $
                    CENTER_LATITUDE=90.0)
               mapStruct = mapCoord -> GetMapStructure()
               uv = Map_Proj_Forward(map_Structure=mapstruct, $
                  [168.35D, 102.34D, 350.03D, 279.26], $
                  [30.98D, 31.37d, 34.35, 33.92])
               xrange = [(uv[0,0] + uv[0,3])/2.0, (uv[0,1] + uv[0,2])/2.0]
               yrange = [(uv[1,3] + uv[1,2])/2.0, (uv[1,0] + uv[1,1])/2.0]
               mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
               lats = Indgen(9)*10
               lons = Indgen(11)*36
               lonlab = 55
            END
        'S': BEGIN
               mapCoord = Obj_New('MAPCOORD', 106, $
                    SEMIMAJOR_AXIS=6378273L, $
                    SEMIMINOR_AXIS=6356889L, $
                    CENTER_LONGITUDE=0.0, $
                    CENTER_LATITUDE=-90.0)
               mapStruct = mapCoord -> GetMapStructure()
               xrange = mapStruct.uv_box[[0,2]]
               yrange = mapStruct.uv_box[[1,3]]
               uv = Map_Proj_Forward(map_Structure=mapstruct, $
                  [317.76d, 42.24d, 135.00d, 225.00d], $
                  [-39.23d, -39.23d, -41.45d, -41.45d])
               xrange = [(uv[0,0] + uv[0,3])/2.0, (uv[0,1] + uv[0,2])/2.0]
               yrange = [(uv[1,3] + uv[1,2])/2.0, (uv[1,0] + uv[1,1])/2.0]
               mapCoord -> SetProperty, XRANGE=xrange, YRANGE=yrange
               lats = -Reverse(Indgen(9)*10)
               lons = Indgen(11)*36
               lonlab = -65
            END
   ENDCASE

   ; Set up outline and grid for the image.
   outline = Obj_New('Map_Outline', MAP_OBJECT=mapCoord, COLOR=outline_color)
   grid = Obj_New('Map_Grid', MAP_OBJECT=mapCoord, COLOR=grid_color, $
        LATS=lats, LONS=lons, LATLAB=latlab, LONLAB=lonlab)
   mapCoord -> SetProperty, OUTLINE_OBJECT=outline, GRID_OBJECT=grid
   
   ; Read the data file.
   OpenR, lun, filename, /GET_LUN, SWAP_IF_BIG_ENDIAN=1
   ReadU, lun, image
   Free_Lun, lun

   ; Locate missing values, if any.
   s = Size(image, /Dimensions)
   missing = 1100
   i = Where(image EQ missing, count)

   ; It is possible that these data files have no data in them!
   IF count EQ (s[0] * s[1]) THEN BEGIN
        baseName = File_Basename(filename)
        ok = Dialog_Message(['File ' + baseName, ' has no valid image data.'])
        success = 0
        RETURN, -1
   ENDIF

   temp = Float(image)
   IF count GT 0 THEN temp[i] = !Values.F_NAN
   minImage = MIN(temp, MAX=maximage, /NAN)
   Undefine, temp
   
   ; In these images, values of 0 should be set to missing.
   zeros = Where(image EQ 0, zerocount)
   IF zerocount GT 0 THEN image[zeros] = 1100
   
   ; Scaling factor.
   sclmin = 0.0
   sclmax = 100.0
   
   ; Landmask value
   landmask = 1200

   ; Create an info structure containing information for this image.
   info = {directory :   theDirectory, $
           filename:     filename, $
           extension:    theExtension, $
           sensor:       StrUpCase(sensor), $
           day:          Fix(day), $
           year:         Fix(year), $
           month:        Fix(month), $
           missing:      missing / 10.0, $
           landmask:     landmask / 10.0, $
           nsidc_tag:    'nsidc_0079', $
           colorChangeAllowed: colorChangeAllowed, $
           colorChangeNColors: colorChangeNColors, $
           xsize:        s[0], $
           ysize:        s[1], $
           sclmin:       sclmin, $
           sclmax:       sclmax, $
           mapCoord:     mapCoord }
   
   success = 1  
   RETURN, Reverse(image / 10.0, 2) ; Scaled properly

 END


