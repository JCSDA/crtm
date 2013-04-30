;*****************************************************************************************************
;+
; NAME:
;       Parse_NSIDC_AMSR_E_L3_SeaIce
;
; PURPOSE:
;
;       The purpose of this routine is to read and obtain information about
;       NSIDC AMSR-E/Aqua Daily L3 12.5 km Brightness Temperature, Sea Ice 
;       Concentration, & Snow Depth Polar Grids.
;       
;          http://nsidc.org/data/docs/daac/ae_si12_12km_tb_sea_ice_and_snow.gd.html
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
;       image = Parse_NSIDC_AMSR_E_L3_SeaIce(filename, variable)
;       
; RETURN_VALUE:
; 
;       image:       The image data in the file.
;       
; ARGUMENTS:
; 
;       filename:    The name of an AMSR-E L3 Sea Ice HDF file. 
;       
;       variable:    The scientific dataset variable to read from the file.
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
;       Written by: David W. Fanning, 6 January 2009.
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
FUNCTION Parse_NSIDC_AMSR_E_L3_SeaIce, filename, variable, INFO=info, SUCCESS=success

   COMPILE_OPT idl2

   ; Simple error handling. Return to caller.
   ON_ERROR, 2

   ; Check parameters.
   success = 0 ; Assume no success
   IF N_Elements(filename) EQ 0 THEN Message, "A filename is a required input parameter."
   IF N_Elements(variable) EQ 0 THEN Message, "A variable name is a required input parameter."

   ; Parse the root file name to determine the parameters that need to be set appropriately.
   ; If this is not a compressed file, with extension .gz, then we will have to add the extension
   ; back to the filename and set the extension to a null string.
   root_name = cgRootName(filename, DIRECTORY=theDirectory, EXTENSION=theExtension)

   ; Pull out information from the filename and the variable for info structure and for further processing.
   product_code = StrMid(root_name, 21, 1)
   version = StrMid(root_name, 22, 2)
   year = StrMid(root_name, 25, 4)
   month = StrMid(root_name, 29, 2)
   day = StrMid(root_name, 31, 2)
   IF StrPos(variable, '_NH_') NE -1 THEN coverage = 'NH' ELSE coverage = 'SH'
   IF StrPos(variable, 'ICE') NE -1 THEN icevar = 1 ELSE icevar = 0

   ; Colors?
   colorChangeNColors = 250
   colorChangeAllowed = 1
   
   ; Open the HDF file.
   hdfID = HDF_SD_Start(filename)
   
   ; Get the variable.
   HDF_SD_VarRead, hdfID, variable, image
   
   ; Close the file
   HDF_SD_End, hdfID
   
   s = Size(image, /DIMENSIONS)
   minImage = MIN(image, MAX=maximage, /NAN)
   IF icevar THEN BEGIN
       IF StrPos(variable, 'ICEDIFF') NE -1 THEN BEGIN
           sclmin = -100
           sclmax = 100
           missing = 110
           landmask = 120
           colorchangeAllowed = 0
           colorChangeNColors = 200
           scale = 1
       ENDIF ELSE BEGIN
           sclmin = 0
           sclmax = 100
           missing = 110
           landmask = 120
           scale = 1
       ENDELSE
   ENDIF ELSE BEGIN
       IF StrPos(variable, 'SNOWDEPTH') NE -1 THEN BEGIN
           sclmin = 0
           sclmax = 100
           missing = 110
           landmask = 120    
           i = Where(image EQ 130, count) ; Open water
           IF count GT 0 THEN image[i] = missing   
           scale = 1
       ENDIF ELSE BEGIN
           sclmin = 0
           sclmax = 330.0 ; Just a guess, but degrees Kelvin, means 60 degrees C. Pretty hot!
           missing = 0
           scale = 10.0
       ENDELSE
   ENDELSE
   
   ; Create map projection information.  Polar stereographic projection
   CASE coverage OF
   
        'NH': BEGIN
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
            END
            
        'SH': BEGIN
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
            END
            
   ENDCASE

   ; Create an info structure about the image.
   fullFileName = Filepath(Root_Dir=theDirectory, root_name + '.' + theExtension + '@' + variable)
   info = {directory :   theDirectory, $
           filename:     fullFileName, $
           displayName:  variable, $
           extension:    theExtension, $
           year:         Fix(year), $
           month:        Fix(month), $
           day:          Fix(day), $
           nsidc_tag:    root_name, $
           missing: missing, $
           colorChangeAllowed: colorChangeAllowed, $
           colorChangeNColors: colorChangeNColors, $
           xsize:        s[0], $
           ysize:        s[1], $
           sclmin:       sclmin, $
           sclmax:       sclmax, $
           mapCoord:     mapCoord }
           
   IF icevar THEN info = Create_Struct(info, 'landmask', landmask)

   ; Return the image, scaled properly and reversed for proper display in IDL.
   success = 1
   RETURN, Reverse(image, 2) / scale

END
