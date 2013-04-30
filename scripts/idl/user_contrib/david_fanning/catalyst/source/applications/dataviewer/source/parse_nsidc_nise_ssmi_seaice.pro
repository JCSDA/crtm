;*****************************************************************************************************
;+
; NAME:
;       Parse_NSIDC_NISE_SSMI_SeaIce
;
; PURPOSE:
;
;       The purpose of this routine is to read and obtain information about
;       NSIDC Near Real Time SSM/I Daily Global Sea Ice Concentration and Snow Extent 
;       (in EASE grids).
;       
;          http://nsidc.org/data/nise1.html
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
;       image = Parse_NSIDC_NISE_SSMI_SeaIce(filename, variable)
;       
; RETURN_VALUE:
; 
;       image:       The image data in the file.
;       
; ARGUMENTS:
; 
;       filename:    The name of an NISE-SSMI Sea Ice HDF file. 
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
FUNCTION Parse_NSIDC_NISE_SSMI_SeaIce, filename, variable, INFO=info, SUCCESS=success

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
   root_name = cgRootname(filename, DIRECTORY=theDirectory, EXTENSION=theExtension)

   ; root name example: NISE_SSMIF13_20070810
   ; 
   ; Pull out information from the filename and the variable for info structure and for further processing.
   platform = StrMid(root_name, 9, 3)
   year = StrMid(root_name, 13, 4)
   month = StrMid(root_name, 17, 2)
   day = StrMid(root_name, 19, 2)

   ; Colors?
   colorChangeNColors = 100
   colorChangeAllowed = 0
   
   ; Open the HDF file.
   hdfID = HDF_SD_Start(filename)
   
   ; Get the variable.
   HDF_SD_VarRead, hdfID, variable, image
   
   ; Close the file
   HDF_SD_End, hdfID
   
   s = Size(image, /DIMENSIONS)
   minImage = MIN(image, MAX=maximage, /NAN)
   
   IF StrPos(variable, 'AGE') NE -1 THEN BEGIN
        sclmin = 0
        sclmax = 255
        missing = 255
        colorchangeAllowed = 0
        colorChangeNColors = 254
   ENDIF ELSE BEGIN
        sclmin = 0
        sclmax = 255
        missing = 256
        landmask = 120
        colorchangeAllowed = 1
        colorChangeNColors = 101
        i = Where(image EQ 0, count)
        IF count THEN image[i] = 120
        
   ENDELSE
   
   
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
           missing: 254, $
           colorChangeAllowed: colorChangeAllowed, $
           colorChangeNColors: colorChangeNColors, $
           xsize:        s[0], $
           ysize:        s[1], $
           sclmin:       1, $
           sclmax:       100};, $
          ; mapinfo:      {mapStruct:mapStruct, xrange:xrange, yrange:yrange} }
           

   ; Return the image, scaled properly and reversed for proper display in IDL.
   success = 1
   RETURN, Reverse(image, 2)

END
