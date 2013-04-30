;*****************************************************************************************************
;+
; NAME:
;       PARSE_NSIDC_FILENAME_0071
;
; PURPOSE:
;
;       The purpose of this routine is to read and obtain information about
;       NSIDC images nsidc-0071, Nimbus-7 SMMR Pathfinder Daily EASE-Grid 
;       Brightness Temperature images.
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
;       image = Parse_NSIDC_Filename_0071(filename)
;       
; RETURN_VALUE:
; 
;       image:       The image data in the file.
;       
; ARGUMENTS:
; 
;       filename:    The name of an NSIDC-0071 image file. 
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
FUNCTION Parse_NSIDC_Filename_0071, filename, INFO=info, SUCCESS=success

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
   gpd = StrMid(root_name, 10, 2)
   year = StrMid(root_name, 12, 4)
   doy = StrMid(root_name, 16, 3)
   direction = StrMid(root_name, 19, 1)
   freq = StrMid(root_name, 21, 2)

   ; Careful of timing images in the mix.
   IF StrUpCase(freq) EQ 'TI' THEN BEGIN
         timeimg = 1
         freq = '00'
         colorChangeAllowed = 0
   ENDIF ELSE BEGIN
        timeimg = 0
        colorChangeAllowed = 1
   ENDELSE
   polarization = (timeimg) ? "" : StrMid(root_name, 23, 1)
   colorChangeNColors = 250

   ; Image size depends on what part of the world we are viewing.
   CASE StrUpCase(gpd) OF
       'ML': image = (timeimg) ? INTARR(1383, 586) : UINTARR(1383, 586)
       ELSE: image = (timeimg) ? INTARR(721,721) : UINTARR(721, 721)
   ENDCASE
   
   ; Create map projection information.  Polar views are Lambert Equal Area Azimuthal.
   ; Otherwise, Albers equal area.
   CASE StrUpCase(gpd) OF
        'NL': BEGIN
               mapCoord = Obj_New('MAPCOORD', 111, SPHERE_RADIUS=6371228L, CENTER_LONGITUDE=0.0, $
                    CENTER_LATITUDE=90.0, LIMIT=[0.,-180., 90.0, 180.0])
               lats = Indgen(9)*10
               lons = Indgen(11)*36
            END
        'SL': BEGIN
               mapCoord = Obj_New('MAPCOORD', 111, SPHERE_RADIUS=6371228L, CENTER_LONGITUDE=0.0, $
                    CENTER_LATITUDE=-90.0, LIMIT=[-90.,-180., 0.0, 180.0])
               lats = -Reverse(Indgen(9)*10)
               lons = Indgen(11)*36
            END
        'ML': BEGIN
               mapCoord = Obj_New('MAPCOORD', 103, SEMIMAJOR_AXIS=6371228L, SEMIMINOR_AXIS=6371228L,CENTER_LONGITUDE=0.0, $
                    CENTER_LATITUDE=0.0, STANDARD_PAR1=45, STANDARD_PAR2=-44.5)
               lats = Indgen(13)*10 - 60
               lons = Indgen(11)*36
               latlab = 18
               lonlab = 0
             END
   ENDCASE

   ; Set up outline and grid for the image.
   outline = Obj_New('Map_Outline', MAP_OBJECT=mapCoord, COLOR=outline_color)
   grid = Obj_New('Map_Grid', MAP_OBJECT=mapCoord, COLOR=grid_color, $
        LATS=lats, LONS=lons, LATLAB=latlab, LONLAB=lonlab)
   mapCoord -> SetProperty, OUTLINE_OBJECT=outline, GRID_OBJECT=grid

   ; Read the image data.
   IF StrUpCase(theExtension) EQ 'GZ' THEN compress = 1 ELSE compress = 0
   OpenR, lun, filename, /GET_LUN, COMPRESS=compress, SWAP_IF_BIG_ENDIAN=1
   ReadU, lun, image
   Free_Lun, lun

   ; Locate missing values, if any.
   s = Size(image, /Dimensions)
   missing = (timeimg) ? -32768 : 0
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

   ; Scaling will depend on frequency and polarization.
   CASE freq OF
     '00': BEGIN ; Time images.
           sclmin = minImage
           sclmax = maxImage
           END

      '06': BEGIN
            IF StrUpCase(polarization) EQ 'H' THEN BEGIN
               sclmin = 70
               sclmax = 320
            ENDIF ELSE BEGIN
               sclmin = 140
               sclmax = 320
            ENDELSE
            END

      '10': BEGIN
            IF StrUpCase(polarization) EQ 'H' THEN BEGIN
               sclmin = 80
               sclmax = 290
            ENDIF ELSE BEGIN
               sclmin = 140
               sclmax = 320
            ENDELSE
            END

      '18': BEGIN
            IF StrUpCase(polarization) EQ 'H' THEN BEGIN
               sclmin = 70
               sclmax = 320
            ENDIF ELSE BEGIN
               sclmin = 140
               sclmax = 320
            ENDELSE
            END

      '21': BEGIN
            IF StrUpCase(polarization) EQ 'H' THEN BEGIN
               sclmin = 70
               sclmax = 320
            ENDIF ELSE BEGIN
               sclmin = 140
               sclmax = 320
            ENDELSE
            END

      '37': BEGIN
            IF StrUpCase(polarization) EQ 'H' THEN BEGIN
               sclmin = 120
               sclmax = 290
            ENDIF ELSE BEGIN
               sclmin = 170
               sclmax = 300
            ENDELSE
            END

      ELSE: BEGIN
            IF StrUpCase(polarization) EQ 'H' THEN BEGIN
               sclmin = 70
               sclmax = 320
            ENDIF ELSE BEGIN
               sclmin = 140
               sclmax = 320
            ENDELSE
            END
   ENDCASE

   ; Create an info structure about the image.
   info = {directory :   theDirectory, $
           filename:     filename, $
           extension:    theExtension, $
           gpd:          StrUpCase(gpd), $
           year:         Fix(year), $
           doy:          Fix(doy), $
           missing:      missing, $
           nsidc_tag:    'nsidc_0071', $
           direction:    StrUpCase(direction), $
           frequency:    Fix(freq), $
           polarization: polarization, $
           colorChangeAllowed: colorChangeAllowed, $
           colorChangeNColors: colorChangeNColors, $
           xsize:        s[0], $
           ysize:        s[1], $
           sclmin:       sclmin, $
           sclmax:       sclmax, $
           mapCoord:     mapCoord }

   ; Return the image, scaled properly and reversed for proper display in IDL.
   success = 1
   RETURN, (timeimg) ? Reverse(image, 2) : Reverse(image / 10.0, 2) ; Scaled properly

END

