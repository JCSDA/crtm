;*****************************************************************************************************
;+
; NAME:
;       PARSE_NSIDC_FILENAME
;
; PURPOSE:
;
;       The purpose of this routine is to parse the name of an NSIDC image
;       file so that the program can call the specific file reading program
;       that returns the image and information about the image. When this 
;       information is provided, this program creates and returns an 
;       NSIDC image object.
;       
;       If you wanted to modify the DataViewer program, for example, to
;       read another type of NSIDC image, you would first add a section
;       to this file that could identify an image of that type from its
;       filename. Then you would call the reader for that program (which
;       you would also have to write). The reader would read the file,
;       extract the image and information about the image, which this 
;       program can then package into the NSIDC image object.
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
;       nsidcImage = Parse_NSIDC_Filename(filename)
;       
; RETURN_VALUE:
; 
;       nsidcImage:  Either an NSIDC_IMAGE object, or the actual image data, depending
;                    upon whether the RETURN_IMAGE keyword is set.
;       
; ARGUMENTS:
; 
;       filename:    The name of an NSIDC image file. Or, alternatively, any
;                    the name of any image file capable of being read by READ_IMAGE. 
;                    That is JPEG, PNG, TIFF, etc. files.
;                    
; INPUT_KEYWORDS:
; 
;       RETURN_IMAGE:  Set this keyword to have the function return the actual image
;                      data, rather than packaging the image up into an NSIDC_IMAGE
;                      object.
;                      
; OUTPUT_KEYWORDS:
; 
;        INFO:         An output structure containing information about the image. The structure
;                      varies, depending upon the image, but a typical structure looks like this:
;                      
;                        info = {directory :   "", $      ; The image directory
;                                filename:     "", $      ; The image file name
;                                extension:    "", $      ; The image file extension
;                                gpd:          "", $      ; The geographical location
;                                year:         0, $       ; The year
;                                doy:          0, $       ; The day-of-year.
;                                missing:      0.0, $     ; The missing value.
;                                nsidc_tag:    "", $      ; An image tag, e.g., nsidc-0032
;                                direction:    "", $      ; The direction of satellite travel
;                                frequency:    "", $      ; The frequency of radiation sampled
;                                polarization: "", $      ; The image polarization
;                                colorChangeAllowed: 0, $ ; Set to 1 if color change allowed for image
;                                colorChangeNColors: 0, $ ; Number of colors that can change in image
;                                xsize:        0, $       ; The X size of the image
;                                ysize:        0, $       ; The Y size of the image
;                                sclmin:       0.0, $     ; The minimum value for image scaling.
;                                sclmax:       0.0, $     ; The maximum value for image scaling.
;                                mapinfo:      {mapStruct:mapStruct, xrange:xrange, yrange:yrange} } ; Map information.  
;                                
; 
;        SUCCESS:      Set to 1 if the file was successfully read, otherwise to 0.
;       
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 23 June 2008.
;       Added nsidc-0342 SSM/I Near Real Time Brightness Temperatures. 22 June 2009. DWF.
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
FUNCTION Parse_NSIDC_Filename, filename, INFO=info, SUCCESS=success, RETURN_IMAGE=return_image

   COMPILE_OPT idl2

   ; Simple error handling. Return to caller.
   ON_ERROR, 2
   
   ; Check parameters.
   success = 0 ; Assume no success
   IF N_Elements(filename) EQ 0 THEN Message, "A filename is a required input parameter."
   
   ; Set up colors for reading data.
   missing_color = CatGetDefault('DATAVIEWER_MISSING_COLOR')
   oob_low_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR')
   oob_high_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR')
   annotate_color = CatGetDefault('DATAVIEWER_ANNOTATE_COLOR')
   landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
   grid_color = CatGetDefault('DATAVIEWER_GRID_COLOR')
   vector_color = CatGetDefault('DATAVIEWER_VECTOR_COLOR')
   outline_color = CatGetDefault('DATAVIEWER_OUTLINE_COLOR')
   
   ; Does the filename have a "@" symbol in the name. It if does, then this is a special
   ; type of file (HDF, netCDF, CDF, etc.)
   IF StrPos(filename, '@') NE -1 THEN BEGIN
   
       parts = StrSplit(filename, '@', /EXTRACT)
       filename = parts[0]
       variable = parts[1]
       rootName = cgRootName(filename, EXTENSION=ext)
       
       ; HDFEOS file processed in the same way as HDF files.
       IF StrUpCase(ext) EQ 'HDFEOS' THEN ext = 'HDF'
       
       CASE StrUpCase(ext) OF
       
            'HDF': BEGIN
            
                   IF StrUpCase(StrMid(rootname, 0, 16)) EQ 'AMSR_E_L3_SEAICE' THEN BEGIN
                        theImage = Parse_NSIDC_AMSR_E_L3_SeaIce(filename, variable, INFO=info, SUCCESS=success)
                        IF ~success THEN RETURN, -1
                        mapCoord = info.mapCoord
                        outline = Obj_New('Map_Outline', MAP_OBJECT=mapCoord, COLOR=outline_color)
                        grid = Obj_New('Map_Grid', MAP_OBJECT=mapCoord, COLOR=grid_color, /AUTODRAW)
                        mapCoord -> SetProperty, OUTLINE_OBJECT=outline, GRID_OBJECT=grid
                        theImageObject = Obj_New('NSIDC_Image', theImage, MISSING_COLOR=missing_color, $
                           MISSING_VALUE=info.missing, $
                           SCLMIN=info.sclmin, SCLMAX=info.sclmax, FILENAME=info.filename, NCOLORS=250, $
                           COLORCHANGEALLOWED=info.colorChangeAllowed, COLORCHANGENCOLORS=info.colorChangeNColors, $
                           NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord, DISPLAYNAME=info.displayName)
                        index = Where(Tag_Names(info) EQ 'LANDMASK')
                        IF index NE -1 THEN theImageObject -> SetProperty, LANDMASK_VALUE=info.landmask
                   ENDIF

                END ; of HDF file extension.
            
            'CDF': BEGIN
                void = Dialog_Message('DataViewer does not currently know how to parse this CDF file.')
                RETURN, ""
                END
                
             'NETCDF': BEGIN
                void = Dialog_Message('DataViewer does not currently know how to parse this netCDF file.')
                RETURN, ""
                 END
                 
            ELSE: Message, 'Do not know how to process this kind of special file.'
       ENDCASE
   ENDIF
   
   
   ; As a first test, we check the file extension to see if this is a file IDL
   ; knows how to read (e.g., JPEG, TIFF, PNG, etc.)
   root_name = cgRootName(filename, DIRECTORY=theDirectory, EXTENSION=theExtension)
   check = Query_Image(filename, CHANNELS=channels, HAS_PALETTE=palette, TYPE=image_type) 
   IF check THEN BEGIN
   
   
           IF image_type EQ 'TIFF' THEN BEGIN
             ok = Query_TIFF(filename, fileInfo, GEOTIFF=geotiff)
             IF ok THEN BEGIN
                CASE fileInfo.channels OF
                   3: theImage = Read_TIFF(filename, _EXTRA=extra, ORIENTATION=orientation)
                   ELSE: theImage = Read_TIFF(filename, r, g, b, _EXTRA=extra, ORIENTATION=orientation)
                ENDCASE
                IF fileInfo.has_palette EQ 1 THEN colorPalette = [[r], [g], [b]]
                IF orientation EQ 1 THEN BEGIN
                    dims = Image_Dimensions(theImage, YINDEX=yindex)
                    theImage = Reverse(Temporary(theImage), yindex+1)
                ENDIF
             ENDIF
             IF Size(geotiff, /TNAME) EQ 'STRUCT' THEN BEGIN
                mapCoord = GeoCoord(filename, SUCCESS=success, /SILENT)
                grid = Obj_New('Map_Grid', MAP_OBJECT=mapCoord, COLOR=grid_color, /AUTODRAW)
                countries = Obj_New('Map_Outline', MAP_OBJECT=mapCoord, /COUNTRIES, /COASTS, COLOR=outline_color)
                mapCoord -> SetProperty, MAP_OVERLAY=countries, OVERLAY_POSITION=0
                mapCoord -> SetProperty, MAP_OVERLAY=grid, OVERLAY_POSITION=1
                IF success EQ 1 THEN BEGIN
                    IF fileInfo.has_palette EQ 1 THEN colors = Obj_New('CatColors', COLORPALETTE=colorpalette)
                    IF Obj_Valid(colors) THEN BEGIN
                        theImageObject = Obj_New('NSIDC_Image', theImage, FILENAME=filename, COORD_OBJECT=mapCoord, $
                           COLORCHANGENCOLORS=256, COLORCHANGEALLOWED=colorChangeAllowed, NSIDC_TAG=image_type, $
                           COLOR_OBJECT=colors, SCALETYPE=scaletype, GRID_COLOR=grid_color, OUTLINE_COLOR=outline_color)
                        
                    ENDIF ELSE BEGIN
                        theImageObject = Obj_New('NSIDC_Image', theImage, FILENAME=filename, COORD_OBJECT=mapCoord, $
                           COLORCHANGENCOLORS=256, COLORCHANGEALLOWED=colorChangeAllowed, NSIDC_TAG=image_type, $
                           COLOR_OBJECT=colors, SCALETYPE=scaletype, GRID_COLOR=grid_color, OUTLINE_COLOR=outline_color)
                    ENDELSE
                    
                    imageIsObject = 1
                ENDIF
             ENDIF
           ENDIF ELSE BEGIN
                theImage = Read_Image(filename, r, g, b, IMAGE_INDEX=0)
                IF N_Elements(theImage) EQ 1 THEN Message, 'Image file can not be read with READ_IMAGE.'
                imageIsObject = 0
           ENDELSE
           
       ; Some PNG files (from ImageMagick, for example) are created with 16-bits per channel.
       ; Convert this to 8-bits per channel, and also remove any alpha channel for this application.
       IF StrUpCase(theExtension) EQ 'PNG' THEN BEGIN
            IF Size(theImage, /TNAME) NE 'BYTE' THEN theImage = BytScl(theImage)
            IF Size(theImage, /N_DIMENSIONS) GT 3 THEN BEGIN
                dims = Image_Dimensions(theImage, TRUEINDEX=trueindex)
                CASE trueIndex OF
                    0: theImage = theImage[0:2, *, *]
                    1: theImage = theImage[*, 0:2, *]
                    2: theImage = theImage[*, *, 0:2]
                ENDCASE
                channels = 3
            ENDIF
       ENDIF
       IF ~imageIsObject THEN BEGIN
           CASE channels OF
                1: BEGIN
                   IF palette THEN BEGIN
                        thisPalette = [[r], [g], [b]]
                        colors = Obj_New('COLORTOOL', COLORPALETTE=thisPalette)
                        colorChangeAllowed = 0
                        scaletype = 'NONE'
                        ncolors = 250
                   ENDIF ELSE BEGIN
                        ncolors = 250
                        colorChangeAllowed = 1
                        scaletype = 'LINEAR'
                   ENDELSE
                   theImageObject = Obj_New('NSIDC_Image', theImage, FILENAME=filename, $
                        COLORCHANGENCOLORS=ncolors, COLORCHANGEALLOWED=colorChangeAllowed, NSIDC_TAG=image_type, $
                        COLOR_OBJECT=colors, SCALETYPE=scaletype)
                   success = 1
                   END
                   
                3: BEGIN
                   theImageObject = Obj_New('NSIDC_Image', theImage, FILENAME=filename, $
                       COLORCHANGEALLOWED=0, NSIDC_TAG=image_type)
                   success = 1
                   END
                   
                ELSE: Message, 'Cannot read image with ' + StrTrim(channels,2) + ' channels.'
           ENDCASE
       ENDIF
       IF Keyword_Set(return_image) THEN RETURN, theImage ELSE RETURN, theImageObject    
   ENDIF

   ; Parse the root file name to determine the parameters that need to be set appropriately.
   ; If this is not a compressed file, with extension .gz, then we will have to add the extension
   ; back to the filename and set the extension to a null string.
   IF StrUpCase(theExtension) NE 'GZ' THEN BEGIN
        root_name = root_name + '.' + theExtension
        theExtension = ''
   ENDIF
   
   ; The first two letters of the root_name can be used as the first division point.
   firstTwoLetters = StrUpCase(StrMid(root_name, 0, 2))
   
   ; DMSP SSM/I Gridded Brightness Temperatures (e.g, nsidc_0081).
   IF firstTwoLetters EQ 'NT' THEN BEGIN
   
        ; Is this near real time data?
        nearRealTime = StrUpCase(StrMid(root_name, 16, 3))
        IF nearRealTime EQ 'NRT' THEN BEGIN
            theImage = Parse_NSIDC_Filename_0081(filename, INFO=info, SUCCESS=success)
            IF ~success THEN RETURN, -1
            mapCoord = info.mapCoord
            theImageObject = Obj_New('NSIDC_Image', theImage, MISSING_VALUE=info.missingValue, $
                SCLMIN=info.sclmin, SCLMAX=info.sclmax, FILENAME=info.filename, NCOLORS=250, $
                COLORCHANGEALLOWED=info.colorChangeAllowed, COLORCHANGENCOLORS=info.colorChangeNColors, $
                NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord, LANDMASK_VALUE=info.landmaskValue)
        ENDIF ELSE BEGIN
        
            theImage = Parse_NSIDC_Filename_0051(filename, INFO=info, SUCCESS=success)
            IF ~success THEN RETURN, -1
            mapCoord = info.mapCoord
            theImageObject = Obj_New('NSIDC_Image', theImage, MISSING_VALUE=info.missingValue, $
                SCLMIN=info.sclmin, SCLMAX=info.sclmax, FILENAME=info.filename, NCOLORS=250, $
                COLORCHANGEALLOWED=info.colorChangeAllowed, COLORCHANGENCOLORS=info.colorChangeNColors, $
                NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord, LANDMASK_VALUE=info.landmaskValue)
        ENDELSE 
        
   ENDIF
   
   ; TB - DMSP SSM/I Gridded Brightness Temperatures (e.g., nsidc_0001, nsidc_0080).
   IF firstTwoLetters EQ 'TB' THEN BEGIN
      
        ; Is this near real time data?
        nearRealTime = StrUpCase(StrMid(root_name, 16, 3))
        IF nearRealTime EQ 'NRT' THEN BEGIN
            theImage = Parse_NSIDC_Filename_0080(filename, INFO=info, SUCCESS=success)
            IF ~success THEN RETURN, -1
            mapCoord = info.mapCoord
            theImageObject = Obj_New('NSIDC_Image', theImage, MISSING_VALUE=info.missing, $
                SCLMIN=info.sclmin, SCLMAX=info.sclmax, FILENAME=info.filename, NCOLORS=250, $
                COLORCHANGEALLOWED=info.colorChangeAllowed, COLORCHANGENCOLORS=info.colorChangeNColors, $
                NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord)
                
        ENDIF ELSE BEGIN
        
            theImage = Parse_NSIDC_Filename_0001(filename, INFO=info, SUCCESS=success)
            IF ~success THEN RETURN, -1
            mapCoord = info.mapCoord
            theImageObject = Obj_New('NSIDC_Image', theImage, MISSING_VALUE=info.missing, $
                SCLMIN=info.sclmin, SCLMAX=info.sclmax, FILENAME=info.filename, NCOLORS=250, $
                COLORCHANGEALLOWED=info.colorChangeAllowed, COLORCHANGENCOLORS=info.colorChangeNColors, $
                NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord)
                
        ENDELSE
   ENDIF
   
   ; EA - EASE Gridded files, either SSM/I Brightness Temperature (nsidc_0032)
   ; or SMMR Brightness Temperatures (nsidc_0071)
   IF firstTwoLetters EQ 'EA' THEN BEGIN
   
        easeType = StrMid(root_name, 5, 4)
        CASE StrUpCase(easeType) OF
        
            'SMMR': BEGIN
                theImage = Parse_NSIDC_Filename_0071(filename, INFO=info, SUCCESS=success)
                IF ~success THEN RETURN, -1
                mapCoord = info.mapCoord
                theImageObject = Obj_New('NSIDC_Image', theImage,MISSING_VALUE=info.missing, $
                     SCLMIN=info.sclmin, SCLMAX=info.sclmax, FILENAME=info.filename, NCOLORS=250, $
                     COLORCHANGEALLOWED=info.colorChangeAllowed, COLORCHANGENCOLORS=info.colorChangeNColors, $
                     NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord)
               END
               
            ELSE: BEGIN
                theImage = Parse_NSIDC_Filename_0032(filename, INFO=info, SUCCESS=success)
                IF ~success THEN RETURN, -1
                mapCoord = info.mapCoord
                theImageObject = Obj_New('NSIDC_Image', theImage, MISSING_VALUE=info.missing, $
                     SCLMIN=info.sclmin, SCLMAX=info.sclmax, FILENAME=info.filename, NCOLORS=250, $
                     COLORCHANGEALLOWED=info.colorChangeAllowed, COLORCHANGENCOLORS=info.colorChangeNColors, $
                     NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord)
                     
               END
               
        ENDCASE
   ENDIF
   
   ; ID - AMSR-E Gridded Brightness Temperatures in Quarter-Degree Grids (nsidc_0302)
   ; or in EASE grids (nsidc_0301). Added Near Real Time SSM/I (nsidc_0342) data, too.
   IF firstTwoLetters EQ 'ID' THEN BEGIN
   
        IF StrMid(root_name, 0, 4) EQ 'ID2-' THEN BEGIN
                theImage = Parse_NSIDC_Filename_0342(filename, INFO=info, SUCCESS=success)
                IF ~success THEN RETURN, -1
                mapCoord = info.mapCoord
                theImageObject = Obj_New('NSIDC_Image', theImage, MISSING_VALUE=info.missing, $
                     SCLMIN=info.sclmin, SCLMAX=info.sclmax, FILENAME=info.filename, NCOLORS=250, $
                     COLORCHANGEALLOWED=info.colorChangeAllowed, COLORCHANGENCOLORS=info.colorChangeNColors, $
                     NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord)
        ENDIF
        
        
        IF StrPos(root_name, 'D.25') NE -1 THEN BEGIN
        
                theImage = Parse_NSIDC_Filename_0302(filename, INFO=info, SUCCESS=success)
                IF ~success THEN RETURN, -1
                mapCoord = info.mapCoord
                theImageObject = Obj_New('NSIDC_Image', theImage, MISSING_VALUE=info.missing, $
                     SCLMIN=info.sclmin, SCLMAX=info.sclmax, FILENAME=info.filename, NCOLORS=250, $
                     COLORCHANGEALLOWED=info.colorChangeAllowed, COLORCHANGENCOLORS=info.colorChangeNColors, $
                     NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord)
         ENDIF 

         IF (StrPos(root_name, 'D.25') EQ -1) AND StrMid(root_name, 0, 4) EQ 'ID2r' THEN BEGIN
                theImage = Parse_NSIDC_Filename_0301(filename, INFO=info, SUCCESS=success)
                IF ~success THEN RETURN, -1
                mapCoord = info.mapCoord
                theImageObject = Obj_New('NSIDC_Image', theImage, MISSING_VALUE=info.missing, $
                     SCLMIN=info.sclmin, SCLMAX=info.sclmax, FILENAME=info.filename, NCOLORS=250, $
                     COLORCHANGEALLOWED=info.colorChangeAllowed, COLORCHANGENCOLORS=info.colorChangeNColors, $
                     NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord)
         ENDIF
               
   ENDIF
   
   ; BT Bootstrap Sea Ice Concentration (nsidc-0079).
   IF firstTwoLetters EQ 'BT' THEN BEGIN
        theImage = Parse_NSIDC_Filename_0079(filename, INFO=info, SUCCESS=success)
        IF ~success THEN RETURN, -1
        mapCoord = info.mapCoord
        theImageObject = Obj_New('NSIDC_Image', theImage, MISSING_VALUE=info.missing, $
            SCLMIN=info.sclmin, SCLMAX=info.sclmax, FILENAME=info.filename, NCOLORS=250, $
            COLORCHANGEALLOWED=info.colorChangeAllowed, LANDMASK_VALUE=info.landmask, $
            COLORCHANGENCOLORS=info.colorChangeNColors, NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord)
   ENDIF

   ; NSIDC-0046 data.
   root_name = cgRootName(root_name, EXTENSION=theExtension)
   IF StrUpCase(theExtension) EQ 'SI' THEN BEGIN
        theImage = Parse_NSIDC_Filename_0046(filename, INFO=info, SUCCESS=success)
        IF ~success THEN RETURN, -1
        mapCoord = info.mapCoord
        colors = info.colors
        
        ; Turn color bars off.
        CatSetDefault, 'DATAVIEWER_COLORBARS_OFF', 1
        theImageObject = Obj_New('NSIDC_Image', theImage, FILENAME=info.filename, NCOLORS=256, $
            COLORCHANGEALLOWED=0, COLORCHANGENCOLORS=info.colorChangeNColors, CB_TYPE=2, $
            NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord, COLOR_OBJECT=colors)
   ENDIF

   ; Ice motion data.
   iceMotion = StrMid(root_name, 0, 9)
   IF StrUpCase(iceMotion) EQ 'ICEMOTION' THEN BEGIN
        theImage = Parse_NSIDC_Filename_0116(filename, INFO=info, SUCCESS=success)
        IF ~success THEN RETURN, -1
        mapCoord = info.mapCoord
        
        ; Turn color bars off and grid on.
        CatSetDefault, 'DATAVIEWER_COLORBARS_OFF', 1
        CatSetDefault, 'DATAVIEWER_MAP_GRID_ON', 1
        colors = Obj_New('CatColors')
        colors -> LoadCT, 0
        colors -> LoadColor, 'light gray', 0
        theImageObject = Obj_New('NSIDC_Image', theImage, FILENAME=info.filename, NCOLORS=250, $
            COLORCHANGEALLOWED=0, COLORCHANGENCOLORS=info.colorChangeNColors, CB_TYPE=2, $
            NSIDC_TAG=info.nsidc_tag, COORD_OBJECT=mapCoord, COLOR_OBJECT=colors)
   ENDIF

   ; Are we successful so far?
   IF success EQ 0 THEN BEGIN
        ok = Dialog_Message(['Cannot find code to read this image file:', filename], /ERROR)
        RETURN, -1
    ENDIF
   
   ; Otherwise, return the image or image object, whichever is requested.
   IF Keyword_Set(return_image) THEN RETURN, theImage ELSE RETURN, theImageObject
    
END