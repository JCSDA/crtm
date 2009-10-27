;+
; NAME:
;       READ_TOMS_AEROSOL
;
; PURPOSE:
;
;       This is a demonstration program, used in the TOMS Tutorial
;       (http://www.dfanning.com/graphics_tips/toms_tutorial.html),
;       to show how to write a program that looks the same on the
;       display and in a PostScript file. TOMS satellite data is
;       overlaid on a map projection.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Graphics
;
; CALLING SEQUENCE:
;
;       Read_TOMS_Aerosol, filename
;
; ARGUMENTS:
;
;       filename:   The name of the TOMS data file. If not provided, the user
;                   will be prompted for the name of the file.
;
; KEYWORDS:
;
;       DATA:       Set this keyword to a named variable that on return will
;                   contain a 288 by 180 array of floating point values. Missing
;                   data is indicated by !Values.F_NAN.
;
;       HEADER:     Set this keyword to a named variable that on return will
;                   contain the first three lines of the TOMS data set.
;
;       LIMIT:      A four-element input vector that limits the extent of the map
;                   projection. Identical to the LIMIT keyword for MAP_SET.
;                   The form of the keyword is:
;
;                       LIMIT = [latmin, lonmin, latmax, lonmax]
;
;       PS:         Set this keyword to 1 if you wish to create PostScript output
;                   instead of sending graphics output to the display.
;
; COMMON BLOCKS:
;
;       None
;
; RESTRICTIONS:
;
;       TOMS data can be freely obtained from NASA at http://toms.gsfc.nasa.gov/.
;
;       Requires numerious files from the Coyote Library:
;
;           http://www.dfanning.com/programs/coyoteprograms.zip
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 18 January 2006
;-
;
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
PRO Read_TOMS_Aerosol, filename, DATA=data, HEADER=header, LIMIT=limit, PS=ps

   ; Error handling
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      IF N_Elements(lun) NE 0 THEN Free_Lun, lun ; Free file logical unit number.
      IF N_Elements(theState) NE 0 THEN Device, Decomposed=theState ; Preserve color mode.
      RETURN
   ENDIF

   ; Open the file for reading.
   IF N_Elements(filename) EQ 0 THEN BEGIN
      filename = Dialog_Pickfile(Filter='*.txt', Title='Select EP/TOMS file for READING...')
      IF filename EQ "" THEN RETURN
   ENDIF
   OpenR, lun, filename, /Get_Lun

   ; First three lines are header information.
   header = StrArr(3)
   READF, lun, header

   ; TOMS data is a floating point array of [288,180]. Data is on 1 degree
   ; grid in lat and 1.25 degree grid in lon. Data stored as 16-bit integers,
   ; and must be converted to floats by dividing by 10.0. NAN stored as 999.
   data = IntArr(288, 180)
   line = IntArr(288)
   FOR j=0,179 DO BEGIN
      READF, lun, line, Format='(1x,25I3)'
      data[0,j] = line
   ENDFOR
   Free_Lun, lun

   ; Find 999.
   indices = Where(data EQ 999, count)

   ; Convert data to floating point values.
   data = data / 10.0
   IF count GT 0 THEN data[indices] = !Values.F_NAN

   ; Create corresponding lat/lon vectors.
   lat = Scale_Vector(Findgen(180), -89.5, 89.5)
   lon = Scale_Vector(Findgen(288), -179.375, 179.375)

   ; If LIMITS are set, then find the proper subscripted values.
   IF N_Elements(limit) EQ 0 THEN BEGIN
      latsubs = [0,179]
      lonsubs = [0,287]
   ENDIF ELSE BEGIN
      IF N_Elements(limit) NE 4 THEN $
         Message, 'Usage: LIMIT=[latmin, lonmin, latmax, lonmax].'
      latsubs = Value_Locate(lat, [limit[0],limit[2]])
      lonsubs = Value_Locate(lon, [limit[1],limit[3]])
   ENDELSE

   ; Subscript the data
   lat_sub = lat[latsubs[0]:latsubs[1]]
   lon_sub = lon[lonsubs[0]:lonsubs[1]]
   data_sub = data[lonsubs[0]:lonsubs[1], latsubs[0]:latsubs[1]]

   ; Scale the data between 1 and 4.5 into 6 colors. Set the minimum value to 1.
   img = BytScl(data_sub, Min=1, Max=4.5, Top=6, /NAN) + 1B

   ; Locate the NANs in the subsetted data. These values will be set to background.
   indices = Where(Finite(data_sub) EQ 0, count)
   IF count GT 0 THEN img[indices] = 10

   ; Locate the values in the data that are less than 1. These values will be set to background.
   lessThanOne = Where(data_sub LT 1.0, ltcount)
   IF ltcount GT 0 THEN img[lessThanOne] = 10

   ; Device independent graphics is most easily accomplished with decomposed color.
   Device, Decomposed=0, Get_Decomposed=theState

   ; Load image display colors.
   LoadCT, 0, /Silent

   ; Aerosol colors, indices 1-7;
   TVLCT, FSC_Color(['LIGHT GRAY', 'DARK GRAY', 'YELLOW', 'GREEN YELLOW', $
                     'RED', 'MAROON', 'DARK RED'], /Triple), 1
   TVLCT, FSC_Color('BLACK', /Triple), 8 ; Drawing color.
   TVLCT, FSC_Color('SKY BLUE', /Triple), 250 ; Ocean color.
   TVLCT, FSC_Color('FOREST GREEN', /Triple), 251 ; Landmass color.
   TVLCT, FSC_Color('WHITE', /Triple), 252 ; Background color.

   ; The next part of the program takes place in the Z-graphics buffer.
   ; It is here we need to create a seamask and landmask image in order
   ; to create the proper background image for our graphics display.
   x_winsize = 750 ; This will turn int 7.5 inches in PostScript.
   y_winsize = 600 ; This will turn into 6.0 inches in PostScript.
   thisDevice = !D.Name
   Set_Plot, 'Z'
   Erase
   Device, Set_Resolution=[x_winsize, y_winsize], Z_Buffer=0

   ; Set up the map projection center coordinates.
   lat_center = (Max(lat_sub) - Min(lat_sub)) / 2 + Min(lat_sub)
   lon_center = (Max(lon_sub) - Min(lon_sub)) / 2 + Min(lon_sub)

   ; Set up the map projection coordinate space, and draw the continents in a filled color.
   Map_Set, /Cylindrical, lat_center, lon_center, Position=[0.1, 0.25, 0.9, 0.9], $
      Limit=[Min(lat_sub), Min(lon_sub), Max(lat_sub), Max(lon_sub)], /NoBorder, /Isotropic
   Map_Continents, /Fill, Color=251

   ; Warp the image.
   warp = Map_Image(img, xx, yy, xs, ys, LatMin=Min(lat_sub), LonMin=Min(lon_sub), $
      LatMax=Max(lat_sub), LonMax=Max(lon_sub), Compress=1, Missing=252)

   ; Determine the size of the warped image.
   dims = Size(warp, /Dimensions)

   ; Grab this portion of the window to obtain sea and land mask images.
   snap = TVRD(xx, yy, dims[0], dims[1])
   seamask = snap EQ 0
   landmask = snap NE 0
   seacolor = 250
   seaimage = seamask * (BytArr(dims[0],dims[1]) + seacolor)
   landcolor = 251
   landimage = landmask * (BytArr(dims[0],dims[1]) + landcolor)

   ; Create the background image.
   backgroundImage = seaimage > landimage

   ; Back to the current device.
   Set_Plot, thisDevice

   ; Prepare display image. Set all pixels in the warped image with value 0
   ; are set to the background image color.
   indices = Where(warp EQ 10, count)
   IF count GT 0 THEN warp[indices] = backgroundImage[indices]


   ; Set up the appropriate window size for PostScript or display.
   IF Keyword_Set(PS) THEN BEGIN
      ps_xsize = x_winsize/100
      ps_ysize = y_winsize/100
      keywords = PSConfig(Cancel=cancelled, XSize=ps_xsize, YSize=ps_ysize, $
         Inches=1, XOffset=(8.5 - ps_xsize)/2.0, YOffset=(11.0 - ps_ysize)/2.0, $
         Filename='aerosol.ps')

      ; If the user cancelled, then forget about it.
      IF cancelled THEN BEGIN
         Device, Decomposed=theState
         RETURN
      ENDIF
      thisDevice = !D.Name
      Set_Plot, 'PS', /Copy
      Device, _Extra=keywords

      ; Have to perform the image warp again in order to get the proper size and start information.
      void = Map_Image(img, xx, yy, xs, ys, LatMin=Min(lat_sub), LonMin=Min(lon_sub), $
         LatMax=Max(lat_sub), LonMax=Max(lon_sub))

   ENDIF ELSE BEGIN

      Window, /Free, Title='TOMS Aerosol Data Plot', XSize=x_winsize, YSize=y_winsize
      Erase, Color=252

   ENDELSE

   ; Display the image.
   TV, warp, xx, yy, XSize=xs, YSize=ys

   ; Add continents, countries, and grids.
   Map_Continents, Color=8
   Map_Continents, Color=8, Countries=1
   Map_Grid, Color=8

   ; Add a colorbar.
   xstart = Float(xx)/!D.X_Size
   xend = Float(xs)/!D.X_Size + xstart
   yend = Float(yy)/!D.Y_Size -  0.10
   ystart = yend - 0.075
   tickvalues = ['1.0', '1.5', '2.0', '2.5', '3.0', '3.5', '4.0', '4.5+']
   Colorbar, NColors=7, Bottom=1, Range=[1.0, 4.5], Divisions=7, Format='(F3.1)', $
      Position=[xstart, ystart, xend, yend], XMinor=1, XTicklen=1, $
      Title='Aerosol Index', Font=0, Color=8, Ticknames=tickvalues

   ; Finish.
   IF Keyword_Set(PS) THEN BEGIN
      Device, /Close_File
      Set_Plot, thisDevice
   ENDIF

   ; Restore system state.
   Device, Decomposed=theState

END