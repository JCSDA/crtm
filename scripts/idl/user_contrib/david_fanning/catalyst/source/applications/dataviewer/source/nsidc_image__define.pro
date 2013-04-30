;*****************************************************************************************************
;+
; NAME:
;       NSIDC_IMAGE__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to define a standard NSIDC image object.
;       NSIDC image objects are scaled for display into the range of 0 to 249. Other indices
;       are allocated as follows:
;       
;       255: Missing color.
;       254: Out of bounds low color.
;       253: Out of bounds high color.
;       252: Land mask color.
;       251: Motion vector color
;       250: Currently unused. Could be used for image overlay colors, etc.
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
;       Objects.
;
; SYNTAX:
;
;       theObject = Obj_New("NSIDC_IMAGE")
;
; SUPERCLASSES:
;
;       SCALEIMAGE
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { NSIDC_IMAGE, $
;             annotatePixmap: Obj_New(), $ ; The window used for annotations.
;             cb_format: "", $             ; The colorbar format.
;             cb_type:                     ; The type of color bar allowed. 0 (default) normal, 1 discrete, 2 none.
;             colorChangeAllowed: 0B, $    ; A flag that indicates this image's colors can be changed by the user.
;             colorChangeNColors: 0L, $    ; If colors can be changed, this indicates how many colors can be changed.
;             contextmenu: Obj_New(), $    ; The context menu for the image selection events.
;             filename: "", $              ; The filename of the image.
;             displayName: "" , $          ; The name that should be used on the display.
;             fn_color: "", $              ; The name of the filename color.
;             landmask_color: "", $        ; The name of the landmask color.
;             landmask_value: Ptr_New(), $ ; The value of the landmask in the image.
;             grid_color: "", $            ; The name of the map grid color.
;             outline_color: "", $         ; The name of the map outline or fill color.
;             vector_color: "", $          ; The name of the color for drawing motion vectors on images.
;             map_fill: 0B, $              ; A flag that indicates a filled map outline should be drawn on the image.
;             map_outline: 0B, $           ; A flag that indicates a map outline should be drawn on the image.
;             map_grid: 0B, $              ; A flag that indicates a map grid should be drawn on the image.
;             no_name_display:0B, $        ; A flag that indicates the filename should NOT be displayed.
;             no_colorbar_display:0B, $    ; A flag that indicates the colorbar should NOT be displayed.
;             nsidc_tag: "", $             ; The NSIDC number associated with this image, eg, "nsidc-0032".
;             oob_low_color: "", $         ; The name of the out-of-bounds low color.
;             oob_high_color: "", $        ; The name of the out-of-bounds high color.
;             INHERITS SCALEIMAGE $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 23 June 2008.
;       Added a DISPLAYNAME keyword and modified the program to use the "display name" 
;          rather than the "filename" in the image DRAW method. I did this to facilitate
;          the display of HDF variables as images. 8 January 2009. DWF.
;       Added a vector color field and ability to display motion vector overlays. 15 June 2010. DWF.
;       Renamed Colorbar procedure to cgColorbar to avoid conflict with IDL 8 Colorbar function.
;          26 September 2010. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2008-2010, Regents of the University of Colorado. All rights reserved.    ;
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
;+
; NAME:
;       NSIDC_IMAGE::ADD
;
; PURPOSE:
;
;       This method is where you can screen what kinds of objects are
;       added to this object's hierarchy. The method is not always needed.
;       If you do create it, be CERTAIN to call the superclass ADD method
;       or your program will not work correctly.
;
; SYNTAX:
;
;       theObject -> Add, object
;
; ARGUMENTS:
;
;     object:     The object to be added to this one.
;
; KEYWORDS:
;
;     _EXTRA:     Any keywords appropriate for the superclass Add method.
;-
;*****************************************************************************************************
PRO NSIDC_Image::Add, object, _EXTRA=extraKeywords

   @cat_pro_error_handler

   self -> SCALEIMAGE::Add, object, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       NSIDC_IMAGE::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the NSIDC_IMAGE object. A
;       control panel is a graphical user interface for setting object
;       properties. If you create a control panel, the events are typically
;       sent to the EVENTHANDLER method.
;
; SYNTAX:
;
;       theObject -> ControlPanel, baseObject
;
; ARGUMENTS:
;
;       baseObject:    The object reference of a base widget for this control to
;                      be added to. If not supplied, the control panel will be in a
;                      self contained window (i.e., a TOPLEVELBASE object).
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the CatControlPanel::INIT method.
;
;-
;*****************************************************************************************************
PRO NSIDC_Image::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.
   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Image Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)
   self -> SetProperty, Description='NSIDC Image Properties '

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.
   IF (NOT OBJ_VALID (cp)) THEN RETURN

   IF self.map_outline OR self.map_grid THEN numProps = 25 ELSE numProps = 21
   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, $
      Name='IMAGE PROPERTYSHEET', YSize=numProps, Description='NSIDC Image Properties ')
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       NSIDC_Image::CREATEDISPLAYIMAGE
;
; PURPOSE:
;
;       This method creates a display image for the object
;
; SYNTAX:
;
;       imageObject -> CreateDisplayImage
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO NSIDC_Image::CreateDisplayImage

   @cat_pro_error_handler
   
    ; We have to set aside space for the filename and a colorbar, if the user wants them.
    ; The unit if space is going to be the space given by !D.Y_CH_SIZE / !D.Y_SIZE. This
    ; will be a normalized unit.
    unit = !D.Y_CH_SIZE / Float(!D.Y_SIZE)

    ; Doing multiple plots?
    IF Total(!P.Multi) GT 0 THEN multi = 1 ELSE multi = 0
    IF Keyword_Set(multi) THEN BEGIN
    
          ; Draw an invisible plot to get plot position. In pixmap to avoid background
          ; color change in window.
          currentWindow = !D.WINDOW
          Window, XSIZE=!D.X_SIZE, YSIZE=!D.Y_SIZE, /PIXMAP
          IF self.no_name_display THEN BEGIN
            Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, XMargin=[0,0], YMargin=[0,0]
          ENDIF ELSE BEGIN
            Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, XMargin=[0,0], YMargin=[0,0]
          ENDELSE
          WDelete, !D.Window
          IF currentWindow GE 0 THEN WSet, currentWindow
          currentPosition = self._position
          self._position = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
          
          ; If needed, subtract space for the title.
          IF ~self.no_name_display THEN BEGIN
              self._position[1] = self._position[1] + (1.5 * unit)
          ENDIF
          
          ; If needed, subtract space for the colorbar.
          IF ~self.no_colorbar_display AND (Size(self, /N_DIMENSIONS) NE  3) THEN BEGIN
              self._position[3] = self._position[3] - (3.75 * unit)
          ENDIF
        
   ENDIF ELSE BEGIN
          currentPosition = self._position
          IF ~self.no_name_display THEN BEGIN
              self._position[1] = self._position[1] + (1.5 * unit)
          ENDIF
          IF ~self.no_colorbar_display THEN BEGIN
              self._position[3] = self._position[3] - (3.75 * unit)
          ENDIF
   ENDELSE

   ;Keep the aspect ratio of the image?
   IF self._keep_aspect THEN BEGIN

      ; Find aspect ratio of image.
      ratio = FLOAT(self._ysize) / self._xsize

      ; Find the proposed size of the image in pixels without aspect considerations.
      xpixSize = (self._position[2] - self._position[0]) * !D.X_VSize
      ypixSize = (self._position[3] - self._position[1]) * !D.Y_VSize

      ; Try to fit the image width. If you can't maintain
      ; the aspect ratio, fit the image height.
      trialX = xpixSize
      trialY = trialX * ratio
      IF trialY GT ypixSize THEN BEGIN
         trialY = ypixSize
         trialX = trialY / ratio
      ENDIF

      ; Recalculate the position of the image in the window.
      position = FltArr(4)
      position[0] = (((xpixSize - trialX) / 2.0) / !D.X_VSize) + self._position[0]
      position[2] = position[0] + (trialX/Double(!D.X_VSize))
      position[1] = (((ypixSize - trialY) / 2.0) / !D.Y_VSize)  + self._position[1]
      position[3] = position[1] + (trialY/Double(!D.Y_VSize))

   ENDIF ELSE position = self._position

   ; Calculate the image size and start locations.
   xsize = (position[2] - position[0]) * !D.X_VSIZE
   ysize = (position[3] - position[1]) * !D.Y_VSIZE
   xstart = position[0] * !D.X_VSIZE
   ystart = position[1] * !D.Y_VSIZE

   ; Update the location variables, as these may have changed.
   self._location[*,0] = Round([xstart, ystart, xstart + xsize, ystart + ysize, Double(!D.X_VSize), Double(!D.Y_VSize)])
   self._location[*,1] = [ self._location[0,0]/self._location[4,0], $
                           self._location[1,0]/self._location[5,0], $
                           self._location[2,0]/self._location[4,0], $
                           self._location[3,0]/self._location[5,0], $
                           self._location[4,0]/self._location[4,0], $
                           self._location[5,0]/self._location[5,0] ]

  ; Now set the position back to what it was before. Otherwise, the image gets
  ; smaller and smaller the more it is displayed!
  self._position = currentPosition
  
  ; Is there a contained coordinate object that needs updating?
  IF Obj_Valid(self._coords) THEN BEGIN
     self._coords -> SetProperty, Position=self._location[0:3,1]
  END
  IF Obj_Valid(self._zoomCoords) THEN BEGIN
     self._zoomCoords -> SetProperty, Position=self._location[0:3,1]
  END

  ; Compute a display image.
  IF Ptr_Valid(self._dataPtr) THEN BEGIN

     ; If this is PostScript, then get the image and RETURN.
     IF (!D.Flags AND 1) NE 0 THEN BEGIN

        ; Get the image itself.
        CASE self._interleaving OF
            0: image = (*self._dataPtr)[self._x1:self._x2, self._y1:self._y2]
            1: image = (*self._dataPtr)[*, self._x1:self._x2, self._y1:self._y2]
            2: image = (*self._dataPtr)[self._x1:self._x2, *, self._y1:self._y2]
            3: image = (*self._dataPtr)[self._x1:self._x2, self._y1:self._y2, *]
        ENDCASE

        ; Do you have missing values?
        IF Ptr_Valid(self.missing_value) THEN BEGIN
           missing = Where(image EQ *self.missing_value, missing_cnt)
           IF missing_cnt NE 0 THEN BEGIN
              image = Float(Temporary(image))
              image[missing] = !VALUES.F_NAN
           ENDIF
        ENDIF ELSE missing_cnt = 0
        
        ; Do you have landmask values?
        IF Ptr_Valid(self.landmask_value) THEN BEGIN
           landmask = Where(image EQ *self.landmask_value, landmask_cnt)
           IF landmask_cnt NE 0 THEN BEGIN
              image = Float(Temporary(image))
              image[landmask] = !VALUES.F_NAN
           ENDIF
        ENDIF ELSE landmask_cnt = 0
        
        ; Find the values that are out of bounds, both low and high.
        goodvalues = Where(Finite(image) EQ 1)
        oob_low =  Where(image[goodvalues] LT self.sclmin, oob_low_cnt)
        oob_high = Where(image[goodvalues] GT self.sclmax, oob_high_cnt)

        ; Locate the NAN values.
        image = Temporary(self->ScaleTheImage(image)) + self.bottom
        IF missing_cnt GT 0 THEN image[missing] = 255 ; Missing values.
        IF oob_low_cnt GT 0 THEN image[goodvalues[oob_low]] = 254 ; Out-of-bounds low.
        IF oob_high_cnt GT 0 THEN image[goodvalues[oob_high]] = 253 ; Out of bounds high.
        IF landmask_cnt GT 0 THEN image[landmask] = 252 ; Landmask
        
        IF Ptr_Valid(self._displayImage) THEN BEGIN
           *self._displayImage = image
        ENDIF ELSE BEGIN
           self._displayImage = Ptr_New(image)
        ENDELSE
        RETURN
     ENDIF

     ; It is not PostScript, so find the right image based on interleaving.
     CASE self._interleaving OF
         0: image = (*self._dataPtr)[self._x1:self._x2, self._y1:self._y2]
         1: image = (*self._dataPtr)[*, self._x1:self._x2, self._y1:self._y2]
         2: image = (*self._dataPtr)[self._x1:self._x2, *, self._y1:self._y2]
         3: image = (*self._dataPtr)[self._x1:self._x2, self._y1:self._y2, *]
     ENDCASE

     ; Do you have missing values?
     IF Ptr_Valid(self.missing_value) THEN BEGIN
        missing = Where(image EQ *self.missing_value, missing_cnt)
        IF missing_cnt NE 0 THEN BEGIN
           image = Float(Temporary(image))
           image[missing] = !VALUES.F_NAN
        ENDIF
     ENDIF ELSE missing_cnt = 0

     ; Do you have landmask values?
     IF Ptr_Valid(self.landmask_value) THEN BEGIN
        landmask = Where(image EQ *self.landmask_value, landmask_cnt)
        IF landmask_cnt NE 0 THEN BEGIN
           image = Float(Temporary(image))
           image[landmask] = !VALUES.F_NAN
        ENDIF
     ENDIF ELSE landmask_cnt = 0
     
     ; Find the values that are out of bounds, both low and high.
     goodvalues = Where(Finite(image) EQ 1, goodCount)
     oob_low =  Where(image[goodvalues] LT self.sclmin, oob_low_cnt)
     oob_high = Where(image[goodvalues] GT self.sclmax, oob_high_cnt)
        
     CASE self._interleaving OF

         0: BEGIN

            image = Temporary(self->ScaleTheImage(image)) + self.bottom
            IF missing_cnt GT 0 THEN image[missing] = 255 ; Missing values.
            IF oob_low_cnt GT 0 THEN image[goodvalues[oob_low]] = 254 ; Out-of-bounds low.
            IF oob_high_cnt GT 0 THEN image[goodvalues[oob_high]] = 253 ; Out of bounds high.
            IF landmask_cnt GT 0 THEN image[landmask] = 252 ; Landmask color.
            IF Ptr_Valid(self._displayImage) THEN BEGIN
               *self._displayImage = Congrid(image, ROUND(xsize), ROUND(ysize), $
                  INTERP=self._interpolate)
            ENDIF ELSE BEGIN
               self._displayImage = Ptr_New(Congrid(image, ROUND(xsize), ROUND(ysize), $
                  INTERP=self._interpolate), /No_Copy)
            ENDELSE

            END

        1: BEGIN
        
            image = Temporary(self->ScaleTheImage(image)) + self.bottom
            IF missing_cnt GT 0 THEN image[missing] = 255 ; Missing values.
            IF oob_low_cnt GT 0 THEN image[goodvalues[oob_low]] = 254 ; Out-of-bounds low.
            IF oob_high_cnt GT 0 THEN image[goodvalues[oob_high]] = 253 ; Out of bounds high.
            IF landmask_cnt GT 0 THEN image[landmask] = 252 ; Landmask color.
            IF Ptr_Valid(self._displayImage) THEN BEGIN
               *self._displayImage = Congrid(image, 3, ROUND(xsize), ROUND(ysize), $
                   INTERP=self._interpolate)
            ENDIF ELSE BEGIN
               self._displayImage = Ptr_New(Congrid(image, 3, ROUND(xsize), ROUND(ysize), $
                  INTERP=self._interpolate), /No_Copy)
            ENDELSE

            END

        2: BEGIN

            image = Temporary(self->ScaleTheImage(image)) + self.bottom
            IF missing_cnt GT 0 THEN image[missing] = 255 ; Missing values.
            IF oob_low_cnt GT 0 THEN image[goodvalues[oob_low]] = 254 ; Out-of-bounds low.
            IF oob_high_cnt GT 0 THEN image[goodvalues[oob_high]] = 253 ; Out of bounds high.
            IF landmask_cnt GT 0 THEN image[landmask] = 252 ; Landmask color.
            IF Ptr_Valid(self._displayImage) THEN BEGIN
              *self._displayImage = Congrid(image, ROUND(xsize), 3, ROUND(ysize), $
               INTERP=self._interpolate)
            ENDIF ELSE BEGIN
              self._displayImage = Ptr_New(Congrid(image, ROUND(xsize), 3, ROUND(ysize), $
               INTERP=self._interpolate), /No_Copy)
           ENDELSE

           END

        3: BEGIN

            image = Temporary(self->ScaleTheImage(image)) + self.bottom
            IF missing_cnt GT 0 THEN image[missing] = 255 ; Missing values.
            IF oob_low_cnt GT 0 THEN image[goodvalues[oob_low]] = 254 ; Out-of-bounds low.
            IF oob_high_cnt GT 0 THEN image[goodvalues[oob_high]] = 253 ; Out of bounds high.
            IF landmask_cnt GT 0 THEN image[landmask] = 252 ; Landmask color.
            IF Ptr_Valid(self._displayImage) THEN BEGIN
              *self._displayImage = Congrid(image, ROUND(xsize), ROUND(ysize), 3, $
               INTERP=self._interpolate)
            ENDIF ELSE BEGIN
              self._displayImage = Ptr_New(Congrid(image, ROUND(xsize), ROUND(ysize), 3, $
               INTERP=self._interpolate), /No_Copy)
            ENDELSE
             
            END

     ENDCASE
  ENDIF
END



;*****************************************************************************************************
;+
; NAME:
;       NSIDC_IMAGE::DRAW
;
; PURPOSE:
;
;       This method may or may not be needed by your object, depending
;       upon whether a graphical representation of the object is required.
;       If you wish the DRAW method to automatically propogates down to any
;       objects contained in this object's container, call the superclass DRAW
;       method.
;
; SYNTAX:
;
;       theObject -> Draw
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
; 
;       _Extra:       Any keywords appropriate for the superclass DRAW method..
;
;-
;*****************************************************************************************************
PRO NSIDC_Image::Draw,  _Extra=extrakeywords


   ; Set up the error handler.
   @cat_pro_error_handler

    ; We have to set aside space for the filename and a colorbar, if the user wants them.
    ; The unit if space is going to be the space given by !D.Y_CH_SIZE / !D.Y_SIZE. This
    ; will be a normalized unit.
    unit = !D.Y_CH_SIZE / Float(!D.Y_SIZE)

  ; Set to display window, if you have one. Otherwise, draw in current window.
  IF Obj_Valid(self._wid) THEN self._wid -> SetWindow

   ; Create a display image.
   self -> CreateDisplayImage

   ; Get the starting and size information from the location.
   xstart = self._location[0,0]
   ystart = self._location[1,0]
   xsize =  self._location[2,0] - self._location[0,0]
   ysize =  self._location[3,0] - self._location[1,0]

   ; Display the image. Sizing differently for scalable pixels devices.
   IF (!D.Flags AND 1) NE 0 THEN $
   BEGIN

      ; Set up colors and coordinates (if they exist).
      self -> ApplyColors
      self -> ApplyCoords

       ; Need a gray-scale color table if this is a true color image.
       IF self._interleaving GT 0 THEN LOADCT, 0, /Silent
       TV, *self._displayImage, ROUND(xstart), ROUND(ystart), XSIZE=ROUND(xsize), YSIZE=ROUND(ysize), $
           True=self._interleaving, Order=self._order
           
       ; Draw map outlines?
       self -> GetProperty, COORD_OBJECT=coords
       IF Obj_Class(coords) EQ 'MAPCOORD' THEN BEGIN
       
         IF self.map_outline THEN BEGIN
            coords -> GetProperty, MAP_OVERLAY=map_overlays, OVERLAY_POSITION=0
            validIndices = Where(Obj_Valid(map_overlays) EQ 1, count)
            IF count GT 0 THEN BEGIN
                FOR j=0,count-1 DO BEGIN
                    map_overlays[validIndices[j]] -> SetProperty, COLOR=self.outline_color, $
                        LAND_COLOR=self.landmask_color
                    map_overlays[validIndices[j]] -> Draw
                ENDFOR
            ENDIF
         ENDIF
         
         IF self.map_grid THEN BEGIN
            coords -> GetProperty, MAP_OVERLAY=map_overlays, OVERLAY_POSITION=1
            validIndices = Where(Obj_Valid(map_overlays) EQ 1, count)
            IF count GT 0 THEN BEGIN
                FOR j=0,count-1 DO BEGIN
                    map_overlays[validIndices[j]] -> SetProperty, COLOR=self.grid_color
                    map_overlays[validIndices[j]] -> Draw
                ENDFOR
            ENDIF
         ENDIF
         
          ; Any object in any overlay position EXCEPT 0 and 1 should be drawn.
         coords -> GetProperty, MAP_OVERLAY=overlayObjects
         index = Where(Obj_Valid(overlayObjects), count)
         IF count GT 0 THEN BEGIN
            FOR j=0,count-1 DO BEGIN
                IF index[j] GE 2 THEN overlayObjects[index[j]] -> Draw
            ENDFOR
         ENDIF
         
       ENDIF
           
       p = self._location[0:3,1]
       
       IF ~Keyword_Set(self.no_name_display) THEN BEGIN
           DEVICE, SET_FONT='TIMES', /TT_FONT
           XYOUTS, (p[2] - p[0]) / 2 + p[0], /NORMAL, $
              p[1] - (1.5 * unit), $
              self.displayName, $
              ALIGNMENT=0.5, COLOR=cgColor(self.fn_color), $
              CHARSIZE=0.75, FONT=(StrUpCase(!Version.OS_Family) EQ 'WINDOWS') ? 1 : 0
           DEVICE, SET_FONT='Helvetica', /TT_FONT
      ENDIF 
      IF ~Keyword_Set(self.no_colorbar_display) AND $
         (Size(self, /N_DIMENSIONS) NE  3) AND $
         self.cb_type NE 2 THEN BEGIN
        self -> GetProperty, COLOR_OBJECT=colors, OOB_LOW_COLOR=oob_low_color, OOB_HIGH_COLOR=oob_high_color
        IF Obj_Valid(colors) THEN BEGIN
            colors -> GetProperty, NCOLORS=ncolors
            ydistance = 2.0 * unit
            xdistance = 3.0 * !D.X_CH_SIZE / Float(!D.X_Size)
            self -> GetProperty, SCLMIN=sclmin, SCLMAX=sclmax
            colors -> Draw
            cgColorbar, NCOLORS=ncolors, RANGE=[sclmin, sclmax], DIVISIONS=2, FORMAT=self.cb_format, $
               POSITION=[p[0]+xdistance, p[3]+ 0.01, p[2]-xdistance, p[3]+ydistance/2.], /TOP, ANNOTATECOLOR=self.fn_color, $
               XCharsize = StrUpCase(!Version.OS_Family) EQ 'WINDOWS' ? 0.8 : 1.0, $
               FONT=(StrUpCase(!Version.OS_Family) EQ 'WINDOWS') ? 1 : 0, XTINKLEN=1.0, XMINOR=0
            POLYFILL, [p[0]+xdistance, p[0], p[0]+xdistance, p[0]+xdistance], $
                      [p[3] + 0.01,((p[3]+ydistance/2.)-(p[3]+ 0.01))/2. + (p[3]+ 0.01) , p[3]+ydistance/2.0, p[3]+ 0.01], /NORMAL, $
                      COLOR=cgColor(oob_low_color)
            PLOTS, [p[0]+xdistance, p[0], p[0]+xdistance, p[0]+xdistance], $
                      [p[3] + 0.01,((p[3]+ydistance/2.)-(p[3]+ 0.01))/2. + (p[3]+ 0.01) , p[3]+ydistance/2.0, p[3]+ 0.01], /NORMAL, $
                      COLOR=cgColor(self.fn_color)
            POLYFILL, [p[2]-xdistance, p[2], p[2]-xdistance, p[2]-xdistance], $
                      [p[3] + 0.01,((p[3]+ydistance/2.)-(p[3]+ 0.01))/2. + (p[3]+ 0.01) , p[3]+ydistance/2.0, p[3]+ 0.01], /NORMAL, $
                      COLOR=cgColor(oob_high_color)
            PLOTS, [p[2]-xdistance, p[2], p[2]-xdistance, p[2]-xdistance], $
                      [p[3] + 0.01,((p[3]+ydistance/2.)-(p[3]+ 0.01))/2. + (p[3]+ 0.01) , p[3]+ydistance/2.0, p[3]+ 0.01], /NORMAL, $
                      COLOR=cgColor(self.fn_color)
        ENDIF
      ENDIF
   ENDIF ELSE $
   BEGIN ; All other devices.

      Device, Get_Decomposed=theState
      IF self._interleaving GT 0 THEN Device, Decomposed=1 ELSE Device, Decomposed=0

      ; Set up colors and coordinates (if they exist).
      self -> ApplyColors
      self -> ApplyCoords

      ; Display the display image.
      TV, *self._displayImage, True=self._interleaving, xstart, ystart, order=self._order
      
      ; Clean up.
      Device, Decomposed=theState

      ; Draw map outlines? Map outlines (overlay position 0) and map grids (overlay
      ; position 1) are optional. Any other map overlays are drawn automatically.
      self -> GetProperty, COORD_OBJECT=coords
      IF Obj_Class(coords) EQ 'MAPCOORD' THEN BEGIN
      
         IF self.map_outline THEN BEGIN
            coords -> GetProperty, MAP_OVERLAY=map_overlays, OVERLAY_POSITION=0
            validIndices = Where(Obj_Valid(map_overlays) EQ 1, count)
            IF count GT 0 THEN BEGIN
                FOR j=0,count-1 DO BEGIN
                    map_overlays[validIndices[j]] -> SetProperty, COLOR=self.outline_color, $
                        LAND_COLOR=self.landmask_color
                    map_overlays[validIndices[j]] -> Draw
                ENDFOR
            ENDIF
         ENDIF
         
         IF self.map_grid THEN BEGIN
            coords -> GetProperty, MAP_OVERLAY=map_overlays, OVERLAY_POSITION=1
            validIndices = Where(Obj_Valid(map_overlays) EQ 1, count)
            IF count GT 0 THEN BEGIN
                FOR j=0,count-1 DO BEGIN
                    map_overlays[validIndices[j]] -> SetProperty, COLOR=self.grid_color
                    map_overlays[validIndices[j]] -> Draw
                ENDFOR
            ENDIF
         ENDIF
         
          ; Any object in any overlay position EXCEPT 0 and 1 should be drawn.
         coords -> GetProperty, MAP_OVERLAY=overlayObjects
         index = Where(Obj_Valid(overlayObjects), count)
         IF count GT 0 THEN BEGIN
            FOR j=0,count-1 DO BEGIN
                IF index[j] GE 2 THEN overlayObjects[index[j]] -> Draw
            ENDFOR
         ENDIF
         
      ENDIF
      
      p = self._location[0:3,1]
      IF ~Keyword_Set(self.no_name_display) THEN BEGIN
            XYOUTS, (p[2] - p[0]) / 2 + p[0], /NORMAL, $
               p[1] - (1.5 * unit), $
               self.displayName, $
               ALIGNMENT=0.5, COLOR=cgColor(self.fn_color), $
               FONT=0
      ENDIF 
      
      IF ~Keyword_Set(self.no_colorbar_display) AND (Size(self, /N_DIMENSIONS) NE  3) $
        AND self.cb_type NE 2 THEN BEGIN
        self -> GetProperty, COLOR_OBJECT=colors, OOB_LOW_COLOR=oob_low_color, OOB_HIGH_COLOR=oob_high_color
        IF Obj_Valid(colors) THEN BEGIN
            colors -> GetProperty, NCOLORS=ncolors
            ydistance = 2.75 * unit
            xdistance = 3.0 * !D.X_CH_SIZE / Float(!D.X_Size)
            self -> GetProperty, SCLMIN=sclmin, SCLMAX=sclmax
            colors -> Draw
            cgColorbar, NCOLORS=ncolors, RANGE=[sclmin, sclmax], DIVISIONS=2, FORMAT=self.cb_format, $
               POSITION=[p[0]+xdistance, p[3]+ 0.01, p[2]-xdistance, p[3]+ydistance/2.], /TOP, ANNOTATECOLOR=self.fn_color, $
               XCharsize = StrUpCase(!Version.OS_Family) EQ 'WINDOWS' ? 0.8 : 1.0, $
               FONT=(StrUpCase(!Version.OS_Family) EQ 'WINDOWS') ? -1 : 0, XTICKLEN=1.0, XMINOR=0
 
            POLYFILL, [p[0]+xdistance, p[0], p[0]+xdistance, p[0]+xdistance], $
                      [p[3] + 0.01,((p[3]+ydistance/2.)-(p[3]+ 0.01))/2. + (p[3]+ 0.01) , p[3]+ydistance/2.0, p[3]+ 0.01], /NORMAL, $
                      COLOR=cgColor(oob_low_color)
            PLOTS, [p[0]+xdistance, p[0], p[0]+xdistance, p[0]+xdistance], $
                      [p[3] + 0.01,((p[3]+ydistance/2.)-(p[3]+ 0.01))/2. + (p[3]+ 0.01) , p[3]+ydistance/2.0, p[3]+ 0.01], /NORMAL, $
                      COLOR=cgColor(self.fn_color)
            POLYFILL, [p[2]-xdistance, p[2], p[2]-xdistance, p[2]-xdistance], $
                      [p[3] + 0.01,((p[3]+ydistance/2.)-(p[3]+ 0.01))/2. + (p[3]+ 0.01) , p[3]+ydistance/2.0, p[3]+ 0.01], /NORMAL, $
                      COLOR=cgColor(oob_high_color)
            PLOTS, [p[2]-xdistance, p[2], p[2]-xdistance, p[2]-xdistance], $
                      [p[3] + 0.01,((p[3]+ydistance/2.)-(p[3]+ 0.01))/2. + (p[3]+ 0.01) , p[3]+ydistance/2.0, p[3]+ 0.01], /NORMAL, $
                      COLOR=cgColor(self.fn_color)
        ENDIF
      ENDIF
      
      
   ENDELSE

     ; Are there any image axes? Update their position.

  containedObjects = self -> Get(/All)

  FOR jj=0,N_Elements(containedObjects)-1 DO BEGIN

     IF Obj_Class(containedObjects[jj]) EQ 'IMGAXES' THEN $
        containedObjects[jj] -> SetProperty, Position=self._location[0:3,1]

  ENDFOR

  ; Any children to draw?
  self -> CatDataAtom::Draw, _Extra=extrakeywords

  self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       NSIDC_IMAGE::DRAWPNG
;
; PURPOSE:
;
;       This method write the image to a PNG file.
;
; SYNTAX:
;
;       theObject -> Draw, filename
;
; ARGUMENTS:
;
;       filename:    The name of the PNG file.
;
; KEYWORDS:
; 
;       XSIZE:        The XSize of the PNG output in pixels.
;       
;       YSIZE:        The YSize of the PNG output in pixels.
;       
;       MAXSIZE:      If neither the XSIZE or the YSIZE keyword is used, then this
;                     keyword is considered. If either is used, then this keyword is ignored.
;                     It will set the maximum size of the PNG output, in pixels. This may
;                     be either the xsize or ysize of the resulting image, depending upon the
;                     aspect ratio of the image itself. Set to 800 by default.
;
;-
;*****************************************************************************************************
PRO NSIDC_Image::DrawPNG, filename, XSIZE=xsize, YSIZE=ysize, MAXSIZE=maxsize


   ; Set up the error handler.
   @cat_pro_error_handler
   
   IF N_Elements(filename) EQ 0 THEN BEGIN
        rootname = cgRootName(self.filename)
        filename = Dialog_Pickfile(TITLE='Select Name of PNG File...', FILE=rootname + '.png')
        IF filename EQ "" THEN RETURN
   ENDIF

   IF N_Elements(maxsize) EQ 0 THEN maxsize = 800
   self -> GetProperty, ASPECT_RATIO=aspect
   IF (N_Elements(xsize) EQ 0) AND (N_Elements(ysize) EQ 0) THEN BEGIN
            IF aspect GE 1.0 THEN BEGIN
                ysize = maxsize
                xsize = ysize / aspect
            ENDIF ELSE BEGIN
                xsize = maxsize
                ysize = xsize * aspect
            ENDELSE
   ENDIF ELSE BEGIN
        IF N_Elements(xsize) EQ 0 THEN xsize = maxsize
        IF N_Elements(ysize) EQ 0 THEN ysize = maxsize
   ENDELSE
   
   ; Get a window to draw in.
   Window, XSIZE=xsize, YSIZE=ysize, /PIXMAP
   thisWindow = !D.Window
   
   self -> Draw
   IF StrUpCase(StrMid(filename, 3, 4, /REVERSE_OFFSET)) EQ '.PNG' $
        THEN filename = StrMid(filename, 0, StrLen(filename)-4)
   void = cgSnapshot(/PNG, /NODIALOG, FILENAME=filename)
   
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;        NSIDC_IMAGE::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the NSIDC_IMAGE object. It will typically
;        be used to respond to events from widget objects created in the CONTROLPANEL
;        method.
;
; SYNTAX:
;
;        This method is called automatically by the event handling mechanism.
;
; ARGUMENTS:
;
;       event: The event structure as described in the IDL help files, except
;              that the ID, TOP and HANDLER tags will be object references.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO NSIDC_Image::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; When events come from the context menu, we have to remove it from the draw widget
   ; that is its parent and destroy it. Otherwise, context menus (invalid objects) will
   ; accumulate in the draw widget container.

   CASE event.name OF
   
     'ANNOTATE': BEGIN
            self.contextMenu -> GetProperty, UVALUE=drawID
            drawID -> Remove, self.contextMenu
            Obj_Destroy, self.contextMenu
            currentWindow = !D.Window
            self -> GetProperty, PARENT=parent, IMAGE=image, FILENAME=filename
            parent -> GetProperty, INITIAL_COLOR=bg_color
            
            ; See if you can find the size of the annotation window.
            maxSize = CatGetDefault('DATAVIEWER_ANNOTATE_MAXSIZE', SUCCESS=success)
            IF ~success THEN maxSize = 550
            dims = Image_Dimensions(image, XSIZE=xsize, YSIZE=ysize)
            aspect = Float(ysize)/xsize
            IF aspect GE 1 THEN BEGIN
                ysize = maxsize
                xsize = maxsize / aspect
            ENDIF ELSE BEGIN
                xsize = maxsize
                ysize = maxsize * aspect
            ENDELSE
            IF Obj_Valid(self.annotatePixmap) THEN Obj_Destroy,self.annotatePixmap
            self.annotatePixmap = Obj_New('PixmapWidget', XSIZE=xsize, $
                     YSIZE=ysize, BACKGROUNDCOLOR=bg_color)
            self -> GetProperty, Location=currentLocation, Position=currentPosition, $
                Filename=filename, FN_COLOR=fn_color, NO_NAME_DISPLAY=displayName
            
            
            self -> SetProperty, POSITION=[0.0, 0.0, 1.0, 1.0], NO_NAME_DISPLAY=1
            self.annotatePixmap -> GetProperty, WINDOWID=wid
            self.annotatePixmap -> SetWindow
            wid = !D.Window
            self -> Draw
            self -> SetProperty, Location=currentLocation, Position=currentPosition, NO_NAME_DISPLAY=displayName
            coords = Obj_New('CatCoord', XRange=[0,1], YRange=[0,1], Position=[0,0,1,1])
            thisImageFilename = Obj_New('TEXTLINE', FILE_BASENAME(filename), COLOR=fn_color, $
                ALIGNMENT=1, X=0.5, Y=0.05, COORD_OBJECT=coords)
            AnnotateWindow, wid, GROUP_LEADER=drawID, BG_COLOR=bg_color, $
                ADD_OBJECT=thisImageFilename, COLOR='black', OUTPUT_FILENAME=cgRootName(filename)
            END
            
     'CHANGE_COLORS': BEGIN
            self.contextMenu -> GetProperty, UVALUE=drawID
            drawID -> Remove, self.contextMenu
            Obj_Destroy, self.contextMenu
            currentWindow = !D.Window
            self -> GetProperty, COLOR_OBJECT=colors
            colors -> XColors, GROUP_LEADER=drawID, TITLE='Change NSIDC Image Colors'
;            self -> GetProperty, COLOR_OBJECT=colors, BOTTOM=bottom, COLORCHANGENCOLORS=ncolors
;            colors -> GetProperty, COLORPALETTE=palette, BREWER=brewer
;            TVLCT, palette
;            XColors, GROUP_LEADER=drawID->GetID(), NCOLORS=ncolors, BREWER=brewer, $
;                NOTIFYOBJ={XCOLORS_NOTIFYOBJ, self, 'XColors_Notification'}, BOTTOM=bottom, $
;                TITLE='Change NSIDC Image Colors'
            END
            
     'CHANGE_LANDMASK_COLOR': BEGIN
            self.contextMenu -> GetProperty, UVALUE=drawID
            drawID -> Remove, self.contextMenu
            Obj_Destroy, self.contextMenu
            currentWindow = !D.Window
            self -> GetProperty, COLOR_OBJECT=colors, LANDMASK_COLOR=landmask_color
            colorname = colors -> cgPickColorName(landmask_color, TITLE='Change LANDMASK Color')
            colors -> GetProperty, RED=r, GREEN=g, BLUE=b
            color = cgColor(colorname, /TRIPLE)
            r[252] = color[0]
            g[252] = color[1]
            b[252] = color[2]
            colors -> SetProperty, RED=r, GREEN=g, BLUE=b
            self -> SetProperty, LANDMASK_COLOR=colorname
            drawID -> SetWindow
            self -> Draw
            END

     'CHANGE_MISSING_COLOR': BEGIN
            self.contextMenu -> GetProperty, UVALUE=drawID
            drawID -> Remove, self.contextMenu
            Obj_Destroy, self.contextMenu
            currentWindow = !D.Window
            self -> GetProperty, COLOR_OBJECT=colors, MISSING_COLOR=missing_color
            colorname = colors -> cgPickColorName(missing_color, TITLE='Change MISSING Color')
            colors -> GetProperty, RED=r, GREEN=g, BLUE=b
            color = cgColor(colorname, /TRIPLE)
            r[255] = color[0]
            g[255] = color[1]
            b[255] = color[2]
            colors -> SetProperty, RED=r, GREEN=g, BLUE=b
            self -> SetProperty, MISSING_COLOR=colorname
            drawID -> SetWindow
            self -> Draw
            END
            
     'CHANGE_OOB_LOW_COLOR': BEGIN
            self.contextMenu -> GetProperty, UVALUE=drawID
            drawID -> Remove, self.contextMenu
            Obj_Destroy, self.contextMenu
            currentWindow = !D.Window
            self -> GetProperty, COLOR_OBJECT=colors, OOB_LOW_COLOR=oob_low_color
            colorname = colors -> cgPickColorName(oob_low_color, TITLE='Change OUT-OF-BOUNDS LOW Color')
            colors -> GetProperty, RED=r, GREEN=g, BLUE=b
            color = cgColor(colorname, /TRIPLE)
            r[254] = color[0]
            g[254] = color[1]
            b[254] = color[2]
            colors -> SetProperty, RED=r, GREEN=g, BLUE=b
            self -> SetProperty, OOB_LOW_COLOR=oob_low_color
            drawID -> SetWindow
            self -> Draw
            END
            
     'CHANGE_OOB_HIGH_COLOR': BEGIN
            self.contextMenu -> GetProperty, UVALUE=drawID
            drawID -> Remove, self.contextMenu
            Obj_Destroy, self.contextMenu
            currentWindow = !D.Window
            self -> GetProperty, COLOR_OBJECT=colors, OOB_HIGH_COLOR=oob_high_color
            colorname = colors -> cgPickColorName(oob_high_color, TITLE='Change OUT-OF-BOUNDS HIGH Color')
            colors -> GetProperty, RED=r, GREEN=g, BLUE=b
            color = cgColor(colorname, /TRIPLE)
            r[253] = color[0]
            g[253] = color[1]
            b[253] = color[2]
            colors -> SetProperty, RED=r, GREEN=g, BLUE=b
            self -> SetProperty, OOB_HIGH_COLOR=oob_high_color
            drawID -> SetWindow
            self -> Draw
            END
            
      'IMAGE PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN
            CASE StrUpCase(event.identifier) OF

               'FN_COLOR': BEGIN

                  event.component -> GetProperty, FN_COLOR=color, COLOR_OBJECT=colors, PARENT=parent
                  event.id -> GetProperty, ID=group_leader
                  color = cgPickColorName(color, Group_Leader=group_leader, TITLE='Select FILENAME Color')
                  event.component -> SetProperty, FN_COLOR=color

                  ; Redraw yourself.
                  IF Obj_Isa_Valid(parent, 'DRAWWIDGET') THEN BEGIN
                    parent -> SetWindow
                    self -> Draw
                  ENDIF ELSE CatRefreshDraw, self, Stop_At='DrawWidget'
                  RETURN
               END
               
               'GRID_COLOR': BEGIN

                  event.component -> GetProperty, GRID_COLOR=color, COLOR_OBJECT=colors, PARENT=parent
                  event.id -> GetProperty, ID=group_leader
                  color = cgPickColorName(color, Group_Leader=group_leader, TITLE='Select Map Grid Color')
                  event.component -> SetProperty, GRID_COLOR=color

                  ; Redraw yourself.
                  IF Obj_Isa_Valid(parent, 'DRAWWIDGET') THEN BEGIN
                    parent -> SetWindow
                    self -> Draw
                  ENDIF ELSE CatRefreshDraw, self, Stop_At='DrawWidget'
                  RETURN
               END
               
               'MISSING_COLOR': BEGIN

                  event.component -> GetProperty, MISSING_COLOR=color, COLOR_OBJECT=colors
                  event.id -> GetProperty, ID=group_leader
                  color = cgPickColorName(color, Group_Leader=group_leader, TITLE='Select MISSING Color')
                  event.component -> SetProperty, MISSING_COLOR=color
                  colors -> LoadColor, color, 255

                  ; Redraw yourself.
                  IF Obj_Isa_Valid(parent, 'DRAWWIDGET') THEN BEGIN
                    parent -> SetWindow
                    self -> Draw
                  ENDIF ELSE CatRefreshDraw, self, Stop_At='DrawWidget'
                  RETURN
               END

               'OOB_LOW_COLOR': BEGIN

                  event.component -> GetProperty, OOB_LOW_COLOR=color, COLOR_OBJECT=colors, PARENT=parent
                  event.id -> GetProperty, ID=group_leader
                  color = cgPickColorName(color, Group_Leader=group_leader, TITLE='Select OUT-OF-BOUNDS LOW Color')
                  event.component -> SetProperty, OOB_LOW_COLOR=color
                  colors -> LoadColor, color, 254

                  ; Redraw yourself.
                  IF Obj_Isa_Valid(parent, 'DRAWWIDGET') THEN BEGIN
                    parent -> SetWindow
                    self -> Draw
                  ENDIF ELSE CatRefreshDraw, self, Stop_At='DrawWidget'
                  RETURN
               END
               
               'OOB_HIGH_COLOR': BEGIN

                  event.component -> GetProperty, OOB_LOW_COLOR=color, COLOR_OBJECT=colors, PARENT=parent
                  event.id -> GetProperty, ID=group_leader
                  color = cgPickColorName(color, Group_Leader=group_leader, TITLE='Select OUT-OF-BOUNDS LOW Color')
                  event.component -> SetProperty, OOB_LOW_COLOR=color
                  colors -> LoadColor, color, 253

                  ; Redraw yourself.
                  IF Obj_Isa_Valid(parent, 'DRAWWIDGET') THEN BEGIN
                    parent -> SetWindow
                    self -> Draw
                  ENDIF ELSE CatRefreshDraw, self, Stop_At='DrawWidget'
                  RETURN
               END
               
               'LANDMASK_COLOR': BEGIN

                  event.component -> GetProperty, OOB_LOW_COLOR=color, COLOR_OBJECT=colors, PARENT=parent
                  event.id -> GetProperty, ID=group_leader
                  color = cgPickColorName(color, Group_Leader=group_leader, TITLE='Select OUT-OF-BOUNDS LOW Color')
                  event.component -> SetProperty, OOB_LOW_COLOR=color
                  colors -> LoadColor, color, 252

                  ; Redraw yourself.
                  IF Obj_Isa_Valid(parent, 'DRAWWIDGET') THEN BEGIN
                    parent -> SetWindow
                    self -> Draw
                  ENDIF ELSE CatRefreshDraw, self, Stop_At='DrawWidget'
                  RETURN
               END
               
               'OUTLINE_COLOR': BEGIN

                  event.component -> GetProperty, OUTLINE_COLOR=color, COLOR_OBJECT=colors, PARENT=parent
                  event.id -> GetProperty, ID=group_leader
                  color = cgPickColorName(color, Group_Leader=group_leader, TITLE='Select Map Outline Color')
                  event.component -> SetProperty, OUTLINE_COLOR=color

                  ; Redraw yourself.
                  IF Obj_Isa_Valid(parent, 'DRAWWIDGET') THEN BEGIN
                    parent -> SetWindow
                    self -> Draw
                  ENDIF ELSE CatRefreshDraw, self, Stop_At='DrawWidget'
                  RETURN
               END
               
               'POSITION': BEGIN
                  event.component -> GetProperty, Position=pos
                  event.id -> GetProperty, ID=group_leader
                  position = AdjustPosition(pos, Group_Leader=group_leader)
                  event.component -> SetProperty, Position=position
                  CatRefreshDraw, self, Stop_At='DrawWidget'
                  RETURN
               END
               
               'SCALETYPE': BEGIN
                  event.component -> GetProperty, SCALETYPE=scaletype
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=event.component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value
                  
                  ; After scale type changes, you need to update the min and max.
                  event.component -> GetProperty, SCLMIN=sclmin, SCLMAX=sclmax
                  event.component -> SetPropertyByIdentifier, 'SCLMIN', sclmin
                  event.component -> SetPropertyByIdentifier, 'SCLMAX', sclmax
                  CatRefreshDraw, self, Stop_At='DrawWidget'
                  self._controlPanel -> Refresh_Properties
               END

               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value

                  ; Refresh the graphics hierarchy. (Exit if you have deleted the object.)
                  IF Obj_Valid(self) THEN CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase ELSE RETURN

               END

            ENDCASE
         ENDIF

         END

     'OTHER_PROPERTIES': BEGIN
           self -> ControlPanel
           END

     'REFRESH_IMAGE': BEGIN
            self.contextMenu -> GetProperty, UVALUE=drawID
            drawID -> Remove, self.contextMenu
            Obj_Destroy, self.contextMenu
            currentWindow = !D.Window
            self -> GetProperty, FILENAME=filename, DISPLAYNAME=displayName
            root_name = cgRootName(filename, EXTENSION=extension)
            IF StrUpCase(extension) EQ 'HDF' THEN filename = filename + '@' + displayName
                
           ; Make a new image from this file.
           newImage = Parse_NSIDC_Filename(filename)
           newImage -> GetProperty,  $
               BETA=beta, $
               EXPONENT=exponent, $
               GAMMA=gamma, $
               MEAN=mean, $
               NEGATIVE=negative, $
               SCALETYPE=scaletype, $
               SCLMIN=sclmin, $
               SCLMAX=sclmax, $
               SIGMA=sigma
            
            ; Set the properties of this image.
            self -> SetProperty, $
               BETA=beta, $
               EXPONENT=exponent, $
               GAMMA=gamma, $
               MEAN=mean, $
               NEGATIVE=negative, $
               SCALETYPE=scaletype, $
               SCLMIN=sclmin, $
               SCLMAX=sclmax, $
               SIGMA=sigma
            
            ; Destroy the new image
            Obj_Destroy, newImage
            
            ; Draw it.
            drawID -> SetWindow
            self -> Draw
            END

     'RESCALE_IMAGE': BEGIN
            self.contextMenu -> GetProperty, UVALUE=drawID
            drawID -> Remove, self.contextMenu
            Obj_Destroy, self.contextMenu
            currentWindow = !D.Window
            self -> GetProperty, IMAGE=image, $
                FILENAME=filename, $
                PARENT=parent, $
                COLOR_OBJECT=colors, $
                BETA=beta, $
                BOTTOM=bottom, $
                EXPONENT=exponent, $
                GAMMA=gamma, $
                MEAN=mean, $
                MISSING_VALUE=missing_value, $
                MISSING_COLOR=missing_color, $
                NCOLORS=ncolors, $
                POSITION=p, $
                SCALETYPE=scaletype, $
                SCLMIN=sclmin, $
                SCLMAX=sclmax
            drawID -> SetWindow
            IF StrUpCase(self.nsidc_tag) EQ 'NSIDC_0079' THEN BEGIN
                indices = Where(image GT 100, count)
                IF count GT 0 THEN image[indices] = !VALUES.F_NAN
            ENDIF
            cgStretch, image, GROUP_LEADER=drawID->GetID(), /NO_WINDOW, $
                BETA=beta, $
                BOTTOM=bottom, $
                EXPONENT=exponent, $
                GAMMA=gamma, $
                MEAN=mean, $
                MINTHRESH=sclmin, $
                MAXTHRESH=sclmax, $
                MAXVALUE=0.15, $
                NCOLORS=ncolors, $
                TYPE=scaletype, $
                NOTIFY_OBJ={XCOLORS_NOTIFYOBJ, self, 'XStretch_Notification'}
            END
            
     'SAVE_TO_MAIN': BEGIN

            self.contextMenu -> GetProperty, UVALUE=drawID
            drawID -> Remove, self.contextMenu
            Obj_Destroy, self.contextMenu
            self -> GetProperty, FILENAME=filename
            basename = cgRootName(filename)
            varname = TextBox(Title='Provide IDL Main-Level Variable Name...', Group_Leader=drawID->GetID(), $
               Label='Image Variable Name: ', Cancel=cancelled, XSize=300, Value=IDL_ValidName(basename, /CONVERT_ALL))
            IF NOT cancelled THEN BEGIN
               self -> GetProperty, IMAGE=image
               (Scope_VarFetch(varname, LEVEL=1, /ENTER)) = image
            ENDIF
            END
            
      'ZOOM_IMAGE': BEGIN
           window = CatGetGraphicsWindow(self)
           IF Obj_Valid(window) THEN window -> GetProperty, XSIZE=xsize, YSIZE=ysize, INITIAL_COLOR=background
           self -> GetProperty, $
                AXES=axes, $
                CB_TYPE=cb_type, $
                COLORCHANGEALLOWED=colorChangeAllowed, $
                COLORCHANGENCOLORS=colorChangeNColors, $
                COLOR_OBJECT=color_object, $
                COORD_OBJECT=coord_object, $   
                IMAGE=image, $
                KEEP_ASPECT=keep_aspect, $
                MAP_OUTLINE=map_outline, $
                MAP_GRID=map_grid, $
                NOINTERPOLATE=nointerpolate, $
                ORDER=order, $
;                POSITION=position, $
                SELECTABLE=selectable, $
                VISIBLE=visible, $
                FILENAME=filename, $
                FN_COLOR=fn_color, $
                LANDMASK_COLOR=landmask_color, $
                LANDMASK_VALUE=landmask_value, $
                NO_NAME_DISPLAY=no_name_display, $
                NSIDC_TAG=nsidc_tag, $
                OOB_LOW_COLOR=oob_low_color, $
                OOB_HIGH_COLOR=oob_high_color
                
           ; Need to create a new color object, otherwise notification
           ; is going on. Get old properties and configure a new object
           ; with similar properties.
           color_object -> GetProperty, $
                RED=red, $
                GREEN=green, $
                BLUE=blue, $
                BREWER=brewer, $
                NCOLORS=ncolors, $
                COLORPALETTE=colorPalette, $
                INDEX=index
           new_color_object = Obj_New('CatColors', $
                NCOLORS=ncolors, $
                BREWER=brewer, $
                INDEX=index, $
                COLORPALETTE=colorPalette) 
                
;           ; We need to do a similar thing with the coordinate object.
;           IF Obj_Class(coord_object) EQ 'MAPCOORD' THEN BEGIN
;               coord_object -> GetProperty, $
;                    DRAW_OVERLAYS=draw_overlays, $
;                    POSITION=position, $
;                    MAP_OVERLAY=map_overlay, $
;                    MAP_PROJ_KEYWORDS=map_proj_keywords, $
;                    MAP_PROJECTION=map_projection, $
;                    XRANGE=xrange, $
;                    YRANGE=yrange, $
;                    ; MAP_PROJ_INIT keywords (partial list)
;                    DATUM=datum, $
;                    SPHERE_RADIUS=sphere_radius, $
;                    SEMIMAJOR_AXIS=semimajor_axis, $
;                    SEMIMINOR_AXIS=semiminor_axis, $
;                    CENTER_LATITUDE=center_latitude, $
;                    CENTER_LONGITUDE=center_longitude, $
;                    LIMIT=limit               
;                new_coord_object = Obj_New('MapCoord', map_projection, $
;                    DRAW_OVERLAYS=draw_overlays, $
;                    POSITION=[0,0,1,1], $
;                    MAP_OVERLAY=map_overlay, $
;                    MAP_PROJ_KEYWORDS=map_proj_keywords, $
;                    XRANGE=xrange, $
;                    YRANGE=yrange, $
;                    ; MAP_PROJ_INIT keywords (partial list)
;                    DATUM=datum, $
;                    SPHERE_RADIUS=sphere_radius, $
;                    SEMIMAJOR_AXIS=semimajor_axis, $
;                    SEMIMINOR_AXIS=semiminor_axis, $
;                    CENTER_LATITUDE=center_latitude, $
;                    CENTER_LONGITUDE=center_longitude, $
;                    LIMIT=limit)
;           ENDIF ELSE BEGIN
;               coord_object -> GetProperty, $
;                   POSITION=position, $
;                   XRANGE=xrange, $
;                   YRANGE=yrange
;               new_coord_object = Obj_New('CatCoord', $
;                   POSITION=[0,0,1,1.], $
;                   XRANGE=xrange, $
;                   YRANGE=yrange)
;           ENDELSE
           
           ; Create a new image object. Turn name display off.   
           newObject = Parse_NSIDC_Filename(filename, INFO=info, SUCCESS=success)
           IF ~success THEN Message, 'Problem creating image object.'
           newObject -> SetProperty, $
                AXES=axes, $
                COLOR_OBJECT=new_color_object, $
;                COORD_OBJECT=new_coord_object, $
                MAP_OUTLINE=map_outline, $
                MAP_GRID=map_grid, $
                ORDER=order, $
                POSITION=[0,0,1,1.], $
                VISIBLE=1, $
                FN_COLOR=fn_color, $
                LANDMASK_COLOR=landmask_color, $
                LANDMASK_VALUE=landmask_value, $
                NO_NAME_DISPLAY=1, $
                NO_COLORBAR_DISPLAY=1, $
                OOB_LOW_COLOR=oob_low_color, $
                OOB_HIGH_COLOR=oob_high_color
           
           ImgWin, newObject, TLB_Title='NSIDC DataViewer Full-Resolution Image', $
                /FULL_RESOLUTION, XSIZE=xsize, YSIZE=ysize, BACKGROUND=background, $
                NCOLORS=colorchangeNColors
           END
            
      ELSE: Print, 'Received an event from: ' +  event.name + ' in NSIDC_Image::EventHandler.'
      
   ENDCASE

   ; Report completion
   IF Obj_Valid(self) THEN self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       NSIDC_IMAGE::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain NSIDC_IMAGE properties. Be sure
;       you ALWAYS call the superclass GETPROPERTY method if you have extra
;       keywords!
;
; SYNTAX:
;
;       theObject -> GetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
; 
;    CB_FORMAT:       The current colorbar format.
;    
;    CB_TYPE:         The color bar type for this image.
;
;    COLORCHANGEALLOWED: If set to 1 if changing image colors is allowed. 
;                     If set to 0, if image color change is not allowed.
;                     
;    COLORCHANGENCOLORS: If the ColorChangeAllowed keyword is set, this keyword
;                      returns the number of colors that can change.
; 
;   DISPLAYNAME:     The name that should be displayed with the image.
;                     
;    FILENAME:        The file name associated with the image.
;
;    FN_COLOR:        The name of the file name color.
;
;    GRID_COLOR:      The name of the color for the map grid.
;     
;    LANDMASK_COLOR:  The name of the land mask color. 
;     
;    LANDMASK_VALUE:  The value of the land mask in the image. 
;     
;    MAP_COLOR:       The name of the map fill or outline color.
;     
;    MAP_FILL:        Set this keyword if wish to draw a continental outline with a filled color polygon.
;     
;    MAP_OUTLINE:     Flag to indicate a continental outline is drawn on the image.
;     
;    NO_NAME_DISPLAY: Set this keyword to suppress the filename being displayed with the image.
;     
;    NSIDC_TAG:       The NSIDC tag or number associated with this particular image. For example, "nsidc_0032".
;
;    OOB_LOW_COLOR:   The name of the out-of-bounds low color. By default, "charcoal'.
;     
;    OOB_HIGH_COLOR:  The name of the out-of-bounds high color. By default, "crimson".
;
;    OUTLINE_COLOR:   The name of the color for the map outline.
;     
;     _EXTRA:         Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************
PRO NSIDC_Image::GetProperty, $
    CB_FORMAT=cb_format, $
    CB_TYPE=cb_type, $
    COLORCHANGEALLOWED=colorChangeAllowed, $
    COLORCHANGENCOLORS=colorChangeNColors, $
    DISPLAYNAME=displayname, $
    FILENAME=filename, $
    FN_COLOR=fn_color, $
    GRID_COLOR=grid_color, $
    LANDMASK_COLOR=landmask_color, $
    LANDMASK_VALUE=landmask_value, $
    MAP_GRID=map_grid, $
    MAP_FILL=map_fill, $    
    MAP_OUTLINE=map_outline, $
    NO_NAME_DISPLAY=no_name_display, $
    NO_COLORBAR_DISPLAY=no_colorbar_display, $
    NSIDC_TAG=nsidc_tag, $
    OOB_LOW_COLOR=oob_low_color, $
    OOB_HIGH_COLOR=oob_high_color, $
    OUTLINE_COLOR=outline_color, $
     _REF_EXTRA=extraKeywords

   @cat_pro_error_handler
   
   IF Arg_Present(cb_format) THEN cb_format = self.cb_format
   IF Arg_Present(cb_type) THEN cb_type = self.cb_type
   IF Arg_Present(colorChangeAllowed) THEN colorChangeAllowed = self.colorChangeAllowed
   IF Arg_Present(colorChangeNColors) THEN colorChangeNColors = self.colorChangeNColors
   IF Arg_Present(imageinfo) THEN IF Ptr_Valid(self.imageInfo) THEN imageinfo = *self.imageInfo
   IF Arg_Present(displayName) THEN filename = self.displayName
   IF Arg_Present(filename) THEN filename = self.filename
   IF Arg_Present(fn_color) THEN fn_color = self.fn_color
   IF Arg_Present(grid_color) THEN grid_color = self.grid_color
   IF Arg_Present(map_grid) THEN map_grid = self.map_grid
   IF Arg_Present(map_fill) THEN map_fill = self.map_fill
   IF Arg_Present(map_outline) THEN map_outline = self.map_outline
   IF Arg_Present(landmask_value) THEN IF Ptr_Valid(self.landmask_value) THEN landmask_value = *self.landmask_value
   IF Arg_Present(no_name_display) THEN no_name_display = self.no_name_display
   IF Arg_Present(no_colorbar_display) THEN no_colorbar_display = self.no_colorbar_display
   IF Arg_Present(nsidc_tag) THEN nsidc_tag = self.nsidc_tag
   IF Arg_Present(oob_low_color) THEN oob_low_color = self.oob_low_color
   IF Arg_Present(oob_high_color) THEN oob_high_color = self.oob_high_color
   IF Arg_Present(outline_color) THEN outline_color = self.outline_color

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SCALEIMAGE::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;;*****************************************************************************************************
;+
; NAME:
;       NSIDC_IMAGE::MESSAGEHANDLER
;
; PURPOSE:
;
;       This method receives notification of a SENDMESSAGE call from another object's
;       method. This method should be overridden in any object that expects to receive
;       messages from objects. Be sure your MessageHandler methods define the arguments
;       and keywords shown here. If the message gets here, we issue an error message.
;
; SYNTAX:
;
;       thisObject -> MessageHandler, title, SENDER=sender, MESSAGE=message
;
; ARGUMENTS:
;
;       TITLE:   The title of the message.
;
; KEYWORDS:
;
;       DATA:    A keyword that contains any information the sender wishes to pass
;                with the message. It can be empty.
;
;       SENDER:  The object that generated the message
;
;-
;*****************************************************************************************************
PRO NSIDC_Image::MessageHandler, title, SENDER=sender, DATA=data

   @cat_pro_error_handler

   IF N_Elements(title) EQ 0 THEN Message, 'Ill-formed message received. No title.'

   ; If a message gets here, there is a problem.
   CASE title OF
   
      'NSIDC_IMAGE_PROPERTY_CHANGE': BEGIN
            IF sender->GetName() EQ 'ZOOMEDIMAGE' THEN BEGIN
                sender -> GetProperty, $
                    FN_COLOR=fn_color, $
                    OOB_LOW_COLOR=oob_low_color, $
                    OOB_HIGH_COLOR=oob_high_color, $
                    EXPONENT=exponent, $
                    GAMMA=gamma, $
                    MEAN=mean, $
                    MISSING_COLOR=missing_color, $
                    NEGATIVE=negative, $
                    SCALETYPE=scaletype, $
                    SCLMIN=sclmin, $
                    SCLMAX=sclmax, $
                    SIGMA=sigma
                self -> SetProperty, $
                    FN_COLOR=fn_color, $
                    OOB_LOW_COLOR=oob_low_color, $
                    OOB_HIGH_COLOR=oob_high_color, $
                    EXPONENT=exponent, $
                    GAMMA=gamma, $
                    MEAN=mean, $
                    MISSING_COLOR=missing_color, $
                    NEGATIVE=negative, $
                    SCALETYPE=scaletype, $
                    SCLMIN=sclmin, $
                    SCLMAX=sclmax, $
                    SIGMA=sigma
                self -> GetProperty, PARENT=parent
                parent -> SetWindow
                self -> Draw
            ENDIF
            END
            
       'COLORTOOL_TABLECHANGE': BEGIN
            graphicsWindow = CatGetGraphicsWindow(self)
            IF Obj_ISA_Valid(graphicsWindow, 'DRAWWIDGET') THEN graphicsWindow -> SetWindow
            self -> Draw
            END

      ELSE: BEGIN
         sender -> GetProperty, Name=senderName
         Message, 'Unhandled message: ' + title + ' coming from ' + senderName + '.'
         END

   ENDCASE

   ; Report success
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       NSIDC_Image::SELECTPANEL
;
; PURPOSE:
;
;       Similar to a Control Panel, it gives context menu access to properties
;       of selectable objects.
;
; SYNTAX:
;
;       selectedObject = theObject -> SelectPanel, x, y, drawID
;
; ARGUMENTS:
;
;       X:       The X location of a point in device or window coordinates.
;
;       Y:       The Y location of a point in device or window coordinates.
;
;       DRAWID:  The identifer of the draw widget object in which the selection is taking place.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO NSIDC_Image::SelectPanel, x, y, drawID

   @cat_pro_error_handler

   IF N_Params() NE 3 THEN Message, 'Incorrect number of positonal parameters.'

   ; It is easy to leave the context menus accumulating in the draw widget that is
   ; their parent. Here we attempt to remove the context menus before they are destroyed.
   IF Obj_Valid(self.contextmenu) THEN BEGIN
        self.contextMenu -> GetProperty, UVALUE=drawID
        drawID -> Remove, self.contextMenu
        Obj_Destroy, self.contextMenu
   ENDIF
   self.contextMenu = Obj_New('ContextMenuBase', drawID, Column=1, Event_Object=self, UVALUE=drawID)
   button = Obj_New('ButtonWidget', self.contextMenu, Value='View Full-Size Image', Name='ZOOM_IMAGE')
   button = Obj_New('ButtonWidget', self.contextMenu, Value='Annotate Image', Name='ANNOTATE')
   self -> GetProperty, N_DIMENSIONS=ndims
   IF ndims EQ 2 THEN BEGIN
       IF self.colorChangeAllowed THEN button = Obj_New('ButtonWidget', self.contextMenu, Value='Histogram Stretch Image', Name='RESCALE_IMAGE')
       IF self.scaletype NE 8 THEN BEGIN
           IF self.colorChangeAllowed THEN button = Obj_New('ButtonWidget', self.contextMenu, Value='Change Image Colors', Name='CHANGE_COLORS')
       ENDIF
   ENDIF
   button = Obj_New('ButtonWidget', self.contextMenu, Value='Refresh Image', Name='REFRESH_IMAGE')
   
   IF Total( LMGR(/DEMO) + LMGR(/RUNTIME) + LMGR(/VM) ) EQ 0 THEN $
     button = Obj_New('ButtonWidget', self.contextMenu, Value='Save to Main IDL Level', Name='SAVE_TO_MAIN')

   button = Obj_New('ButtonWidget', self.contextMenu, Value='Other Image Properties', Name='OTHER_PROPERTIES')
   Widget_DisplayContextMenu, drawID -> GetID(), x+10, y-5, self.contextMenu->GetID()

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       NSIDC_IMAGE::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the NSIDC_IMAGE object's properties. Be sure
;       you ALWAYS call the superclass SETPROPERTY method if you have extra keywords!
;
;
; SYNTAX:
;
;       theObject -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
; 
;    CB_FORMAT:      A string variable that sets the colorbar format. For example: '(F6.3)'.
;
;    CB_TYPE:        The "type" of colorbar allowed for NSIDC images. The default is 0, a normal
;                    type color bar. A type of 1 is a discrete color bar. A type of 2 is no color
;                    bar whatsoever. Currently (15 June 2010), a type of 1 has not been implemented.
;
;    COLORCHANGEALLOWED: If set to 1 if changing image colors is allowed. 
;                     If set to 0, if image color change is not allowed.
; 
;    COLORCHANGENCOLORS: Set this keyword to the number of colors allowed to
;                     change in the image, if the ColorChangeAllowed keyword is set.
;                     For example, if ColorChangeNColors=100, color indices 0 to 99 
;                     are allowed to change.
;                     
;     DISPLAYNAME:     The name that should be displayed with the image. Set to the FILENAME by default.
;                     
;     FILENAME:        The file name associated with the image.
;    
;     FN_COLOR:        The name of the file name color.
;
;     GRID_COLOR:      The name of the color for the map grid.
;     
;     LANDMASK_COLOR:  The name of the land mask color. 
;     
;     LANDMASK_VALUE:  The value of the land mask in the image.  
;     
;     MAP_COLOR:       The name of the map fill or outline color.
;     
;     MAP_FILL:        Set this keyword if wish to draw a continental outline with a filled color polygon.
;     
;     MAP_OUTLINE:     Set this keyword if you would like to draw a continental outline on the image.
;     
;     NO_NAME_DISPLAY: Set this keyword to suppress the filename being displayed with the image.
;     
;     NO_COLORBAR_DISPLAY: Set this keyword to suppress the colorbar being displayed with the image.
;     
;     NSIDC_TAG:       The NSIDC tag or number associated with this particular image. For example, "nsidc_0032".
;
;     OOB_LOW_COLOR:   The name of the out-of-bounds low color. By default, "charcoal'.
;     
;     OOB_HIGH_COLOR:  The name of the out-of-bounds high color. By default, "crimson".
;
;     OUTLINE_COLOR:   The name of the map outline color. By default, 'indian red'
;
;     _EXTRA:         Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO NSIDC_Image::SetProperty, $
    CB_FORMAT=cb_format, $
    CB_TYPE=cb_type, $
    COLORCHANGEALLOWED=colorChangeAllowed, $
    COLORCHANGENCOLORS=colorChangeNColors, $
    DISPLAYNAME=displayName, $
    FILENAME=filename, $
    FN_COLOR=fn_color, $
    GRID_COLOR=grid_color, $
    LANDMASK_COLOR=landmask_color, $
    LANDMASK_VALUE=landmask_value, $
    MAP_GRID=map_grid, $
    MAP_FILL=map_fill, $
    MAP_OUTLINE=map_outline, $
    NO_NAME_DISPLAY=no_name_display, $
    NO_COLORBAR_DISPLAY=no_colorbar_display, $
    NSIDC_TAG=nsidc_tag, $
    OOB_LOW_COLOR=oob_low_color, $
    OOB_HIGH_COLOR=oob_high_color, $
    OUTLINE_COLOR=outline_color, $
    _EXTRA=extraKeywords

   @cat_pro_error_handler
   
   IF N_Elements(cb_format) NE 0 THEN BEGIN
      IF cb_format EQ "" THEN cb_format='I0'
      firstChar = StrMid(cb_format, 0, 1)
      IF  firstChar EQ '"' OR firstChar EQ "'" THEN cb_format = StrMid(cb_format, 1)
      firstChar = StrMid(cb_format, 0, 1)
      IF  firstChar NE '(' THEN cb_format = '(' + cb_format
      cb_format = String(Reverse(Byte(cb_format)))
      firstChar = StrMid(cb_format, 0, 1)
      IF  firstChar EQ '"' OR firstChar EQ "'" THEN cb_format = StrMid(cb_format, 1)
      firstChar = StrMid(cb_format, 0, 1)
      IF  firstChar NE ')' THEN cb_format = ')' + cb_format
      cb_format = String(Reverse(Byte(cb_format)))
      self.cb_format = cb_format
   ENDIF
   IF N_Elements(cb_type) NE 0 THEN self.cb_type = cb_type
   IF N_Elements(colorChangeAllowed) NE 0 THEN self.colorChangeAllowed = colorChangeAllowed
   IF N_Elements(colorChangeNColors) NE 0 THEN self.colorChangeNColors = colorChangeNColors
   IF N_Elements(displayName) NE 0 THEN self.displayName = displayName
   IF N_Elements(filename) NE 0 THEN self.filename = filename
   IF N_Elements(fn_color) NE 0 THEN self.fn_color = fn_color
   IF N_Elements(grid_color) NE 0 THEN self.grid_color = grid_color
   IF N_Elements(outline_color) NE 0 THEN self.outline_color = outline_color
   IF N_Elements(map_grid) NE 0 THEN self.map_grid = Keyword_Set(map_grid)
   IF N_Elements(map_fill) NE 0 THEN self.map_fill = Keyword_Set(map_fill)      
   IF N_Elements(map_outline) NE 0 THEN self.map_outline = Keyword_Set(map_outline)
   IF N_Elements(landmask_color) NE 0 THEN self.landmask_color = landmask_color
   IF N_Elements(landmask_value) NE 0 THEN IF Ptr_Valid(self.landmask_value) $
        THEN *self.landmask_value = landmask_value $
        ELSE self.landmask_value = Ptr_New(landmask_value)
   IF N_Elements(no_name_display) NE 0 THEN self.no_name_display = Keyword_Set(no_name_display)
   IF N_Elements(no_colorbar_display) NE 0 THEN self.no_colorbar_display = Keyword_Set(no_colorbar_display)
   IF N_Elements(nsidc_tag) NE 0 THEN self.nsidc_tag = nsidc_tag
   IF N_Elements(oob_low_color) NE 0 THEN self.oob_low_color = oob_low_color
   IF N_Elements(oob_high_color) NE 0 THEN self.oob_high_color = oob_high_color

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SCALEIMAGE::SetProperty, _EXTRA=extraKeywords
   
   ; Send a propery change message.
   self -> SendMessage, 'NSIDC_IMAGE_PROPERTY_CHANGE'

   self -> Report, /Completed

END


PRO NSIDC_IMAGE::XColors_Notification
    TVLCT, r, g, b, /GET
    self -> GetProperty, COLOR_OBJECT=colors
    colors -> SetProperty, RED=r, GREEN=g, BLUE=b
    self -> Draw
END



PRO NSIDC_Image::XStretch_Notification, info

   ; If this image doesn't allow color changing, just leave.
   IF self.colorChangeAllowed EQ 0 THEN RETURN
   
   ; If it does, get the new scaling parameters and re-draw yourself.
   gridWindow = CatGetGraphicsWindow(self)
   IF Obj_Valid(gridWindow) THEN gridWindow -> SetWindow
   self -> SetProperty, SCALETYPE=info.type, SCLMIN=info.minThresh, SCLMAX=info.maxThresh, $
       GAMMA=info.gamma, BETA=info.beta, MEAN=info.mean, EXPONENT=info.exponent
   gridWindow -> GetProperty, INITIAL_COLOR=bgcolor
   self -> GetProperty, Position=loc
   gridWindow -> SetWindow
   Polyfill, [loc[0], loc[0], loc[2], loc[2], loc[0]], $
             [loc[1], loc[3], loc[3], loc[1], loc[1]], /NORMAL, $
             COLOR=cgColor(bgcolor)
   self -> Draw
 
END


;*****************************************************************************************************
;+
; NAME:
;       NSIDC_IMAGE::CLEANUP
;
; PURPOSE:
;
;       This is the NSIDC_IMAGE object class destructor method.
;
; SYNTAX:
;
;       Called automatically when the object is destroyed.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO NSIDC_Image::CLEANUP

   @cat_pro_error_handler
   
   Obj_Destroy, self.annotatePixmap
   Obj_Destroy, self.contextmenu
   Ptr_Free, self.landmask_value
   
   self -> SCALEIMAGE::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       NSIDC_IMAGE::INIT
;
; PURPOSE:
;
;       This is the NSIDC_IMAGE object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     image:          The NSIDC image array.
;
; KEYWORDS:
; 
;    CB_FORMAT:      A string variable that sets the colorbar format. For example: '(F6.3)'.
;    
;    CB_TYPE:        The "type" of colorbar allowed for NSIDC images. The default is 0, a normal
;                    type color bar. A type of 1 is a discrete color bar. A type of 2 is no color
;                    bar whatsoever. Currently (15 June 2010), a type of 1 has not been implemented.
;
;    COLORCHANGEALLOWED: If set to 1 if changing image colors is allowed. 
;                     If set to 0, if image color change is not allowed.
; 
;    COLORCHANGENCOLORS: Set this keyword to the number of colors allowed to
;                     change in the image, if the ColorChangeAllowed keyword is set.
;                     For example, if ColorChangeNColors=100, color indices 0 to 99 
;                     are allowed to change. Set to 250 by default.
;                     
;     DISPLAYNAME:    The name that should be displayed with the image. Set to the FILENAME by default.
;                     
;     FILENAME:        The filename associated with this image. (Note that the NAME keyword is
;                      always set equal the the base name of this filename, if it is used.
;
;     FN_COLOR:        The name of the color the filename should be displayed in. By default, "black".
;     
;     GRID_COLOR:      The name of the color for the map grid.
;     
;     LANDMASK_COLOR:  The name of the land mask color. By default, "dark gray'.
;     
;     LANDMASK_VALUE:  The value of the land mask in the image. Set to a null pointer by default.
;  
;     MAP_COLOR:       The name of the map fill or outline color.
;     
;     MAP_FILL:        Set this keyword if wish to draw a continental outline with a filled color polygon.
;     
;     MAP_GRID:        Set this keyword if you would like to draw a map grid on the image. By default, 'charcoal'.
;     
;     MAP_OUTLINE:     Set this keyword if you would like to draw a continental outline on the image.
;     
;     NO_NAME_DISPLAY: Set this keyword to suppress the filename being displayed with the image.
;
;     NO_COLORBAR_DISPLAY: Set this keyword to suppress the colorbar being displayed with the image.
;
;     NSIDC_TAG:       The NSIDC tag or number associated with this particular image. For example, "nsidc_0032".
;                      Some images might be processed differently, expectially with respect to color tables,
;                      than others and they can be descriminated by tag name. By default, "".
;     
;     OOB_LOW_COLOR:   The name of the out-of-bounds low color. By default, "blue'.
;     
;     OOB_HIGH_COLOR:  The name of the out-of-bounds high color. By default, "crimson".
;     
;     OUTLINE_COLOR:   The name of the map outline color. By default, 'indian red'
;     
;     VECTOR_COLOR:    The name of the color used to display ice motion vectors. By default, 'PBG7'.
;
;     _EXTRA:          Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION NSIDC_Image::INIT, image, $
    CB_FORMAT=cb_format, $
    CB_TYPE=cb_type, $
    COLORCHANGEALLOWED=colorChangeAllowed, $
    COLORCHANGENCOLORS=colorChangeNColors, $
    DISPLAYNAME=displayname, $
    FILENAME=filename, $
    FN_COLOR=fn_color, $
    GRID_COLOR=grid_color, $
    LANDMASK_COLOR=landmask_color, $
    LANDMASK_VALUE=landmask_value, $
    MAP_COLOR=map_color, $
    MAP_FILL=map_fill, $
    MAP_GRID=map_grid, $
    MAP_OUTLINE=map_outline, $
    NO_COLORBAR_DISPLAY=no_colorbar_display, $
    NO_NAME_DISPLAY=no_name_display, $
    NSIDC_TAG=nsidc_tag, $
    OOB_LOW_COLOR=oob_low_color, $
    OOB_HIGH_COLOR=oob_high_color, $
    OUTLINE_COLOR=outline_color, $
    VECTOR_COLOR=vector_color, $
    _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler
   
   ; Check keywords.
   IF N_Elements(cb_format) EQ 0 THEN BEGIN
      cb_format = CatGetDefault('DATAVIEWER_CB_FORMAT', SUCCESS=success)
      IF success EQ 0 THEN cb_format = '(F0.1)'
   ENDIF
   IF N_Elements(cb_type) EQ 0 THEN cb_type = 0
   IF N_Elements(fn_color) EQ 0 THEN BEGIN
      fn_color = CatGetDefault('DATAVIEWER_IMAGENAME_COLOR', SUCCESS=success)
      IF success EQ 0 THEN fn_color = 'BLACK'
   ENDIF
   IF N_Elements(grid_color) EQ 0 THEN BEGIN
      grid_color = CatGetDefault('DATAVIEWER_GRID_COLOR', SUCCESS=success)
      IF success EQ 0 THEN grid_color = 'CHARCOAL'
   ENDIF
   IF N_Elements(landmask_color) EQ 0 THEN BEGIN
      landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR', SUCCESS=success)
      IF success EQ 0 THEN landmask_color = 'DARK GRAY'
   ENDIF
   IF N_Elements(oob_low_color) EQ 0 THEN BEGIN
      oob_low_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR', SUCCESS=success)
      IF success EQ 0 THEN oob_low_color = 'BLUE'
   ENDIF
   IF N_Elements(oob_high_color) EQ 0 THEN BEGIN
      oob_high_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR', SUCCESS=success)
      IF success EQ 0 THEN oob_high_color = 'CRIMSON'
   ENDIF
   IF N_Elements(outline_color) EQ 0 THEN BEGIN
      outline_color = CatGetDefault('DATAVIEWER_OUTLINE_COLOR', SUCCESS=success)
      IF success EQ 0 THEN outline_color = 'CHARCOAL'
   ENDIF
   IF N_Elements(vector_color) EQ 0 THEN BEGIN
      vector_color = CatGetDefault('DATAVIEWER_VECTOR_COLOR', SUCCESS=success)
      IF success EQ 0 THEN vector_color = 'PBG7'
   ENDIF
   IF N_Elements(colorChangeAllowed) EQ 0 THEN colorChangeAllowed = 1
   colorChangeAllowed = Keyword_Set(colorChangeAllowed)
   IF N_Elements(colorChangeNColors) EQ 0 THEN colorChangeNColors = 250
   IF N_Elements(nsidc_tag) EQ 0 THEN nsidc_tag = ""
   
   ; NSIDC images should always keep their aspect ratio and should not interpolate.
   ok = self -> SCALEIMAGE::INIT (image, _EXTRA=extraKeywords, KEEP_ASPECT=1, NOINTERPOLATE=1)
   IF ~ok THEN RETURN, 0
   
   
   ; Get the color object and restrict the number of changeable colors.
   self -> GetProperty, COLOR_OBJECT=colors
   IF colorChangeAllowed THEN colors -> SetProperty, NCOLORS=250
      
   ; Load up the object.
   self.cb_format = cb_format
   self.cb_type = cb_type
   self.fn_color = fn_color
   self.grid_color = grid_color
   self.landmask_color = landmask_color
   IF N_Elements(landmask_value) NE 0 THEN self.landmask_value = Ptr_New(landmask_value)
   self.outline_color = outline_color
   self.map_grid = Keyword_Set(map_grid)
   self.map_fill = Keyword_Set(map_fill)
   self.map_outline = Keyword_Set(map_outline)
   self.oob_low_color = oob_low_color
   self.oob_high_color = oob_high_color
   self.vector_color = vector_color
   self.colorChangeAllowed = colorChangeAllowed
   self.colorChangeNColors = colorChangeNColors
   self.map_outline = Keyword_Set(map_outline)
   self.no_name_display = Keyword_Set(no_name_display)
   self.no_colorbar_display = Keyword_Set(no_colorbar_display)
   self.nsidc_tag = nsidc_tag
   IF N_Elements(filename) NE 0 THEN self.filename = filename
   IF self.filename NE "" THEN self -> SetProperty, NAME=cgRootName(self.filename)
   IF N_Elements(displayName) EQ 0 THEN displayName = cgRootName(self.filename)
   self.displayName = displayName

   ; Register properties.
   self->RegisterProperty, 'MISSING_COLOR', 0, NAME="Missing Color", USERDEF="Missing Color"
   self->RegisterProperty, 'OOB_LOW_COLOR', 0, NAME="Out of Bounds Low Color", USERDEF="Out-of-Bounds Low Color"
   self->RegisterProperty, 'OOB_HIGH_COLOR', 0, NAME="Out of Bounds High Color", USERDEF="Out-of-Bounds High Color"
   self->RegisterProperty, 'LANDMASK_COLOR', 0, NAME="Landmask Color", USERDEF="Landmask Color"
   self->RegisterProperty, 'FN_COLOR', 0, NAME="Filename Color", USERDEF="Filename Color"
   self->RegisterProperty, 'NO_NAME_DISPLAY', 1, NAME="No Name Display"
   self->RegisterProperty, 'NO_COLORBAR_DISPLAY', 1, NAME="No Colorbar Display"
   self->RegisterProperty, 'CB_FORMAT', 4, NAME="Colorbar Value Format"
   
   ; If you have a map coordinate object, then you can do map things, otherwise turn them off.
   self -> GetProperty, COORD_OBJECT=coords
   IF Obj_Class(coords) EQ 'MAPCOORD' THEN BEGIN
      self->RegisterProperty, 'MAP_GRID', 1, NAME="Display Map Grid"
      self->RegisterProperty, 'GRID_COLOR', 0, NAME="Map Grid Color", USERDEF="Grid Color"
      self->RegisterProperty, 'MAP_OUTLINE', 1, NAME="Display Map Outline" 
      self->RegisterProperty, 'OUTLINE_COLOR', 0, NAME="Map Outline Color", USERDEF="Outline Color"
      self->RegisterProperty, 'VECTOR_COLOR', 0, NAME="Motion Vector Color", USERDEF="Motion Vector Color"
   ENDIF ELSE BEGIN
      self.map_grid = 0
      self.map_outline = 0
   ENDELSE
   
   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       NSIDC_IMAGE CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the NSIDC_IMAGE object.
;
;*****************************************************************************************************
PRO NSIDC_Image__DEFINE, class

   class = { NSIDC_IMAGE, $
             annotatePixmap: Obj_New(), $ ; The window used for annotations.
             cb_format: "", $             ; The colorbar format.
             cb_type: 0, $                ; The type of color bar allowed. 0 (default) normal, 1 discrete, 2 none.
             colorChangeAllowed: 0B, $    ; A flag that indicates this image's colors can be changed by the user.
             colorChangeNColors: 0L, $    ; If colors can be changed, this indicates how many colors can be changed.
             contextmenu: Obj_New(), $    ; The context menu for the image selection events.
             filename: "", $              ; The filename of the image.
             displayName: "" , $          ; The name that should be used on the display.
             fn_color: "", $              ; The name of the filename color.
             landmask_color: "", $        ; The name of the landmask color.
             landmask_value: Ptr_New(), $ ; The value of the landmask in the image.
             grid_color: "", $            ; The name of the map grid color.
             outline_color: "", $         ; The name of the map outline or fill color.
             vector_color: "", $          ; The name of the color for drawing motion vectors on images.
             map_fill: 0B, $              ; A flag that indicates a filled map outline should be drawn on the image.
             map_outline: 0B, $           ; A flag that indicates a map outline should be drawn on the image.
             map_grid: 0B, $              ; A flag that indicates a map grid should be drawn on the image.
             no_name_display:0B, $        ; A flag that indicates the filename should NOT be displayed.
             no_colorbar_display:0B, $    ; A flag that indicates the colorbar should NOT be displayed.
             nsidc_tag: "", $             ; The NSIDC number associated with this image, eg, "nsidc-0032".
             oob_low_color: "", $         ; The name of the out-of-bounds low color.
             oob_high_color: "", $        ; The name of the out-of-bounds high color.
             INHERITS SCALEIMAGE $
           }

END


