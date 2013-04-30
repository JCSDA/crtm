;*****************************************************************************************************
;+
; NAME:
;       IMGWIN
;
; PURPOSE:
;
;       This is an image display routine to allow the user to interact with
;       an image. Moving the cursor in the window gives the value and location
;       inside the image. The image is enclosed in a SCALEIMAGE object, so many of 
;       the input keywords are used to set the scaling parameters for that object. 
;       If axes are requested, an IMGAXIS object is added to the SCALEIMAGE object. 
;       Other keywords are used to set up the axis object. The entire window can
;       be saved to file in various formats. The image can be scaled interactively,
;       image colors can be changed, and various image and axes properties can be
;       accessed directly from the File menu of the image window.
;       
; AUTHORS:
;
;        FANNING SOFTWARE CONSULTING   BURRIDGE COMPUTING
;        1645 Sheely Drive             18 The Green South
;        Fort Collins                  Warborough, Oxon
;        CO 80526 USA                  OX10 7DN, ENGLAND
;        Phone: 970-221-0438           Phone: +44 (0)1865 858279
;        E-mail: davidf@dfanning.com   E-mail: davidb@burridgecomputing.co.uk
;
; CATEGORY:
;
;       Graphics display.
;
; SYNTAX:
;
;       ImgWin, image
;       
; ARGUMENTS:
; 
;     image    An 8-bit or 24-bit image array, or the name of an image file that
;              can be opened with READ_IMAGE. 
;       
; INPUT_KEYWORDS:
;
;     AXES:        Set this keyword to draw a set of axes around the image.
;
;     BACKGROUND:  This keyword specifies the name of a background color. By default, 'ivory'.
;
;     BETA:        The beta factor in a Hyperpolic Sine stretch. Default is 3.0.
;
;     BOTTOM:      The lowest value of the scaled image.
;     
;     BREWER:      Set if the color table index number (CT) is the index of a Brewer color table.
;                  To use Brewer color tables, the file fsc_brewer.tbl must be in your IDL path.
;                  
;     COLOR:       Set this keyword to the name of the axis color. The name is passed to cgColor 
;                  for processing. Default is "charcoal". Used only if AXES is set.
;
;     CTINDEX:     The index of the color table to use to display the image. Applies only to 
;                  2D image arrays. By default, 0, gray scale. If set to -1, uses current color table.
;
;     EXPONENT:    The logarithm exponent in a logarithmic stretch. Default is 4.0.
;     
;     FULL_RESOLUTION: Set this keyword if you want the image to be displayed in its full, native resolution.
;                   If this keyword is set, the XSIZE and YSIZE keywords set the scrollable window size.
;
;     GAMMA:       The gamma factor in a gamma stretch. Default is 1.5.
;
;     KEEP_ASPECT:  Normally, the image will be resized to fit the specified position in the 
;                   window. If you prefer, you can force the image to maintain its aspect ratio 
;                   in the window (although not its natural size) by setting this keyword.
;                   The image width is fitted first. If, after setting the image width, the image 
;                   height is too big for the window, then the image height is fitted into the window. 
;                   The appropriate values of the POSITION keyword are honored during this fitting 
;                   process. Once a fit is made, the POSITION coordiates are re-calculated to center 
;                   the image in the window. You can recover these new position coordinates as the 
;                   output from the POSITION keyword. This keyword is turned ON by default. In other 
;                   words, to allow free positioning, set KEEP_ASPECT=0. Note that if this keyword is
;                   set, and the XSIZE and YSIZE keywords are undefined, that the window will have the
;                   same aspect ratio as the image.
;                   
;     MAP_COORD:    A MapCoord object that can be used to georeference the image in the display window.
;
;     MAP_GRID:     If the file can be georegistered (e.g. a GeoTIFF file), then setting this keyword
;                   will add a map grid to the MapCoord object. Otherwise, the keyword is
;                   ignored. Can only be used when passing ImgWin a GeoTIFF filename.
;
;     MAP_OUTLINE:  If the file can be georegistered (e.g. a GeoTIFF file), then setting this keyword
;                   will add a continental outline to the MapCoord object. Otherwise, the keyword is
;                   ignored. Can only be used when passing ImgWin a GeoTIFF filename.
;                  
;     MEAN:         The mean factor in a logarithmic stretch. Default is 0.5.
;
;     MISSING_COLOR: The name of the missing color. The default is "gray".
;
;     MISSING_VALUE: The number that represents missing value in the image.
;
;     NCOLORS:       The number of colors to scale the data into, as in this: (Default: 256)
;
;                       displayImage = BYTSCL(image, MIN=self.sclmin, MAX=self.sclmax, TOP=self.ncolors-1)
;
;     NOINTERP:      Setting this keyword disables the default bilinear
;                    interpolation done to the image when it is resized. Nearest
;                    neighbor interpolation is done instead. This is preferred
;                    when you do not wish to change the pixel values of the image.
;                    This keyword is turned ON by default. In other words, to allow
;                    interpolation, set NOINTERP=0.
;
;     POSITION:      The position of the image in the display window. The position is given
;                    as a four-element array in normalized (0 to 1) coordinates of the form
;                    [x0, y0, x1, y1], where (x0,y0) is the lower-left corner of the image and
;                    (x1,y1) is the upper-right corner of the image. If the KEEP_ASPECT keyword
;                    is set, the image will be located within the specified POSITION in a way
;                    that preserves the aspect ratio of the image. The default is [0.075, 0.075, 0.925, 0.925].
;                    
;                    
;     SCALETYPE:     The type of scaling performed prior to display. Default is 0, linear scaling.
;                    May be specified as a number or as a string (e.g, 3 or "ASINH").
;
;           Number   Type of Stretch
;             0         Linear         scaled = BytScl(image, MIN=minThresh, MAX=maxThresh)
;             1         Gamma          scaled = GmaScl(image, MIN=minThresh, MAX=maxThresh, Gamma=gamma)
;             2         Log            scaled = LogScl(image, MIN=minThresh, MAX=maxThresh, Mean=mean, Exponent=exponent)
;             3         Asinh          scaled = AsinhScl(image, MIN=minThresh, MAX=maxThresh, Beta=beta)
;             4         Linear 2%      A linear stretch, with 2 percent of pixels clipped at both the top and bottom
;             5         Square Root    A linear stretch of the square root histogram of the image values.
;             6         Equalization   A linear stretch of the histogram equalized image histogram.
;             7         Gaussian       A Gaussian normal function is applied to the image histogram.
;             8         None           No scaling whatsoever is done.
;
;     SCLMIN:         The image data is scaled between SCLMIN and SCLMAX before display. Default = 0.
;
;     SCLMAX:         The image data is scaled between SCLMIN and SCLMAX before display. Default = 255.
;
;     SIGMA:          The sigma scale factor for Gaussian scaling. Default is 1.0.
;     
;     TLB_TITLE:      The title of the IMGWIN top-level base. Default: "Catalyst Image Window
;     
;     WIN_KEEP_ASPECT: If this keyword is set, the IMGWIN window will maintain the same aspect ratio
;                     as the image as it is resized.
;     
;     XRANGE:         If the AXES keyword is set, this keyword is a two-element vector
;                     giving the X axis range. By default, [0, size of image in X].
;                    
;     XSIZE:          The X size of the initial image window. If undefined, appoximately 600 pixels.
;                     (Acutally size determined by the aspect ratio of the image.)
;                    
;     XTICKFORMAT:    The tick formatting for the X axis, if the AXES keyword is set.
;                    
;     XTILE:          The title of the X axis, if the AXES keyword is set.
;
;     YRANGE:         If the AXES keyword is set, this keyword is a two-element vector
;                     giving the Y axis range. By default, [0, size of image in Y].
;                    
;     YSIZE:          The Y size of the initial image window. If undefined, appoximately 600 pixels.
;                     (Acutally size determined by the aspect ratio of the image.)
;
;     YTICKFORMAT:    The tick formatting for the Y axis, if the AXES keyword is set.
;                    
;     YTILE:          The title of the Y axis, if the AXES keyword is set.
;     
; OUTPUT_KEYWORDS:
; 
;     OUTIMAGE:        The image object that is created inside the program.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 12 October 2008.
;       Fixed a problem with no image updates after writing the image to PostScript output. 8 Nov 2008. DWF.
;       Whoops! Fixed a problem with the KEEP_ASPECT keyword not working. 11 Dec 2008. DWF.
;       Added WIN_KEEP_ASPECT to allow the display window to keep the same aspect ratio as the image
;          upon resize. 11 Dec 2008. DWF.
;       A change to XStretch on 15 March revealed a small bug in the way XStretch was being called
;          in IMGWIN. It is now being called with the proper keywords. 18 March 2009. DWF.
;       Fixed a small problem with the way the CTINDEX keyword was handled. 8 May 2009. DWF.
;       PNG files with more than 8-bits per channel are now being handled correctly. 5 August 2009. DWF.
;       Modified to georegister GEOTIFF files when they are opened via a GeoTIFF file name. 30 August 2009. DWF.
;       Modified code so that input parameter data type would not change. 13 December 2009. DWF.
;       Fixed a problem with the image variable name when the name of a GeoTIFF is passed to
;           the program. 10 February 2010. DWF.
;       Fixed a problem with CTINDEX=-1 not working when passed GeoTIFF files. 21 Febrary 2010. DWF.
;       Added AUTODRAWGRID keyword to MAP_GRID calls. 11 March 2010.
;       Overly ambitious cleanup was killing passed in map coordinate variable Fixed. 24 March 2010. DWF.
;-
;*****************************************************************************************************
PRO ImgWin::CreateStatusBar

; The purpose of this method is to create the statusbar for the program.

   ; Create a statusbar object.
   self._statusbar = OBJ_NEW('STATUSBAR', self, Name='Statusbar', /Align_Left)


END
;*****************************************************************************************************



PRO ImgWin::EventHandler, event

; This is the main event handler for the text program. All widget objects
; causing events have been named so we can branch on EVENT.NAME.

   ; Branch on the object name. Check the GUI method to see which names apply
   ; to which widgets. The event names are listed in alphabetical order in the
   ; following CASE statement.
   CASE StrUpCase(event.name) OF

      ; Allows the user to set the axes properties interactively.
      'AXES_PROPERTIES': BEGIN
            self.theDrawWidget -> SetWindow
            self.theAxes -> ControlPanel
        END

      ; Exits the program and destroys the TLB self object.
      'EXIT' : OBJ_DESTROY, self
      
;      ; Creates a full-resolution image.
;      'FULL_RESOLUTION': BEGIN
;            Widget_Control, /HOURGLASS
;            imageCopy = self.theImage -> Copy()
;            ImgWin, imageCopy, /FULL_RESOLUTION, /KEEP_ASPECT, $
;                /NOINTERP, TLB_TITLE='Catalyst Full-Resolution Window', $
;                GROUP_LEADER=self
;         END
;         
;      ; Creates a reduced-resolution image.
;      'REDUCED_RESOLUTION': BEGIN
;            self.theImage -> GetProperty, XSIZE=xsize, YSIZE=ysize, ASPECT=imgAspect
;            maxSize = 600 
;            maximageSize = Max([xsize, ysize])
;            IF 2*maximageSize LT maxsize THEN maxsize = 350 > 2*maximagesize
;            IF imgAspect GE 1 THEN BEGIN
;                ywinsize = maxsize
;                xwinsize = maxsize / imgAspect
;            ENDIF ELSE BEGIN
;                xwinsize = maxsize
;                ywinsize = maxsize * imgAspect
;            ENDELSE
;            Widget_Control, /HOURGLASS
;            imageCopy = self.theImage -> Copy()
;            
;            ImgWin, imageCopy, /KEEP_ASPECT, $
;                /NOINTERP, TLB_TITLE='Catalyst Image Window', $
;                XSIZE = xwinsize, YSIZE=ywinsize, $
;                GROUP_LEADER=self
;         END
                
      ; Allows the user to flip the image vertically.
      'FLIP_IMAGE': BEGIN
            self.theDrawWidget -> SetWindow
            self.theImage -> GetProperty, IMAGE=theImage
            dims = Image_Dimensions(theImage, YINDEX=yindex)
            image = Reverse(Temporary(theImage), yindex+1)
            self.theImage ->SetProperty, IMAGE=image
            self.theDrawWidget -> Draw
        END


      ; Allows the user to change image colors.
      'IMAGE_COLORS': BEGIN
      
            ; Make sure this is the current graphics widnow.
            self.theDrawWidget -> SetWindow
            
            ; Load the current image colors so that XCOLORS starts
            ; up with the proper colors.
            self.theImage -> GetProperty, COLOR_OBJECT=colors
            colors -> GetProperty, COLORPALETTE=palette, BREWER=brewer, NCOLORS=ncolors
            TVLCT, palette
            
            ; Put the XCOLORS palette next to this window.
            self -> GetProperty, XOFFSET=xoffset, YOFFSET=yoffset, XSIZE=xsize, YSIZE=ysize
            colors -> XColors, XOFFSET=xoffset + xsize + 15, $
                 YOFFSET=yoffset + 100, GROUP_LEADER=self
                
        END

      ; Allows the user to set the image properties interactively.
      'IMAGE_PROPERTIES': BEGIN
            self.theDrawWidget -> SetWindow
            self.theImage -> ControlPanel
        END

      ; This is where draw widget events are handled.
      'IMGWIN_DRAWWIDGET': BEGIN

         ; Make this draw widget the current window.
         event.id -> SetWindow

         ; Get the image associated with this draw widget.
         imageObject = event.ID -> Get(Position=0)

         ; Find the RGB value of the image at this location and display in statusbar.
         value = imageObject -> Pixel_To_Value(event.x, event.y, Inside=inside, XPixel=xpix, YPixel=ypix, $
            XData=xdata, YData=ydata)
         IF inside EQ 0 THEN self._statusbar -> SetProperty, Text='Outside Image' ELSE $
         BEGIN
            imageObject -> GetProperty, N_DIMENSIONS=ndims
            IF ndims EQ 2 THEN BEGIN
                s = 'Value: ' + StrTrim(value,2) 
                s = s + '    Pixel Loc: (' + StrTrim(xpix,2) + ', ' + StrTrim(ypix,2) + ')' + $
                '    Data Loc: (' + String(xdata,Format='(F0.2)') + ', ' + String(ydata,Format='(F0.2)') + ')'                
            ENDIF ELSE BEGIN
                s = 'R: ' + StrTrim(value[0],2) + '  G: ' + StrTrim(value[1],2) + '  B: ' + StrTrim(value[2],2)
                s = s + '    Pixel Loc: (' + StrTrim(xpix,2) + ', ' + StrTrim(ypix,2) + ')' + $
                '    Data Loc: (' + String(xdata,Format='(F0.2)') + ', ' + String(ydata,Format='(F0.2)') + ')'
            ENDELSE
            self._statusbar -> SetProperty, Text=s
         ENDELSE
         END
                         
       ; This is a resize event from the TLB.
       'IMGWIN_TLB': BEGIN
       
            ; Get the size of the image, as you might need it.
            self.theImage -> GetProperty, XSIZE=imgxsize, YSIZE=imgysize
            
            ; Set the size of the draw widget resize.
            IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' THEN BEGIN
                s = [event.x - self.xoffset, event.y - self.yoffset]
            ENDIF ELSE BEGIN
                s = [event.x-self.xoffset+1, event.y]
            ENDELSE
            
            IF self.win_keep_aspect THEN BEGIN
                aspectRatio = Float(imgysize) / imgxsize
                IF aspectRatio GT 1 THEN BEGIN
                   s = [ s[1]/aspectRatio, s[1]]
                ENDIF ELSE BEGIN
                   s = [ s[0], s[0]*aspectRatio]
                ENDELSE
            ENDIF
                
            ; Resize the draw widget.
            IF self.full_resolution EQ 0 THEN BEGIN
                self.theDrawWidget -> Resize, s[0], s[1]
            ENDIF ELSE BEGIN
               self.theDrawWidget -> Resize, s[0], s[1], /VIEWPORT
            ENDELSE
            
            ; Update the size of the statusbar to reflect the size of the draw widget.
            self._statusbar -> Resize, self.theDrawWidget
            
            ; Redraw the image in the newly-resized draw widget.
            self.theDrawWidget -> Draw
            
        END
        
      ; The SAVE AS... buttons come here. Just get the type of file out of the UVALUE of the
      ; button object and tell the draw widget to create a file of this type.
      'SAVE_WINDOW': BEGIN
     
            ; Call the OUTPUT method on the draw widget.
            event.ID -> GetProperty, UVALUE=fileType
            self.theDrawWidget -> SetWindow
            self.theDrawWidget -> Output, TYPE=fileType, FILENAME='imgwin'

            ; If the filetype is POSTSCRIPT, then the image objects can come
            ; back from drawing into the PostScript device with their locations
            ; changed (the PostScript "window" may not have the same aspect ratio
            ; as the display window). Since image "selection" depends on location
            ; in a display window, it may not be possible to select these images
            ; when you have returned. The solution is to update the image "locations".
            ; The easiest way to do this is to simply redraw the images in the display
            ; window. An alternative is the set the DRAW keyword on the call to the
            ; OUTPUT method above. But, I've keep this for backward compatibility issues.
            IF StrUpCase(filetype) EQ 'POSTSCRIPT' THEN self.theDrawWidget -> Draw
            END
            
      ; This allows the user to scale the image interactively. XSTRETCH is called, and stretch
      ; events are sent to the XSTRETCH_NOTIFICATION method.
      'SCALE_IMAGE': BEGIN

            ; Get the current stretch parameters from the image so we can configure
            ; XSTRETCH properly.
            self.theImage -> GetProperty, $
               BETA=beta, $
               BOTTOM=bottom, $
               EXPONENT=exponent, $
               GAMMA=gamma, $
               IMAGE=image, $
               MEAN=mean, $
               NCOLORS=ncolors, $
               NEGATIVE=negative, $
               SCALETYPE=scaletype, $
               SCLMIN=sclmin, $
               SCLMAX=sclmax, $
               SIGMA=sigma

            ; Make sure you are drawing in the right window. Start XSTRETCH.
            self.theDrawWidget -> SetWindow
            cgStretch, image, GROUP_LEADER=self->GetID(), /NO_WINDOW, $
               BETA=beta, $
               EXPONENT=exponent, $
               GAMMA=gamma, $
               MEAN=mean, $
               NEGATIVE=negative, $
               TYPE=scaletype, $
               MINTHRESH=sclmin, $
               MAXTHRESH=sclmax, $
               SIGMA=sigma, $
               NOTIFY_OBJ={object:self, method:'XStretch_Notification'}
      
            END
            
      'SHRINK_TO_FIT': BEGIN
            self.theImage -> GetProperty, LOCATION=location
            xsize = location[2,0] - location[0,0] + 1
            ysize = location[3,0] - location[1,0] + 1
            self.theDrawWidget -> Resize, xsize, ysize, /Draw
            self._statusbar -> Resize, self.theDrawWidget
            END
                  
       ELSE  : self._statusbar -> SetProperty, Text= 'Unexpected event in IMGWIN'

   ENDCASE

END
;*****************************************************************************************************


PRO ImgWin::GUI, menuBar, $
        XWINSIZE=xwinsize, $
        YWINSIZE=ywinsize, $
        BACKGROUND=background, $
        AXES=axes, $
        FULL_RESOLUTION=full_resolution, $
        WIN_KEEP_ASPECT=win_keep_aspect

        
; The purpose of this method is create all the graphical user interface elements.
; Widgets that cause events are named, and the EventHandler method descriminates
; based on the NAME field of the event structure.

   IF N_Elements(xwinsize) EQ 0 THEN xwinsize = 500
   IF N_Elements(ywinsize) EQ 0 THEN ywinsize = 500
   IF N_Elements(background) EQ 0 THEN background = 'ivory'
   axes = Keyword_Set(axes)
   full_resolution = Keyword_Set(full_resolution)
   win_keep_aspect = Keyword_Set(win_keep_aspect)
   
   ; Create a status bar.
   self -> CreateStatusBar
   
   ; Create a Quit button in the menu bar.
   fileMenu = OBJ_NEW ('ButtonWidget', menuBar ,  Value='File', /MENU)
   
   ; Add color change and scaling buttons if image is 2D.
   self.theImage -> GetProperty, N_DIMENSIONS=n_dims, XSIZE=imgXsize, YSIZE=imgYsize
   
   ; If the window aspect ratio is to be maintained, you will have to modify the window
   ; sizes here.
   IF win_keep_aspect THEN BEGIN
     aspectRatio = Float(imgYsize) / imgXsize
     IF aspectRatio GE 1 THEN BEGIN
        xwinsize = ywinsize / aspectRatio
     ENDIF ELSE BEGIN
        ywinwise = xwinsize * aspectRatio
     ENDELSE
   ENDIF
   IF n_dims EQ 2 THEN BEGIN
        button = OBJ_NEW('ButtonWidget', fileMenu, Name='IMAGE_COLORS', Value='Change Image Colors')
        button = OBJ_NEW('ButtonWidget', fileMenu, Name='SCALE_IMAGE', Value='Scale Image')
        separator = 1
   ENDIF ELSE separator = 0
   button = OBJ_NEW('ButtonWidget', fileMenu, Name='FLIP_IMAGE', Value='Flip Image Vertically')
   saveasID = OBJ_NEW ('ButtonWidget', fileMenu,  Value='Save Window As...', /MENU, SEPARATOR=separator)
   button = OBJ_NEW ('ButtonWidget', saveasID,  Name='SAVE_WINDOW', Value='JPEG file', UVALUE='JPEG')
   button = OBJ_NEW ('ButtonWidget', saveasID,  Name='SAVE_WINDOW', Value='TIFF File', UVALUE='TIFF')
   button = OBJ_NEW ('ButtonWidget', saveasID,  Name='SAVE_WINDOW', Value='BMP File', UVALUE='BMP')
   button = OBJ_NEW ('ButtonWidget', saveasID,  Name='SAVE_WINDOW', Value='PNG File', UVALUE='PNG')
   button = OBJ_NEW ('ButtonWidget', saveasID,  Name='SAVE_WINDOW', Value='PostScript File', UVALUE='POSTSCRIPT')
   
;   IF full_resolution THEN BEGIN
;       button = OBJ_NEW ('ButtonWidget', fileMenu,  Name='REDUCED_RESOLUTION', $
;            Value='Open Reduced Resolution Image', /Separator)
;       button = OBJ_NEW ('ButtonWidget', fileMenu,  Name='SHRINK_TO_FIT', $
;            Value='Shrink Window to Fit Image')
;   ENDIF ELSE BEGIN
;       button = OBJ_NEW ('ButtonWidget', fileMenu,  Name='FULL_RESOLUTION', $
;            Value='Open Full Resolution Image', /Separator)
;       button = OBJ_NEW ('ButtonWidget', fileMenu,  Name='SHRINK_TO_FIT', $
;            Value='Shrink Window to Fit Image')
;   ENDELSE
   
   properties = OBJ_NEW ('ButtonWidget', fileMenu,  Value='Properties...', /MENU, SEPARATOR=1)
   button = OBJ_NEW ('ButtonWidget', properties,  Name='IMAGE_PROPERTIES', Value='Image Properties')
   IF Keyword_Set(axes) THEN button = OBJ_NEW ('ButtonWidget', properties,  Name='AXES_PROPERTIES', $
        Value='Axes Properties')
 
    exitBttn = OBJ_NEW ('ButtonWidget', fileMenu,  Name='Exit', Value='Exit', /Separator)
   
    IF Keyword_Set(self.full_resolution) THEN BEGIN
        IF StrUpCase(!VERSION.os_family) EQ 'WINDOWS' THEN BEGIN
            drawObj = OBJ_NEW ('SelectableDrawWidget', self, XSIZE=imgXsize, YSize=imgYsize, $
                 Erase_Window=1, INITIAL_COLOR=background, BUTTON_EVENTS=1, Name='IMGWIN_DRAWWIDGET', $
                 /Notify_Realize, MOTION_EVENTS=1, X_SCROLL_SIZE=xwinsize < imgXsize, $
                 Y_SCROLL_SIZE=ywinsize < imgYsize)
         ENDIF ELSE BEGIN
            ; Any mention of SCROLL bars, puts them on, even if they are not needed. :-(
            IF (imgXSize GT xwinsize) OR (imgYsize GT ywinsize) THEN BEGIN
                drawObj = OBJ_NEW ('SelectableDrawWidget', self, XSIZE=imgXsize, YSize=imgYsize, $
                     Erase_Window=1, INITIAL_COLOR=background, BUTTON_EVENTS=1, Name='IMGWIN_DRAWWIDGET', $
                     /Notify_Realize, MOTION_EVENTS=1, X_SCROLL_SIZE=xwinsize < imgXsize, $
                     Y_SCROLL_SIZE=ywinsize < imgYsize)
            ENDIF ELSE BEGIN
                drawObj = OBJ_NEW ('SelectableDrawWidget', self, XSIZE=imgXsize, YSize=imgYsize, $
                     Erase_Window=1, INITIAL_COLOR=background, BUTTON_EVENTS=1, Name='IMGWIN_DRAWWIDGET', $
                     /Notify_Realize, MOTION_EVENTS=1)
                 self.full_resolution = 0
            ENDELSE
         
         ENDELSE
    ENDIF ELSE BEGIN
        drawObj = OBJ_NEW ('SelectableDrawWidget', self, XSIZE=xwinsize < imgxsize, YSize=ywinsize < imgysize, $
             Erase_Window=1, INITIAL_COLOR=background, BUTTON_EVENTS=1, Name='IMGWIN_DRAWWIDGET', $
             /Notify_Realize, MOTION_EVENTS=1)
    ENDELSE
    drawObj -> Add, self.theImage
    self.theDrawWidget = drawObj
   
    ; Display the entire application in the window.
    self -> Draw, /Center

    self -> GetProperty, GEOMETRY=tlbgeo
    drawObj -> GetProperty, GEOMETRY=drawgeo
    menubar -> GetProperty, GEOMETRY=menugeo
    self._statusBar -> GetProperty, GEOMETRY=statgeo
    
    ; Set up offsets for resizing the window. This is machine
    ; dependent because UNIX machines report their "size" differently
    ; than WINDOWS machines.
    self.xoffset = tlbgeo.scr_xsize - drawgeo.scr_xsize 
    self.yoffset = tlbgeo.scr_ysize - (drawgeo.scr_ysize  + menugeo.scr_ysize + statgeo.scr_ysize)

    ; Make sure the image object *always* draws into this window.
    self.theImage -> SetProperty, WID=drawObj

END
;*****************************************************************************************************


PRO ImgWin::XStretch_Notification, info

; When stretch parameters are changed in XSTRETCH, those parameters are bundled up
; in an info structure, which is passed to this method. Use the info parameters to 
; configure the image.

    @cat_pro_error_handler
    
    ; Pass the new XSTRETCH parameters on to the image.
    IF StrUpCase(info.event_handler) EQ 'XSTRETCH_STRETCHTYPE' THEN BEGIN
        self.theImage -> SetProperty, SCALETYPE=info.type, SCLMIN=info.minThresh, SCLMAX=info.maxThresh, $
               GAMMA=info.gamma, BETA=info.beta, MEAN=info.mean, EXPONENT=info.exponent
    ENDIF ELSE BEGIN
        self.theImage -> SetProperty, SCLMIN=info.minThresh, SCLMAX=info.maxThresh
    ENDELSE
          
    ; Redraw the image in the window
    self.theDrawWidget -> SetWindow
    self.theImage -> Draw
    
END
;*****************************************************************************************************


PRO ImgWin::CLEANUP
    
    @cat_pro_error_handler
    
    IF Obj_Valid(self.theImage) THEN self.theImage -> RemoveParent, self
    Obj_Destroy, self.theDrawWidget
    Obj_Destroy, self.theAxes
    Obj_Destroy, self._statusbar
    
    self -> TOPLEVELBASE::Cleanup
    
    self -> Report, /Completed
    
END
;*****************************************************************************************************


FUNCTION ImgWin::INIT, image, $
    AXES=axes, $
    BACKGROUND=background, $
    BETA=beta, $
    BOTTOM=bottom, $
    BREWER=brewer, $
    COLOR=color, $
    CTINDEX=ctindex, $
    CTOBJECT=ctobject, $
    EXPONENT=exponent, $
    FULL_RESOLUTION=full_resolution, $
    GAMMA=gamma, $
    KEEP_ASPECT=keep_aspect, $
    MAP_COORD=map_coord, $
    MAP_GRID=map_grid, $
    MAP_OUTLINE=map_outline, $
    MEAN=mean, $
    MISSING_COLOR=missing_color, $
    MISSING_VALUE=missing_value, $
    NCOLORS=ncolors, $
    NOINTERPOLATION=nointerp, $
    POSITION=position, $
    SCALETYPE=scaletype, $
    SCLMIN=sclmin, $
    SCLMAX=sclmax, $
    SIGMA=sigma, $
    TLB_TITLE=tlb_title, $
    WIN_KEEP_ASPECT=win_keep_aspect, $
    XRANGE=xrange, $
    XSIZE=xwinsize, $
    XTICKFORMAT=xtickformat, $
    XTITLE=xtitle, $
    YRANGE=yrange, $
    YSIZE=ywinsize, $
    YTICKFORMAT=ytickformat, $
    YTITLE=ytitle, $
    OUTIMAGE=theImage, $   An output keyword.
    _Ref_Extra=extra
    
    ; Catch error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Must have an image to display. This could be the name of an image file,
    ; or it could be the image itself. Give the user a change to pick an image
    ; file is an image is not specified.
    IF N_Elements(image) EQ 0 THEN BEGIN
       imageIsObject = 0
       imageFile = Dialog_Pickfile(TITLE='Select image file...')
       IF imageFile EQ "" THEN RETURN, 0
       ok = Query_Image(imageFile, HAS_PALETTE=has_palette, TYPE=imagetype)
       IF ~ok THEN Message, 'Image file can not be read with READ_IMAGE.'
       IF imagetype EQ 'TIFF' THEN BEGIN
         ok = Query_TIFF(imageFile, fileInfo, GEOTIFF=geotiff)
         IF ok THEN BEGIN
            CASE fileInfo.channels OF
               3: theImage = Read_TIFF(imageFile, _EXTRA=extra, ORIENTATION=orientation)
               ELSE: theImage = Read_TIFF(imageFile, r, g, b, _EXTRA=extra, ORIENTATION=orientation)
            ENDCASE
            IF fileInfo.has_palette EQ 1 THEN colorPalette = [[r], [g], [b]]
            IF orientation EQ 1 THEN BEGIN
                dims = Image_Dimensions(theImage, YINDEX=yindex)
                theImage = Reverse(Temporary(theImage), yindex+1)
            ENDIF
         ENDIF
         IF Size(geotiff, /TNAME) EQ 'STRUCT' THEN BEGIN
             mapCoord = GeoCoord(imageFile, SUCCESS=success, /SILENT)
             IF success AND Keyword_Set(map_outline) THEN BEGIN
                    outline = Obj_New('Map_Outline', MAP_OBJECT=mapCoord, /HIRES, $
                        COLOR='indian red', FILL=Keyword_Set(fill))
                    mapCoord -> SetProperty, OUTLINE_OBJECT=outline
             ENDIF
             IF success AND Keyword_Set(map_grid) THEN BEGIN
                    grid = Obj_New('Map_Grid', MAP_OBJECT=mapCoord, COLOR='indian red', /AUTODRAWGRID)
                    mapCoord -> SetProperty, GRID_OBJECT=grid
             ENDIF
             IF success EQ 1 THEN BEGIN
                IF fileInfo.has_palette EQ 1 THEN colors = Obj_New('CatColors', COLORPALETTE=colorpalette)
                IF Obj_Valid(colors) THEN BEGIN
                    theImageObj = Obj_New('CatImage', theImage, COORD_OBJECT=mapCoord, COLOR_OBJECT=colors)
                ENDIF ELSE BEGIN
                    theImageObj = Obj_New('ScaleImage', theImage, theImage, $
                        NAME='IMGWIN_IMAGE', $
                        BETA=beta, $
                        BOTTOM=bottom, $
                        COORD_OBJECT=coords, $
                        EXPONENT=exponent, $
                        GAMMA=gamma, $
                        KEEP_ASPECT=keep_aspect, $
                        MEAN=mean, $
                        MISSING_COLOR=missing_color, $
                        MISSING_VALUE=missing_value, $
                        NCOLORS=ncolors, $
                        NOINTERPOLATE=nointerp, $
                        POSITION=position, $
                        SCALETYPE=scaletype, $
                        SCLMIN=sclmin, $
                        SCLMAX=sclmax, $
                        SELECTABLE=1, $
                        SIGMA=sigma)
                ENDELSE
                imageIsObject = 1
                GOTO, DefineWidgets
            ENDIF
         ENDIF
       ENDIF ELSE BEGIN
            theImage = Read_Image(imageFile, r, g, b)
                IF N_Elements(theImage) EQ 1 THEN Message, 'Image file can not be read with READ_IMAGE.'
       ENDELSE
       IF has_palette THEN BEGIN
            colorPalette = [[r], [g], [b]]
            scaletype = 'NONE'
       ENDIF
       
       ; Some PNG files (from ImageMagick, for example) are created with 16-bits per channel.
       ; Convert this to 8-bits per channel, and also remove any alpha channel for this application.
       root_name = cgRootName(imageFile, EXTENSION=ext)
       IF StrUpCase(ext) EQ 'PNG' THEN BEGIN
            IF Size(theImage, /TNAME) NE 'BYTE' THEN theImage = BytScl(theImage)
            IF Size(theImage, /N_DIMENSIONS) GT 3 THEN BEGIN
                dims = Image_Dimensions(theImage, TRUEINDEX=trueindex)
                CASE trueIndex OF
                    0: theImage = theImage[0:2, *, *]
                    1: theImage = theImage[*, 0:2, *]
                    2: theImage = theImage[*, *, 0:2]
                ENDCASE
            ENDIF
       ENDIF
    ENDIF
    
    ; If you have an image at this point, skip the next section.
    IF N_Elements(theImage) NE 0 THEN Goto, DefineWidgets
    
    ; This can be the name of a file to read, an actual image variable, or an image object.
    imageIsObject = 0
    CASE Size(image, /TNAME) OF
    
        'STRING': BEGIN
           IF image EQ "" THEN Return, 0
           imagefile = image
           ok = Query_Image(imagefile, HAS_PALETTE=has_palette, TYPE=imagetype)
           IF ~ok THEN Message, 'Image is not of type IMGWIN can read.'
           IF imagetype EQ 'TIFF' THEN BEGIN
             ok = Query_TIFF(imageFile, fileInfo, GEOTIFF=geotiff)
             IF ok THEN BEGIN
                CASE fileInfo.channels OF
                   3: theImage = Read_TIFF(imageFile, _EXTRA=extra, ORIENTATION=orientation)
                   ELSE: theImage = Read_TIFF(imageFile, r, g, b, _EXTRA=extra, ORIENTATION=orientation)
                ENDCASE
                IF fileInfo.has_palette EQ 1 THEN BEGIN
                    colorPalette = [[r], [g], [b]]
                ENDIF ELSE BEGIN
                    ; Did the user say to use the current color table?
                    IF N_Elements(ctindex) NE 0 THEN BEGIN
                        IF ctindex EQ -1 THEN BEGIN
                            TVLCT, r, g, b, /GET
                            colorPalette = [[r], [g], [b]]
                        ENDIF
                    ENDIF
                ENDELSE
                IF orientation EQ 1 THEN BEGIN
                    dims = Image_Dimensions(theImage, YINDEX=yindex)
                    theImage = Reverse(Temporary(theImage), yindex+1)
                ENDIF
             ENDIF
             IF Size(geotiff, /TNAME) EQ 'STRUCT' THEN BEGIN
                mapCoord = GeoCoord(imageFile, SUCCESS=success, /SILENT)
                IF success AND Keyword_Set(map_outline) THEN BEGIN
                    outline = Obj_New('Map_Outline', MAP_OBJECT=mapCoord, /HIRES, $
                        COLOR='indian red', FILL=Keyword_Set(fill))
                    mapCoord -> SetProperty, OUTLINE_OBJECT=outline
                ENDIF
                IF success AND Keyword_Set(map_grid) THEN BEGIN
                    grid = Obj_New('Map_Grid', MAP_OBJECT=mapCoord, $
                        COLOR='indian red', /AUTODRAWGRID)
                    mapCoord -> SetProperty, GRID_OBJECT=grid
                ENDIF
                IF success EQ 1 THEN BEGIN
                    IF fileInfo.has_palette EQ 1 THEN colors = Obj_New('CatColors', COLORPALETTE=colorpalette)
                    IF Obj_Valid(colors) THEN BEGIN
                        theImageObj = Obj_New('CatImage', theImage, COORD_OBJECT=mapCoord, COLOR_OBJECT=colors)
                    ENDIF ELSE BEGIN
                        theImageObj = Obj_New('ScaleImage', theImage,$
                            NAME='IMGWIN_IMAGE', $
                            BETA=beta, $
                            BOTTOM=bottom, $
                            COORD_OBJECT=coords, $
                            EXPONENT=exponent, $
                            GAMMA=gamma, $
                            KEEP_ASPECT=keep_aspect, $
                            MEAN=mean, $
                            MISSING_COLOR=missing_color, $
                            MISSING_VALUE=missing_value, $
                            NCOLORS=ncolors, $
                            NOINTERPOLATE=nointerp, $
                            POSITION=position, $
                            SCALETYPE=scaletype, $
                            SCLMIN=sclmin, $
                            SCLMAX=sclmax, $
                            SELECTABLE=1, $
                            SIGMA=sigma)
                    ENDELSE
                    imageIsObject = 1
                    GOTO, DefineWidgets
                ENDIF
             ENDIF
           ENDIF ELSE BEGIN
                theImage = Read_Image(imageFile, r, g, b)
                IF N_Elements(theImage) EQ 1 THEN Message, 'Image file can not be read with READ_IMAGE.'
           ENDELSE
           IF has_palette THEN BEGIN
                colorPalette = [[r], [g], [b]]
                scaletype = 'NONE'
           ENDIF
           END
           
         'OBJREF': BEGIN
                imageIsObject = 1
                theImageObj = image
                END
         
         ELSE: BEGIN
         
            ; Could be an integer from cancelling a file loading application.
            IF N_Elements(image) EQ 1 THEN RETURN, 0
            
            ; If the image is not a 2D array or true-color image, then return.
            ndims = Size(image, /N_DIMENSIONS)
            IF ndims EQ 3 THEN BEGIN
               index = Where(Size(image, /DIMENSIONS) EQ 3, count)
               IF count LT 0 THEN Message, 'Image does not appear to be a true-color image.'
            ENDIF ELSE BEGIN
               IF ndims LT 2 OR ndims GT 3 THEN Message, 'Image does not appear to be a 2D array.'
            ENDELSE
            theImage = image
            END
            
    ENDCASE
    
    ; A label to jump to if you have already have an image.
    DefineWidgets:
    
    ; This INIT method simply instantiates a top-level base object with a status bar.
    ok = self->TOPLEVELBASE::INIT(_Extra=extra)
    IF ~ok THEN RETURN, 0
    
    ; No need to go through all this if we have an object.
    IF imageIsObject THEN BEGIN
        ; Add a parent, so this object doesn't die prematurely.
        theImageObj -> AddParent, self
        theImageObj -> GetProperty, XSIZE=xsize, YSIZE=ysize, TRUEINDEX=trueindex
        imgAspect = Float(ysize) / xsize
        winxsize = xsize
        winysize = ysize
    ENDIF ELSE BEGIN
        dims = Image_Dimensions(theImage, XSize=xsize, YSize=ysize, TrueIndex=trueindex, $  
           XIndex=xindex, YIndex=yindex)
        imgAspect = Float(ysize) / xsize
    ENDELSE
    
    ; Check keywords.
    IF N_Elements(background) EQ 0 THEN background = 'ivory'
    brewer = Keyword_Set(brewer)
    IF N_Elements(color) EQ 0 THEN color = 'charcoal'
    IF N_Elements(ctindex) EQ 0 THEN ctindex = 0
    IF N_Elements(keep_aspect) EQ 0 THEN keep_aspect = 1
    keep_aspect = Keyword_Set(keep_aspect)
    IF N_Elements(ncolors) EQ 0 THEN ncolors = 256
    IF N_Elements(nointerp) EQ 0 THEN nointerp = 1
    nointerp = Keyword_Set(nointerp)
    IF N_Elements(position) EQ 0 THEN BEGIN
        IF Keyword_Set(axes) THEN BEGIN
           position = [0.10, 0.075, 0.925, 0.925]
           IF N_Elements(xtitle) NE 0 THEN position[0] = 0.150
           IF N_Elements(ytitle) NE 0 THEN position[1] = 0.125
        ENDIF ELSE BEGIN
           position = [0,0,1,1]
        ENDELSE
    ENDIF
    IF N_Elements(scaletype) EQ 0 $
        THEN scaletype = 0 $
        ELSE BEGIN
             IF Size(scaletype, /TNAME) EQ 'STRING' THEN BEGIN
                  possibleTypes = ['LINEAR', 'GAMMA', 'LOG', 'ASINH', $
                                   'LINEAR 2%', 'SQUARE ROOT', 'EQUALIZATION', 'GAUSSIAN', 'NONE']
                  index = Where(possibleTypes EQ StrUpCase(scaletype), count)
                  IF count EQ 0 THEN Message, 'Unknown scaling type encountered.'
                  scaletype = index
            ENDIF
        ENDELSE
    
    ; Check ranges.
    IF N_Elements(xrange) EQ 0 THEN xrange = [0, xsize]
    IF N_Elements(yrange) EQ 0 THEN yrange = [0, ysize]
    
        ; Calculate window size.
    IF (N_Elements(xwinsize) EQ 0) AND (N_Elements(ywinsize) EQ 0) THEN BEGIN
        maximageSize = Max([xsize, ysize])
        maxSize = 700 < maxImageSize
        IF 2*maximageSize LT maxsize THEN maxsize = 350 > 2*maximagesize
        IF imgAspect GE 1 THEN BEGIN
            ywinsize = maxsize
            xwinsize = maxsize / imgAspect
        ENDIF ELSE BEGIN
            xwinsize = maxsize
            ywinsize = maxsize * imgAspect
        ENDELSE
    ENDIF ELSE BEGIN
        IF N_Elements(xwinsize) EQ 0 THEN xwinsize = 700
        IF N_Elements(ywinsize) EQ 0 THEN ywinsize = 700
    ENDELSE
    
    ; Can leave now, if an object was passed in.
    IF imageIsObject THEN Goto, imageAsObject
    
    ; If a colorpalette has not been determined already (from READ_IMAGE), then do it here.
    IF (N_Elements(colorpalette) EQ 0) AND Obj_Valid(ctobject) EQ 0 THEN BEGIN
    
        IF N_Elements(ctindex) EQ 0 THEN BEGIN
            ctindex = 0
            TVLCT, rr, gg, bb, /Get
            cgLoadCT, ctindex, BREWER=brewer, NCOLORS=ncolors
            TVLCT, r, g, b, /GET
            colorPalette = [[r], [g], [b]]
            TVLCT, rr, gg, bb
        ENDIF ELSE BEGIN  
            IF ctindex LT 0 THEN BEGIN
                TVLCT, r, g, b, /GET
                colorPalette = [[r], [g], [b]]
            ENDIF ELSE BEGIN
                TVLCT, rr, gg, bb, /Get
                cgLoadCT, ctindex, BREWER=brewer, NCOLORS=ncolors
                TVLCT, r, g, b, /GET
                colorPalette = [[r], [g], [b]]
                TVLCT, rr, gg, bb
            ENDELSE   
        ENDELSE
    ENDIF
    
    ; Create a coordinate system for the image if one is not passed in.
    IF N_Elements(map_coord) EQ 0 THEN BEGIN
        coords = Obj_New('CATCOORD', Name='IMG WIN COORDS OBJECT', XRANGE=xrange, YRANGE=yrange)
    ENDIF ELSE BEGIN
        coords = map_coord
        coords -> AddParent, self
    ENDELSE
    
    ; If AXES are required.
    IF Keyword_Set(axes) THEN BEGIN
       theAxes = Obj_New('IMGAXES', $
            NAME='IMGWIN_AXES', $
            COLOR=color, $
            COORD_OBJECT=coords, $
            POSITION=position, $
            XRANGE=xrange, $
            XTICKFORMAT=xtickformat, $
            XTITLE=xtitle, $
            YRANGE=yrange, $
            YTICKFORMAT=ytickformat, $
            YTITLE=ytitle)
        self.theAxes = theAxes
    ENDIF

    ; Create the image.
    theImageObj = Obj_New('ScaleImage', theImage, $
        AXES=theAxes, $
        NAME='IMGWIN_IMAGE', $
        BETA=beta, $
        BOTTOM=bottom, $
        COORD_OBJECT=coords, $
        EXPONENT=exponent, $
        GAMMA=gamma, $
        KEEP_ASPECT=keep_aspect, $
        MEAN=mean, $
        MISSING_COLOR=missing_color, $
        MISSING_VALUE=missing_value, $
        NCOLORS=ncolors, $
        NOINTERPOLATE=nointerp, $
        POSITION=position, $
        SCALETYPE=scaletype, $
        SCLMIN=sclmin, $
        SCLMAX=sclmax, $
        SELECTABLE=1, $
        SIGMA=sigma)
        
    ; Add the axes, if created.
    IF Obj_Valid(theAxes) THEN theImageObj -> Add, theAxes
    
    ; Come straight here if you pass an object in.
    imageAsObject:
    
    ; Update the colors.
    theImageObj -> GetProperty, COLOR_OBJECT=colors
    colors -> SetProperty, COLORPALETTE=colorPalette, BREWER=Keyword_Set(brewer)
    IF Obj_Valid(ctobject) THEN theImageObj -> SetProperty, COLOR_OBJECT=ctobject
            
    ; Store the image.
    self.theImage = theImageObj
    
    ; Set flags.
    self.full_resolution = Keyword_Set(full_resolution)
    self.imageIsObject = imageIsObject
    self.win_keep_aspect = Keyword_Set(win_keep_aspect)

    ; Success!
    RETURN, 1
END
;*****************************************************************************************************


PRO ImgWin__Define, class

; The IMGWIN class definition. It is a top-level base object widget.

   compile_opt idl2

   class = { IMGWIN, $
             INHERITS TopLevelBase, $     ; This is an application window.
             theImage: Obj_New(), $       ; The image object to display (a SCALEIMAGE object).
             theDrawWidget: Obj_New(), $  ; The application draw widget.
             theAxes: Obj_New(), $        ; The image axes object (a IMGAXIS object), if required.
             full_resolution: 0B, $       ; Flag that indicates the image is at full-resolution.
             win_keep_aspect: 0B, $       ; Flag indicating that the window should keep the same aspect ratio as the image.
             imageIsObject: 0B, $         ; Flag that indicates the image argument came in as an object.
             xoffset: 0L, $               ; A fudge factor for re-sizing TLB widgets.
             yoffset: 0L, $               ; A fudge factor for re-sizing TLB widgets.
             have_resized: 0L, $          ; Flag to indicate window has been resized once. Unix bug?
             _statusbar: Obj_New()}       ; A status bar for program updates.
END
;*****************************************************************************************************

; This is the driver program for the IMGWIN object.
PRO ImgWin, image, $
    AXES=axes, $
    BACKGROUND=background, $
    BETA=beta, $
    BOTTOM=bottom, $
    BREWER=brewer, $
    COLOR=color, $
    CTOBJECT=ctobject, $
    CTINDEX=ctindex, $
    EXPONENT=exponent, $
    FULL_RESOLUTION=full_resolution, $
    GAMMA=gamma, $
    KEEP_ASPECT=keep_aspect, $
    MAP_COORD=map_coord, $
    MAP_GRID=map_grid, $
    MAP_OUTLINE=map_outline, $
    MEAN=mean, $
    MISSING_COLOR=missing_color, $
    MISSING_VALUE=missing_value, $
    NCOLORS=ncolors, $
    NOINTERPOLATION=nointerp, $
    POSITION=position, $
    SCALETYPE=scaletype, $
    SCLMIN=sclmin, $
    SCLMAX=sclmax, $
    SIGMA=sigma, $
    TLB_TITLE=tlb_title, $
    WIN_KEEP_ASPECT=win_keep_aspect, $
    XRANGE=xrange, $
    XSIZE=xwinsize, $
    XTICKFORMAT=xtickformat, $
    XTITLE=xtitle, $
    YRANGE=yrange, $
    YSIZE=ywinsize, $
    YTICKFORMAT=ytickformat, $
    YTITLE=ytitle, $
    _EXTRA=extra, $
    OUTIMAGE=outimage  ; An output keyword

   @cat_pro_error_handler
   
   IF N_Elements(tlb_title) EQ 0 THEN tlb_title = 'Catalyst Image Window'
   
   ; Create the widgets that make up the application. Run it.
   tlb = OBJ_NEW('IMGWIN', image, Column=1, NAME='IMGWIN_TLB', $
        SIZE_EVENTS=1, $
        MBar=menubar, Title=tlb_title, $
        AXES=axes, $
        BACKGROUND=background, $
        BETA=beta, $
        BOTTOM=bottom, $
        BREWER=brewer, $
        COLOR=color, $
        CTINDEX=ctindex, $
        CTOBJECT=ctobject, $
        EXPONENT=exponent, $
        FULL_RESOLUTION=full_resolution, $
        GAMMA=gamma, $
        KEEP_ASPECT=keep_aspect, $
        MAP_COORD=map_coord, $
        MAP_GRID=map_grid, $
        MAP_OUTLINE=map_outline, $
        MEAN=mean, $
        MISSING_COLOR=missing_color, $
        MISSING_VALUE=missing_value, $
        NCOLORS=ncolors, $
        NOINTERPOLATION=nointerp, $
        POSITION=position, $
        SCALETYPE=scaletype, $
        SCLMIN=sclmin, $
        SCLMAX=sclmax, $
        SCROLL=scroll, $
        SIGMA=sigma, $
        TLB_TITLE=tlb_title, $
        WIN_KEEP_ASPECT=win_keep_aspect, $
        XRANGE=xrange, $
        XSIZE=xwinsize, $
        XTICKFORMAT=xtickformat, $
        XTITLE=xtitle, $
        YRANGE=yrange, $
        YSIZE=ywinsize, $
        YTICKFORMAT=ytickformat, $
        YTITLE=ytitle, $
        ; Output keywords
        OUTIMAGE=outimage, $
        _Extra=extra)

   ; Pass information you will need in the GUI method. I only need it once, so I 
   ; am not storing it in the object.
   IF Obj_Valid(tlb) THEN tlb -> GUI, menubar, $
        XWINSIZE=xwinsize, $
        YWINSIZE=ywinsize, $
        BACKGROUND=background, $
        AXES=axes, $
        FULL_RESOLUTION=full_resolution, $
        WIN_KEEP_ASPECT=win_keep_aspect

        
   
END
;*****************************************************************************************************
