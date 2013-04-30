;*****************************************************************************************************
;+
; NAME:
;       DATAVIEWER
;
; PURPOSE:
;
;       The purpose of this routine is to provide an application for viewing
;       passive microwave image files found at NSIDC.
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
;       Data Visualization
;
; SYNTAX:
;
;       IDL> DataViewer, files
;
; ARGUMENTS:
;
;       files:   The definition of "files" is fairly broad. This can be the name of one or more
;                of the image files DataViewer can read (see below), an actual image variable,
;                or it can be the name of a directory containing image files that DataViewer can
;                read, in which case DataViewer reads all the image files in the directory. This
;                is an optional argument.
;
; DATA_FILES:
;
;   Currently the DataViewer program reads the following NSIDC image data files.
;
;   nsidc-0001: DMSP-SSM Daily Polar Gridded Brightness Temperatures
;               http://nsidc.org/data/nsidc-0001.html
;   nsidc-0032: DMSP SSM/I Pathfinder Daily EASE-Grid Brightness Temperatures
;               http://nsidc.org/data/nsidc-0032.html
;   nsidc-0046: Northern Hemisphere EASE-Grid Weekly Snow Cover and Sea Ice Extent
;               http://nsidc.org/data/nsidc-0046.html
;   nsidc-0051: Sea Ice Concentrations from SMMR and SSM/I platforms
;               http://nsidc.org/data/nsidc-0051.html
;   nsidc-0071: Nimbus-7 SMMR Pathfinder Daily EASE-Grid Brightness Temperatures
;               http://nsidc.org/data/nsidc-0071.html
;   nsidc-0079: Bootstrap Sea Ice Concentrations from Nimbus-7 SMMR and DMST SSM/I
;               http://nsidc.org/data/nsidc-0079.html
;   nsidc-0080: Near Real Time SMSP SSM/I Gridded Brightness Temperatures
;               http://nsidc.org/data/nsidc-0080.html
;   nsidc-0081: Near Real Time SMSP SSM/I Daily Polar Gridded Sea Ice Concentrations
;               http://nsidc.org/data/nsidc-0081.html
;   nsidc-0301: AMSR-E/Aqua Daily EASE-Grid Brightness Temperatures
;               http://nsidc.org/data/nsidc-0301.html
;   nsidc-0302: AMSR-E/Aqua Daily Global Quarter-Degree Gridded Brightness Temperatures
;               http://nsidc.org/data/nsidc-0302.html
;   nsidc-0342: Near Real Time SSM/I and SSMIS Daily Gridded Brightness Temperatures
;               http://nsidc.org/data/nsidc-0342.html
;   nsidc-ae_si25: AMSR-E Daily 25 km Gridded Brightness Temperature and Sea Ice Concentration
;               http://nsidc.org/data/ae_si25.html
;
;   Additionally, the program reads BMP, GIF, JPEG, PNG, PPM, SRF, TIFF, DICOM, or
;   JPEG2000 image files .
;
;   The DataViewer no longer supports the following NSIDC data set:
;
;   nsidc-0116: Sea Ice Motion Vectors for Daily, Monthly, and Yearly Gridded Products
;               http://nsidc.org/data/nsidc-0116.html

; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning and version 1.0 released to the public 11 November 2008.
;       Extensively modified to read more passive microware data products from NSIDS. Changes
;          include better ways of handling map projection information. 17 June 2010. DWF.
;       Fixed problem caused by loading a configuration file before images were selected,
;           and fixed a problem with Coyote Library routine cgLoadCT and the way it looks
;           for the Brewer color table file. 28 June 2010. DWF.
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
;        DATAVIEWER::CHANGEGRID
;
; PURPOSE:
;
;        This method changes the grid in the dataviewer.
;
; SYNTAX:
;
;        theObject -> ChangeGrid, grid
;
; ARGUMENTS:
;
;       grid:   The new grid. If not provided, the grid from DATAVIEWER_GRIDWINDOW_GRID is used.
;
; KEYWORDS:
;
;       NOLOAD: If this keyword is set, the grid is changed, but no images are loaded and the
;               file pointer is not changes. This is primarily for changing the grid prior to image
;               display.
;
;-
;*****************************************************************************************************
PRO DataViewer::ChangeGrid, grid, NOLOAD=noload

   @cat_pro_error_handler

   IF N_Elements(grid) EQ 0 THEN grid = CatGetDefault('DATAVIEWER_GRIDWINDOW_GRID')

   ; No more than 64 images per page due to memory considerations.
   grid = 1 > grid < 8

   ; Update the grid in various locations.
   self.numImages = grid[0] * grid[1]
   self.gridWindow -> SetProperty, GRID=grid

   ; If you are not loading images, then out of here.
   IF Keyword_Set(noload) THEN RETURN

   ; Remove all the current images from the window.
   currentImages = self.gridWindow -> Get(/ALL, ISA='NSIDC_IMAGE', COUNT=count)

   ; Find the first current image in the list of names.
   IF count GT 0 THEN BEGIN
       currentImages[0] -> GetProperty, FILENAME=filename
       currentFilePtr = Where(*self.theFiles EQ filename)
       self.fileStackPtr = (currentFilePtr / self.numimages) * self.numimages

       ; Remove all the image objects from the window and destroy them.
       FOR j=0,count-1 DO currentImages[j] -> RemoveParent, self.gridWindow
       self.gridWindow -> Remove, /ALL, ISA='NSIDC_IMAGE'
  ENDIF

   ; Read the new images.
   IF count GT 0 THEN BEGIN
       FOR j=0,self.numImages-1 DO BEGIN
            newObject = Parse_NSIDC_Filename((*self.theFiles)[self.fileStackPtr], INFO=info, SUCCESS=success)
            IF success EQ 0 THEN RETURN
            namesOff = CatGetDefault('DATAVIEWER_IMAGE_NAMES_OFF', SUCCESS=success)
            IF success NE 1 THEN namesOff = 1
            cbOff = CatGetDefault('DATAVIEWER_COLORBARS_OFF', SUCCESS=success)
            IF success NE 1 THEN cbOff = 1
            moOn = CatGetDefault('DATAVIEWER_MAP_OUTLINE_ON', SUCCESS=success)
            IF success NE 1 THEN moOn = 0
            gridOn = CatGetDefault('DATAVIEWER_MAP_GRID_ON', SUCCESS=success)
            IF success NE 1 THEN gridOn = 0
            missing_color = CatGetDefault('DATAVIEWER_MISSING_COLOR')
            oob_low_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR')
            oob_high_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR')
            landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
            annotate_color = CatGetDefault('DATAVIEWER_ANNOTATE_COLOR')
            newObject -> SetProperty, SELECTABLE=1, MISSING_COLOR=missing_color, OOB_LOW_COLOR=oob_low_color, $
                OOB_HIGH_COLOR=oob_high_color, LANDMASK_COLOR=landmask_color, NO_NAME_DISPLAY=namesOff, $
                NO_COLORBAR_DISPLAY=cbOff, FN_COLOR=annotate_color, MAP_OUTLINE=moOn, MAP_GRID=gridOn
            self.gridWindow -> Add, newObject
            self.fileStackPtr = self.fileStackPtr + 1
            IF self.filestackPtr EQ N_Elements(*self.theFiles) THEN BREAK
      ENDFOR
  ENDIF

   ; The images in the window need new positions.
   self.gridWindow -> GetProperty, POSITIONS=positions
   currentImages = self.gridWindow -> Get(/ALL, COUNT=numCurrentImages, ISA='NSIDC_IMAGE')
   FOR j=0,numCurrentImages-1 DO currentImages[j] -> SetProperty, POSITION=positions[*,j]

   ; You have to unregister the GridWindow Color object from messages, and re-register
   ; it, otherwise, it will get the notification to redraw the images before the images
   ; have received the message to change their colors. This results in a one-click "delay"
   ; when changing color tables.
   self.gridWindow -> GetProperty, COLOR_OBJECT=gridColors
   IF Obj_Valid(gridColors) THEN BEGIN
       gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_TABLECHANGE', /UNREGISTER
       gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_SETPROPERTY', /UNREGISTER
       gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_TABLECHANGE'
       gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_SETPROPERTY'
   ENDIF
   ; Draw the images in the window.
   self.gridWindow -> Draw

   ; Check to see if buttons should be turned on or off.
   IF Obj_Valid(self.theFiles) THEN BEGIN
       IF self.fileStackPtr EQ (N_Elements(*self.theFiles)) $
           THEN self.nextButton -> SetProperty, SENSITIVE=0 $
           ELSE self.nextButton -> SetProperty, SENSITIVE=1
   ENDIF
   IF self.fileStackPtr GT self.numimages $
       THEN self.prevButton -> SetProperty, SENSITIVE=1 $
       ELSE self.prevButton -> SetProperty, SENSITIVE=0

END


;*****************************************************************************************************
;+
; NAME:
;        DATAVIEWER::CREATESTATUSBAR
;
; PURPOSE:
;
;        This method creates the status bar at the bottom of the application.
;
; SYNTAX:
;
;        theObject -> CreateStatusBar
;
; ARGUMENTS:
;
;       baseObject:   The parent of the statusbar. If undefined, use SELF.
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the STATUSBAR object..
;
;-
;*****************************************************************************************************
PRO DataViewer::CreateStatusBar, baseObject, _Extra=extrakeywords

   IF N_Elements(baseObject) EQ 0 THEN baseObject = self
   self._statusbar = OBJ_NEW('STATUSBAR', baseObject, Name='Statusbar', /Align_Left, $
      Font='Times*14', Frame=1,  _Extra=extrakeywords)

END



;*****************************************************************************************************
;+
; NAME:
;        DATAVIEWER::EditConfigFile
;
; PURPOSE:
;
;        This method allows the user to edit the configuration file and save it
;        as itself or as another configuration file.
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
PRO DataViewer::EditConfigFile, event

  @cat_pro_error_handler

  success = DataViewer_Edit_Config_File(self.configFile, self->GetID(), self)
  IF ~success THEN RETURN

END



;*****************************************************************************************************
;+
; NAME:
;        DATAVIEWER::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the DATAVIEWER object. It will typically
;        be used to respond to events from widget objects created in application.
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
PRO DataViewer::EventHandler, event

   @cat_pro_error_handler

   ; Branch on the object name.
   CASE StrUpCase(event.name) OF

     'ANIMATE_IMAGES': BEGIN

            ; Get the maximum size for the animation window.
            maxsize = CatGetDefault('DATAVIEWER_ANIMATE_MAXSIZE', SUCCESS=success)
            IF success NE 1 THEN maxsize = 400

            ; Get the first image in the window. Find size of image so we can
            ; create an annomation the correct size.
            firstImage = self.gridWindow -> Get(POSITION=0)
            firstImage -> GetProperty, IMAGE=image, NO_NAME_DISPLAY=no_name, NO_COLORBAR_DISPLAY=no_colorbar
            aspect = image_aspect( image )
            IF aspect GE 1 THEN BEGIN
                ysize = maxsize
                xsize = maxsize / aspect
            ENDIF ELSE BEGIN
                xsize = maxsize
                ysize = maxsize * aspect
            ENDELSE

            ; How many images do we have?
            numfiles = N_Elements(*self.theFiles)

            ; Start it up.
            XInterAnimate, /SHOWLOAD, SET=[xsize, ysize, numfiles], TITLE='DataViewer Animation'
            animateWindow = !D.Window
            missing_color = CatGetDefault('DATAVIEWER_MISSING_COLOR')
            oob_low_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR')
            oob_high_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR')
            landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
            background_color = CatGetDefault('DATAVIEWER_BACKGROUND_COLOR')
            annotate_color = CatGetDefault('DATAVIEWER_ANNOTATE_COLOR')
            moOn = CatGetDefault('DATAVIEWER_MAP_OUTLINE_ON', SUCCESS=success)
            IF success NE 1 THEN moOn = 0
            gridOn = CatGetDefault('DATAVIEWER_MAP_GRID_ON', SUCCESS=success)
            IF success NE 1 THEN gridOn = 0

            ; Read each file, display it in the window, then leave.
            FOR j=0,numfiles-1 DO BEGIN
               newObject = Parse_NSIDC_Filename((*self.theFiles)[j], INFO=info, SUCCESS=success)
               IF success EQ 0 THEN Message, 'Problem reading image file. Returning...'
               self.gridWindow -> GetProperty, COLOR_OBJECT=colors
               newObject -> SetProperty, SELECTABLE=1, MISSING_COLOR=missing_color, OOB_LOW_COLOR=oob_low_color, $
                     OOB_HIGH_COLOR=oob_high_color, LANDMASK_COLOR=landmask_color, POSITION=[0,0,1,1], $
                     NO_NAME_DISPLAY=no_name, NO_COLORBAR_DISPLAY=no_colorbar, FN_COLOR=annotate_color, $
                     MAP_OUTLINE=moOn, MAP_GRID=gridOn
               newObject -> GetProperty, NSIDC_TAG=nsidc_tag, FILENAME=filename, COLORCHANGEALLOWED=colorChangeAllowed
               IF colorChangeAllowed THEN newObject -> SetProperty, COLOR_OBJECT=colors
               WSet, animateWindow
               Erase, Color=cgColor(background_color)
               newObject -> Draw
               Obj_Destroy, newObject
               XInterAnimate, WINDOW=animateWindow, FRAME=j
            ENDFOR

            ; Set it loose.
            XInterAnimate, 25, GROUP=self->GetID()


            END

      'CHANGE_ANNOTATE_COLOR': BEGIN
            annotateColor = CatGetDefault('DATAVIEWER_ANNOTATE_COLOR')
            colorname = cgPickColorName(GROUP_LEADER=self->GetID(), annotateColor, TITLE='Change Annotation Color')
            CatSetDefault, 'DATAVIEWER_ANNOTATE_COLOR', colorname
            theImages = self.gridWindow -> Get(/ALL, COUNT=count)
            FOR j=0,count-1 DO BEGIN
                IF Obj_Isa_Valid(theImages[j], 'NSIDC_IMAGE') THEN BEGIN
                theImages[j] -> SetProperty, FN_COLOR=colorname
                ENDIF
             ENDFOR
             self.gridWindow -> Draw
             END

      'CHANGE_BACKGROUND_COLOR': BEGIN
            self -> GetProperty, ID=tlbID
            backgroundColor = CatGetDefault('DATAVIEWER_BACKGROUND_COLOR')
            color = cgPickColorName(GROUP_LEADER=tlbID, backgroundcolor, TITLE='Change BACKGROUND Color')
            CatSetDefault, 'DATAVIEWER_BACKGROUND_COLOR', color
            self.gridWindow -> SetProperty, INITIAL_COLOR=color
            self.gridWindow -> Draw
            END

      'CHANGE_DATA_DIRECTORY': BEGIN
            self -> GetProperty, ID=tlbID
            dataDir = CatGetDefault('DATAVIEWER_DATA_DIRECTORY')
            newDataDir = Dialog_Pickfile(PATH=dataDir, /DIRECTORY, TITLE='Select Data Directory...')
            IF newDataDir EQ "" THEN RETURN
            CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', newDataDir
            self -> Write_Config_File, self.configFile
            END

     'CHANGE_GRID': BEGIN

            ; Get the new grid size from the user.
            self.gridWindow -> GetProperty, GRID=grid
            grid = DataViewer_GridSize(GRID=grid, CANCEL=cancelled, GROUP_LEADER=self->GetID())
            IF cancelled THEN RETURN

            ; Change the grid
            self -> ChangeGrid, grid
            CatSetDefault, 'DATAVIEWER_GRIDWINDOW_GRID', grid

            ; Worry about the NEXT and PREVIOUS buttons.
            IF self.fileStackPtr GE (N_Elements(*self.theFiles)) THEN BEGIN
                self.nextButton -> SetProperty, SENSITIVE=0
                IF (self.fileStackPtr - self.gridWindow->Count()) GT 0 THEN $
                     self.prevButton -> SetProperty, SENSITIVE=1 ELSE $
                     self.prevButton -> SetProperty, SENSITIVE=0
            ENDIF ELSE BEGIN
                IF self.fileStackPtr GT self.numImages THEN BEGIN
                    self.prevButton -> SetProperty, SENSITIVE=1
                ENDIF ELSE BEGIN
                    self.prevButton -> SetProperty, SENSITIVE=0
                    IF ((N_Elements(*self.theFiles)-1) - self.fileStackPtr) GT 0 THEN $
                        self.nextButton -> SetProperty, SENSITIVE=1 ELSE $
                        self.nextButton -> SetProperty, SENSITIVE=0
                ENDELSE
            ENDELSE

            END

      'CHANGE_GRID_COLOR': BEGIN
            gridColor = CatGetDefault('DATAVIEWER_GRID_COLOR')
            colorname = cgPickColorName(GROUP_LEADER=self->GetID(), gridColor, TITLE='Change Map Grid Color')
            CatSetDefault, 'DATAVIEWER_GRID_COLOR', colorname
            theImages = self.gridWindow -> Get(/ALL, COUNT=count)
            FOR j=0,count-1 DO BEGIN
                IF Obj_Isa_Valid(theImages[j], 'NSIDC_IMAGE') THEN BEGIN
                theImages[j] -> SetProperty, GRID_COLOR=colorname
                ENDIF
             ENDFOR
             self.gridWindow -> Draw
             END

      'CHANGE_IMAGE_COLORS': BEGIN
            self.gridWindow -> GetProperty, COLOR_OBJECT=colors
            colors -> XColors, TITLE='Change Image Colors', GROUP_LEADER=self.gridWindow
            END

      'CHANGE_LANDMASK_COLOR': BEGIN
            landmaskColor = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
            colorname = cgPickColorName(GROUP_LEADER=self->GetID(), landmaskColor, TITLE='Change LANDMASK Color')
            CatSetDefault, 'DATAVIEWER_LANDMASK_COLOR', colorname
            self.gridWindow -> GetProperty, COLOR_OBJECT=colors
            color = cgColor(colorname, /Triple)
            colors -> GetProperty, RED=r, GREEN=g, BLUE=b
            r[252] = color[0]
            g[252] = color[1]
            b[252] = color[2]
            colors -> SetProperty, COLORPALETTE=[[r],[g],[b]]
            theImages = self.gridWindow -> Get(/ALL, COUNT=count)
            FOR j=0,count-1 DO BEGIN
                IF Obj_Isa_Valid(theImages[j], 'NSIDC_IMAGE') THEN BEGIN
                theImages[j] -> GetProperty, COLOR_OBJECT=imgcolors
                imgcolors -> LoadColor, colorname, 252
                theImages[j] -> SetProperty, LANDMASK_COLOR=colorname
                ENDIF
             ENDFOR
             self.gridWindow -> Draw
             END

      'CHANGE_MISSING_COLOR': BEGIN
            missingColor = CatGetDefault('DATAVIEWER_MISSING_COLOR')
            colorname = cgPickColorName(GROUP_LEADER=self->GetID(), missingColor, TITLE='Change MISSING Color')
            CatSetDefault, 'DATAVIEWER_MISSING_COLOR', colorname
            self.gridWindow -> GetProperty, COLOR_OBJECT=colors
            color = cgColor(colorname, /Triple)
            colors -> GetProperty, RED=r, GREEN=g, BLUE=b
            r[255] = color[0]
            g[255] = color[1]
            b[255] = color[2]
            colors -> SetProperty, COLORPALETTE=[[r],[g],[b]]
            theImages = self.gridWindow -> Get(/ALL, COUNT=count)
            FOR j=0,count-1 DO BEGIN
                IF Obj_Isa_Valid(theImages[j], 'NSIDC_IMAGE') THEN BEGIN
                theImages[j] -> GetProperty, COLOR_OBJECT=imgcolors
                imgcolors -> LoadColor, colorname, 255
                theImages[j] -> SetProperty, MISSING_COLOR=colorname
                ENDIF
             ENDFOR
             self.gridWindow -> Draw
             END

      'CHANGE_OOB_HIGH_COLOR': BEGIN
            oob_high_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR')
            colorname = cgPickColorName(GROUP_LEADER=self->GetID(), oob_high_color, TITLE='Change OUT-OF-BOUNDS HIGH Color')
            CatSetDefault, 'DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR', colorname
            self.gridWindow -> GetProperty, COLOR_OBJECT=colors
            color = cgColor(colorname, /Triple)
            colors -> GetProperty, RED=r, GREEN=g, BLUE=b
            r[253] = color[0]
            g[253] = color[1]
            b[253] = color[2]
            colors -> SetProperty, COLORPALETTE=[[r],[g],[b]]
            theImages = self.gridWindow -> Get(/ALL, COUNT=count)
            FOR j=0,count-1 DO BEGIN
                IF Obj_Isa_Valid(theImages[j], 'NSIDC_IMAGE') THEN BEGIN
                theImages[j] -> GetProperty, COLOR_OBJECT=imgcolors
                imgcolors -> LoadColor, colorname, 253
                theImages[j] -> SetProperty, OOB_HIGH_COLOR=colorname
                ENDIF
             ENDFOR
             self.gridWindow -> Draw
             END

      'CHANGE_OOB_LOW_COLOR': BEGIN
            oob_low_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR')
            colorname = cgPickColorName(GROUP_LEADER=self->GetID(), oob_low_color, TITLE='Change OUT-OF-BOUNDS LOW Color')
            CatSetDefault, 'DATAVIEWER_OUTOFBOUNDS_LOW_COLOR', colorname
            self.gridWindow -> GetProperty, COLOR_OBJECT=colors
            color = cgColor(colorname, /Triple)
            colors -> GetProperty, RED=r, GREEN=g, BLUE=b
            r[254] = color[0]
            g[254] = color[1]
            b[254] = color[2]
            colors -> SetProperty, COLORPALETTE=[[r],[g],[b]]
            theImages = self.gridWindow -> Get(/ALL, COUNT=count)
            FOR j=0,count-1 DO BEGIN
                IF Obj_Isa_Valid(theImages[j], 'NSIDC_IMAGE') THEN BEGIN
                theImages[j] -> GetProperty, COLOR_OBJECT=imgcolors
                imgcolors -> LoadColor, colorname, 254
                theImages[j] -> SetProperty, OOB_LOW_COLOR=colorname
                ENDIF
             ENDFOR
             self.gridWindow -> Draw
             END

      'CHANGE_OUTLINE_COLOR': BEGIN
            outlineColor = CatGetDefault('DATAVIEWER_OUTLINE_COLOR')
            colorname = cgPickColorName(GROUP_LEADER=self->GetID(), outlineColor, TITLE='Change Map Outline Color')
            CatSetDefault, 'DATAVIEWER_OUTLINE_COLOR', colorname
            theImages = self.gridWindow -> Get(/ALL, COUNT=count)
            FOR j=0,count-1 DO BEGIN
                IF Obj_Isa_Valid(theImages[j], 'NSIDC_IMAGE') THEN BEGIN
                theImages[j] -> SetProperty, OUTLINE_COLOR=colorname
                ENDIF
             ENDFOR
             self.gridWindow -> Draw
             END

      'COLORBARS ON/OFF': BEGIN

         event.id -> GetProperty, UVALUE=buttonChangeValue, VALUE=buttonValue
         CatSetDefault, 'DATAVIEWER_COLORBARS_OFF', buttonChangeValue
         allObjects = self.gridwindow -> Get(/ALL, ISA='CatImage', COUNT=count)
         FOR j=0,count-1 DO allObjects[j] -> SetProperty, NO_COLORBAR_DISPLAY=buttonChangeValue
         self.gridWindow -> Draw
         IF buttonValue EQ 'Colorbars On' THEN $
            event.id -> SetProperty, VALUE='Colorbars Off' ELSE $
            event.id -> SetProperty, VALUE='Colorbars On'
         event.id -> SetProperty, UVALUE=1-buttonChangeValue

         END

      'EDIT_CONFIGURATION_FILE': BEGIN

         self -> EditConfigFile, event

         END

      'EXIT': OBJ_DESTROY, self

      'GRIDWINDOW' : BEGIN

           ; If this is a right button down, we are going to activate the control panel for
           ; the image, if we can find it.
           CASE event.event_name OF
                'WIDGET_DRAW': BEGIN

                   ; Make this the current window/
                   self.gridWindow -> SetWindow

                   IF (event.type EQ 2) THEN BEGIN
                        ; Which image are we talking about?
                        images = event.ID -> Get(/ALL, Count=count)
                        success = 0
                        FOR j=0,count-1 DO BEGIN
                           selectedImage = images[j] -> Select(event.x, event.y, SUCCESS=success)
                           IF success THEN BREAK
                        ENDFOR
                        ; If there is no success, you haven't clicked on an image.
                        IF success EQ 0 THEN RETURN

                               selectedImage -> GetProperty, Filename=filename, SCLMIN=sclmin, $
                                    SCLMAX=sclmax, IMAGE=image, MISSING_VALUE=missing_value
                               value = selectedImage -> Pixel_To_Value(event.x, event.y, XDATA=xpix, YDATA=ypix)
                               IF N_Elements(value) EQ 1 THEN BEGIN
                                    IF value EQ missing_value THEN value = 'missing'
                                   i = Where(image NE missing_value, goodCount)
                                   IF goodCount EQ 0 THEN RETURN
                                   IF Size(image, /TNAME) EQ 'BYTE' THEN $
                                        minimage = MIN(Fix(image[i]), MAX=maximage, /NAN) ELSE $
                                        minimage = MIN(image[i], MAX=maximage, /NAN)
                               ENDIF
                               IF N_Elements(*self.theFiles) EQ 1 THEN $
                                    filename = "" ELSE $
                                    filename = File_Basename(filename, '.gz')
                               CASE N_Elements(value) OF

                                   1: BEGIN
                                     IF Size(value, /TNAME) NE 'STRING' THEN  value = String(value, Format='(F0.2)')
                                     theText = filename + $
                                         '  Value: ' + value + '  Pixel Loc: ('+ StrTrim(xpix,2) + ',' + StrTrim(ypix,2) + ')'
                                     END
                                  3: BEGIN
                                     IF Size(value, /TNAME) NE 'STRING' THEN  BEGIN
                                         value1 = String(value[0],Format='(I0)')
                                         value2 = String(value[1],Format='(I0)')
                                         value3 = String(value[2],Format='(I0)')
                                     ENDIF ELSE BEGIN
                                         value1 = value[0]
                                         value2 = value[1]
                                         value3 = value[2]
                                     ENDELSE
                                     theText = filename + $
                                         ' Value: [' + value1 + ', ' + value2 + ', ' + value3 + ']' + $
                                         '  Pixel Loc: (' + StrTrim(xpix,2) + ',' + StrTrim(ypix,2) + ')'
                                     END

                                  ELSE: Message, 'Unexpected VALUE returned. Exitiing.'
                               ENDCASE
                               self._statusbar -> SetProperty, Text=theText
                   ENDIF

                   ; Only interested in  BUTTON DOWN.
                    IF (event.type EQ 0) THEN BEGIN

                        ; Which image are we talking about?
                        images = event.ID -> Get(/ALL, Count=count)
                        FOR j=0,count-1 DO BEGIN
                           selectedImage = images[j] -> Select(event.x, event.y, SUCCESS=success)
                           IF success THEN BREAK
                        ENDFOR

                        ; If there is no success, you haven't clicked on an image.
                        IF success EQ 0 THEN RETURN

                        ; We always do statistics, but on right button, we also call Selection Panel.
                        CASE event.press OF

                            1: BEGIN
                               selectedImage -> GetProperty, Filename=filename, SCLMIN=sclmin, $
                                    SCLMAX=sclmax, IMAGE=image, MISSING_VALUE=missing_value
                               value = selectedImage -> Pixel_To_Value(event.x, event.y, XPIXEL=xpix, YPIXEL=ypix)
                               IF N_Elements(value) EQ 1 THEN BEGIN
                                    IF value EQ missing_value THEN value = 'missing'
                                   i = Where(image NE missing_value, goodCount)
                                   IF goodCount EQ 0 THEN RETURN
                                   IF Size(image, /TNAME) EQ 'BYTE' THEN $
                                        minimage = MIN(Fix(image[i]), MAX=maximage, /NAN) ELSE $
                                        minimage = MIN(image[i], MAX=maximage, /NAN)
                               ENDIF
                               IF N_Elements(*self.theFiles) EQ 1 THEN $
                                    filename = "" ELSE $
                                    filename = File_Basename(filename, '.gz')
                               CASE N_Elements(value) OF
                                  1: BEGIN
                                    IF Size(value, /TNAME) NE 'STRING' THEN  value = String(value, Format='(F0.2)')
                                    theText = filename + $
                                         '  SCALING: [' + String(sclmin, Format='(F0.2)') + ',' + String(sclmax, Format='(F0.2)') + ']' + $
                                         '  MINMAX:  [' + String(minimage, Format='(F0.2)') + ',' + String(maximage, Format='(F0.2)') + ']' + $
                                         '  VALUE at (' + StrTrim(xpix,2) + ',' + StrTrim(ypix,2) + '): ' + value
                                     END
                                  3: BEGIN
                                    IF Size(value, /TNAME) NE 'STRING' THEN  BEGIN
                                      value1 = String(value[0],Format='(I0)')
                                      value2 = String(value[1],Format='(I0)')
                                      value3 = String(value[2],Format='(I0)')
                                    ENDIF ELSE BEGIN
                                      value1 = value[0]
                                      value2 = value[1]
                                      value3 = value[2]
                                    ENDELSE
                                    theText = filename + $
                                         '  LOCATION: [' + StrTrim(xpix,2) + ',' + StrTrim(ypix,2) + '] ' + $
                                         ' VALUE: [' + value1 + ', ' + value2 + ', ' + value3 + ']'
                                     END

                                  ELSE: Message, 'Unexpected VALUE returned. Exitiing.'
                               ENDCASE
                               self._statusbar -> SetProperty, Text=theText
                               END

                            4: BEGIN

                               ; Do statistics.
                               selectedImage -> GetProperty, Filename=filename, SCLMIN=sclmin, $
                                    SCLMAX=sclmax, IMAGE=image, MISSING_VALUE=missingValue, N_DIMENSIONS=ndims
                               value = selectedImage -> Pixel_To_Value(event.x, event.y, XPIXEL=xpix, YPIXEL=ypix)
                               i = Where(image NE missingValue, goodCount)
                               IF goodCount EQ 0 THEN RETURN
                               IF Size(image, /TNAME) EQ 'BYTE' THEN $
                                    minimage = MIN(Fix(image[i]), MAX=maximage, /NAN) ELSE $
                                    minimage = MIN(image[i], MAX=maximage, /NAN)
                               filename = File_Basename(filename, '.gz')
                               self._statusbar -> SetProperty, Text=theText

                               ; Call the Selection Panel.
                               selectedImage -> SelectPanel, event.x, event.y, event.id
                               END

                            ELSE:

                        ENDCASE
                    ENDIF
                END

                ELSE: Message, 'Error handling events from GridWindow in DataViewer::EventHandler'
           ENDCASE

         END

      'HELP': BEGIN
         IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
         dataViewerDir = ProgramRootDir(ONEUP=oneup)
         readmeFile = Filepath(ROOT_DIR=dataViewerDir, 'README.txt')
         self -> GetProperty, XOFFSET=xoffset, XSIZE=xsize, YOFFSET=yoffset, YSIZE=ysize
         xoffset = xoffset + (xsize/2)
         yoffset = yoffset + (ysize/2)
         XDisplayFile, readmeFile, GROUP=self._ID, TITLE='NSIDC DataViewer Help', RETURN_ID=displayTLB
         cgCenterTLB, displayTLB, xoffset, yoffset, /DEVICE
         END

      'IMAGE NAMES ON/OFF': BEGIN

         event.id -> GetProperty, UVALUE=buttonChangeValue, VALUE=buttonValue
         CatSetDefault, 'DATAVIEWER_IMAGE_NAMES_OFF', buttonChangeValue
         allObjects = self.gridwindow -> Get(/ALL, ISA='CatImage', COUNT=count)
         FOR j=0,count-1 DO allObjects[j] -> SetProperty, NO_NAME_DISPLAY=buttonChangeValue
         self.gridWindow -> Draw
         IF buttonValue EQ 'Image Names On' THEN $
            event.id -> SetProperty, VALUE='Image Names Off' ELSE $
            event.id -> SetProperty, VALUE='Image Names On'
         event.id -> SetProperty, UVALUE=1-buttonChangeValue

         END

      'MAP_OUTLINES ON/OFF': BEGIN

         event.id -> GetProperty, UVALUE=buttonChangeValue, VALUE=buttonValue
         CatSetDefault, 'DATAVIEWER_MAP_OUTLINE_ON', buttonChangeValue
         allObjects = self.gridwindow -> Get(/ALL, ISA='NSIDC_Image', COUNT=count)
         FOR j=0,count-1 DO allObjects[j] -> SetProperty, MAP_OUTLINE=buttonChangeValue
         self.gridWindow -> Draw
         IF buttonValue EQ 'Map Outlines On' THEN $
            event.id -> SetProperty, VALUE='Map Outlines Off' ELSE $
            event.id -> SetProperty, VALUE='Map Outlines On'
         event.id -> SetProperty, UVALUE=1-buttonChangeValue

         END

      'MAP_GRID ON/OFF': BEGIN

         event.id -> GetProperty, UVALUE=buttonChangeValue, VALUE=buttonValue
         CatSetDefault, 'DATAVIEWER_MAP_GRID_ON', buttonChangeValue
         allObjects = self.gridwindow -> Get(/ALL, ISA='NSIDC_Image', COUNT=count)
         FOR j=0,count-1 DO allObjects[j] -> SetProperty, MAP_GRID=buttonChangeValue
         self.gridWindow -> Draw
         IF buttonValue EQ 'Map Grid On' THEN $
            event.id -> SetProperty, VALUE='Map Grid Off' ELSE $
            event.id -> SetProperty, VALUE='Map Grid On'
         event.id -> SetProperty, UVALUE=1-buttonChangeValue

         END

      'NEXT_PAGE': BEGIN

            ; How many files are there to load?
            numFilesToDisplay = N_Elements(*self.theFiles) - self.fileStackPtr
            numFilesToLoad = numFilesToDisplay < self.numImages

            ; Remove the image files currently on the display.
            currentImages = self.gridWindow -> Get(/ALL)
            self.gridWindow -> Remove, /ALL
            Obj_Destroy, currentImages

            ; Load the new images.
            missing_color = CatGetDefault('DATAVIEWER_MISSING_COLOR')
            oob_low_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR')
            oob_high_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR')
            landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
            annotate_color = CatGetDefault('DATAVIEWER_ANNOTATE_COLOR')
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_MISSING_COLOR'), /Triple), 255
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR'), /Triple), 254
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR'), /Triple), 253
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_LANDMASK_COLOR'), /Triple), 252
            FOR j=0,numFilesToLoad-1 DO BEGIN
                 newObject = Parse_NSIDC_Filename((*self.theFiles)[self.fileStackPtr], INFO=info, SUCCESS=success)
                 IF success EQ 0 THEN RETURN
                 namesOff = CatGetDefault('DATAVIEWER_IMAGE_NAMES_OFF', SUCCESS=success)
                 IF success NE 1 THEN namesOff = 1
                 cbOff = CatGetDefault('DATAVIEWER_COLORBARS_OFF', SUCCESS=success)
                 IF success NE 1 THEN cbOff = 1
                 moOn = CatGetDefault('DATAVIEWER_MAP_OUTLINE_ON', SUCCESS=success)
                 IF success NE 1 THEN moOn = 0
                 gridOn = CatGetDefault('DATAVIEWER_MAP_GRID_ON', SUCCESS=success)
                 IF success NE 1 THEN gridOn = 0
                 newObject -> SetProperty, SELECTABLE=1, MISSING_COLOR=missing_color, OOB_LOW_COLOR=oob_low_color, $
                     OOB_HIGH_COLOR=oob_high_color, LANDMASK_COLOR=landmask_color, NO_NAME_DISPLAY=namesOff, $
                     NO_COLORBAR_DISPLAY=cbOff, FN_COLOR=annotate_color, MAP_OUTLINE=moOn, MAP_GRID=gridOn

                 ; If you have a scaling_value pointer active, set the properties of the new object
                 ; to the scaling values.
                 IF Ptr_Valid(self.scaling_values) THEN BEGIN
                     newObject -> GetProperty, COLORCHANGEALLOWED=colorChangeAllowed
                     IF colorChangeAllowed THEN BEGIN
                         newObject -> SetProperty, $
                            BETA=(*self.scaling_values).beta, $
                            EXPONENT=(*self.scaling_values).exponent, $
                            GAMMA=(*self.scaling_values).gamma, $
                            MEAN=(*self.scaling_values).mean, $
                            SCALETYPE=(*self.scaling_values).scaletype, $
                            SCLMIN=(*self.scaling_values).sclmin, $
                            SCLMAX=(*self.scaling_values).sclmax
                     ENDIF
                 ENDIF

                 ; Add the image object to the window and update the filestack pointer.
                 self.gridWindow -> Add, newObject
                 self.fileStackPtr = self.fileStackPtr + 1
            ENDFOR

            ; Display them.
            self.gridWindow -> Draw

            ; You have to unregister the GridWindow Color object from messages, and re-register
            ; it, otherwise, it will get the notification to redraw the images before the images
            ; have received the message to change their colors. This results in a one-click "delay"
            ; when changing color tables.
            self.gridWindow -> GetProperty, COLOR_OBJECT=gridColors
            gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_TABLECHANGE', /UNREGISTER
            gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_SETPROPERTY', /UNREGISTER
            gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_TABLECHANGE'
            gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_SETPROPERTY'

            ; Check to see if buttons should be turned on or off.
            IF self.fileStackPtr EQ (N_Elements(*self.theFiles)) $
                THEN self.nextButton -> SetProperty, SENSITIVE=0 $
                ELSE self.nextButton -> SetProperty, SENSITIVE=1
            IF self.fileStackPtr GT self.numimages $
                THEN self.prevButton -> SetProperty, SENSITIVE=1 $
                ELSE self.prevButton -> SetProperty, SENSITIVE=0

            END

      'LOAD_CONFIG_FILE': BEGIN

             suggestFilename = cgRootName(self.configFile, DIRECTORY=thePath, EXTENSION=theExtension)
             filename = Dialog_Pickfile(FILE=suggestFilename + '.txt', $
                  PATH=thepath, TITLE='Open CONFIG file...')
             IF filename EQ "" THEN RETURN

             ; Parse the config file.
             DataViewer_Parse_Configuration, filename

               ; If the configuration file in the default file is something different than
               ; itself, you will have to now parse the real configuration file.
               config_file = CatGetDefault('DATAVIEWER_CONFIG_FILE', SUCCESS=success)
               IF success THEN BEGIN
                    IF StrUpCase(File_BaseName(config_file, '.txt')) NE 'DATAVIEWER_DEFAULT' THEN BEGIN

                       ; Is this a relative file name or a complete file name?
                       IF StrUpCase(File_BaseName(config_file)) EQ StrUpCase(config_file) THEN BEGIN
                           IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
                           config_file = Filepath(ROOT_DIR=ProgramRootDir(ONEUP=oneup), $
                              SUBDIR='config', config_file)
                       ENDIF
                       IF File_Test(config_file, /READ) EQ 0 THEN $
                            Message, 'Configuration file specified in "dataviewer_default.txt" cannot be found.'
                        DataViewer_Parse_Configuration, config_file
                    ENDIF
               ENDIF

               ; Check to be sure the data directory has a file separator at the end of the directory
               ; name. If it doesn't, the path is indeterminate.
               dataDir = CatGetDefault('DATAVIEWER_DATA_DIRECTORY', SUCCESS=success)
               IF StrLowCase(dataDir) EQ 'default' THEN BEGIN
                   IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
                   dataDir = FilePath(ROOT_DIR=ProgramRootDir(ONEUP=oneup), $
                       SUBDIR='data', "")
               ENDIF
               IF StrLowCase(dataDir) EQ 'data' THEN BEGIN
                   IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
                   dataDir = FilePath(ROOT_DIR=ProgramRootDir(ONEUP=oneup), $
                       SUBDIR=dataDir, "")
               ENDIF
               IF success THEN BEGIN
                    lastChar = StrMid(dataDir, 0, 1, /REVERSE)
                    IF lastChar NE '/' AND lastChar NE '\' THEN BEGIN
                        dataDir = dataDir + Path_Sep()
                        CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', dataDir
                    ENDIF

                    ; Make sure the file separators are appropriate for the machine
                    ; we are running on. Byte('\') = 92, Byte('/') = 47.
                    CASE Path_Sep() OF
                        '/': BEGIN ; UNIX
                             thisArray = Byte(dataDir)
                             i = Where(thisArray EQ 92, count)
                             IF count GT 0 THEN BEGIN
                                thisArray[i] = Byte('/')
                                dataDir = String(thisArray)
                                CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', dataDir
                             ENDIF
                             END

                        '\': BEGIN ; Windows
                             thisArray = Byte(dataDir)
                             i = Where(thisArray EQ 47, count)
                             IF count GT 0 THEN BEGIN
                                thisArray[i] = Byte('\')
                                dataDir = String(thisArray)
                                CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', dataDir
                             ENDIF
                             END

                         ELSE: ; Strange machine. Everyone for himself...

                    ENDCASE
               ENDIF

             ; Set the data directory.
             CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', dataDir

             self -> Update_GUI_From_Config, filename

            END

      'OPEN_HDF_FILE': BEGIN
            self -> Open_HDF_File
            END

      'PREVIOUS_PAGE': BEGIN

            ; Reset the fileStackPtr.
            IF (self.fileStackPtr MOD self.numImages) EQ 0 THEN BEGIN
                self.fileStackPtr = 0 > (self.fileStackPtr - (2 * self.numImages)) < N_Elements(*self.theFiles)
            ENDIF ELSE BEGIN
                self.fileStackPtr =  0 > (self.fileStackPtr - (self.fileStackPtr MOD self.numImages) $
                    - self.numImages) <  N_Elements(*self.theFiles)
            ENDELSE

            ; Remove the image files currently on the display.
            currentImages = self.gridWindow -> Get(/ALL)
            self.gridWindow -> Remove, /ALL
            Obj_Destroy, currentImages

            ; Calculate the starting and ending index number.
            startIndex = self.fileStackPtr
            endIndex = self.fileStackPtr + self.numImages - 1

            ; Load the new images.
            missing_color = CatGetDefault('DATAVIEWER_MISSING_COLOR')
            oob_low_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR')
            oob_high_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR')
            landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
            annotate_color = CatGetDefault('DATAVIEWER_ANNOTATE_COLOR')
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_MISSING_COLOR'), /Triple), 255
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR'), /Triple), 254
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR'), /Triple), 253
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_LANDMASK_COLOR'), /Triple), 252
            FOR j=startIndex, endindex DO BEGIN
                 newObject = Parse_NSIDC_Filename((*self.theFiles)[j], INFO=info, SUCCESS=success)
                 IF success EQ 0 THEN RETURN
                 namesOff = CatGetDefault('DATAVIEWER_IMAGE_NAMES_OFF', SUCCESS=success)
                 IF success NE 1 THEN namesOff = 1
                 cbOff = CatGetDefault('DATAVIEWER_COLORBARS_OFF', SUCCESS=success)
                 IF success NE 1 THEN cbOff = 1
                 moOn = CatGetDefault('DATAVIEWER_MAP_OUTLINE_ON', SUCCESS=success)
                 IF success NE 1 THEN moOn = 0
                 gridOn = CatGetDefault('DATAVIEWER_MAP_GRID_ON', SUCCESS=success)
                 IF success NE 1 THEN gridOn = 0
                 newObject -> SetProperty, SELECTABLE=1, MISSING_COLOR=missing_color, OOB_LOW_COLOR=oob_low_color, $
                     OOB_HIGH_COLOR=oob_high_color, LANDMASK_COLOR=landmask_color, NO_NAME_DISPLAY=namesOff, $
                     NO_COLORBAR_DISPLAY=cbOff, FN_COLOR=annotate_color, MAP_OUTLINE=moOn, MAP_GRID=gridOn

                 ; If you have a scaling_value pointer active, set the properties of the new object
                 ; to the scaling values.
                 IF Ptr_Valid(self.scaling_values) THEN BEGIN
                     newObject -> GetProperty, COLORCHANGEALLOWED=colorChangeAllowed
                     IF colorChangeAllowed THEN BEGIN
                         newObject -> SetProperty, $
                            BETA=(*self.scaling_values).beta, $
                            EXPONENT=(*self.scaling_values).exponent, $
                            GAMMA=(*self.scaling_values).gamma, $
                            MEAN=(*self.scaling_values).mean, $
                            SCALETYPE=(*self.scaling_values).scaletype, $
                            SCLMIN=(*self.scaling_values).sclmin, $
                            SCLMAX=(*self.scaling_values).sclmax
                     ENDIF
                 ENDIF

                 self.gridWindow -> Add, newObject
                 self.fileStackPtr = self.fileStackPtr + 1
             ENDFOR

            ; Display them.
            self.gridWindow -> Draw

            ; You have to unregister the GridWindow Color object from messages, and re-register
            ; it, otherwise, it will get the notification to redraw the images before the images
            ; have received the message to change their colors. This results in a one-click "delay"
            ; when changing color tables.
            self.gridWindow -> GetProperty, COLOR_OBJECT=gridColors
            gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_TABLECHANGE', /UNREGISTER
            gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_SETPROPERTY', /UNREGISTER
            gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_TABLECHANGE'
            gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_SETPROPERTY'

            ; Check to see if buttons should be turned on or off.
            IF self.fileStackPtr EQ self.numImages $
                THEN self.prevButton -> SetProperty, SENSITIVE=0 $
                ELSE self.prevButton -> SetProperty, SENSITIVE=1
            IF self.fileStackPtr LT self.numImages $
                THEN self.nextButton -> SetProperty, SENSITIVE=0 $
                ELSE self.nextButton -> SetProperty, SENSITIVE=1

            END

     'REFRESH_IMAGES': BEGIN ; Re-read the images currently in the display window.

            imageObjects = self.gridWindow -> Get(/ALL, ISA='NSIDC_IMAGE', COUNT=count)
            tempFiles = StrArr(count)
            FOR j=0,count-1 DO BEGIN
               imageObjects[j] -> GetProperty, FILENAME=filename
               tempFiles[j] = filename
            ENDFOR

            ; Remove the images and distroy them.
            self.gridWindow -> Remove, /ALL
            self.fileStackPtr = self.fileStackPtr - count

            ; Learn the names of various colors we need for NSIDC images.
            missing_color = CatGetDefault('DATAVIEWER_MISSING_COLOR')
            oob_low_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR')
            oob_high_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR')
            landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
            annotate_color = CatGetDefault('DATAVIEWER_ANNOTATE_COLOR')

            ; Load colors here, because we are going to copy for the color palette below.
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_MISSING_COLOR'), /Triple), 255
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR'), /Triple), 254
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR'), /Triple), 253
            TVLCT, cgColor(CatGetDefault('DATAVIEWER_LANDMASK_COLOR'), /Triple), 252
            cttype = CatGetDefault('DATAVIEWER_DEFAULT_CT_TYPE')
            IF StrUpCase(cttype) EQ 'BREWER' THEN brewer=1 ELSE brewer = 0
            colorIndex = CatGetDefault('DATAVIEWER_DEFAULT_CT', SUCCESS=success)
            IF success EQ 0 THEN colorIndex = 4
            cgLoadCT, colorIndex, BREWER=brewer, NCOLORS=250
            TVLCT, r, g, b, /GET

            ; Read the images again.
            FOR j=0,count-1 DO BEGIN
                   newObject = Parse_NSIDC_Filename(tempFiles[j], INFO=info, SUCCESS=success)
                   IF success EQ 0 THEN RETURN
                   namesOff = CatGetDefault('DATAVIEWER_IMAGE_NAMES_OFF', SUCCESS=success)
                   IF success NE 1 THEN namesOff = 1
                   cbOff = CatGetDefault('DATAVIEWER_COLORBARS_OFF', SUCCESS=success)
                   IF success NE 1 THEN cbOff = 1
                   moOn = CatGetDefault('DATAVIEWER_MAP_OUTLINE_ON', SUCCESS=success)
                   IF success NE 1 THEN moOn = 0
                   gridOn = CatGetDefault('DATAVIEWER_MAP_GRID_ON', SUCCESS=success)
                   IF success NE 1 THEN gridOn = 0
                   newObject -> SetProperty, SELECTABLE=1, MISSING_COLOR=missing_color, OOB_LOW_COLOR=oob_low_color, $
                        OOB_HIGH_COLOR=oob_high_color, LANDMASK_COLOR=landmask_color, NO_NAME_DISPLAY=namesOff, $
                     NO_COLORBAR_DISPLAY=cbOff, FN_COLOR=annotate_color, MAP_OUTLINE=moOn, MAP_GRID=gridOn
                   newObject -> GetProperty, COLOR_OBJECT=colors, NSIDC_TAG=nsidc_tag, $
                        COLORCHANGEALLOWED=colorChangeAllowed
                         IF colorChangeAllowed THEN $
                            colors -> SetProperty, COLORPALETTE=[[r],[g],[b]], BREWER=brewer

                   ; If this is the first object, then make a color object for GridWindow that has
                   ; the same properties as this one.
                   IF (j EQ 0) AND (colorChangeAllowed) THEN BEGIN
                      colors -> GetProperty, COLORPALETTE=colorPalette
                      gridColors = Obj_New('CatColors', COLORPALETTE=colorPalette, BREWER=brewer, $
                        NAME='GRIDWINDOW_COLORS', NCOLORS=ncolors, INDEX=colorIndex)
                      self.gridWindow -> SetProperty, COLOR_OBJECT=gridColors
                   ENDIF
                   IF (j EQ 0) AND (colorChangeAllowed EQ 0) THEN BEGIN
                      gridColors = Obj_New('CatColors', COLORPALETTE=[[r],[g],[b]], BREWER=brewer, $
                        NAME='GRIDWINDOW_COLORS', NCOLORS=ncolors, INDEX=colorIndex)
                      self.gridWindow -> SetProperty, COLOR_OBJECT=gridColors
                   ENDIF
                   self.gridWindow -> Add, newObject
                   self.fileStackPtr = self.fileStackPtr + 1
            ENDFOR

            ; Draw the images.
            self.gridWindow -> Draw

            END

     'RESCALE_IMAGES': BEGIN

            ; If there is no current setting for rescaling image, then grab the first
            ; image in the window, and use its settings for the image rescaling.
            IF Ptr_Valid(self.scaling_values) EQ 0 THEN BEGIN

                imageCount = self.gridWindow -> Count()
                firstImage = self.gridWindow -> Get(Position=0)
                IF Obj_Isa_Valid(firstImage, 'NSIDC_IMAGE') EQ 0 THEN BEGIN
                    ok = Dialog_Message('Only NSIDC images can be rescaled.', /INFORMATION)
                    RETURN
                ENDIF

                ; Make sure this image allows color changing.
                firstImage -> GetProperty, COLORCHANGEALLOWED=colorChangeAllowed
                IF colorChangeAllowed EQ 0 THEN BEGIN
                    pos = 1
                    numImages = self.gridWindow -> Count()
                    WHILE (colorChangeAllowed EQ 0) AND pos LT (numImages-1) DO BEGIN
                        nextImage = self.gridWindow -> Get(Position=pos)
                        firstImage = nextImage
                        firstImage -> GetProperty, COLORCHANGEALLOWED=colorChangeAllowed
                        pos = pos + 1
                        IF pos GT imageCount THEN  BEGIN
                            ok = Dialog_Message('Color change is not possible for these images.')
                            RETURN
                        ENDIF
                    ENDWHILE
                    IF colorChangeAllowed EQ 0 THEN BEGIN
                       ok = Dialog_Message('The images in this window cannot be stretched.')
                       RETURN
                    ENDIF
                ENDIF
                firstImage -> GetProperty, IMAGE=image, $
                    BETA=beta, $
                    BOTTOM=bottom, $
                    EXPONENT=exponent, $
                    GAMMA=gamma, $
                    MEAN=mean, $
                    NCOLORS=ncolors, $
                    SCALETYPE=scaletype, $
                    SCLMIN=sclmin, $
                    SCLMAX=sclmax, $
                    NSIDC_TAG=nsidc_tag
                 self.scaling_values = Ptr_New({ $
                    BETA:beta, $
                    BOTTOM:bottom, $
                    EXPONENT:exponent, $
                    GAMMA:gamma, $
                    MEAN:mean, $
                    NCOLORS:ncolors, $
                    SCALETYPE:scaletype, $
                    SCLMIN:sclmin, $
                    SCLMAX:sclmax }, /NO_COPY)

            ENDIF ELSE BEGIN

                imageCount = self.gridWindow -> Count()
                firstImage = self.gridWindow -> Get(Position=0)

                ; Make sure this image allows color changing.
                firstImage -> GetProperty, COLORCHANGEALLOWED=colorChangeAllowed
                IF colorChangeAllowed EQ 0 THEN BEGIN
                    pos = 1
                    WHILE colorChangeAllowed EQ 0 DO BEGIN
                        nextImage = self.gridWindow -> Get(Position=1)
                        firstImage = nextImage
                        firstImage -> GetProperty, COLORCHANGEALLOWED=colorChangeAllowed
                        pos = pos + 1
                        IF pos GT imageCount THEN  BEGIN
                            ok = Dialog_Message('Color change is not possible for these images.')
                            RETURN
                        ENDIF
                    ENDWHILE
                ENDIF
                firstImage -> GetProperty, IMAGE=image, NSIDC_TAG=nsidc_tag
                beta = (*self.scaling_values).beta
                bottom = (*self.scaling_values).bottom
                exponent = (*self.scaling_values).exponent
                gamma = (*self.scaling_values).gamma
                mean = (*self.scaling_values).mean
                ncolors = (*self.scaling_values).ncolors
                scaletype = (*self.scaling_values).scaletype
                sclmin = (*self.scaling_values).sclmin
                sclmax = (*self.scaling_values).sclmax

            ENDELSE
            self.gridWindow -> SetWindow
            IF StrUpCase(nsidc_tag) EQ 'NSIDC_0079' THEN BEGIN
                indices = Where(image GT 100, count)
                IF count GT 0 THEN image[indices] = !VALUES.F_NAN
            ENDIF
            cgStretch, image, GROUP_LEADER=self->GetID(), /NO_WINDOW, $
                BETA=beta, $
                BOTTOM=bottom, $
                EXPONENT=exponent, $
                GAMMA=gamma, $
                MEAN=mean, $
                MINTHRESH=sclmin, $
                MAXTHRESH=sclmax, $
                MAXVALUE=0.15, $
                NCOLORS=ncolors, $
                SCALETYPE=scaletype, $
                NOTIFY_OBJ={object:self, method:'XStretch_Notification'}
            END

      'SAVE_CONFIG_FILE': self -> Write_Config_File

      'SAVE_WINDOW': BEGIN

            ; Call the OUTPUT method on the GridWindow.
            event.ID -> GetProperty, UVALUE=fileType

            ; If the filetype is POSTSCRIPT, then the image objects can come
            ; back from drawing into the PostScript device with their locations
            ; changed (the PostScript "window" may not have the same aspect ratio
            ; as the display window). Since image "selection" depends on location
            ; in a display window, it may not be possible to select these images
            ; when you have returned. The solution is to update the image "locations".
            ; The easiest way to do this is to simply redraw the images in the display
            ; window.
            self.gridWindow -> SetWindow
            self.gridWindow -> Output, TYPE=fileType, FILENAME='dataviewer', /DRAW

            END

      'SELECT_FILES': self -> Select_Files

      'SELECT_DIRECTORY': BEGIN

            ; Let the user select the file directory.
            dataDir = CatGetDefault('DATAVIEWER_DATA_DIRECTORY')
            theDirectory = Dialog_Pickfile(Title='Select Image Direcory', PATH=dataDir, /DIRECTORY)
            IF theDirectory[0] EQ '' THEN RETURN

            ; File all the files in the directory.
            CD, CURRENT=thisDir, theDirectory
            theFiles = File_Search('*.*', COUNT=count)
            IF count EQ 0 THEN BEGIN
                ok = Dialog_Message('No image files found in search directory.', /INFORMATION)
                CD, thisDir
                RETURN
            ENDIF
            CD, thisDir

            ; The files need to have the complete path name.
            theFiles = File_Search(theDirectory + '*.*', COUNT=newcount)

            ; Set the grid to its default values. (Necessary to load image correctly!)
            self -> ChangeGrid, /NOLOAD

            ; Load the files into the DataViewer.
            self -> LoadFiles, theFiles

            ; Make this the default directory
            CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', theDirectory
            END

      'SPLASHWINDOW': BEGIN

            ; Only button down events.
            IF event.type NE 0 THEN RETURN

            ; Get the location in normalized coordinates.
            event.id -> GetProperty, XSIZE=xsize, YSIZE=ysize
            xnorm = event.x / Float(xsize)
            ynorm = event.y / Float(ysize)

            ; Are you inside the information button?
            IF (xnorm GE 0.88) AND (xnorm LE 0.98) AND (ynorm GE 0.89) AND (ynorm LE 0.99) THEN BEGIN
                 IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
                 dataViewerDir = ProgramRootDir(ONEUP=oneup)
                 readmeFile = Filepath(ROOT_DIR=dataViewerDir, 'README.txt')
                 self -> GetProperty, XOFFSET=xoffset, XSIZE=xsize, YOFFSET=yoffset, YSIZE=ysize
                 xoffset = xoffset + (xsize/2)
                 yoffset = yoffset + (ysize/2)
                 XDisplayFile, readmeFile, GROUP=self._ID, TITLE='NSIDC DataViewer Help', RETURN_ID=displayTLB
            ENDIF ELSE BEGIN
                ok = Dialog_Message(['Image courtesy of Matt Savoie, NSIDC.', $
                                     'The minimum Arctic Sea Ice Extent, 2007.', $
                                     'Click the INFO button on the DataViewer ', $
                                     'splash screen for DataViewer information.'], /INFORMATION)
            ENDELSE
            END


      'TLB': BEGIN ; A TLB re-size event. Resize the draw widget and re-draw.

         drawObject = self -> Get("GridWindow", /Recursive_Search)

         ; Subtract the offsets
         IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' THEN BEGIN
            s = [event.x - self.xoffset, event.y - self.yoffset]
         ENDIF ELSE BEGIN
            s = [event.x - self.xoffset + 1, event.y - self.yoffset + 1]
         ENDELSE

         ; Resize the draw widget.
         drawObject -> Resize, s[0], s[1]

         ; Update the size of the statusbar to reflect the size of the draw widget.
         self._statusbar -> Resize, drawObject

         ; Redraw the image in the newly-resized draw widget.
         drawObject -> Draw

         END

       ELSE : Message, 'Unexpected event in DataViewer::EventHandler.'

   ENDCASE

   IF Obj_Valid(self) THEN self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       DATAVIEWER::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain DATAVIEWER properties. Be sure
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
;    VERSION:         The current version number of the program.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO DataViewer::GetProperty,  VERSION=version, _Ref_Extra=extrakeywords

   @cat_pro_error_handler

   version = self.version

   IF N_Elements(extrakeywords) NE 0 THEN self -> TOPLEVELBASE::GetProperty, _Extra=extrakeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       DATAVIEWER::GUI
;
; PURPOSE:
;
;       This method creates the graphical user interface elements of the program.
;
; SYNTAX:
;
;       theObject -> GUI
;
; ARGUMENTS:
;
;     menuBarID:      The identifier of the menuBar widget, if any.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO DataViewer::GUI, menubarID

   ; Create a status bar here if not Windows.
   IF StrUpCase(!Version.OS_Family) NE 'WINDOWS' THEN BEGIN
       self -> CreateStatusBar, self
       self._statusbar -> SetProperty, TEXT='Select images to view from the File menu.'
   ENDIF

   ; Create a Quit button in the menu bar.
   fileMenu = OBJ_NEW ('ButtonWidget', menuBarID ,  Value='File', /MENU)
   colors = OBJ_NEW('ButtonWidget', menuBarID, Value='Colors', /MENU)
   ops = OBJ_NEW('ButtonWidget', menuBarID, Value='Operations', /MENU)
   helpObj = OBJ_NEW('ButtonWidget', menuBarID, Value='Help', /MENU)


   button = OBJ_NEW('ButtonWidget', fileMenu, Name='SELECT_FILES', $
      Value='Open Image Files...')
   button = OBJ_NEW('ButtonWidget', fileMenu, Name='SELECT_DIRECTORY', $
      Value='Open Image Directory of Files...')
;   button = OBJ_NEW('ButtonWidget', fileMenu, NAME='OPEN_HDF_FILE', $
;      Value='Open HDF File...')
   button = OBJ_NEW('ButtonWidget', fileMenu, Name='CHANGE_GRID', Value='Change Window Layout Grid...', Sensitive=0, /SEPARATOR)

   saveAs = OBJ_NEW ('ButtonWidget', fileMenu,  Value='Save Window As...', /MENU, $
      Name='SAVE_WINDOW', /SEPARATOR)
   void = OBJ_NEW ('ButtonWidget', saveAs,  Name='SAVE_WINDOW', $
      UVALUE='JPEG', Value='JPEG File', Event_Object=saveAs)
   void = OBJ_NEW ('ButtonWidget', saveAs,  Name='SAVE_WINDOW', $
      UVALUE='TIFF', Value='TIFF File', Event_Object=saveAs)
   void = OBJ_NEW ('ButtonWidget', saveAs,  Name='SAVE_WINDOW', $
      UVALUE='BMP', Value='BMP File', Event_Object=saveAs)
   void = OBJ_NEW ('ButtonWidget', saveAs,  Name='SAVE_WINDOW', $
      UVALUE='PNG', Value='PNG File', Event_Object=saveAs)
   void = OBJ_NEW ('ButtonWidget', saveAs,  Name='SAVE_WINDOW', $
      UVALUE='POSTSCRIPT', Value='PostScript File', Event_Object=saveAs)

   button = OBJ_NEW('ButtonWidget', colors, Name='CHANGE_IMAGE_COLORS', $
        Value='Image Colors', Sensitive=0)
   button = OBJ_NEW('ButtonWidget', colors, Name='CHANGE_MISSING_COLOR', $
        Value='Missing Color', Sensitive=0)
   button = OBJ_NEW('ButtonWidget', colors, Name='CHANGE_BACKGROUND_COLOR', $
        Value='Background Color', Sensitive=0)
   button = OBJ_NEW('ButtonWidget', colors, Name='CHANGE_OOB_LOW_COLOR', $
        Value='Out-of-Bounds Low Color', Sensitive=0)
   button = OBJ_NEW('ButtonWidget', colors, Name='CHANGE_OOB_HIGH_COLOR', $
        Value='Out-of-Bounds High Color', Sensitive=0)
   button = OBJ_NEW('ButtonWidget', colors, Name='CHANGE_LANDMASK_COLOR', $
        Value='Landmask Color', Sensitive=0)
   button = OBJ_NEW('ButtonWidget', colors, Name='CHANGE_ANNOTATE_COLOR', $
        Value='Annotation Color', Sensitive=0)
   button = OBJ_NEW('ButtonWidget', colors, Name='CHANGE_OUTLINE_COLOR', $
        Value='Continental Outline Color', Sensitive=0)
   button = OBJ_NEW('ButtonWidget', colors, Name='CHANGE_GRID_COLOR', $
        Value='Map Grid Color', Sensitive=0)

   button = OBJ_NEW('ButtonWidget', ops, Name='ANIMATE_IMAGES', $
        Value='Animate All Images', Sensitive=0)
   button = OBJ_NEW('ButtonWidget', ops, Name='RESCALE_IMAGES', $
        Value='Histogram Stretch All Images', Sensitive=0)
   button = OBJ_NEW('ButtonWidget', ops, Name='REFRESH_IMAGES', $
        Value='Refresh All Images', Sensitive=0)

   namesOff = CatGetDefault('DATAVIEWER_IMAGE_NAMES_OFF', SUCCESS=success)
   IF success EQ 0 THEN namesOff = 0
   IF namesOff THEN nameValue = 'Image Names On' ELSE nameValue = 'Image Names Off'
   button = OBJ_NEW ('ButtonWidget', ops,  Name='IMAGE NAMES ON/OFF', $
      Value=nameValue, /Separator, /DYNAMIC_RESIZE, UVALUE=1-namesOff, SENSITIVE=0)

   cbOff = CatGetDefault('DATAVIEWER_COLORBARS_OFF', SUCCESS=success)
   IF success EQ 0 THEN cbOff = 0
   IF cbOff THEN cbValue = 'Colorbars On' ELSE cbValue = 'Colorbars Off'
   button = OBJ_NEW ('ButtonWidget', ops,  Name='COLORBARS ON/OFF', $
      Value=cbValue, /DYNAMIC_RESIZE, UVALUE=1-cbOff, SENSITIVE=0)

   moOn = CatGetDefault('DATAVIEWER_MAP_OUTLINE_ON', SUCCESS=success)
   IF success EQ 0 THEN moOn = 0
   IF moOn THEN moValue = 'Map Outlines Off' ELSE moValue = 'Map Outlines On'
   button = OBJ_NEW ('ButtonWidget', ops,  Name='MAP_OUTLINES ON/OFF', $
      Value=moValue, /DYNAMIC_RESIZE, UVALUE=1-moOn, SENSITIVE=0)

   gridOn = CatGetDefault('DATAVIEWER_MAP_GRID_ON', SUCCESS=success)
   IF success EQ 0 THEN gridOn = 0
   IF gridOn THEN gridValue = 'Map Grid Off' ELSE gridValue = 'Map Grid On'
   button = OBJ_NEW ('ButtonWidget', ops,  Name='MAP_GRID ON/OFF', $
      Value=gridValue, /DYNAMIC_RESIZE, UVALUE=1-gridOn, SENSITIVE=0)


   ; Configuration buttons.
   button = OBJ_NEW('ButtonWidget', fileMenu, Name='LOAD_CONFIG_FILE', $
        Value='Load a Configuration File...', /Separator)
   button = OBJ_NEW('ButtonWidget', fileMenu, Name='SAVE_CONFIG_FILE', $
        Value='Save Current Configuration As...')
   button = OBJ_NEW('ButtonWidget', fileMenu, Name='EDIT_CONFIGURATION_FILE', $
        Value='Edit Current Configuration File...', UVALUE=self)
   button = OBJ_NEW('ButtonWidget', fileMenu, Name='CHANGE_DATA_DIRECTORY', $
        Value='Change Default Data Directory...')


   button = OBJ_NEW ('ButtonWidget', helpObj,  Name='Help', Value='Program Instructions')
   button = OBJ_NEW ('ButtonWidget', fileMenu,  Name='Exit', Value='Exit', /Separator)

   ; Gather configuration variables.
   grid = CatGetDefault('DATAVIEWER_GRIDWINDOW_GRID')
   xsize = CatGetDefault('DATAVIEWER_GRIDWINDOW_XSIZE')
   ysize = CatGetDefault('DATAVIEWER_GRIDWINDOW_YSIZE')

   ; No matter what the preferred size, constrain the data viewer
   ; to no more than 95% of the screen size.
   s = Get_Screen_Size()
   xsize = xsize < (s[0] * 0.95)
   ysize = ysize < (s[1] * 0.95)
   backgroundColor = CatGetDefault('DATAVIEWER_BACKGROUND_COLOR')

   ; Create a draw widget.
   layoutBase = OBJ_NEW('BASEWIDGET', self, ROW=1, XPAD=0, YPAD=0, SPACE=0)
   gridBase = OBJ_NEW('BASEWIDGET', layoutBase, XPAD=0, YPAD=0)
   buttonBase = OBJ_NEW('BASEWIDGET', layoutBase, Column=1, XPAD=0, YPAD=0)
   self.splashWindow = OBJ_NEW('DRAWWIDGET', gridBase, XSize=xsize, YSize=ysize, NAME='SplashWindow', $
        EVENT_OBJECT=self, INITIAL_COLOR=backgroundColor, /BUTTON_EVENTS)
   self.gridWindow = OBJ_NEW('GRIDWINDOW', gridBase, GRID=1, XSize=xsize, YSize=ysize, NAME='GridWindow', $
        EVENT_OBJECT=self, INITIAL_COLOR=backgroundColor, /BUFFER_OUTPUT)

   ; Register for GRIDWINDOW_MOVE_SELECTON events.
   self.gridWindow -> RegisterForMessage, self, 'GRIDWINDOW_MOVE_SELECTION'

   ; Create movement buttons.
   self.nextButton = Obj_New('ButtonWidget', buttonBase, Value='Next', NAME='NEXT_PAGE', Sensitive=0)
   self.prevButton = Obj_New('ButtonWidget', buttonBase, Value='Previous', NAME='PREVIOUS_PAGE', Sensitive=0)

   ; Add a splash screen.
   IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
   iceimagefile = Filepath(Root_Dir=ProgramRootDir(ONEUP=oneup), $
      Subdirectory='resources', 'dataviewer_splash.jpg')
   IF File_Test(iceimagefile) EQ 0 THEN BEGIN
      iceimagefile = Find_Resource_File('dataviewer_splash.jpg')
   ENDIF
   Read_JPEG, iceimagefile, image
   iceimage = Obj_New('CatImage', image, Position=[0.0, 0.0, 1.0, 1.0], /KEEP_ASPECT, NAME='SPLASH_IMAGE')
   self.splashWindow -> Add, iceimage
   infoimagefile = Filepath(Root_Dir=ProgramRootDir(ONEUP=oneup), $
      Subdirectory='resources', 'information.jpg')
   Read_JPEG, infoimagefile, infoimage
   infoimage = Obj_New('CatImage', infoimage, Position=[0.88, 0.89, 0.98, 0.99], /KEEP_ASPECT, NAME='INFO_IMAGE')
   self.splashWindow -> Add, infoimage
   self.gridWindow -> SetProperty, GRID=grid

   ; Create a status bar here if Windows.
   IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' THEN BEGIN
       self -> CreateStatusBar, self
       self._statusbar -> SetProperty, TEXT='Select images to view from the File menu.'
   ENDIF

   ; Keep draw widgets from becoming active windows.
   Widget_Control, self->getID(), /Managed

   ; Display the entire application in the window.
   self -> Draw, /Center, REGISTER_NAME='nsidc_dataviewer'

    self -> GetProperty, GEOMETRY=tlbgeo
    self.gridWindow -> GetProperty, GEOMETRY=drawgeo
    menubarID -> GetProperty, GEOMETRY=menugeo
    self._statusBar -> GetProperty, GEOMETRY=statgeo

    ; Set up offsets for resizing the window. This is machine
    ; dependent because UNIX machines report their "size" differently
    ; than WINDOWS machines.
    IF StrUpCase(!VERSION.os_family) EQ 'WINDOWS' THEN BEGIN
        menuspace = menugeo.scr_ysize
        yfudge = 4
        xfudge = 6
    ENDIF ELSE BEGIN
        menuspace = 0.0
        xfudge = 0
        yfudge = -18
    ENDELSE
    self.xoffset = tlbgeo.scr_xsize - drawgeo.scr_xsize - xfudge
    self.yoffset = (tlbgeo.scr_ysize - (statgeo.scr_ysize + menuspace)) - drawgeo.scr_ysize - yfudge

END


;*****************************************************************************************************
;+
; NAME:
;       DATAVIEWER::LOADFILES
;
; PURPOSE:
;
;       This method loads files that were passed to the program on initialization.
;
; SYNTAX:
;
;       thisObject -> LoadFiles, files
;
; ARGUMENTS:
;
;       files:    The name of image files. It can be a scalar or array of file names, or it can
;                 a directory name. If it is a directory, then all the files in the directory are loaded.
;-
;*****************************************************************************************************
PRO DataViewer::LoadFiles, files

    @cat_pro_error_handler

    Obj_Destroy, self.splashWindow

    ; Is the argument there?
    IF N_Elements(files) EQ 0 THEN Message, 'A file or directory name must be supplied.'

    ; Is this a directory?
    IF N_Elements(files) EQ 1 THEN BEGIN

        IF File_Test(files, /DIRECTORY) THEN BEGIN
            CD, CURRENT=thisDir, files
            theFiles = File_Search('*', COUNT=filecount, /FULLY_QUALIFY_PATH)
            CD, thisDir
            IF filecount EQ 0 THEN Message, 'There are no files in this directory.'
        ENDIF ELSE theFiles = files

    ENDIF ELSE theFiles = files

    theFiles = theFiles[SORT(theFiles)]
    self.theFiles = Ptr_New(theFiles)
    self.fileStackPtr = 0

    ; If there is only one file, then change the grid to a 1x1.
    ; Don't go through change grid, because I only want to do this
    ; for a single file. I don't wish to change the default GRID value.
    IF N_Elements(theFiles) EQ 1 $
        THEN self -> ChangeGrid, [1,1], /NOLOAD $
        ELSE self -> SetProperty, TITLE='NSIDC DataViewer'

    ; Remove any image in the display window.
    objects = self.gridWindow -> Get(/ALL)
    self.gridWindow -> Remove, /All
    Obj_Destroy, objects

    ; Create image objects and add them to the grid window.
    numImagesToLoad = self.numImages < N_Elements(*self.theFiles)

    ; Learn the names of various colors we need for NSIDC images.
    missing_color = CatGetDefault('DATAVIEWER_MISSING_COLOR')
    oob_low_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR')
    oob_high_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR')
    landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
    annotate_color = CatGetDefault('DATAVIEWER_ANNOTATE_COLOR')

    ; Load colors here, because we are going to copy for the color palette below.
    TVLCT, cgColor(CatGetDefault('DATAVIEWER_MISSING_COLOR'), /Triple), 255
    TVLCT, cgColor(CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR'), /Triple), 254
    TVLCT, cgColor(CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR'), /Triple), 253
    TVLCT, cgColor(CatGetDefault('DATAVIEWER_LANDMASK_COLOR'), /Triple), 252
    cttype = CatGetDefault('DATAVIEWER_DEFAULT_CT_TYPE')
    IF StrUpCase(cttype) EQ 'BREWER' THEN brewer=1 ELSE brewer = 0
    colorIndex = CatGetDefault('DATAVIEWER_DEFAULT_CT', SUCCESS=success)
    IF success EQ 0 THEN colorIndex = 27
    cgLoadCT, colorIndex, BREWER=brewer, NCOLORS=250
    TVLCT, r, g, b, /GET

    FOR j=0,numImagesToLoad-1 DO BEGIN
           newObject = Parse_NSIDC_Filename((*self.theFiles)[self.fileStackPtr], INFO=info, SUCCESS=success)
           IF success EQ 0 THEN CONTINUE
           namesOff = CatGetDefault('DATAVIEWER_IMAGE_NAMES_OFF', SUCCESS=success)
           IF success NE 1 THEN namesOff = 1
           cbOff = CatGetDefault('DATAVIEWER_COLORBARS_OFF', SUCCESS=success)
           IF success NE 1 THEN cbOff = 1
           moOn = CatGetDefault('DATAVIEWER_MAP_OUTLINE_ON', SUCCESS=success)
           IF success NE 1 THEN moOn = 0
           gridOn = CatGetDefault('DATAVIEWER_MAP_GRID_ON', SUCCESS=success)
           IF success NE 1 THEN gridOn = 0

           newObject -> SetProperty, SELECTABLE=1, MISSING_COLOR=missing_color, MISSING_INDEX=255, OOB_LOW_COLOR=oob_low_color, $
                OOB_HIGH_COLOR=oob_high_color, LANDMASK_COLOR=landmask_color, NO_NAME_DISPLAY=namesOff, $
                NO_COLORBAR_DISPLAY=cbOff, FN_COLOR=annotate_color, MAP_OUTLINE=moOn, MAP_GRID=gridOn
           newObject -> GetProperty, COLOR_OBJECT=colors, NSIDC_TAG=nsidc_tag, $
                COLORCHANGEALLOWED=colorChangeAllowed, COLORCHANGENCOLORS=ncolors, COORD_OBJECT=coords
           IF colorChangeAllowed THEN $
                    colors -> SetProperty, COLORPALETTE=[[r],[g],[b]], BREWER=brewer, NCOLORS=ncolors, INDEX=colorIndex
           colors -> SetProperty, NAME = 'Image ' + StrTrim(j,2) + ' COLORS'

           ; If this is the first object, then make a color object for GridWindow that has
           ; the same properties as this one.
           IF (j EQ 0) AND (colorChangeAllowed) THEN BEGIN
              colors -> GetProperty, COLORPALETTE=colorPalette
              gridColors = Obj_New('CatColors', COLORPALETTE=colorPalette, BREWER=brewer, $
                NAME='GRIDWINDOW_COLORS', NCOLORS=ncolors, INDEX=colorIndex)
              self.gridWindow -> SetProperty, COLOR_OBJECT=gridColors
           ENDIF
           IF (j EQ 0) AND (colorChangeAllowed EQ 0) THEN BEGIN
              gridColors = Obj_New('CatColors', COLORPALETTE=[[r],[g],[b]], BREWER=brewer, $
                NAME='GRIDWINDOW_COLORS', NCOLORS=ncolors, INDEX=colorIndex)
              self.gridWindow -> SetProperty, COLOR_OBJECT=gridColors
              newObject -> SetProperty, /NO_COLORBAR_DISPLAY
           ENDIF

           self.gridWindow -> Add, newObject
           self.fileStackPtr = self.fileStackPtr + 1
    ENDFOR

    ; We must have loaded at least one image to continue with this method.
    IF self.fileStackPtr EQ 0 THEN RETURN

    ; Make sure the Grid Window is notified if the gridColors object changes.
    ; This is so it can redraw itself.
    gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_TABLECHANGE'
    gridColors -> RegisterForMessage, self.gridWindow, 'COLORTOOL_SETPROPERTY'

    ; Make the GUI buttons sensitive.
    button = self -> Get('CHANGE_IMAGE_COLORS', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('CHANGE_GRID', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('CHANGE_MISSING_COLOR', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('CHANGE_BACKGROUND_COLOR', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('CHANGE_OOB_LOW_COLOR', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('CHANGE_OOB_HIGH_COLOR', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('CHANGE_LANDMASK_COLOR', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('CHANGE_ANNOTATE_COLOR', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('CHANGE_OUTLINE_COLOR', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('CHANGE_GRID_COLOR', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('RESCALE_IMAGES', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('ANIMATE_IMAGES', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('IMAGE NAMES ON/OFF', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('MAP_OUTLINES ON/OFF', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('MAP_GRID ON/OFF', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
    button = self -> Get('REFRESH_IMAGES', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1

    ; Special handling if there is just a single file.
    IF N_Elements(theFiles) EQ 1 $
        THEN self -> SingleFileSetup, newObject $
        ELSE BEGIN

           self.gridWindow -> GetProperty, GRID=grid
           IF (grid[1] EQ 1) AND (grid[0] EQ 1) THEN self -> ChangeGrid

           ; Set the name on/off button.
           namesOff = CatGetDefault('DATAVIEWER_IMAGE_NAMES_OFF', SUCCESS=success)
           IF success EQ 0 THEN namesOff = 0
           IF namesOff THEN nameValue = 'Image Names On' ELSE nameValue = 'Image Names Off'
           button = self -> Get('IMAGE NAMES ON/OFF', /RECURSIVE_SEARCH)
           IF Obj_Valid(button) THEN button -> SetProperty, VALUE=nameValue, UVALUE=1-namesOff, SENSITIVE=1

           ; Set the colorbar on/off button.
           cbOff = CatGetDefault('DATAVIEWER_COLORBARS_OFF', SUCCESS=success)
           IF success EQ 0 THEN cbOff = 0
           IF cbOff THEN cbValue = 'Colorbars On' ELSE cbValue = 'Colorbars Off'
           button = self -> Get('COLORBARS ON/OFF', /RECURSIVE_SEARCH)
           IF Obj_Valid(button) THEN button -> SetProperty, VALUE=cbValue, UVALUE=1-cbOff, SENSITIVE=1

           ; Set the map grid on/off button.
           gridOn = CatGetDefault('DATAVIEWER_MAP_GRID_ON', SUCCESS=success)
           IF success EQ 0 THEN gridOn = 0
           IF gridOn THEN gridValue = 'Map Grid Off' ELSE gridValue = 'Map Grid On'
           button = self -> Get('MAP_GRID ON/OFF', /RECURSIVE_SEARCH)
           IF Obj_Valid(button) THEN button -> SetProperty, VALUE=gridValue, UVALUE=1-gridOn, SENSITIVE=1

           ; Set the map outline on/off button.
           moOn = CatGetDefault('DATAVIEWER_MAP_OUTLINE_ON', SUCCESS=success)
           IF success EQ 0 THEN moOn = 0
           IF moOn THEN moValue = 'Map Outlines Off' ELSE moValue = 'Map Outlines On'
           button = self -> Get('MAP_OUTLINES ON/OFF', /RECURSIVE_SEARCH)
           IF Obj_Valid(button) THEN button -> SetProperty, VALUE=moValue, UVALUE=1-moOn, SENSITIVE=1
        ENDELSE

    ; Draw the images.
    self.gridWindow -> Draw

    ; Reset the scaling values pointer.
    Ptr_Free, self.scaling_values

    ; Should the NEXT button be made sensitive?

    IF N_Elements(*self.theFiles) GT numImagesToLoad THEN BEGIN
        self.nextButton -> SetProperty, SENSITIVE=1
        IF self.fileStackPtr LE self.numimages $
            THEN self.prevButton -> SetProperty, SENSITIVE=0 $
            ELSE self.prevButton -> SetProperty, SENSITIVE=1
    ENDIF ELSE BEGIN
        self.nextButton -> SetProperty, SENSITIVE=0
        IF self.fileStackPtr LE self.numimages $
            THEN self.prevButton -> SetProperty, SENSITIVE=0 $
            ELSE self.prevButton -> SetProperty, SENSITIVE=1
    ENDELSE

    ; Turn button events on in GridWindows.
    self.gridWindow -> SetProperty, BUTTON_EVENTS=1, MOTION_EVENTS=1
END



;*****************************************************************************************************
;+
; NAME:
;       DATAVIEWER::MESSAGEHANDLER
;
; PURPOSE:
;
;       This method receives notification of a SENDMESSAGE call from another object's
;       method.
;
; SYNTAX:
;
;       thisObject -> MessageHandler, title, SENDER=sender, DATA=message
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
PRO DataViewer::MessageHandler, title, DATA=data, SENDER=sender

    @cat_pro_error_handler

    CASE StrUpCase(title) OF

        'GRIDWINDOW_MOVE_SELECTION': BEGIN

           ; Get the object at the new position. Find out it's name
           movedObject = self.gridWindow -> Get(Position=data.movedTo)
           numImagesInWindow = self.gridWindow -> Count()
           IF Obj_Isa_Valid(movedObject, 'CatImage') THEN $
                movedObject -> GetProperty, FILENAME=movedFilename ELSE $
                Message, 'Did not find an image in the expected position in the window.'
;            print, movedFilename
;            print, ''
;            FOR j=0,N_Elements(*self.thefiles)-1 do print, (*self.theFiles)[j]
;            print, ''
            filestackPtr = self.filestackPtr - numImagesInWindow
            currentFileIndex = filestackPtr + data.movedFrom
            moveToFileIndex =  filestackPtr + data.movedTo

            ; Make sure these indices are inside the number of files you actually
            ; have in the window.
            numFiles = N_Elements(*self.theFiles)
            currentFileIndex = 0 > currentFileIndex < (numFiles-1)
            moveToFileIndex = 0 > moveToFileIndex < (numFiles-1)

            ; Remove the file from the file list.
            copyOfFiles = *self.theFiles
            theFilename = copyOfFiles[currentFileIndex]
            CASE currentFileIndex OF
                0: copyOfFiles = copyOfFiles[1:*]
                numFiles-1: copyOfFiles = copyOfFiles[0:numFiles-2]
                ELSE: copyOfFiles = [copyOfFiles[0:currentFileIndex-1], copyOfFiles[currentFileIndex+1:*]]
            ENDCASE

            ; Insert the file into the file list at the correct location.
            CASE moveToFileIndex OF
                0: copyOfFiles = [theFilename, copyOfFiles]
                numFiles-1: copyOfFiles = [copyOfFiles, theFilename]
                ELSE: copyOfFiles = [copyOfFiles[0:moveToFileIndex-1], theFilename, copyOfFiles[moveToFileIndex:*]]
            ENDCASE

            ; Put the re-orgainized files back in the list.
            *self.theFiles = copyOfFiles

            END

        ELSE: Message, 'Received an upexpected message in the message handler.'

    ENDCASE


END ;**************************************************************************************************


;+
; NAME:
;       DATAVIEWER::OPEN_HDF_FILE
;
; PURPOSE:
;
;       This method is called to open an HDF file and read and display the 2D scientific
;       data sets contained within it.
;
; SYNTAX:
;
;       theObject -> Open_HDF_File
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
;PRO DataViewer::Open_HDF_File
;
;    @cat_pro_error_handler
;
;    ; Select an HDF file to open.
;    dataDir = CatGetDefault('DATAVIEWER_DATA_DIRECTORY')
;    hdf_file = Dialog_Pickfile(FILTER='*.hdf', TITLE='Select HDF File', PATH=dataDir)
;    IF hdf_file[0] EQ '' THEN RETURN
;
;    ; Set the data directory system variable.
;    void = cgRootName(hdf_file[0], DIRECTORY=newDir)
;    CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', newDir
;
;    ; Set the grid to its default values. (Necessary to load image correctly!)
;    self -> ChangeGrid, /NOLOAD
;
;    ; Open the HDF file and search through it for SDs containing 2D images.
;    ; It is these we will display.
;    hdf_id = HDF_SD_START(hdf_file[0])
;
;    ; How many variables are there in the file?
;    HDF_SD_FILEINFO, hdf_id, numVariables, numGlobalAtts
;
;    ; Let the user know if there are no variables in this file.
;    IF numVariables EQ 0 THEN BEGIN
;         IF (!D.FLAGS AND 256) NE 0 THEN $
;            void = Dialog_Message('There are no scientific variables in file: ' + hdf_file) ELSE $
;            Message, 'There are no scientific variables in file: ' + hdf_file, /INFORMATIONAL
;         RETURN
;    ENDIF
;
;    ; Get each variable, see if it is 2D, and save its name, if so.
;    vars2d = StrArr(numVariables)
;    count = 0
;    FOR index=0,numVariables-1 DO BEGIN
;         varID = HDF_SD_SELECT(hdf_id, index)
;         HDF_SD_GETINFO, varID, NAME=varName, NDIMS=ndims
;         IF ndims EQ 2 THEN BEGIN
;            vars2d[count] = varName
;            count = count + 1
;         ENDIF
;    ENDFOR
;    IF count GT 0 THEN vars2D = vars2D[0:count-1]
;    numVariables = N_Elements(vars2D)
;    IF numVariables EQ 0 THEN BEGIN
;         IF (!D.FLAGS AND 256) NE 0 THEN $
;            void = Dialog_Message('There are no 2D scientific variables in file: ' + hdf_file) ELSE $
;            Message, 'There are no 2D scientific variables in file: ' + hdf_file, /INFORMATIONAL
;         RETURN
;    ENDIF
;
;    ; Close the HDF file access.
;    HDF_SD_END, hdf_id
;
;    ; Save the HDF filename.
;    self.hdf_filename = hdf_file
;
;    vars = Name_Selector(vars2d, Label='Select SD to Read and Display', CANCEL=cancelled)
;    IF cancelled THEN RETURN
;
;
;    ; Load the variables.
;    self -> LoadFiles, vars2D, /HDF
;
;END ;**************************************************************************************************

;+
; NAME:
;       DATAVIEWER::SELECT_FILES
;
; PURPOSE:
;
;       This method is an event handler method that is called from the EventHandler
;       when the Select Files button is clicked.
;
; SYNTAX:
;
;       theObject -> Select_FIles
;
; ARGUMENTS:
;
;     imageObject:     The image object that is to be displayed.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO DataViewer::Select_Files

    ; Let the user select the file.
    dataDir = CatGetDefault('DATAVIEWER_DATA_DIRECTORY')
    theFiles = Dialog_Pickfile(/MULTIPLE, Title='Select Image Files', PATH=dataDir)
    IF theFiles[0] EQ '' THEN RETURN

    ; Special processing if this is an HDF file.
    testFile = theFiles[0]
    filename = cgRootName(testFile, EXTENSION=ext, DIRECTORY=dir)
    IF StrLowCase(ext) EQ 'hdf' OR StrLowCase(ext) EQ 'hdfeos' THEN BEGIN

       ; Open the HDF file
       hdfID = HDF_SD_Start(testFile)

       ; Which variables would the user like to display from this file or files?
       ; We are only going to allow 2D scientific data sets.
       sd_list = HDF_SD_Varlist(hdfid)
       list2dvar = StrArr(sd_list.nvars)
       listCnt = 0
       FOR j=0, sd_list.nvars-1 DO BEGIN
          varinfo = HDF_SD_Varinfo(hdfID, sd_list.varnames[j])
          IF varinfo.ndims NE 2 THEN CONTINUE
          list2dvar[j] = sd_list.varnames[j]
          listCnt = listCnt + 1
       ENDFOR
       IF listCnt EQ 0 THEN BEGIN
          void = Dialog_Message('Selected HDF file has no 2D arrays inside it. Returning.')
          HDF_SD_END, hdfID
          RETURN
       ENDIF
       list2dvar = list2dvar[0:listCnt-1]

       ; Allow the user to select the variables.
       selectList = List_Selector(list2dvar, COUNT=selectCnt, CANCEL=cancelled, GROUP_LEADER=self->GetID())
       IF cancelled THEN BEGIN
          HDF_SD_END, hdfID
          RETURN
       ENDIF

       tempFiles = StrArr(selectCnt)
       FOR j=0,selectCnt-1 DO BEGIN
          tempFiles[j] = testFile + '@' + selectList[j]
       ENDFOR

       ; Close the file
       HDF_SD_END, hdfID

        ; Set the data directory system variable.
        void = cgRootName(theFiles[0], DIRECTORY=newDir)
        CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', newDir

       ; Set the grid to its default values. (Necessary to load image correctly!)
       self -> ChangeGrid, /NOLOAD

       ; Load the files into the DataViewer.
       self -> LoadFiles, tempFiles

       RETURN

    ENDIF

    ; Set the data directory system variable.
    void = cgRootName(theFiles[0], DIRECTORY=newDir)
    CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', newDir

    ; Set the grid to its default values. (Necessary to load image correctly!)
    self -> ChangeGrid, /NOLOAD

    ; Load the files into the DataViewer.
    self -> LoadFiles, theFiles

END ;--------------------------------------------------------------------------------------------------

;
;
;+
; NAME:
;       DATAVIEWER::SINGLEFILESETUP
;
; PURPOSE:
;
;       This method is called to set up the DataViewer for single-image viewing. In particular,
;       the DataViewer window is sized to accommodate the single image.
;
; SYNTAX:
;
;       theObject -> SingleFileSetup, imageObject
;
; ARGUMENTS:
;
;     imageObject:     The image object that is to be displayed.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO DataViewer::SingleFileSetup, imageObject

    @cat_pro_error_handler

    ; Make the aspect ratio of the window the same as the image
    imageObject -> GetProperty, IMAGE=image
    self.gridWindow -> GetProperty, XSIZE=gxsize, YSIZE=gysize
    max_xSize = CatGetDefault('DATAVIEWER_GRIDWINDOW_XSIZE', SUCCESS=success)
    IF ~success THEN max_xSize = 550
    max_ySize = CatGetDefault('DATAVIEWER_GRIDWINDOW_YSIZE', SUCCESS=success)
    IF ~success THEN max_ySize = 550
    maxSize = MAX([max_xSize > gxsize, max_ySize > gysize])
    dims = Image_Dimensions(image, XSIZE=imgxsize, YSIZE=imgysize)
    aspect = Float(imgysize)/imgxsize
    IF aspect GE 1 THEN BEGIN
        ysize = maxsize
        xsize = maxsize / aspect
    ENDIF ELSE BEGIN
        xsize = maxsize
        ysize = maxsize * aspect
    ENDELSE
    self.gridWindow -> SetProperty, XSIZE=xsize, YSIZE=ysize
    self._statusbar -> Resize, self.gridWindow
    self -> SetProperty, TITLE='NSIDC DataViewer: ' + cgRootName((*self.theFiles)[0])


      ; Set the name on/off button.
      namesOff = CatGetDefault('DATAVIEWER_IMAGE_NAMES_OFF', SUCCESS=success)
      IF success EQ 0 THEN namesOff = 0
      IF namesOff THEN nameValue = 'Image Names On' ELSE nameValue = 'Image Names Off'
      button = self -> Get('IMAGE NAMES ON/OFF', /RECURSIVE_SEARCH)
      IF Obj_Valid(button) THEN button -> SetProperty, VALUE=nameValue, UVALUE=1-namesOff, SENSITIVE=1

      ; Set the colorbar on/off button.
      cbOff = CatGetDefault('DATAVIEWER_COLORBARS_OFF', SUCCESS=success)
      IF success EQ 0 THEN cbOff = 0
      IF cbOff THEN cbValue = 'Colorbars On' ELSE cbValue = 'Colorbars Off'
      button = self -> Get('COLORBARS ON/OFF', /RECURSIVE_SEARCH)
      IF Obj_Valid(button) THEN button -> SetProperty, VALUE=cbValue, UVALUE=1-cbOff, SENSITIVE=1

      ; Set the map grid on/off button.
      gridOn = CatGetDefault('DATAVIEWER_MAP_GRID_ON', SUCCESS=success)
      IF success EQ 0 THEN gridOn = 0
      IF gridOn THEN gridValue = 'Map Grid Off' ELSE gridValue = 'Map Grid On'
      button = self -> Get('MAP_GRID ON/OFF', /RECURSIVE_SEARCH)
      IF Obj_Valid(button) THEN button -> SetProperty, VALUE=gridValue, UVALUE=1-gridOn, SENSITIVE=1

      ; Set the map outline on/off button.
      moOn = CatGetDefault('DATAVIEWER_MAP_OUTLINE_ON', SUCCESS=success)
      IF success EQ 0 THEN moOn = 0
      IF moOn THEN moValue = 'Map Outlines Off' ELSE moValue = 'Map Outlines On'
      button = self -> Get('MAP_OUTLINES ON/OFF', /RECURSIVE_SEARCH)
      IF Obj_Valid(button) THEN button -> SetProperty, VALUE=moValue, UVALUE=1-moOn, SENSITIVE=1

    ; Turn certain control off.
    imageObject -> GetProperty, COLORCHANGEALLOWED=colorChangeAllowed, N_DIMENSIONS=ndims, SCALETYPE=scaletype
    CASE ndims OF
        2: BEGIN
           IF colorChangeAllowed EQ 0 THEN BEGIN
                button = self -> Get('RESCALE_IMAGES', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
           ENDIF
           button = self -> Get('CHANGE_GRID', /RECURSIVE_SEARCH)
           IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
           IF scaletype EQ 8 THEN BEGIN
                button = self -> Get('CHANGE_IMAGE_COLORS', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_MISSING_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_BACKGROUND_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_OOB_LOW_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_OOB_HIGH_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_LANDMASK_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_ANNOTATE_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
                button = self -> Get('RESCALE_IMAGES', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
           ENDIF
           END

        3: BEGIN
                button = self -> Get('CHANGE_GRID', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
                button = self -> Get('CHANGE_IMAGE_COLORS', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_MISSING_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_BACKGROUND_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_OOB_LOW_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_OOB_HIGH_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_LANDMASK_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0
                button = self -> Get('CHANGE_ANNOTATE_COLOR', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=1
                button = self -> Get('RESCALE_IMAGES', /RECURSIVE_SEARCH)
                IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0

           END
    ENDCASE
    button = self -> Get('ANIMATE_IMAGES', /RECURSIVE_SEARCH)
    IF Obj_Valid(button) THEN button -> SetProperty, SENSITIVE=0

END

; ************************************************************************************************
;+
; NAME:
;       DATAVIEWER::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set DATAVIEWER properties. Be sure
;       you ALWAYS call the superclass SETPROPERTY method if you have extra
;       keywords!
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
;     _EXTRA:     Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO DataViewer::SetProperty, _Extra=extrakeywords

   @cat_pro_error_handler

   IF N_Elements(extrakeywords) NE 0 THEN self -> TOPLEVELBASE::SetProperty, _Extra=extrakeywords

   self -> Report, /Completed

END


; ************************************************************************************************
;+
; NAME:
;       DATAVIEWER::UPDATE_GUI_FROM_CONFIG
;
; PURPOSE:
;
;       This method updates items in the GUI when a new configuration file is loaded.
;
; SYNTAX:
;
;       theObject -> Update_GUI_From_Config
;
; ARGUMENTS:
;
;     filename:     The name of the configuration file.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO DataViewer::Update_GUI_From_Config, filename

   @cat_pro_error_handler

             ; New background color.
             backgroundColor = CatGetDefault('DATAVIEWER_BACKGROUND_COLOR')
             self.gridWindow -> SetProperty, INITIAL_COLOR=backgroundColor

             ; New drawing colors.
             self.gridWindow -> GetProperty, COLOR_OBJECT=colors
             IF Obj_Valid(colors) EQ 0 THEN BEGIN
                colors = Obj_New('CatColors')
             ENDIF

             ; New colors.
             cttype = CatGetDefault('DATAVIEWER_DEFAULT_CT_TYPE')
             IF StrUpCase(cttype) EQ 'BREWER' THEN brewer=1 ELSE brewer = 0
             index = CatGetDefault('DATAVIEWER_DEFAULT_CT')
             colors -> SetProperty, INDEX=index, BREWER=brewer, NCOLORS=250
             colors -> Loadct, index
             colors -> GetProperty, RED=r, GREEN=g, BLUE=b, NCOLORS=ncolors, $
               BREWER=brewer, INDEX=index
             missingColor = CatGetDefault('DATAVIEWER_MISSING_COLOR')
             color = cgColor(missingColor, /TRIPLE)
             r[255] = color[0]
             g[255] = color[1]
             b[255] = color[2]
             oob_low_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR')
             color = cgColor(oob_low_color, /TRIPLE)
             r[254] = color[0]
             g[254] = color[1]
             b[254] = color[2]
             oob_high_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR')
             color = cgColor(oob_high_color, /TRIPLE)
             r[253] = color[0]
             g[253] = color[1]
             b[253] = color[2]
             landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
             color = cgColor(landmask_color, /TRIPLE)
             r[252] = color[0]
             g[252] = color[1]
             b[252] = color[2]

             ; Update everyone.
             colors -> SetProperty, INDEX=CatGetDefault('DATAVIEWER_DEFAULT_CT'), BREWER=brewer, RED=r, GREEN=g, BLUE=b

             ; Update the config file.
             self.configFile = filename

             ; New grid.
             self -> ChangeGrid

             ; Update the size of the grid window, if necessary.
             xsize = CatGetDefault('DATAVIEWER_GRIDWINDOW_XSIZE')
             ysize =  CatGetDefault('DATAVIEWER_GRIDWINDOW_YSIZE')
             self.gridWindow -> GetProperty, XSIZE=xs, YSIZE=ys
             IF xs NE xsize OR ysize NE ys THEN BEGIN
                self.gridWindow -> Resize, xsize, ysize, DRAW=1
             ENDIF

   self -> Report, /Completed

END

PRO DataViewer::XColors_Notification, XCOLORS_DATA=colorData
    IF colorData.index NE -1 THEN BEGIN
        CatSetDefault, 'DATAVIEWER_DEFAULT_CT', colorData.index
    ENDIF
    CatSetDefault, 'DATAVIEWER_DEFAULT_CT_TYPE', colorData.type
    self.gridWindow -> GetProperty, COLOR_OBJECT=colors
    colors -> SetProperty, BREWER=(colorData.type EQ 'BREWER') ? 1 : 0, INDEX=colorData.index, $
       COLORPALETTE=[[colorData.r],[colorData.g],[colorData.b]]
     self.gridWindow -> Draw
END



PRO DataViewer::XStretch_Notification, info

   ; Get all the images currently in the window.
   images = self.gridWindow -> Get(/All, ISA='NSIDC_IMAGE', Count=count)
   self.gridWindow -> SetWindow
   FOR j=0,count-1 DO BEGIN
       images[j] -> GetProperty, COLORCHANGEALLOWED=colorChangeAllowed
       IF colorChangeAllowed EQ 0 THEN CONTINUE
       images[j] -> SetProperty, SCALETYPE=info.type, SCLMIN=info.minThresh, SCLMAX=info.maxThresh, $
           GAMMA=info.gamma, BETA=info.beta, MEAN=info.mean, EXPONENT=info.exponent
    ENDFOR
    self -> Draw

   ; Update the scaling values pointer.
   IF Ptr_Valid(self.scaling_values) THEN BEGIN
        (*self.scaling_values).beta = info.beta
        (*self.scaling_values).exponent = info.exponent
        (*self.scaling_values).gamma = info.gamma
        (*self.scaling_values).mean = info.mean
        IF Size(info.type, /TNAME) EQ 'STRING' THEN BEGIN
              possibleTypes = ['LINEAR', 'GAMMA', 'LOG', 'ASINH', $
                               'LINEAR 2%', 'SQUARE ROOT', 'EQUALIZATION', 'GAUSSIAN']
              index = Where(possibleTypes EQ StrUpCase(info.type), count)
              IF count EQ 0 THEN Message, 'Unknown scaling type encountered.'
              scaletype = index
        ENDIF ELSE scaletype = info.type
        (*self.scaling_values).scaletype = scaletype
        (*self.scaling_values).sclmin = info.minThresh
        (*self.scaling_values).sclmax = info.maxThresh
   ENDIF
END


;*****************************************************************************************************
;+
; NAME:
;       DATAVIEWER::WRITE_CONFIG_FILE
;
; PURPOSE:
;
;       This method writes the configuration file for the program
;
; SYNTAX:
;
;       theObject -> WRITE_CONFIG_FILE, theConfigFile
;
; ARGUMENTS:
;
;     theConfigFile:   The name of the configuration file to write.
;
; KEYWORDS:
;
;     NOCURRENTWINDOW:   Normally, the window size is taken from the current graphics window.
;                        However, if this keyword is set, the window size is taken from the
;                        current value of DATAVIEWER_GRIDWINDOW_XSIZE and
;                        DATAVIEWER_GRIDWINDOW_YSIZE.
;-
;*****************************************************************************************************
PRO DataViewer::Write_Config_File, filename, NOCURRENTWINDOW=noCurrentWindow

    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF

    ; If the configuration file doesn't exist. Ask for a filename.
    IF N_Elements(filename) EQ 0 THEN BEGIN
      suggestFilename = cgRootName(self.configFile, DIRECTORY=thePath, EXTENSION=theExtension)
      filename = Dialog_Pickfile(FILE=suggestFilename + '.txt', /Write, $
           PATH=thepath, TITLE='Save CONFIG file as...')
      IF filename EQ "" THEN RETURN

    ENDIF

    ; Check keywords.
    noCurrentWindow = Keyword_Set(noCurrentWindow)

    ; Is the filename such that it will write into the config directory?
    basename = cgRootName(filename, DIRECTORY=theDir, EXTENSION=extension)
    IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
    IF theDir EQ "" THEN filename = Filepath(ROOT_DIR=ProgramRootDir(ONEUP=oneup), $
      SUBDIR='config', filename)
    IF extension EQ "" THEN filename = filename + '.txt'

    ; Open a file for writing.
    OpenW, lun, filename, /GET_LUN

    ; Write some comments into the file.
    PrintF, lun, '; This is a configuration file for the DataViewer Program.
    PrintF, lun, '; Created ' + Systime()
    PrintF, lun, ''
    writedefaults = 0

    ; Get the current size of the window and save that, too
    self.gridWindow -> GetProperty, XSIZE=xsize, YSIZE=ysize
    IF ~NoCurrentWindow THEN BEGIN
        CatSetDefault, 'DATAVIEWER_GRIDWINDOW_XSIZE', xsize
        CatSetDefault, 'DATAVIEWER_GRIDWINDOW_YSIZE', ysize
    ENDIF

    ; Get all the CatDefaults. Those that start with "DATAVIEWER" will be
    ; written into the new file.
    CatHelpDefaults, OUTPUT=theDefaults
    FOR j=0, N_Elements(theDefaults)-1 DO BEGIN
        parts = StrSplit(theDefaults[j], ' ', /Extract)
        cmd = parts[0]
        IF StrMid(cmd, 0, 10) EQ 'DATAVIEWER' THEN BEGIN

           ; Get the value of this default.
           theValue = CatGetDefault(cmd)
           theType = Size(theValue, /TNAME)
           num = N_Elements(theValue)
           CASE theType OF
              'STRING': Printf, lun, cmd + ", '" + theValue + "'"
              ELSE: BEGIN
                  CASE num OF
                      1: Printf, lun, cmd + ", " + StrTrim(theValue,2)
                      2: Printf, lun, cmd + ", " +'[' + StrTrim(theValue[0],2) +',' + StrTrim(theValue[1],2) + ']'
                      ELSE: Message, 'Not currently set up to handle defaults with ' + StrTrim(num,2) + ' elements: ' + cmd
                  ENDCASE
               END
            ENDCASE
            writedefaults = 1

        ENDIF

     ENDFOR

     ; Close the file
     Free_Lun, lun

     ; If nothing was written, then delete the file. Else set this as the configuration file.
     IF writedefaults EQ 0 THEN File_Delete, filename ELSE self.configFile = filename

END


;*****************************************************************************************************
;+
; NAME:
;       DATAVIEWER::KILL_NOTIFY
;
; PURPOSE:
;
;       This method is called automatically when the application is destroyed.
;       It's primary purpose is to clean up any objects that have been created
;       to run the Catalyst system. It might also destroy any Catalyst infrastructure
;       by calling CatDestroyDefaults.
;
; SYNTAX:
;
;     None.
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO DataViewer::Kill_Notify

   IF XRegistered('nsidc_dataviewer') EQ 0 THEN CatDestroyDefaults

END


;*****************************************************************************************************
;+
; NAME:
;       DATAVIEWER::CLEANUP
;
; PURPOSE:
;
;       This is the DATAVIEWER object class destructor method.
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
PRO DataViewer::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self.theFiles
   Ptr_Free, self.scaling_values

   self -> TOPLEVELBASE::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       DATAVIEWER::INIT
;
; PURPOSE:
;
;       This is the DATAVIEWER object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     __REF_EXTRA:     Any keywords appropriate for the superclass INIT method.
;                      In this case you MUST use _REF_EXTRA instead of _EXTRA because
;                      you need to return the MENUBAR object reference.
;-
;*****************************************************************************************************
FUNCTION DataViewer::INIT, _Ref_Extra=extra

   @cat_func_error_handler

   ; The application is contained in a top-level base widget.
   ok = self->TOPLEVELBASE::INIT(_Extra=extra, XPAD=0, YPAD=0, SPACE=5, $
      TITLE='NSIDC DataViewer')

   grid = CatGetDefault('DATAVIEWER_GRIDWINDOW_GRID')
   self.numImages = grid[0] * grid[1]
   IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
   self.configFile = Filepath(Root_Dir=ProgramRootDir(ONEUP=oneup), $
      SUBDIRECTORY='config', 'dataviewer_default.txt')

   self.version = 1.0

   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed
   RETURN, ok

END


;*****************************************************************************************************
;
; NAME:
;       DATAVIEWER Class Definition
;
; PURPOSE:
;
;       This is the structure definition code for the DATAVIEWER object. The basic
;       application consists of a top-level base widget with a status bar.
;
;*****************************************************************************************************
PRO DataViewer__Define, class

   class = { DataViewer, $
             INHERITS TopLevelBase, $
             configFile: "", $               ; The name of the current config file.
             numImages: 0L, $                ; The number of possible images in viewer at any one time.
             fileStackPtr: 0L, $             ; The location in the file stack.
             theFiles: Ptr_New(), $          ; A list of image files (or HDF SD variable names).
             hdf_filename: "", $             ; The name of an HDF file to read image variables out of.
             gridWindow: Obj_New(), $        ; The gridWindow object.
             splashWindow: Obj_New(), $      ; The object identifier of the splash screen draw widget.
             nextButton: Obj_New(), $        ; The object identifier of the NEXT button.
             prevButton: Obj_New(), $        ; The object identifier of the PREVIOUS button.
             imageNamesOn: 0L, $             ; A flag to indicate image names are on.
             scaling_values: Ptr_New(), $    ; A pointer to scaling values structure that applies to every image.
             xoffset: 0L, $                  ; X offset for sizing grid window.
             yoffset: 0L, $                  ; Y offset for sizing grid window.
             version: 0.0, $                 ; The current version number of the program.
             _statusbar:Obj_New()}           ; The program's status bar.

END


;*****************************************************************************************************
;
; NAME:
;       DATAVIEWER Program
;
; PURPOSE:
;
;       This is the driver program for the DataViewer.
;*****************************************************************************************************
PRO DataViewer, files

   Catch, theError
   IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = ERROR_MESSAGE()
        RETURN
   ENDIF

   ; Set the main IDL directory to this directory.
   IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
   CD, ProgramRootDir(ONEUP=oneup)

   ; Parse the configuration file.
   DataViewer_Parse_Configuration
   xsize = CatGetDefault('DATAVIEWER_GRIDWINDOW_XSIZE')
   ysize = CatGetDefault('DATAVIEWER_GRIDWINDOW_YSIZE')

   ; If the configuration file in the default file is something different than
   ; itself, you will have to now parse the real configuration file.
   config_file = CatGetDefault('DATAVIEWER_CONFIG_FILE', SUCCESS=success)
   IF success THEN BEGIN
        IF StrUpCase(File_BaseName(config_file, '.txt')) NE 'DATAVIEWER_DEFAULT' THEN BEGIN

           ; Is this a relative file name or a complete file name?
           IF StrUpCase(File_BaseName(config_file)) EQ StrUpCase(config_file) THEN BEGIN
               IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
               config_file = Filepath(ROOT_DIR=ProgramRootDir(ONEUP=oneup), $
                  SUBDIR='config', config_file)
           ENDIF
           IF File_Test(config_file, /READ) EQ 0 THEN $
                Message, 'Configuration file specified in "dataviewer_default.txt" cannot be found.'
            DataViewer_Parse_Configuration, config_file
        ENDIF ELSE BEGIN

            ; This may not be a fully-qualified path to the Configuration file. Make
            ; sure it is.
            ;
           ; Is this a relative file name or a complete file name?
           IF StrUpCase(File_BaseName(config_file)) EQ StrUpCase(config_file) THEN BEGIN
               IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
               config_file = Filepath(ROOT_DIR=ProgramRootDir(ONEUP=oneup), $
                  SUBDIR='config', config_file)
           ENDIF
        ENDELSE
   ENDIF ELSE BEGIN

        ; Look in the config directory for files. If the user can't find any, we
        ; are done.
        IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
        configPath = Filepath(ROOT_DIR=ProgramRootDir(ONEUP=oneup), SUBDIR='config', "")
        config_file = Dialog_Pickfile(PATH=configPath, TITLE='Select Configuration File...', $
            FILTER='*.txt')
        IF config_file EQ "" THEN Message, 'Cannot continue with DataViewer Configuration File.'
   ENDELSE

   ; Check to be sure the data directory has a file separator at the end of the directory
   ; name. If it doesn't, the path is indeterminate.
   dataDir = CatGetDefault('DATAVIEWER_DATA_DIRECTORY', SUCCESS=success)
   IF StrLowCase(dataDir) EQ 'default' THEN BEGIN
       IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
       dataDir = FilePath(ROOT_DIR=ProgramRootDir(ONEUP=oneup), SUBDIR='data', "")
   ENDIF
   IF StrLowCase(dataDir) EQ 'data' THEN BEGIN
       IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
       dataDir = FilePath(ROOT_DIR=ProgramRootDir(ONEUP=oneup), $
           SUBDIR=dataDir, "")
   ENDIF
   IF success THEN BEGIN
        lastChar = StrMid(dataDir, 0, 1, /REVERSE)
        IF lastChar NE '/' AND lastChar NE '\' THEN BEGIN
            dataDir = dataDir + Path_Sep()
            CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', dataDir
        ENDIF

        ; Make sure the file separators are appropriate for the machine
        ; we are running on. Byte('\') = 92, Byte('/') = 47.
        CASE Path_Sep() OF
            '/': BEGIN ; UNIX
                 thisArray = Byte(dataDir)
                 i = Where(thisArray EQ 92, count)
                 IF count GT 0 THEN BEGIN
                    thisArray[i] = Byte('/')
                    dataDir = String(thisArray)
                    CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', dataDir
                 ENDIF
                 END

            '\': BEGIN ; Windows
                 thisArray = Byte(dataDir)
                 i = Where(thisArray EQ 47, count)
                 IF count GT 0 THEN BEGIN
                    thisArray[i] = Byte('\')
                    dataDir = String(thisArray)
                    CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', dataDir
                 ENDIF
                 END

             ELSE: ; Strange machine. Everyone for himself...

        ENDCASE
   ENDIF

   ; Set the data directory.
   CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', dataDir

    ; Create the widgets that make up the application. Widgets that generate
   ; events should be named explicitly and should be children (or grandchildren)
   ; of the TLB.
   tlb = OBJ_NEW('DataViewer', Column=1, MBar=menubarID, /Kill_Notify, $
      Name='TLB', SIZE_EVENTS=1)
   tlb -> GUI, menubarID


   ; Make sure the data directory actually exists. Otherwise, ask the user to
   ; select one.
   test = File_Test(dataDir, /DIRECTORY)
   IF test EQ 0 THEN BEGIN

      IF (StrUpCase(dataDir) EQ 'DEFAULT' + Path_Sep()) THEN dataDir = ProgramRootDir(ONEUP=oneup)
      answer = Dialog_Message(['The DataViewer needs to know where your images are located. ', $
                               'The current Data Directory is set to a currently non-existent directory: ', $
                               ' ', $
                               dataDir,  $
                               ' ', $
                               'Would you like to select a different Data Directory now and make', $
                               'this new selection be the default Data Directory?'], QUESTION=1, CENTER=1, $
                               TITLE='Select the DataViewer Data Directory')
      IF StrUpCase(StrMid(answer,0,1)) EQ 'Y' THEN BEGIN ; Answer is "yes".
          dataDir = Dialog_Pickfile(/DIRECTORY, TITLE='Select the Data Directory...')
          IF dataDir NE '' THEN BEGIN
                CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', dataDir
                tlb -> Write_Config_File, config_file
                ok = Dialog_Message(['The current Data Directory has been set to', dataDir], CENTER=1)
          ENDIF ELSE RETURN
      ENDIF ELSE BEGIN ; Answer is "no".

         ; Answer is no, but the current directory doesn't exist. Get a directory that
         ; does  exist, i.e., the DataViewer directory.
         dataDir = FilePath(ROOT_DIR=ProgramRootDir(ONEUP=oneup),  "")
         CatSetDefault, 'DATAVIEWER_DATA_DIRECTORY', dataDir
         ok = Dialog_Message(['The current Data Directory has been set temporarily to', dataDir], CENTER=1)
      ENDELSE
   ENDIF

   arguments = Command_Line_Args(COUNT=argCount)
   If argCount NE 0 THEN files = argments
   IF N_Elements(files) NE 0 THEN tlb -> LoadFiles, files

END
;*****************************************************************************************************
