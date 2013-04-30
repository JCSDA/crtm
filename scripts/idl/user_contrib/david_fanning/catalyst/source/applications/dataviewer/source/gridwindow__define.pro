;*****************************************************************************************************
;+
; NAME:
;       GRIDWINDOW__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a draw widget window that can
;       draw its children in grids, similar to !P.MULTI.
;
; AUTHOR:
;
;       David W. Fanning, Ph.D
;       National Snow and Ice Data Center (NSIDC)
;       NSIDC/CIRES University of Colorado
;       Boulder, CO 80309
;       E-Mail: fanning@nsidc.org
;
;
; CATEGORY:
;
;       Objects.
;
; SYNTAX:
;
;       theObject = Obj_New("GRIDWINDOW")
;
; SUPERCLASSES:
;
;       SELECTABLEDRAWWIDGET
;       DRAWWIDGET
;       WIDGETATOM
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { GRIDWINDOW, $
;             INHERITS SELECTABLEDRAWWIDGET $
;             _cols: 0L, $              ; Number of columns in the grid.
;             _rows: 0L,  $             ; Number of the rows in the grid.
;             _order: 0L $              ; The order of plots. 0 - row order, 1 - column order.    
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 17 May 2008.
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
;+
; NAME:
;       GRIDWINDOW::ADD
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
PRO GridWindow::Add, object, _EXTRA=extraKeywords

   @cat_pro_error_handler
   
   ; Some things need to be added to the window that are NOT counted
   ; or assigned positions. List them here.
   exclusionList = ['CONTEXTMENUBASE']
   
   ; The position in the window depends on how many things are in the
   ; window already. The added object MUST have a POSITION keyword.
   i = Where(exclusionList EQ Obj_Class(object), excldCnt)
   IF excldCnt EQ 0 THEN BEGIN
       count = self -> Count()
       object -> SetProperty, POSITION=(*self._positions)[*, count]
   ENDIF
        
   IF OBJ_ISA(object, 'NSIDC_IMAGE') THEN BEGIN
   
       ; If this is NSIDC_IMAGE object, then we are going to set its position
       ; in the window and make it's colors correspond to the gridWindow colors.
       ; This is a little convoluted, so let me explain. I want the color object that
       ; is defined for the GridWindow to be able to communicate to the color objects
       ; of the images that colors have changed. But that doesn't affect the images, 
       ; because their color objects are independent of them. So, I also register the
       ; the GridWindow's interest in the image color objects, so that if they change
       ; for some reason, I can redisplay the image in the MessageHandler method of 
       ; GridWindow. In practice, it works well, although it seems like magic.
       self -> GetProperty, COLOR_OBJECT=colors
       IF Obj_Valid(colors) THEN colors -> GetProperty, COLORPALETTE=colorPalette
         
        ; Get the image object's color object. Set it up so that if
        ; I change the GridWindow color object, the image color objects
        ; hear about it. The object is stored in the UVALUE of it's own
        ; color object, so the color object can find who to draw immediately.
        object -> GetProperty, COLOR_OBJECT=imageColors, COLORCHANGEALLOWED=colorChangeAllowed
        IF colorChangeAllowed AND Obj_Valid(colors)THEN BEGIN
                colors -> RegisterForMessage, imageColors, 'COLORTOOL_TABLECHANGE'
                colors -> RegisterForMessage, imageColors, 'COLORTOOL_SETPROPERTY'
                imageColors -> SetProperty, COLORPALETTE=colorPalette, UVALUE=object
        ENDIF 
        
   ENDIF

   self -> SELECTABLEDRAWWIDGET::Add, object, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       GRIDWINDOW::CALCULATEPOSITIONS
;
; PURPOSE:
;
;       This method calculates the positions in the window, given a particular grid.
;
; SYNTAX:
;
;       theObject -> CalculatePositions
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
PRO GridWindow::CalculatePositions

   @cat_pro_error_handler

   ; How many positions to we have?
   count = self._cols * self._rows
   
   ; Make a pixmap to calculate positions.
   self -> GetProperty, XSIZE=xsize, YSIZE=ysize
   Window, XSIZE=xsize, YSIZE=ysize, /PIXMAP
   
   IF count EQ 1 THEN BEGIN
       positions = [0, 0, 1, 1]
   ENDIF ELSE BEGIN
       positions = FltArr(4, count)
       !P.MULTI=[0, self._cols, self._rows, 0, self._order]
       FOR j=0,count-1 DO BEGIN
              Plot, Findgen(11), XStyle=4, YStyle=4, /NoData, XMargin=[1,1], YMargin=[1,1]
              positions[*,j] = [!X.Window[0], !Y.Window[0], !X.Window[1], !Y.Window[1]]
       ENDFOR
       !P.MULTI = 0
   ENDELSE
   
   ; Delete the pixmap.
   WDelete, !D.Window
   
   IF Ptr_Valid(self._positions) THEN *self._positions = positions ELSE self._positions = Ptr_New(positions, /No_Copy)
   
   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       GRIDWINDOW::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the GRIDWINDOW object. A
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
PRO GridWindow::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Object Control Panel', _EXTRA=extraKeywords)

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.

   base = Obj_New('BASEWIDGET', cp, Column=1, Frame=1)

   ; Display the control panel if it created its own TLB.

   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       GRIDWINDOW::DRAW
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
;       _Extra:    Any keywords appropriate for the superclass DRAW method..
;
;-
;*****************************************************************************************************
PRO GridWindow::Draw, $
   BACKGROUND_COLOR=background_color, $
   ERASE_WINDOW=erase_window, $
   HOURGLASS=hourglass, $
   REQUESTER=requester, $
   TARGET_WINDOW=target_window, $
   TARGETS=targets, $
   _EXTRA=extraKeywords

; Special handling to make sure we reset !P.MULTI if an error occurs.
;############################ Start error handler. ############################

   COMPILE_OPT idl2, HIDDEN

   ; Get the ErrorLevel
   IF (Obj_IsA_Valid (self, 'CatAtom')) THEN errorLevel = self._errorLevel $
   ELSE errorLevel = CatGetDefault ('ErrorLevel', Success=ok)

   IF (errorLevel EQ 0) THEN errorLevel = 2 ; default is to pop up a dialog

   ; Set up the Catch error handler (unless catch is inhibited)
   IF (errorLevel LT 3) THEN Catch, theError $
   ELSE theError = 0

   ; If an error has occurred ...
   IF theError NE 0 THEN $
   BEGIN

      ; Cancel the error handler and set up the error handling to "throw" error
      CATCH, /Cancel
      
      ; Make sure !P.MULTI hasn't changed.
      IF N_Elements(currentMulti) NE 0 THEN !P.MULTI=currentMulti
      
      ; Destroy the progress bar if still here.
      IF Obj_Valid(progressBar) THEN progressBar -> Destroy

      ; If this is a standard CATATOM object method, report it's failed
      IF (Obj_IsA_Valid (self, 'CatAtom')) THEN self -> Report, /Failed

      ; Get the call stack.
      HELP, Calls=callstack
      callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
      HELP, /Last_Message, Output=msg
      ON_ERROR, 2

      ; If the error has been previously handled, don't handle it here
      positions = StrPos(msg, '[cat_handled]')
      foundit = Where(positions NE -1, count)
      IF count GT 0 THEN $
      BEGIN
         msg[0] = '[cat_handled]'
         IF Routine_Names(/Level) GT 2 THEN MESSAGE, msg[0] ELSE RETURN
      END
      ; Report the error
      IF (Obj_IsA_Valid (self, 'CatAtom')) THEN self -> Error $
      ELSE CASE errorLevel OF

            1 : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                Print, ''
                FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
             END

            2 : BEGIN
                dialog_msg = TextLineFormat(msg[0])
                junk = Dialog_Message(dialog_msg)
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                Print, ''
                FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
              END

            ELSE : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                Print, ''
                FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
              END

          ENDCASE

      ; Throw the error, signalling that it's already been handled
      IF Routine_Names(/Level) GT 2 THEN MESSAGE, msg[0] + ' [cat_handled]' ELSE RETURN

   ENDIF

   ; If this is a standard CATATOM object method, report it's start
   IF (Obj_IsA_Valid (self, 'CatAtom')) THEN self -> Report, /Started

;############################ End error handler. ############################
   
  ; If required, call the superclass draw method to realize the widget
  ; NB: Call it with NO_CHILDREN set so it does not draw its contents yet
   WIDGET_CONTROL, self._id, GET_VALUE=windowID
   IF (windowID EQ -1) THEN $
   BEGIN
      self -> WidgetAtom::Draw, /NO_CHILDREN, _EXTRA=extraKeywords
      WIDGET_CONTROL, self._id, GET_VALUE=windowID
   ENDIF

   ; If drawing of contents has been inhibited, return immediately.
   IF (self._noDraw) THEN RETURN
   
   ; Make sure you are in DECOMPOSED=1 mode.
   IF (!D.Flags AND 256) NE 0 THEN BEGIN
      Device, Get_Decomposed=theState
      Device, DECOMPOSED=1
   ENDIF
   
   ; If we haven't got a valid window ID yet, issue error.
   IF (windowID EQ -1) THEN Message, 'Draw widget has not got a valid window ID.'
   
   ; Check keywords.
   IF N_Elements(backgroundColor) EQ 0 THEN backgroundColor = self._initialColor
      
   ; Enable the hourglass mouse cursor, if needed.
   IF Keyword_Set(hourglass) THEN WIDGET_CONTROL, /HOURGLASS

   IF (!D.FLAGS AND 256) NE 0 THEN BEGIN

      ; Switch to this window ready to draw
      IF Obj_Valid(target_window) THEN target_window -> SetWindow ELSE WSet, windowID

      ; Set the window.
      IF Obj_Valid(self._buffer_pixmap) THEN BEGIN
         self._buffer_pixmap -> Refresh
         self._buffer_pixmap -> SetWindow
      ENDIF ELSE self -> SetWindow
 
   ENDIF

   ; If you have a colors object, draw it now.
   self -> ApplyColors

   ; If you have a coordinates object, draw it now.
   self -> ApplyCoords

   ; If there are targets, draw just the targets, not everything.
   IF N_Elements(targets) NE 0 THEN BEGIN

      contents = self -> Get(/All, /Recursive_Search)

      FOR j=0,N_Elements(targets)-1 DO BEGIN

         ; If you can find the target in the list of objects, then draw it.
         i = Where(contents EQ targets[j], count)
         IF count GT 0 THEN targets[j] -> Draw

      ENDFOR

   ENDIF ELSE BEGIN

      ; Call the children one at a time and draw them. 
      children = self -> Get(/ALL, COUNT=count)
      
      ; Have to do something else if we are in PostScript mode.
      IF (!D.FLAGS AND 256) NE 0 THEN BEGIN
         IF Obj_Valid(self._buffer_pixmap) THEN $
            self._buffer_pixmap -> SetWindow ELSE $
            self -> SetWindow
         IF count GT 1 THEN progressBar = Obj_New("cgProgressBar", /START, $
            TEXT='Updating images...')
         FOR j=0,count-1 DO BEGIN
               children[j] -> Draw
               IF Obj_Valid(progressBar) THEN progressBar -> Update, Float(j)/count*100
         ENDFOR
      ENDIF ELSE BEGIN
         FOR j=0,count-1 DO children[j] -> Draw
      ENDELSE
    ENDELSE
   
   ; Finished drawing. 
   
   IF (!D.FLAGS AND 256) NE 0 THEN BEGIN
      IF Obj_Valid(self._buffer_pixmap) THEN BEGIN
         self -> SetWindow
         self._buffer_pixmap -> Copy, /ERASE_WINDOW
      ENDIF
      IF Obj_Valid(progressBar) THEN progressBar -> Destroy
   
      ; Send a message that you have drawn something. Include the requester of the DRAW, if available.
      self -> SendMessage, 'DRAWWIDGET_DRAW', DATA=requester
  ENDIF
  
  ; Restore graphics state.
  IF (!D.Flags AND 256) NE 0 THEN Device, DECOMPOSED=theState
   
  self -> Report, /Completed
   
END





;*****************************************************************************************************
;+
; NAME:
;        GRIDWINDOW::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the GRIDWINDOW object. It will typically
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
PRO GridWindow::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler
   
   eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', $
                 'KEYPRESS_CH', 'KEYPRESS_KEY', 'MOUSE_SCROLL']
   
   thisEvent = eventTypes[event.type]
   
   CASE thisEvent OF
   
      'DOWN': BEGIN
          
         ; Only interested in LEFT mouse buttons here. If not LEFT, fall through.
         IF event.press EQ 1 THEN BEGIN
                  
            ; Did you click in a selectable object?
            objects = self -> SelectObjects(event.x, event.y, Count=count)
            allObjects = self -> Get(/ALL)
            
            ; If so, find the index of the object. If this is an image object,
            ; then you are going to move it, possibly.
            IF count GT 0 THEN BEGIN
               theObject = (Reverse(objects))[0]
               index = Where(allObjects EQ theObject, found)
               theObject -> GetProperty, LOCATION=location
               
               ; We only do something if we find an image object.
               IF found EQ 0 THEN RETURN
               
               ; Set up parameters that you will need to draw the image outline, etc.
               self.selectIndex = index[0]
               self.select_x = event.x
               self.select_y = event.y
               self.selectLocation = location[0:3,0]
                  
               ; Turn motion events on for the draw widget. And make
               ; a pixmap for erasing the box as it is drawn. No need to do this
               ; if we have a single image in the window.
               IF (self._cols GT 1) OR (self._rows GT 1) THEN BEGIN
                   self -> SetProperty, MOTION_EVENTS=1
                   self._movingObject = 1
                   self -> GetProperty, XSize=xsize, YSize=ysize
                   IF Obj_Valid(self._buffer_pixmap) THEN $
                         self._buffer_pixmap -> SetProperty, XSIZE=xsize, YSIZE=ysize ELSE $
                         self._buffer_pixmap = Obj_New('PixmapWidget', XSize=xsize,  YSize=ysize)
                   self._buffer_pixmap -> SetWindow
                   self -> Copy, ORIGIN=[0,0], EXTENT=[xsize, ysize], DESTINATION=[0,0]
               ENDIF
            ENDIF
         ENDIF
         END
         
      'MOTION': BEGIN
      
         IF self._movingObject THEN BEGIN
      
             ; Erase the old box by copying the pixmap.
             self -> SetWindow
             self -> GetProperty, XSize=xsize, YSize=ysize
             self._buffer_pixmap -> Copy, ORIGIN=[0,0], EXTENT=[xsize, ysize], DESTINATION=[0,0]
             
             ; Draw a new box.
             loc = self.selectLocation
             xmove = (self.select_x - event.x)
             ymove = (self.select_y - event.y)
             
             ; Don't want to move for jittery mouse clicks, only if something is
             ; really moving.
             IF (Abs(xmove) GT 3) OR (Abs(ymove) GT 3 ) THEN BEGIN
                 loc[[0,2]] = loc[[0,2]] - xmove
                 loc[[1,3]] = loc[[1,3]] - ymove
                 PLOTS, [loc[0], loc[0], loc[2], loc[2], loc[0]], $
                        [loc[1], loc[3], loc[3], loc[1], loc[1]], /DEVICE, $
                        COLOR=cgColor('indian red'), THICK=2
             ENDIF
             RETURN
             
         ENDIF ; Else motion events pass through and are handled by the parent's event handler.
         END
         
      'UP': BEGIN
      
         ; Only interested in LEFT mouse events here.
         IF event.release EQ 1 THEN BEGIN
         
            ; Motion events off and clear any events in the queue.
            ;self -> SetProperty, MOTION_EVENTS=0, /CLEAR_EVENTS
            self._movingObject = 0
            
            ; Repair the window. 
            self -> SetWindow
            self -> GetProperty, XSize=xsize, YSize=ysize
            self._buffer_pixmap -> Copy, ORIGIN=[0,0], EXTENT=[xsize, ysize], DESTINATION=[0,0]
            
            ; If the grid is a 1-by-1 (a single image), don't bother doing anything,
            ; since a single image can't be moved.
            IF (self._cols EQ 1) AND (self._rows EQ 1) THEN RETURN
            
            ; What index did the user release the button in?
            upIndex = self -> GetGridIndex(event.x, event.y)
            
            ; If you are in a different grid in the window from when you
            ; clicked the button down, then you have to move the image.
            IF (upIndex NE self.selectIndex) AND (self.selectIndex GE 0) THEN BEGIN
            
               ; Make sure you are not in a sector that doesn't contain an image.
               imageCount = self -> Count()
               upIndex = 0 > upIndex < (imageCount-1)
               
               ; Move the image in the previous selection to this new position.
               self -> Move, self.selectIndex, upIndex
               
               ; Nothing would change visually now, unless you re-calculate
               ; the positions of all the images in the window. Do that now.
               allObjects = self -> Get(/All, ISA='CATImage', COUNT=count)
               pos = *self._positions
               FOR j=0,count-1 DO BEGIN
                  allobjects[j] -> SetProperty, POSITION=pos[*,j]
               ENDFOR
   
               ; Redraw all the images in their new positions.
               self -> Draw
               
               ; Send a message to anyone listening that you have moved an image.
               self -> SendMessage, 'GRIDWINDOW_MOVE_SELECTION', DATA={movedFrom:self.selectIndex, movedTo:upIndex}
               
            ENDIF
            
            ; Reset all the internal parameters that help us move an image.
            self.selectIndex = -1
            self.select_x = -1
            self.select_y = -1
            self.selectLocation = FltArr(4)
            
            ; Don't pass along this event.
            RETURN
         ENDIF
         END
   
      ELSE: ; Most mouse events fall through
      
   ENDCASE

   ; Call the superclass event handler, where most events are handled.
   self -> SELECTABLEDRAWWIDGET::EventHandler, event

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       GRIDWINDOW::GETGRIDINDEX
;
; PURPOSE:
;
;       This method allows the user to obtain 1D index number of the grid. That is,
;       a 1D index into a 2D array.
;
; SYNTAX:
;
;       index = theObject -> GetGridIndex(xIn, yIn)
;
; ARGUMENTS:
;
;     xIn:      The X location in the grid window (in device coordinates).
;     
;     yIn:      The Y location in the grid window (in device coordinates).
;
; KEYWORDS:
;
;     None.
;*****************************************************************************************************
FUNCTION GridWindow::GetGridIndex, xin, yin

   @cat_func_error_handler
   
   ; Input parameters are required.
   IF N_Params() NE 2 THEN Message, 'Location in window is required.'
   
   ; Convert the input parameters to normalized coordinates.
   self -> GetProperty, XSIZE=xsize, YSIZE=ysize
   xloc = Float(xin) / xsize
   yloc = Float(yin) / ysize
   
   ; Set up the grid vectors. We will located the input locations in these vectors.
   xstep = 1.0 / self._cols
   x = Findgen(self._cols) * xstep
   
   ystep = 1.0 / self._rows
   y = Findgen(self._rows) * ystep
   
   ; Locate the input parameters in these vectors. Y has to be reversed.
   ; Note that Value_Locate cannot work with a 1-element array.
   IF N_Elements(x) EQ 1 THEN xindex = 0 ELSE xindex = Value_Locate(x, xloc)
   IF N_Elements(y) EQ 1 THEN yindex = 0 ELSE yindex = (self._rows -1) - Value_Locate(y, yloc)
   
   ; Calculate the 1D index from the 2D indices.
   IF self._order EQ 0 THEN BEGIN ; Row order
      index = yindex * self._cols + xindex
   ENDIF ELSE BEGIN ; Column order
      index = xindex * self._rows + yindex
   ENDELSE
   
   ; Make sure this is inside the window.
   index = 0 > index < ((self._cols * self._rows)-1)
   
   RETURN, index
   
END



;*****************************************************************************************************
;+
; NAME:
;       GRIDWINDOW::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain GRIDWINDOW properties. Be sure
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
;     GRID:           A two element array with the grid as [cols, rows].
;
;     ORDER:          The current order value.
;
;     POSITIONS:      The positions of the images in the grid.
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO GridWindow::GetProperty, GRID=grid, ORDER=order, POSITIONS=positions, _REF_EXTRA=extraKeywords

   @cat_pro_error_handler
   
   IF Arg_Present(grid) THEN grid = [self._cols, self._rows]
   IF Arg_Present(order) THEN order = self._order
   IF Arg_Present(positions) THEN positions = *self._positions

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEDRAWWIDGET::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



PRO GridWindow::MessageHandler, title, SENDER=sender, DATA=data

   ; Initialise the error handler
   @cat_pro_error_handler

   CASE title OF

      'COLORTOOL_SETPROPERTY': IF sender EQ self._colors THEN self -> Draw

      'COLORTOOL_TABLECHANGE': IF sender EQ self._colors THEN self -> Draw
 
      ELSE: self -> SELECTABLEDRAWWIDGET::MessageHandler, title, SENDER=sender, DATA=data

   ENDCASE

   self -> Report, /Completed

END


PRO GridWindow::Refresh

      IF Obj_Valid(self._buffer_pixmap) THEN BEGIN
         self._buffer_pixmap -> Refresh
         self._buffer_pixmap -> SetWindow
      ENDIF ELSE self -> SetWindow

END



;*****************************************************************************************************
;+
; NAME:
;       GRIDWINDOW::RESIZE
;
; PURPOSE:
;
;       This method resizes the canvas area of the draw widget.
;;
; SYNTAX:
;
;       thisDrawObj -> Resize, xsize, ysize
;
; ARGUMENTS:
;
;       XSIZE:    The new X size of the canvas area of the draw widget, in pixels.
;
;       YSIZE:    The new Y size of the canvas area of the draw widget, in pixels.
;
; KEYWORDS:
;
;       DRAW:     Set this keyword to call the draw method when the draw widget
;                 resizing is completed.
;
;       SCREEN:   Normally, the XSIZE and YSIZE keywords apply to the draw widget canvas.
;                 If the SCREEN keyword is set, the keywords apply to the screen coordinates
;                 of the draw widget. (It's actual size on the display. Usually about 6 pixels
;                 larger than the canvas.)
;
;       VIEWPORT: Normally, the XSIZE and YSIZE keywords apply to the draw widget canvas.
;                 If the VIEWPORT keyword is set, the keywords apply to the viewport size
;                 of the draw widget.
;
;       _EXTRA:   Any extra keywords appropriate for the DRAW method.
;-
;*****************************************************************************************************
PRO GridWindow::Resize, xsize, ysize, DRAW=draw, SCREEN=screen, VIEWPORT=viewport, _EXTRA=extraKeywords

   @cat_pro_error_handler
   
   self -> SELECTABLEDRAWWIDGET::RESIZE, xsize, ysize, SCREEN=screen, VIEWPORT=viewport, _EXTRA=extraKeywords
   
   ; Be sure to recalculate positions in the window.
   self -> CalculatePositions
   
   IF Keyword_Set(draw) THEN self -> Draw
   
   self -> Report, /Completed
END





;*****************************************************************************************************
;+
; NAME:
;       GRIDWINDOW::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the GRIDWINDOW object's properties. Be sure
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
;     GRID:      A two element array with the grid as [cols, rows].
;
;     ORDER:     Set the grid order value to 0 (row ordered) or 1 (column ordered).
;
;     _EXTRA:     Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO GridWindow::SetProperty, GRID=grid, ORDER=order, _EXTRA=extraKeywords

   @cat_pro_error_handler
   
   IF N_Elements(grid) NE 0 THEN BEGIN
      CASE N_Elements(grid) OF
            1: BEGIN
                 self._cols = grid
                 self._rows = grid
               END
            2: BEGIN
                 self._cols = grid[0]
                 self._rows = grid[1]
               END
            ELSE: Message, 'The grid can only be 2D.'
      ENDCASE
      self -> CalculatePositions
   ENDIF
   
   IF N_Elements(order) NE 0 THEN self._order = Keyword_Set(order)

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEDRAWWIDGET::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;PRO GridWindow::XColors_Notification, _EXTRA=extra
;    TVLCT, r, g, b, /GET
;    self -> GetProperty, COLOR_OBJECT=colors
;    colors -> SetProperty, RED=r, GREEN=g, BLUE=b
;END


PRO GridWindow::XStretch_Notification, data

END

;*****************************************************************************************************
;+
; NAME:
;       GRIDWINDOW::CLEANUP
;
; PURPOSE:
;
;       This is the GRIDWINDOW object class destructor method.
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
PRO GridWindow::CLEANUP

   @cat_pro_error_handler

   Ptr_Free, self._positions
  
   self -> SELECTABLEDRAWWIDGET::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       GRIDWINDOW::INIT
;
; PURPOSE:
;
;       This is the GRIDWINDOW object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     parent:     The parent object referece. This object will be added to the parent's container.
;
; KEYWORDS:
;
;    GRID:        A two-elements array, indicating the number of columns and rows in the window grid.
;
;     _EXTRA:     Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION GridWindow::INIT, parent, GRID=grid, _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   ok = self -> SELECTABLEDRAWWIDGET::INIT (parent, _EXTRA=extraKeywords, INITIAL_COLOR='ivory', /ERASE)
   IF ~ok THEN RETURN, 0
  
   ; Check properties.
   IF N_Elements(grid) EQ 0 THEN BEGIN
        grid = [1,1] 
   ENDIF ELSE BEGIN
        IF N_Elements(grid) EQ 1 THEN grid = [grid, grid]
        IF N_Elements(grid) NE 2 THEN Message, 'Specify GRID keyword as [column, row].'
   ENDELSE
   self._cols = grid[0]
   self._rows = grid[1]
   
   self -> CalculatePositions
   
   ; Create a pixmap buffer for buffering output.
   self -> GetProperty, XSIZE=xsize, YSIZE=ysize, INITIAL_COLOR=initial_color
   self._buffer_pixmap = Obj_New('PixmapWidget',  XSIZE=xsize, YSIZE=ysize, $
       BACKGROUNDCOLOR=initial_color)
   self -> RegisterForMessage, self._buffer_pixmap, 'RESIZEDRAWWIDGET'
   
   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END

;*****************************************************************************************************
;
; NAME:
;       GRIDWINDOW TEST PROGRAM
;
; PURPOSE:
;
;       This is the test program for the GRIDWINDOW object. It may not
;       be required in your object.
;
;*****************************************************************************************************
PRO GridWindow_Test, window

   filename = 'C:\IDL\DAAC_DataViewer\data\SMMR_EASE\EASE-SMMR-NL1987002A.06H.gz'
   window = Obj_New('GridWindow', GRID=[3,2], XSIZE=800, YSIZE=500)
   window -> Add, Obj_New('NSIDC_IMAGE', filename)
   window -> Add, Obj_New('NSIDC_IMAGE', filename)
   window -> Add, Obj_New('NSIDC_IMAGE', filename)
   window -> Add, Obj_New('NSIDC_IMAGE', filename)
   window -> Add, Obj_New('NSIDC_IMAGE', filename)
   window -> Add, Obj_New('NSIDC_IMAGE', filename)
   window -> Draw
   

END


PRO XCOLORS_NOTIFYOBJ__DEFINE
   struct = {XCOLORS_NOTIFYOBJ, OBJECT:Obj_New(), METHOD:''}
END

;*****************************************************************************************************
;
; NAME:
;       GRIDWINDOW CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the GRIDWINDOW object.
;
;*****************************************************************************************************
PRO GridWindow__DEFINE, class

   class = { GRIDWINDOW, $
             INHERITS SELECTABLEDRAWWIDGET, $
              _cols: 0L, $              ; Number of columns in the grid.
              _rows: 0L,  $             ; Number of the rows in the grid.
              _order: 0L, $             ; The order of plots. 0 - row order, 1 - column order.   
              _positions: Ptr_New(), $  ; A pointer to the positions in the grid. 
              _movingObject: 0L, $      ; A flag to indicate we are moving an object in the window.
              selectLocation: FltArr(4), $
              selectIndex: 0L, $
              select_x: 0L, $
              select_y: 0L $
           }

END
