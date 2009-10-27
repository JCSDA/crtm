;+
; NAME:
;       XWINDOW
;
; PURPOSE:
;       This routine implements a "smart" resizeable graphics window.
;       It is used as a wrapper for built-in IDL graphics procedures
;       such as SURFACE, CONTOUR, PLOT, SHADE_SURF, etc. In additon,
;       it can be used to display any user-written graphics procedure
;       so long as that procedure follows three simple rules: (1) It
;       does not open it's own graphics windows, (2) It is defined with
;       no more than ten positional arguments (an unlimited number
;       of keyword arguments are allowed), and (3) It is defined
;       with an _EXTRA keyword.
;
;       Keyword arguments permit the window to have its own portion
;       of a color table and to be able to change the colors loaded in
;       that portion of the color table. Colors are updated
;       automatically on both 8-bit and 24-bit color displays. In
;       addition, the window colors can "protect" themselves. I mean
;       by this that the window can re-load its own colors into the
;       color table when the cursor is moved over the window. This
;       prevents other applications from changing the colors used to
;       display data in this window. (This is an issue mainly in
;       IDL 5.x applications where widget applications can run
;       concurrently with commands from the IDL command line.)
;
;       Keyword arguments also permit the window to create output
;       files of its contents. These files can be color and
;       gray-scale PostScript, GIF, TIFF, or JPEG files.
;
; AUTHOR:
;       ************* CM 19 Jan 1999 VERSION **********
;       Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;       craigm@lheamail.gsfc.nasa.gov
;
;       Originally by:
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       2642 Bradbury Court
;       Fort Collins, CO 80521 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;       Widgets, Graphics.
;
; CALLING SEQUENCE:
;       XWINDOW, command, P1, P2, ... , keywords=..., ...
;
; REQUIRED INPUTS:
;       COMMAND: The graphics procedure command to be executed. This parameter
;       must be a STRING. Examples are 'SURFACE', 'CONTOUR', 'PLOT', etc.
;
; OPTIONAL INPUTS:
;       Pn: A positional parameter appropriate for the graphics command.
;           Any number of parameters between 0 and 10 may be given.
;
; INPUT KEYWORD PARAMETERS:
;       CPMENU: Setting this keyword adds a "Color Protection" button to the
;       "Controls" menu. Color protection can then be turned ON or OFF for the
;       window. Otherwise, the color protection scheme used to open the window
;       cannot be changed once the window is open. (See the PROTECT keyword.)
;       The default is to have this keyword OFF.
;
;       ERASE: Setting this keyword "erases" the contents of the current
;       graphics window before re-executing the graphics command. For example,
;       this keyword might need to be set if the graphics "command" is TVSCL.
;       The default is to NOT erase the display before reissuing the graphics
;       command.
;
;       _EXTRA: This keyword forms an anonymous structure of any unrecognized
;       keywords passed to the program. The keywords must be appropriate
;       for the graphics command being executed.
;
;       GROUP_LEADER: The group leader for this program. When the group leader
;       is destroyed, this program will be destroyed.
;
;       OUTPUT: Set this keyword if you want a "File Output" menu on
;       the menu bar. The default is to have this item turned OFF.
;
;       JUST_REGISTER: If this keyword is set, the XWINDOW program is just
;       registered with XMANAGER, but XMANAGER doesn't run. This is
;       useful, for example, if you want to open an XWINDOW window in
;       the widget definition module of another widget program.
;
;       NO_CHANGE_CONFIG: Normally as the XWINDOW graphics window is resized
;       the size (or aspect ratio, in the case of PostScript) of the
;       hardware configuration dialogs change to reflect the new size of
;       the graphics window. This results in file output that resembles
;       the current graphics window in size and aspect ratio. If you want
;       the file output dialogs to remember their current configuration
;       even as the window is resized, set this keyword.
;
;       NOMENU: Setting this keyword results in a graphics window without
;       menu items. The default is to have a "Controls" menu item in the
;       window menu bar with a "Quit" button. Setting this keyword
;       automatically turns of the COLORS, OUTPUT, and CPMENU menu
;       choices. (Note that the values specified by the COLORS keyword
;       will still be valid for color protection, but no "Change Colors..."
;       menu item will appear.)
;
;       PROTECT: If this keyword is set, color protection for the draw
;       widget is turned ON. What this means is that the window colors
;       (see the XCOLOR keyword) will be restored when the cursor enters
;       the draw widget window. This prevents someone at the IDL command
;       line in IDL 5.0 from changing the window display colors permanently.
;
;       WTITLE: This is the window title. It is the string "Resizeable
;       COMMAND Window (1)" by default, where COMMAND is the input
;       parameter. And the number (1 in this case) is the window
;       index number of the draw widget.
;
;       WXPOS: This is the initial X offset of the window. Default is to
;       position the window in the approximate middle of the display.
;
;       WYPOS: This is the initial Y offset of the window. Default is to
;       position the window in the approximate middle of the display.
;
;       WXSIZE: This is the initial X size of the window. Default is 400
;       pixels.
;
;       WYSIZE: This is the initial Y size of the window. Default is 400
;       pixels.
;
;       XCOLORS: Using this keyword adds a "Change Colors..." button to the
;       "Controls" menu. Set this keyword to the number of colors available
;       in the window and the starting index of the first color. For example,
;       to allow the window access to 100 colors, starting at color index 50
;       (i.e., color indices 50 to 149), use XColor=[100, 50]. If you use the
;       keyword syntax "/XColor", all the colors available will be used, not just
;       one color. If the keyword is set to a scalar value greater than 1, the
;       starting color index is set to 0. The default value for this keyword
;       is [(!D.N_COLORS < 256), 0]. Note that color "protection" may be
;       turned on (via the PROTECT keyword) even if this keyword is NOT used.
;
; OUTPUT KEYWORD PARAMETERS:
;       DRAWID: This keyword returns the draw widget identifier of the draw
;       widget created in XWINDOW.
;
;       TOP: This keyword returns the identifier of the top-level base widget
;       created by XWINDOW.
;
;       WID: This keyword returns the window index number of the draw widget
;       created in XWINDOW.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       If color protection is ON, the window colors are reloaded when the
;       cursor enters the XWINDOW graphics windows.
;
; RESTRICTIONS: This program requires three additional programs from
;       the Fanning Software Consulting library: PSWINDOW, PS_FORM (CM
;       version; available at
;       http://astrog.physics.wisc.edu/~craigm/idl), and XCOLORS. You
;       might also want to get the program TVIMAGE if you will be
;       displaying images in XWINDOW graphics windows.
;
;       If the "command" program requires keywords that are also keywords
;       to XWINDOW, then you must use the keyword twice on the command line.
;
; EXAMPLE:
;       To display a surface in the window, type:
;
;       XWINDOW, 'SURFACE', Dist(20), Charsize=1.5
;
;       To enable the Change Colors and File Output menu items, type:
;
;       XWINDOW, 'SHADE_SURF', Dist(30), /XColors, /Output
;
; MODIFICATION HISTORY:
;       Written by: David Fanning, October 96.
;       XSIZE and YSIZE keywords changed to WXSIZE and WYSIZE so as not to
;          conflict with these keywords on other programs. 14 April 1997, DWF.
;        Updated as non-blocking widget for IDL 5.0. 14 April 1997, DWF.
;        Extensively modified to work on either 8-bit or 24-bit displays,
;          to enable color protection schemes, to send the contents to a
;          number of different output files, and to give the user choices
;          about which menu items to enable. 21 April 1997, DWF.
;        Renamed COLORS keyword to XCOLORS and fixed a problem cancelling
;           out of File Configuration dialogs. 23 April 1997, DWF.
;        Modification: Craig Markwardt, 21 October 1997
;           Added capability for up to ten positional parameters
;        Modification: CM 15 May 1998
;           PS_FORM dependencies are not hardcoded now.  Requires
;           CM version of PS_FORM function.
;        Modification: CM 19 Jan 1999
;           Added Parent keyword to widget invocation of PS_FORM, and
;           make widgets MODAL-safe.
;-

PRO NULL_EVENTS, event
END ; of NULL_EVENTS event handler *****************************************



FUNCTION XWINDOW_ALERT, message, XOffSet=xoff, YOffSet=yoff

   ; Put up a message box

IF N_PARAMS() EQ 0 THEN message = 'Please wait...'
Device, Get_Screen_Size=screenSize
IF N_ELEMENTS(xoff) EQ 0 THEN xoff = (screenSize(0)/2.0 - 100)
IF N_ELEMENTS(yoff) EQ 0 THEN yoff = (screenSize(1)/2.0 - 75)

tlb = Widget_Base(Title='Writing a File...', XOffSet=xoff, YOffSet=yoff)
label = Widget_Label(tlb, Value=message)
Widget_Control, tlb, /Realize
RETURN, tlb
END ;*******************************************************************



PRO XWINDOW_COLOR_PROTECTION, event
WIDGET_CONTROL, event.top, GET_UVALUE=info, /NO_COPY
WIDGET_CONTROL, event.id, GET_UVALUE=buttonValue
CASE buttonValue OF
   'ON':  BEGIN
          info.protect = 1
          WIDGET_CONTROL, info.cprotectOFF, Sensitive=1
          WIDGET_CONTROL, info.cprotectON, Sensitive=0
          WIDGET_CONTROL, info.drawID, Tracking_Events=1
          END
   'OFF': BEGIN
          info.protect = 0
          WIDGET_CONTROL, info.cprotectOFF, Sensitive=0
          WIDGET_CONTROL, info.cprotectON, Sensitive=1
          WIDGET_CONTROL, info.drawID, Tracking_Events=0
          END
ENDCASE
WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END ;*******************************************************************



PRO XWINDOW_CONFIGURATION_EVENTS, event

WIDGET_CONTROL, event.top, GET_UVALUE=info, /NO_COPY
WIDGET_CONTROL, event.id, GET_UVALUE=thisEvent
CASE thisEvent OF

   'SELECT_FILE': BEGIN

         ; Start in the current directory.

      CD, Current=startDirectory

         ; Use PICKFILE to pick a filename for writing.

      pick = Pickfile(Path=startDirectory, /NoConfirm, $
         Get_Path=path, /Write)

         ; Make sure the user didn't cancel out of PICKFILE.

      IF pick NE '' THEN Widget_Control, info.filenameID, Set_Value=pick
      END ; of the Select Filename button case

    'CANCEL': BEGIN

         ; Have to exit here gracefully. Set CANCEL field in structure.

       formdata = {cancel:1, create:0}
       Handle_Value, info.ptr, formdata, /Set

         ; Out of here!

       Widget_Control, event.top, /Destroy
       RETURN
       END ; of the Cancel button case

    'ACCEPT': BEGIN  ; Gather the form information.

          ; Get the filename.

       Widget_Control, info.filenameID, Get_Value=filename

       filename = filename(0)

          ; Get the size info.

       Widget_Control, info.xsizeID, Get_Value=xsize
       Widget_Control, info.ysizeID, Get_Value=ysize

          ; Get the color info from the droplist widget.

       listIndex = Widget_Info(info.colordropID, /Droplist_Select)
       colortype = FIX(ABS(1-listindex))

          ; Get the order info from the droplist widget.

       order = Widget_Info(info.orderdropID, /Droplist_Select)
       order = FIX(order)

          ; Get the quality fromt he slider widget, if needed

       IF info.sliderID NE -1 THEN $
          Widget_Control, info.sliderID, Get_Value=quality ELSE quality=-1

          ; Create the formdata structure from the information you collected.

       formdata = {filename:filename, xsize:xsize, ysize:ysize, $
          color:colortype, order:order, quality:quality, create:0}

          ; Store the formdata in the pointer location.

       Handle_Value, info.ptr, formdata, /Set

         ; Out of here!

      Widget_Control, event.top, /Destroy
      RETURN
      END ; of the Accept button case

    'CREATE': BEGIN  ; Gather the form information.

          ; Get the filename.

       Widget_Control, info.filenameID, Get_Value=filename

       filename = filename(0)

          ; Get the size info.

       Widget_Control, info.xsizeID, Get_Value=xsize
       Widget_Control, info.ysizeID, Get_Value=ysize

          ; Get the color info from the droplist widget.

       listIndex = Widget_Info(info.colordropID, /Droplist_Select)
       colortype = FIX(ABS(1-listindex))

          ; Get the order info from the droplist widget.

       order = Widget_Info(info.orderdropID, /Droplist_Select)
       order = FIX(order)

          ; Get the quality fromt he slider widget, if needed

       IF info.sliderID NE -1 THEN $
          Widget_Control, info.sliderID, Get_Value=quality ELSE quality=-1

          ; Create the formdata structure from the information you collected.

       formdata = {filename:filename, xsize:xsize, ysize:ysize, $
          color:colortype, order:order, quality:quality, create:1}

          ; Store the formdata in the pointer location.

       Handle_Value, info.ptr, formdata, /Set

         ; Out of here!

      Widget_Control, event.top, /Destroy
      RETURN
      END ; of the Create button case

   ELSE:
ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END ; of XWINDOW_CONFIGURATION_EVENTS event handler ************************



FUNCTION XWINDOW_CONFIGURATION, filetype, config, TITLE=title, $
   XOFFSET=xoffset, YOFFSET=yoffset, Cancel=cancel, Create=create, $
   PARENT=parent

CATCH, error
IF error NE 0 THEN BEGIN
ok = WIDGET_MESSAGE(!Err_String)
RETURN, -1
ENDIF


IF N_ELEMENTS(filetype) EQ 0 THEN filetype = 'GIF'
IF N_ELEMENTS(config) EQ 0 THEN config = {XSIZE:400, YSIZE:400, $
   COLOR:1, FILENAME:'xwindow.gif', NCOLORS:(!D.N_Colors < 256)}
filetype = STRUPCASE(filetype)
IF N_ELEMENTS(title) EQ 0 THEN title = 'Configure ' + $
   filetype + ' Output File'

   ; Check for placement offsets. Define defaults.

IF (N_ELEMENTS(xoffset) EQ 0) THEN BEGIN
   DEVICE, GET_SCREEN_SIZE=screenSize
   xoffset = (screenSize(0) - 200) / 2.
ENDIF
IF (N_ELEMENTS(yoffset) EQ 0) THEN BEGIN
   DEVICE, GET_SCREEN_SIZE=screenSize
   yoffset = (screenSize(1) - 100) / 2.
ENDIF

   ; Create widgets.

thisRelease = StrMid(!Version.Release, 0, 1)
if thisRelease EQ '5' AND n_elements(parent) GT 0 THEN $
  extra_modal = { Modal: 1, Group_Leader:parent(0) }
tlb = WIDGET_BASE(Column=1, Title=title, XOffset=xoffset, $
   YOffset=yoffset, Base_Align_Center=1, _extra=extra_modal)

bigbox = WIDGET_BASE(tlb, Column=1, Frame=1, Base_Align_Center=1)

   ; Create the filename widgets.
filebox = Widget_Base(bigbox, Column=1, Base_Align_Center=1)
filename = config.filename
filenamebase = Widget_Base(filebox, Row=1)
   filenamelabel = Widget_Label(filenamebase, Value='Filename:')
   filenameID = Widget_Text(filenamebase, Value=filename, /Editable, $
      Event_Pro='NULL_EVENTS', SCR_XSIZE=320)

   ; Create a button to allow user to pick a filename.

pickbutton = Widget_Button(filebox, Value='Select Filename', $
   UVALUE='SELECT_FILE')

   ; Create size widgets
sizebox = Widget_Base(bigbox, Column=1, Base_Align_Left=1)
sizebase = Widget_Base(sizebox, Row=1)
xsizeID = CW_FIELD(sizebase, Value=config.xsize, Title='XSize: ', $
   /Integer)
ysizeID = CW_FIELD(sizebase, Value=config.ysize, Title='YSize: ', $
   /Integer)

   ; File type and order.

orderbase = Widget_Base(sizebox, Row=1)
type = ['Color', 'Grayscale']
order = ['0', '1']
colordropID = Widget_Droplist(orderbase, Value=type, $
   Title='File Type: ', EVENT_PRO='NULL_EVENTS')
orderdropID = Widget_Droplist(orderbase, Value=order, $
   Title='Display Order: ', EVENT_PRO='NULL_EVENTS')

Widget_Control, colordropID, Set_Droplist_Select=FIX(ABS(config.color-1))
Widget_Control, orderdropID, Set_Droplist_Select=config.order

   ; Quality Slider if needed.

IF filetype EQ 'JPEG' THEN $
   sliderID = Widget_Slider(bigbox, Value=config.quality, Max=100, Min=0, $
      Title='Compression Quality', EVENT_PRO='NULL_EVENTS', $
      SCR_XSize=350) ELSE sliderID = -1

   ; Cancel and Accept buttons.

buttonbase = Widget_Base(tlb, Row=1)
cancelID = Widget_Button(buttonbase, Value='Cancel', UValue='CANCEL')
createID = Widget_Button(buttonbase, Value='Create File', UValue='CREATE')
ok = Widget_Button(buttonbase, Value='Accept', UValue='ACCEPT')

Widget_Control, tlb, /Realize

ptr = HANDLE_CREATE()

info = { filenameID:filenameID, xsizeID:xsizeID, $
         ysizeID:ysizeID, colordropID:colordropID, $
         orderdropID:orderdropID, ptr:ptr, sliderID:sliderID}

Widget_Control, tlb, Set_UValue=info, /No_Copy
if thisRelease EQ '4' THEN xmanager_modal = {Modal:1}
XManager, 'xwindow_configuration', tlb, $
  Event_Handler='XWINDOW_CONFIGURATION_EVENTS', _EXTRA=xmanager_modal

Handle_Value, ptr, formdata
Handle_Free, ptr

IF N_ELEMENTS(formdata) EQ 0 THEN BEGIN
   cancel = 1
   create = 0
   RETURN, -1
ENDIF

fields = TAG_NAMES(formdata)
create = formdata.create
cancel = WHERE(fields EQ 'CANCEL')
IF cancel(0) EQ -1 THEN BEGIN
   cancel = 0
   newConfiguration = Create_Struct('XSIZE', formdata.xsize, $
      'YSIZE', formdata.ysize, 'COLOR', formdata.color, $
      'FILENAME', formdata.filename, 'ORDER', formdata.order, $
      'QUALITY', formdata.quality, NAME='XWINDOW_' + filetype)
   RETURN, newConfiguration
ENDIF ELSE BEGIN
   cancel = 1
   create = 0
   RETURN, -1
ENDELSE
END ; of XWINDOW_CONFIGURATION event handler *******************************



FUNCTION XWindow_WhatTypeVariable, variable

   ; Use SIZE function to get variable info.

varInfo = Size(variable)

   ; The next to last element in varInfo has the data type.

typeIndex = varInfo(varInfo(0) + 1)
dataTypes = ['UNDEFINED', 'BYTE', 'INTEGER', 'LONG', 'FLOATING', $
     'DOUBLE', 'COMPLEX', 'STRING', 'STRUCTURE', 'DCOMPLEX']
thisType = dataTypes(typeIndex)

 RETURN, thisType
 END ; of XWindow_WhatTypeVariable utility routine **************************



PRO XWINDOW_QUIT, event
WIDGET_CONTROL, event.top, /DESTROY
END ; of XWINDOW_QUIT procedure *********************************************



PRO XWINDOW_CLEANUP, id
WIDGET_CONTROL, id, GET_UVALUE=info, /NO_COPY
IF N_ELEMENTS(info) EQ 0 THEN RETURN
HANDLE_FREE, info.plotObjPtr
END ; of XWINDOW_CLEANUP cleanup procedure ***********************************



PRO XWINDOW_CONFIGURE_FILES, event
WIDGET_CONTROL, event.top, GET_UVALUE=info, /NO_COPY

   ; What kind of file to configure?

WIDGET_CONTROL, event.id, GET_VALUE=whichFile
CASE whichFile OF

   'Configure PostScript File...': BEGIN
      newkeywords = PS_FORM(DEFAULTS=info.ps, Parent=info.top, $
         LocalDefaults=info.pslocal, Cancel=cancel, Create=create)
      IF NOT cancel THEN info.ps = newkeywords
      IF create THEN WIDGET_CONTROL, info.psID, SEND_EVENT={ID:info.psID, $
         TOP:event.top, HANDLER:0L}
      END

   'Configure GIF File...': BEGIN
      config = info.gif
      newConfiguration = XWINDOW_CONFIGURATION('GIF', config, $
         Cancel=cancel, Create=create, Parent=info.top)
      IF NOT cancel THEN info.gif = newConfiguration
      IF create THEN WIDGET_CONTROL, info.gifID, SEND_EVENT={ID:info.gifID, $
         TOP:event.top, HANDLER:0L}
      END

   'Configure TIFF File...': BEGIN
      config = info.tiff
      newConfiguration = XWINDOW_CONFIGURATION('TIFF', config, $
         Cancel=cancel, Create=create, Parent=info.top)
      IF NOT cancel THEN info.tiff = newConfiguration
      IF create THEN WIDGET_CONTROL, info.tiffID, SEND_EVENT={ID:info.tiffID, $
         TOP:event.top, HANDLER:0L}
      END

   'Configure JPEG File...': BEGIN
      config = info.jpeg
      newConfiguration = XWINDOW_CONFIGURATION('JPEG', config, $
         Cancel=cancel, Create=create, Parent=info.top)
      IF NOT cancel THEN info.jpeg = newConfiguration
      IF create THEN WIDGET_CONTROL, info.jpegID, SEND_EVENT={ID:info.jpegID, $
         TOP:event.top, HANDLER:0L}
      END
ENDCASE

WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END ; of XWINDOW_CONFIGURE_FILES event handler ***********************************



PRO XWINDOW_CREATE_FILES, event
WIDGET_CONTROL, event.top, GET_UVALUE=info, /NO_COPY

   ; There can be all kinds of problems writing a file.
   ; Trap errors here and try to get out of here.

CATCH, error
IF error NE 0 THEN BEGIN
junk=widget_message(!err_string)
   ok = WIDGET_MESSAGE(['Problem writing file. The most', $
      'common problem is a mis-spelled filename.', 'Returning...'])
   IF WIDGET_INFO(id, /Valid_ID) THEN WIDGET_CONTROL, id, /Destroy
   IF N_ELEMENTS(thisDevice) GT 0 THEN SET_PLOT, thisDevice
   IF N_ELEMENTS(info) NE 0 THEN WIDGET_CONTROL, event.top, $
      SET_UVALUE=info, /NO_COPY
   RETURN
ENDIF

id = XWINDOW_ALERT('Please be patient while writing a file...')

   ; Get the Plot Object.

HANDLE_VALUE, info.plotObjPtr, plotObj, /NO_COPY

   ; What kind of file to create?

WIDGET_CONTROL, event.id, GET_VALUE=whichFile
CASE whichFile OF

   'Create PostScript File': BEGIN
      keywords = info.ps
      thisDevice = !D.NAME
      TVLCT, r, g, b, /GET
      SET_PLOT, 'PS'
      TVLCT, info.r, info.g, info.b, info.bottom
      DEVICE, _EXTRA=keywords
      ok = EXECUTE(plotObj.thisCommand)
      DEVICE, /CLOSE_FILE
      SET_PLOT, thisDevice
      TVLCT, r, g, b
      END

   'Create GIF File': BEGIN
      config = info.gif

         ; Render graphic in Z-buffer.

      thisDevice = !D.NAME
      TVLCT, rr, gg, bb, /GET
      SET_PLOT, 'Z'
      ERASE
      DEVICE, SET_RESOLUTION=[config.xsize, config.ysize], $
         SET_COLORS=info.ncolors
      ok = EXECUTE(plotObj.thisCommand)
      thisImage = TVRD()
      IF config.color NE 1 THEN LOADCT, 0, NColors=info.wcolors, $
         Bottom=info.bottom ELSE $
         TVLCT, info.r, info.g, info.b, info.bottom
      TVLCT, r, g, b, /GET
      SET_PLOT, thisDevice
      TVLCT, rr, gg, bb

         ; Write GIF file.

      WRITE_GIF, config.filename, thisImage, r, g, b
      END ; of GIF file creation.

   'Create TIFF File': BEGIN
      config = info.tiff

         ; Render graphic in Z-buffer.

      thisDevice = !D.NAME
      TVLCT, rr, gg, bb, /GET
      SET_PLOT, 'Z'
      TVLCT, info.r, info.g, info.b, info.bottom
      ERASE
      DEVICE, SET_RESOLUTION=[config.xsize, config.ysize], $
         SET_COLORS=info.ncolors
      ok = EXECUTE(plotObj.thisCommand)
      thisImage = TVRD()
      TVLCT, r, g, b, /GET
      SET_PLOT, thisDevice
      TVLCT, rr, gg, bb

         ; Write TIFF file. Use screen resolution.

      IF config.color EQ 1 THEN $
         TIFF_WRITE, config.filename, thisImage, config.order, $
            RED=r, GREEN=g, BLUE=b, XRESOL=ROUND(!D.X_PX_CM * 2.54), $
            YRESOL=ROUND(!D.X_PX_CM * 2.54) ELSE $
         TIFF_WRITE, config.filename, thisImage, config.order, $
            XRESOL=ROUND(!D.X_PX_CM * 2.54), YRESOL=ROUND(!D.X_PX_CM * 2.54)
      END

   'Create JPEG File': BEGIN
      config = info.jpeg

         ; Render graphic in Z-buffer.

      thisDevice = !D.NAME
      TVLCT, rr, gg, bb, /GET
      SET_PLOT, 'Z'
      ERASE
      DEVICE, SET_RESOLUTION=[config.xsize, config.ysize], $
         SET_COLORS=info.ncolors
      TVLCT, info.r, info.g, info.b, info.bottom
      ok = EXECUTE(plotObj.thisCommand)
      thisImage = TVRD()
      TVLCT, r, g, b, /GET
      SET_PLOT, thisDevice
      TVLCT, rr, gg, bb

         ; Write JPEG file.

      IF config.color EQ 1 THEN BEGIN
         image24 = BYTARR(3, config.xsize, config.ysize)
         image24(0,*,*) = r(thisImage)
         image24(1,*,*) = g(thisImage)
         image24(2,*,*) = b(thisImage)
         WRITE_JPEG, config.filename, image24, TRUE=1, $
            QUALITY=config.quality, ORDER=config.order
      ENDIF ELSE $
          WRITE_JPEG, config.filename, thisimage, $
            QUALITY=config.quality, ORDER=config.order
      END
ENDCASE

   ; Put the Plot Object back.

HANDLE_VALUE, info.plotObjPtr, plotObj, /SET, /NO_COPY
WIDGET_CONTROL, id, /Destroy
WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END ; of XWINDOW_CREATE_FILES event handler ***********************************



PRO XWINDOW_COLORS, event
WIDGET_CONTROL, event.top, GET_UVALUE=info, /NO_COPY
WIDGET_CONTROL, event.id, GET_UVALUE=colors

XCOLORS, Group=event.top, NColors=colors(0), Bottom=colors(1), $
   Title='Window ' + STRTRIM(info.wid, 2) + ' Colors', $
   NotifyID=[info.drawID, event.top]

WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END ; of XWINDOW_COLORS event handler ****************************************



PRO XWINDOW_DRAW_EVENT, event
WIDGET_CONTROL, event.top, GET_UVALUE=info, /NO_COPY

   ; Get the Plot Object.

HANDLE_VALUE, info.plotObjPtr, plotObj, /NO_COPY

   ; Need to respond to WIDGET_TRACKING events and
   ; XCOLORS_LOAD events.

thisEvent = TAG_NAMES(event, /Structure)

IF thisEvent EQ 'WIDGET_TRACKING' THEN BEGIN
   IF event.enter EQ 1 THEN $
   TVLCT, info.r, info.g, info.b, info.bottom
ENDIF

IF thisEvent EQ 'XCOLORS_LOAD' THEN BEGIN
   info.r = event.r(info.bottom:info.bottom+info.wcolors-1)
   info.g = event.g(info.bottom:info.bottom+info.wcolors-1)
   info.b = event.b(info.bottom:info.bottom+info.wcolors-1)
ENDIF

   ; Redisplay the command in the window if needed.

ncolors = !D.N_Colors
IF ncolors GT 256 THEN BEGIN
   WSET, info.wid
   IF info.erase THEN ERASE
   ok = EXECUTE(plotObj.thisCommand)
ENDIF

   ; Put the Plot Object back.

HANDLE_VALUE, info.plotObjPtr, plotObj, /SET, /NO_COPY

WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END ; of XWINDOW_DRAW_EVENT event handler **********************************



PRO XWINDOW_RESIZE_EVENTS, event
WIDGET_CONTROL, event.top, GET_UVALUE=info, /NO_COPY

   ; Resize the draw widget.

WIDGET_CONTROL, info.drawid, XSIZE=event.x, YSIZE=event.y

   ; Get the Plot Object.

HANDLE_VALUE, info.plotObjPtr, plotObj, /NO_COPY

   ; Redisplay the command in the window.

WIDGET_CONTROL, info.drawID, GET_VALUE=wid
WSET, wid
IF info.erase EQ 1 THEN ERASE
 ok = EXECUTE(plotObj.thisCommand)

   ; Update file output configuration structures in necessary.

IF (info.output) AND (NOT info.nochange) THEN BEGIN
   info.gif.xsize = event.x
   info.gif.ysize = event.y
   info.tiff.xsize = event.x
   info.tiff.ysize = event.y
   info.jpeg.xsize = event.x
   info.jpeg.ysize = event.y
   IF info.ps.inches EQ 0 THEN newsizes = PSWINDOW(/CM) ELSE $
      newsizes = PSWINDOW()
   info.ps.xsize = newsizes.xsize
   info.ps.ysize = newsizes.ysize
   info.ps.xoff = newsizes.xoffset
   info.ps.yoff = newsizes.yoffset
ENDIF

   ; Put the Plot Object back.

HANDLE_VALUE, info.plotObjPtr, plotObj, /SET, /NO_COPY

WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END ; of XWINDOW_RESIZE_EVENTS event handler *********************************



PRO XWINDOW, proName, $
   param1, param2, param3, param4, param5, $
   param6, param7, param8, param9, param10, $
   GROUP_LEADER=group, $
   _EXTRA=extra, WXSIZE=xsize, WYSIZE=ysize, WID=wid, XCOLORS=colors, $
   DRAWID=drawid, WTITLE=wtitle, JUST_REGISTER=justRegister, $
   OUTPUT=output, NO_CHANGE_CONFIG=nochange, ERASE=erase, $
   TOP=tlb, PROTECT=protect, NOMENU=nomenu, CPMENU=cpmenu, $
   WXPOS=wxpos, WYPOS=wypos

   ; Return to main-level on error.

ON_ERROR, 1

   ; Check keywords.

IF N_ELEMENTS(xsize) EQ 0 THEN xsize = 400
IF N_ELEMENTS(ysize) EQ 0 THEN ysize = 400
nochange = KEYWORD_SET(nochange)
extraFlag = KEYWORD_SET(extra)
justRegister = KEYWORD_SET(justRegister)
protect = KEYWORD_SET(protect)
needColors = KEYWORD_SET(colors)
needOutput = KEYWORD_SET(output)
cpmenu = KEYWORD_SET(cpmenu)
nomenu = KEYWORD_SET(nomenu)
IF nomenu THEN BEGIN
   needOutput = 0
   cpmenu = 0
   cprotectON = -1L
   cprotectOFF = -1L
   psID = -1L
   gifID = -1L
   tiffID = -1L
   jpegID = -1L
ENDIF

   ; Make sure a window has been opened.

thisWindowID = !D.Window
Window, XSize=10, YSize=10, /Free, /Pixmap
WDelete, !D.Window
IF thisWindowID GE 0 THEN WSet, thisWindowID

   ; Set up color variables. If the user typed "/Colors"
   ; then use *all* colors!

IF needcolors THEN BEGIN
   IF N_ELEMENTS(colors) EQ 1L AND (colors(0) EQ 1) THEN $
      colors = FIX([(!D.N_Colors < 256), 0])
   IF (N_ELEMENTS(colors) EQ 1L) THEN colors = [colors(0) < 256L, 0]
ENDIF ELSE colors = [(!D.N_Colors < 256L), 0]

colors = FIX([(colors(0) < 256), colors(1)])
wcolors = colors(0)
bottom = colors(1)
TVLCT, r, g, b, /GET
r = r(bottom:bottom+wcolors-1)
g = g(bottom:bottom+wcolors-1)
b = b(bottom:bottom+wcolors-1)
IF nomenu THEN needcolors = 0

   ; Check for positional parameters. One parameter required.

np = N_PARAMS()

IF np EQ 0 THEN MESSAGE, 'Sorry, at least one argument is required.'

   ; The first positional argument must be a string.

IF np GT 0 THEN BEGIN
   thisType = XWindow_WhatTypeVariable(proName)
   IF thisType NE 'STRING' THEN $
      MESSAGE, 'First argument must be STRING type. Returning...'
ENDIF

IF N_ELEMENTS(wtitle) EQ 0 THEN $
   wtitle = 'Resizeable ' + STRUPCASE(proName) + ' Window'

   ; Set up the Plot Object based on number of parameters and
   ; the extraFlag variable.

IF np EQ 0 then $
  MESSAGE, 'Must call XWINDOW with at least one argument.'

IF np GT 10 then $
  MESSAGE, 'Must have fewer than 10 parameters in call to XWINDOW.'

; Compose the command
thisCommand = proName
FOR i = 2, np DO $
  thisCommand = thisCommand + ', plotObj.param' +STRTRIM(i-1, 2)

; If any extra keywords are present, put them on the command line
IF extraFlag THEN $
  thisCommand = thisCommand + ', _EXTRA=plotObj.extra' ELSE $
  extra = 0

; We need to make sure that all of the parameters are set, at least to 0,
; so that they can be entered into the plotobj structure.
IF NOT KEYWORD_SET(param1) then param1 = 0
IF NOT KEYWORD_SET(param2) then param2 = 0
IF NOT KEYWORD_SET(param3) then param3 = 0
IF NOT KEYWORD_SET(param4) then param4 = 0
IF NOT KEYWORD_SET(param5) then param5 = 0
IF NOT KEYWORD_SET(param6) then param6 = 0
IF NOT KEYWORD_SET(param7) then param7 = 0
IF NOT KEYWORD_SET(param8) then param8 = 0
IF NOT KEYWORD_SET(param9) then param9 = 0
IF NOT KEYWORD_SET(param10) then param10 = 0

plotObj = { thisCommand:thisCommand, extra:extra, $
            param1:param1, param2:param2, param3:param3, param4:param4, $
            param5:param5, param6:param6, param7:param7, param8:param8, $
            param9:param9, param10:param10 }

   ; Store the Plot Object at a pointer location.

plotObjPtr = HANDLE_CREATE()

   ; Create the widgets for this program.

DEVICE, GET_SCREEN_SIZE=screenSize
IF N_ELEMENTS(wxpos) EQ 0 THEN wxpos = (screenSize(0) - xsize) / 2.
IF N_ELEMENTS(wypos) EQ 0 THEN wypos = (screenSize(1) - ysize) / 2.
IF NOT nomenu THEN BEGIN

   tlb = WIDGET_BASE(TLB_SIZE_EVENTS=1, $
      XOFFSET=wxpos, YOFFSET=wypos, MBar=menubase)

   controls = Widget_Button(menubase, Value='Controls', /Menu)

      ; Need a COLORS button?

   IF needColors THEN BEGIN
      colorsID = WIDGET_BUTTON(controls, Value='Change Colors...', $
         Event_Pro='XWINDOW_COLORS', UValue=colors)
   ENDIF

         ; Need color protection buttons?

  IF cpmenu THEN BEGIN
      cprotect = Widget_Button(controls, Value='Color Protection', $
         /Menu, Event_Pro='XWindow_Color_Protection')
      cprotectON = Widget_Button(cprotect, Value='ON', UVALUE='ON')
      cprotectOFF = Widget_Button(cprotect, Value='OFF', UVALUE='OFF')
   ENDIF ELSE BEGIN
      cprotectON = -1L
      cprotectOFF = -1L
   ENDELSE

      ; Need FILE OUTPUT button?

   IF needOutput THEN BEGIN
      outputButton = WIDGET_BUTTON(menubase, Value='File Output', $
         /Menu, Event_Pro='XWindow_Create_Files')
      psID = WIDGET_BUTTON(outputButton, Value='Create PostScript File')
      gifID = WIDGET_BUTTON(outputButton, Value='Create GIF File')
      tiffID = WIDGET_BUTTON(outputButton, Value='Create TIFF File')
      jpegID = WIDGET_BUTTON(outputButton, Value='Create JPEG File')
      configure = WIDGET_BUTTON(outputButton, Value='Configure Output File', $
         /Menu, /Separator, Event_Pro='XWindow_Configure_Files')
      ps_config = WIDGET_BUTTON(configure, Value='Configure PostScript File...')
      gif_config = WIDGET_BUTTON(configure, Value='Configure GIF File...')
      tiff_config = WIDGET_BUTTON(configure, Value='Configure TIFF File...')
      jpeg_config = WIDGET_BUTTON(configure, Value='Configure JPEG File...')
   ENDIF ELSE BEGIN
      psID = -1L
      gifID = -1L
      tiffID = -1L
      jpegID = -1L
   ENDELSE
   quit = Widget_Button(controls, Value='Quit', Event_Pro='XWindow_Quit')
ENDIF ELSE tlb = WIDGET_BASE(TLB_SIZE_EVENTS=1, $
      XOFFSET=wxpos, YOFFSET=wypos)

drawID = WIDGET_DRAW(tlb, XSIZE=xsize, YSIZE=ysize, $
   Event_Pro='XWindow_Draw_Event', Tracking_Events=protect)

WIDGET_CONTROL, tlb, /REALIZE
WIDGET_CONTROL, drawID, GET_VALUE=wid
WSET, wid

   ; Give each window a unique title.

wtitle = wtitle + ' (' + STRTRIM(wid,2) + ')'
Widget_Control, tlb, TLB_SET_TITLE=wtitle

   ; Set color protection button sensitivity.

IF cpmenu THEN BEGIN
   IF protect THEN BEGIN
      WIDGET_CONTROL, cprotectON, Sensitive=0
      WIDGET_CONTROL, cprotectOFF, Sensitive=1
   ENDIF ELSE BEGIN
      WIDGET_CONTROL, cprotectON, Sensitive=1
      WIDGET_CONTROL, cprotectOFF, Sensitive=0
   ENDELSE
ENDIF

   ; If something goes wrong executing the command, trap it.

CATCH, error
   IF error NE 0 THEN BEGIN
      ok = WIDGET_MESSAGE(["There is a problem executing the command", $
                           "string in XWINDOW. Please check keyword", $
                           "spelling and command syntax. Returning..."])
      HANDLE_FREE, plotObjPtr
      WIDGET_CONTROL, tlb, /DESTROY
      RETURN
   ENDIF

   ok = EXECUTE(plotObj.thisCommand)
   IF NOT ok THEN BEGIN
      ok = WIDGET_MESSAGE(["There is a problem executing the command", $
                           "string in XWINDOW. Please check keyword", $
                           "spelling and command syntax. Returning..."])
      HANDLE_FREE, plotObjPtr
      WIDGET_CONTROL, tlb, /DESTROY
      RETURN
   ENDIF

CATCH, /CANCEL

   ; Store the Plot Object in its pointer.

HANDLE_VALUE, plotObjPtr, plotObj, /SET, /NO_COPY

   ; Create an info structure.

info = { top:tlb, $                         ; Top level widget
         xsize:xsize, $                     ; X size of window.
         ysize:ysize, $                     ; Y size of window.
         wid:wid, $                         ; Window index number.
         drawID:drawID, $                   ; Draw widget identifier.
         cprotectON:cprotectON, $           ; Color protection ON button.
         cprotectOFF:cprotectOFF, $         ; Color protection OFF button.
         wtitle:wtitle, $                   ; Window title.
         r:r, $                             ; Red colors in window.
         g:g, $                             ; Green colors in window.
         b:b, $                             ; Blue colors in window.
         wcolors:wcolors, $                 ; Number of window colors.
         gifID:gifID, $                     ; ID of Create GIF file button.
         tiffID:tiffID, $                   ; ID of Create TIFF file button.
         jpegID:jpegID, $                   ; ID of Create JPEG file button.
         psID:psID, $                       ; ID of Create PS file button.
         bottom:bottom, $                   ; Starting color index.
         protect:protect, $                 ; Protect colors flag.
         nomenu:nomenu, $                   ; No menu flag.
         nochange:nochange, $               ; No change flag.
         erase:Keyword_Set(erase), $        ; Need erasure flag.
         ncolors:(!D.N_Colors < 256), $     ; Size of color table.
         plotObjPtr:plotObjPtr, $           ; Pointer to plot object.
         output:needOutput }                ; File Output menu flag.

   ; File Output configuration structures, if needed.

IF Keyword_Set(output) THEN BEGIN
CD, Current=thisDir
ps = ps_form(/init, filename=FilePath(Root_Dir=thisDir,'xwindow.ps'))
pslocal = ps_form(/init, filename=FilePath(Root_Dir=thisDir,'xwindow.ps'), $
                  xsize=10., xoff=0.5, ysize=7.5, yoff=0.5, color=1, $
                  landscape=1)
gif =  {XWINDOW_GIF,XSIZE:400, YSIZE:400, COLOR:1, $
       FILENAME:FilePath(Root_Dir=thisDir,'xwindow.gif'), $
       ORDER:0, QUALITY:-1}
jpeg = {XWINDOW_JPEG,XSIZE:400, YSIZE:400, COLOR:1, $
       FILENAME:FilePath(Root_Dir=thisDir,'xwindow.jpg'), $
       ORDER:0, QUALITY:75}
tiff = {XWINDOW_TIFF,XSIZE:400, YSIZE:400, COLOR:1, $
       FILENAME:FilePath(Root_Dir=thisDir,'xwindow.tif'), $
       ORDER:1, QUALITY:-1}
info = CREATE_STRUCT(info, 'PS', ps, 'PSLOCAL', pslocal, 'GIF', gif, $
                     'JPEG', jpeg, 'TIFF', tiff)
ENDIF

   ; Store the info structure in the TLB.

WIDGET_CONTROL, tlb, SET_UVALUE=info, /NO_COPY

   ; Register the program as on-blocking in 5.0.

thisRelease = StrMid(!Version.Release, 0, 1)
IF thisRelease EQ '5' THEN $
   XManager, 'xwindow', tlb, EVENT_HANDLER='XWINDOW_RESIZE_EVENTS', $
      CLEANUP='XWINDOW_CLEANUP', GROUP_LEADER=group, $
      JUST_REG=justRegister, /No_Block ELSE $
   XManager, 'xwindow', tlb, EVENT_HANDLER='XWINDOW_RESIZE_EVENTS', $
      CLEANUP='XWINDOW_CLEANUP', GROUP_LEADER=group, JUST_REG=justRegister
END
