;-------------------------
PRO OSRF_Viewer_Cleanup, ID
;-------------------------

  ; Get top level base info state
  OSRF_Viewer_GetState, ID, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'OSRF_Viewer_Cleanup'

  ; Free the top level base info state
  OSRF_Viewer_FreeState, ID

END


;-------------------------------
PRO OSRF_Viewer_Exit_Event, Event
;-------------------------------

  ; Get top level base info state
  OSRF_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'OSRF_Viewer_Exit_Event'

  ; Destroy the widget heirarchy
  WIDGET_CONTROL, Event.Top, /DESTROY

END

;-------------------------------
PRO OSRF_Viewer_Open_Event, Event
;-------------------------------

  ; Get top level base info state
  OSRF_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'OSRF_Viewer_Open_Event'

  ; Get an OSRF filename
  OSRF_Filename = DIALOG_PICKFILE( TITLE = 'Select an OSRF file', $
                                   FILTER = '*.osrf.nc', $
                                   /MUST_EXIST )

  ; Do nothing if it's not valid
  IF ( NOT Valid_String( OSRF_Filename ) ) THEN RETURN

  ; Load and display the data
  OSRF_Viewer_Load_File, OSRF_Filename, Event.Top

END


;--------------------------------
PRO OSRF_Viewer_Print_Event, Event
;--------------------------------

  ; Get top level base info state
  OSRF_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'OSRF_Viewer_Print_Event'

  ; Output to file
  pson, FILENAME=Info.Filename+'.channel_'+STRTRIM(Info.Channel,2)+'.ps'
  OSRF_Viewer_Display, Event.Top, $
    FONT=1, CHARSIZE=1.5, THICK=3
  psoff

END


;------------------------------------------
PRO OSRF_Viewer_Slider_Event, Event
;------------------------------------------

  ; Get top level base info state
  OSRF_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'OSRF_Viewer_Slider_Event'

  ; Get the position number
  WIDGET_CONTROL, Event.ID, GET_VALUE = n
  Info.Position = n-1

  ; Save top level base info state
  OSRF_Viewer_SetState, Event.Top, Info

  ; Display the new profile data
  OSRF_Viewer_Display, Event.Top

END


;--------------------------------------------
PRO OSRF_Viewer_Display, ID, $
    FONT=Font, CHARSIZE=Charsize, THICK=Thick
;--------------------------------------------

  ; Get top level base info state
  OSRF_Viewer_GetState, ID, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'OSRF_Viewer_Display'

  ; Get the current oSRF
  oSRF = Info.OSRF_File->Get( Debug=Info.Debug, $
                              Position=Info.Position )
  ; ...Get some current oSRF info
  oSRF->Get_Property, Debug=Info.Debug, $
    Channel=Channel
  Info.Channel = Channel
  
  ; Save top level base info state
  OSRF_Viewer_SetState, ID, Info

  ; Plot it
  oSRF->Plot, $
    Debug=Info.Debug, $
    FONT=Font, CHARSIZE=Charsize, THICK=Thick

END


;---------------------------------
PRO OSRF_Viewer_Load_File, File, ID
;---------------------------------

  @error_codes

  ; Get top level base info state
  OSRF_Viewer_GetState, ID, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'OSRF_Viewer_Load_File'

  WIDGET_CONTROL, /HOURGLASS

  ; Save the filename
  Info.Full_Filename = File
  FileParts = STRSPLIT( File, '/', COUNT = nParts, /EXTRACT )
  Info.Filename = FileParts[nParts-1]

  ; Read the file
  Info.OSRF_File = OBJ_NEW("OSRF_File", File, Debug=Info.Debug)
  Info.OSRF_File->Read, Debug=Info.Debug

  ; Activate the slider
  Info.OSRF_File->Get_Property, $
    Debug=Info.Debug, $
    n_Channels=n_Channels
  WIDGET_CONTROL, Info.Slider_ID, $
                  SET_SLIDER_MIN = 1, $
                  SET_SLIDER_MAX = n_Channels, $
                  SET_VALUE = 1, $
                  SENSITIVE = 1
    
  ; Save top level base info state
  OSRF_Viewer_SetState, ID, Info

  ; Display the new data
  OSRF_Viewer_Display, ID

END



;============================
PRO OSRF_Viewer, Debug = Debug
;============================

  ; ---------------
  ; Check arguments
  ; ---------------
  IF ( NOT KEYWORD_SET( Debug ) ) THEN BEGIN
    Debug = 0 
  ENDIF ELSE BEGIN
    Debug = 1
    PRINT, 'OSRF_Viewer'
  ENDELSE

  ; ----------
  ; Parameters
  ; ----------
  MAXFILES = 11L



  ;#----------------------------------------------------------------------------#
  ;#                       -- CREATE THE WIDGET DISPLAY --                      #
  ;#----------------------------------------------------------------------------#

  NoMap = 0
  Map   = 1


  ; -------------------------
  ; Create the top level base
  ; -------------------------

  Top_Level_Base_ID = WIDGET_BASE( COLUMN=1, $
                                   MAP = Map, $
                                   MBAR = Menu_Bar_ID, $
                                   TITLE = 'OSRF_Viewer' )
  WIDGET_CONTROL, Top_Level_Base_ID, UPDATE = 0


  ; ----------------------------
  ; Create the menu bar contents
  ; ----------------------------

  ; The file menu
  File_Menu_ID = WIDGET_BUTTON( Menu_Bar_ID, $
                                VALUE = 'File', $
                                /MENU )

  File_Open_ID = WIDGET_BUTTON( File_Menu_ID, $
                                VALUE = 'Open', $
                                EVENT_PRO = 'OSRF_Viewer_Open_Event', $
                                UVALUE = 'Open' )
  File_Print_ID = WIDGET_BUTTON( File_Menu_ID, $
                                 VALUE = 'Print', $
                                 EVENT_PRO = 'OSRF_Viewer_Print_Event', $
                                 UVALUE = 'Print' )
  File_Exit_ID = WIDGET_BUTTON( File_Menu_ID, $
                                VALUE = 'Exit', $
                                EVENT_PRO = 'OSRF_Viewer_Exit_Event', $
                                /SEPARATOR, $
                                UVALUE = 'Exit' )


  ; ---------------------------------
  ; Create the right base for drawing
  ; ---------------------------------

  Draw_Base_ID = WIDGET_BASE( Top_Level_Base_ID, $
                              GROUP_LEADER = Top_Level_Base_ID, $
                              ROW = 1, $
                              MAP = Map )



  ; Create the draw widget
  xSize = 800
  ySize = 600
  Draw_Widget_ID = WIDGET_DRAW( Draw_Base_ID, $
                                GROUP_LEADER = Top_Level_Base_ID, $
                                XSIZE = xSize, YSIZE = ySIZE )


  ; ----------------------------
  ; Create the base for controls
  ; ----------------------------

  Control_Base_ID = WIDGET_BASE( Top_Level_Base_ID, $
                                 GROUP_LEADER = Top_Level_Base_ID, $
                                 FRAME = 2, $
                                 MAP = Map, $
                                 COLUMN = 1 )

  ; Create the slider
  Slider_ID = WIDGET_SLIDER( Control_Base_ID, $
                                    GROUP_LEADER = Top_Level_Base_ID, $
                                    DRAG = 0, $
                                    EVENT_PRO = 'OSRF_Viewer_Slider_Event', $
                                    MINIMUM = 0, $
                                    MAXIMUM = 1, $
                                    SENSITIVE = 0, $
                                    TITLE = 'oSRF count', $
                                    UVALUE = 'Position', $
                                    XSIZE=xSize-5 )


  ; --------------------------------------------
  ; Map, update and realise the widget heirarchy
  ; --------------------------------------------

  WIDGET_CONTROL, Top_Level_Base_ID, MAP = Map, $
                                     REALIZE = 1, $
                                     UPDATE = 1

  ; --------------------------------
  ; Obtain the draw widget window ID
  ; --------------------------------

  WIDGET_CONTROL, Draw_Widget_ID, GET_VALUE = Draw_Window_ID



  ;#----------------------------------------------------------------------------#
  ;#                     -- LOAD THE INFORMATION STRUCTURE --                   #
  ;#----------------------------------------------------------------------------#

  Info = { Debug          : Debug, $
           PMulti         : !P.MULTI, $
           Draw_Widget_ID : Draw_Widget_ID, $
           Draw_Window_ID : Draw_Window_ID, $
           Slider_ID      : Slider_ID, $
           Full_Filename  : ' ', $
           Filename       : ' ', $
           Channel        : 0, $
           Position       : 0, $
           OSRF_File      : OBJ_NEW() }

  InfoPtr = PTR_NEW( Info )
  WIDGET_CONTROL, Top_Level_Base_ID, SET_UVALUE = InfoPtr



  ;#----------------------------------------------------------------------------#
  ;#                             -- DISPLAY A FILE  --                          #
  ;#----------------------------------------------------------------------------#

  XYOUTS, 0.5, 0.5, $
          'Use File->Open to select a file', $
          /NORM, ALIGNMENT = 0.5, $
          CHARSIZE = 2.0



  ;#----------------------------------------------------------------------------#
  ;#                           -- START THE XMANAGER --                         #
  ;#----------------------------------------------------------------------------#

  XMANAGER, 'OSRF_Viewer', Top_Level_Base_ID, $
            CLEANUP = 'OSRF_Viewer_Cleanup', $
            GROUP_LEADER = Top_Level_Base_ID

END ; OSRF_Viewer
