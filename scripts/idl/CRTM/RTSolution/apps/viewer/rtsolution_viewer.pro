;-------------------------------------------------
PRO RTSolution_Viewer_Cleanup, ID

  ; Get top level base info state
  RTSolution_Viewer_GetState, ID, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'RTSolution_Viewer_Cleanup'

  ; Free the top level base info state
  RTSolution_Viewer_FreeState, ID

END


;-------------------------------------------------
PRO RTSolution_Viewer_Exit_Event, Event

  ; Get top level base info state
  RTSolution_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'RTSolution_Viewer_Exit_Event'

  ; Destroy the widget heirarchy
  WIDGET_CONTROL, Event.Top, /DESTROY

END


;-------------------------------------------------
PRO RTSolution_Viewer_Open_Event, Event

  ; Get top level base info state
  RTSolution_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'RTSolution_Viewer_Open_Event'

  ; Get an RTSolution filename
  RTSolution_Filename = DIALOG_PICKFILE( TITLE = 'Select an RTSolution file', $
                                         FILTER = '*.RTSolution.bin', $
                                         /MUST_EXIST )

  ; Do nothing if it's not valid
  IF ( NOT Valid_String( RTSolution_Filename ) ) THEN RETURN

  ; Get open "type" from event uvalue
  WIDGET_CONTROL, Event.ID, GET_UVALUE = uvalue
  CASE uvalue OF
    "FileOpen"   : Info.Plot_Type = 0L
    "CompareOpen": Info.Plot_Type = 1L
  ENDCASE
  
  ; Save top level base info state
  RTSolution_Viewer_SetState, Event.Top, Info

  ; Load and display the data
  RTSolution_Viewer_Load_File, RTSolution_Filename, Event.Top

END


;-------------------------------------------------
PRO RTSolution_Viewer_Print_Event, Event

  ; Get top level base info state
  RTSolution_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'RTSolution_Viewer_Print_Event'

  ; Output to file
  pson, FILENAME=Info.Filename[0]+'.profile_'+STRTRIM(Info.Profile_Number,2)+'.ps'
  RTSolution_Viewer_Display, Event.Top, $
    FONT=1, CHARSIZE=1.5, THICK=3
  psoff

END


;-------------------------------------------------
FUNCTION RTSolution_Viewer_BGroup_Event, Event

  @error_codes
  
  ; Get top level base info state
  RTSolution_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'RTSolution_Viewer_BGroup_Event'

  ; Get the channel data name
  Info.Channel_Tag = Event.VALUE

  ; Save top level base info state
  RTSolution_Viewer_SetState, Event.Top, Info

  ; Display the new profile data
  RTSolution_Viewer_Display, Event.Top

  RETURN, TRUE
END


;-------------------------------------------------
PRO RTSolution_Viewer_Slider_Event, Event

  ; Get top level base info state
  RTSolution_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'RTSolution_Viewer_Slider_Event'

  ; Get the position number
  WIDGET_CONTROL, Event.ID, GET_VALUE = n
  Info.Profile_Number = n

  ; Save top level base info state
  RTSolution_Viewer_SetState, Event.Top, Info

  ; Display the new profile data
  RTSolution_Viewer_Display, Event.Top

END


;-------------------------------------------------
PRO RTSolution_Viewer_Display, ID, $
    FONT=Font, CHARSIZE=Charsize, THICK=Thick

  ; Get top level base info state
  RTSolution_Viewer_GetState, ID, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'RTSolution_Viewer_Display'

  ; Get the current data
  data = Info.RTS_File[0]->Get_Data( $
    Info.Profile_Number, $
    Info.Channel_Tag, $
    Debug=Info.Debug )
  desc = " "
  IF ( Info.Plot_Type EQ 1 ) THEN BEGIN
    d2 = Info.RTS_File[1]->Get_Data( $
           Info.Profile_Number, $
           Info.Channel_Tag, $
           Debug=Info.Debug )
    data = data - d2
    desc = " difference "
  ENDIF

  ; Save top level base info state
  RTSolution_Viewer_SetState, ID, Info

  ; Plot the data
  PLOT, LINDGEN(N_ELEMENTS(data))+1L, data, $
        TITLE = Info.Channel_Tag+desc+'for profile #' + $
                STRTRIM(Info.Profile_Number,2)+' of '+Info.Filename[0], $
        /YNOZERO, $
        FONT=Font, CHARSIZE=Charsize, THICK=Thick
  OPLOT, !X.CRANGE, [0,0], $
         LINESTYLE = 2, THICK=Thick

END


;--------------------------------------------
PRO RTSolution_Viewer_Load_File, File, ID

  @error_codes

  ; Get top level base info state
  RTSolution_Viewer_GetState, ID, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'RTSolution_Viewer_Load_File'

  WIDGET_CONTROL, /HOURGLASS

  ; Save the filename
  Info.Full_Filename[Info.Plot_Type] = File
  FileParts = STRSPLIT( File, '/', COUNT = nParts, /EXTRACT )
  Info.Filename[Info.Plot_Type] = FileParts[nParts-1]

  ; Read the file
  IF ( OBJ_VALID(Info.RTS_File[Info.Plot_Type]) ) THEN OBJ_DESTROY, Info.RTS_File[Info.Plot_Type]
  Info.RTS_File[Info.Plot_Type] = OBJ_NEW("RTS_File", File, Debug=Info.Debug)
  Info.RTS_File[Info.Plot_Type]->Read, Debug=Info.Debug

  ; Make the comparison menu sensitive accordingly
  WIDGET_CONTROL, Info.Compare_Menu_ID, $
                  SENSITIVE = 1

  ; Activate/reset the slider
  IF ( Info.Plot_Type EQ 0 ) THEN BEGIN
    Info.RTS_File[Info.Plot_Type]->Get_Property, $
      Debug=Info.Debug, $
      n_Profiles=n_Profiles
    WIDGET_CONTROL, Info.Slider_ID, $
                    SET_SLIDER_MIN = 1, $
                    SET_SLIDER_MAX = n_Profiles, $
                    SET_VALUE = 1
    WIDGET_CONTROL, Info.Control_Base_ID, $
                    SENSITIVE = 1
    Info.Profile_Number = 1
  ENDIF
  
  ; Save top level base info state
  RTSolution_Viewer_SetState, ID, Info

  ; Display the new data
  RTSolution_Viewer_Display, ID

END



;--------------------------------------------
PRO RTSolution_Viewer, Debug = Debug

  ; Setup
  ; ...Check arguments
  IF ( NOT KEYWORD_SET( Debug ) ) THEN BEGIN
    Debug = 0 
  ENDIF ELSE BEGIN
    Debug = 1
    PRINT, 'RTSolution_Viewer'
  ENDELSE
  ; ...Parameters
  MAXFILES = 11L
  ; ...Widget-y defaults
  NoMap = 0
  Map   = 1


  ; Create the top level base
  Top_Level_Base_ID = WIDGET_BASE( ROW=1, $
                                   MAP = Map, $
                                   MBAR = Menu_Bar_ID, $
                                   TITLE = 'RTSolution_Viewer' )
  WIDGET_CONTROL, Top_Level_Base_ID, UPDATE = 0


  ; Create the menu bar contents
  ; ...The file menu
  File_Menu_ID = WIDGET_BUTTON( Menu_Bar_ID, $
                                VALUE = 'File', $
                                /MENU )
    ; ...File->Open
    File_Open_ID = WIDGET_BUTTON( File_Menu_ID, $
                                  VALUE = 'Open', $
                                  EVENT_PRO = 'RTSolution_Viewer_Open_Event', $
                                  UVALUE = 'FileOpen' )
    ; ...File->Print
    File_Print_ID = WIDGET_BUTTON( File_Menu_ID, $
                                   VALUE = 'Print', $
                                   EVENT_PRO = 'RTSolution_Viewer_Print_Event', $
                                   UVALUE = 'FilePrint' )
    ; ...File->Exit
    File_Exit_ID = WIDGET_BUTTON( File_Menu_ID, $
                                  VALUE = 'Exit', $
                                  EVENT_PRO = 'RTSolution_Viewer_Exit_Event', $
                                  /SEPARATOR, $
                                  UVALUE = 'FileExit' )

  ; ...The conmpare menu
  Compare_Menu_ID = WIDGET_BUTTON( Menu_Bar_ID, $
                                   VALUE = 'Compare', $
                                   SENSITIVE = 0, $
                                   /MENU )
    ; ...Compare->Open
    Compare_Open_ID = WIDGET_BUTTON( Compare_Menu_ID, $
                                     VALUE = 'Open', $
                                     EVENT_PRO = 'RTSolution_Viewer_Open_Event', $
                                     UVALUE = 'CompareOpen' )


  ; Create the left base for controls
  Control_Base_ID = WIDGET_BASE( Top_Level_Base_ID, $
                                 GROUP_LEADER = Top_Level_Base_ID, $
                                 FRAME = 2, $
                                 MAP = Map, $
                                 COLUMN = 1, $
                                 SENSITIVE = 0 )

  ; ...Create the data list
  excluded_data = ['N_ALLOCATES','N_LAYERS','SOD','UPWELLING_RADIANCE','LAYER_OPTICAL_DEPTH']
  included_data = TAG_NAMES({RTS_Channel})
  FOR i = 0, N_ELEMENTS(excluded_data)-1 DO BEGIN
    idx = WHERE(included_data NE excluded_data[i])
    included_data = included_data[idx]
  ENDFOR
  BGroup_ID = CW_BGROUP( Control_Base_ID, $
                         included_data, $
                         /COLUMN, $
                         EVENT_FUNC = 'RTSolution_Viewer_BGroup_Event', $
                         /EXCLUSIVE, $
                         FRAME = 2, $
                         /RETURN_NAME, $
                         LABEL_TOP = 'Channel Data', $
                         MAP = Map )

  ; ...Create the profile slider
  Slider_ID = WIDGET_SLIDER( Control_Base_ID, $
                             GROUP_LEADER = Top_Level_Base_ID, $
                             DRAG = 0, $
                             EVENT_PRO = 'RTSolution_Viewer_Slider_Event', $
                             FRAME = 2, $
                             MINIMUM = 0, $
                             MAXIMUM = 1, $
                             TITLE = 'Profile count', $
                             UVALUE = 'Profile' )


  ; Create the right base for drawing
  Draw_Base_ID = WIDGET_BASE( Top_Level_Base_ID, $
                              GROUP_LEADER = Top_Level_Base_ID, $
                              ROW = 1, $
                              MAP = Map )
  ; ...Create the draw widget
  xSize = 800
  ySize = 600
  Draw_Widget_ID = WIDGET_DRAW( Draw_Base_ID, $
                                GROUP_LEADER = Top_Level_Base_ID, $
                                XSIZE = xSize, YSIZE = ySIZE )


  ; Map, update and realise the widget heirarchy
  WIDGET_CONTROL, Top_Level_Base_ID, MAP = Map, $
                                     REALIZE = 1, $
                                     UPDATE = 1


  ; Obtain the draw widget window ID
  WIDGET_CONTROL, Draw_Widget_ID, GET_VALUE = Draw_Window_ID


  ; Load the information structure
  Info = { Debug           : Debug, $
           PMulti          : !P.MULTI, $
           Compare_Menu_ID : Compare_Menu_ID, $
           Control_Base_ID : Control_Base_ID, $
           Draw_Widget_ID  : Draw_Widget_ID, $
           Draw_Window_ID  : Draw_Window_ID, $
           Slider_ID       : Slider_ID, $
           Profile_Number  :  1, $
           Channel_Tag     :  'BRIGHTNESS_TEMPERATURE', $
           n_Files         : 0L, $
           Plot_Type       : 0L, $
           Full_Filename   : STRARR(2), $  ; File and Compare
           Filename        : STRARR(2), $  ; File and Compare
           RTS_File        : OBJARR(2)  }  ; File and Compare
  InfoPtr = PTR_NEW( Info )
  WIDGET_CONTROL, Top_Level_Base_ID, SET_UVALUE = InfoPtr


  ; Display a file
  XYOUTS, 0.5, 0.5, $
          'Use File->Open to select a file', $
          /NORM, ALIGNMENT = 0.5, $
          CHARSIZE = 2.0



  ;#----------------------------------------------------------------------------#
  ;#                           -- START THE XMANAGER --                         #
  ;#----------------------------------------------------------------------------#

  XMANAGER, 'RTSolution_Viewer', Top_Level_Base_ID, $
            CLEANUP = 'RTSolution_Viewer_Cleanup', $
            GROUP_LEADER = Top_Level_Base_ID

END ; RTSolution_Viewer
