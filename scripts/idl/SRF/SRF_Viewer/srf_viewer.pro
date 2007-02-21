;-------------------------
PRO SRF_Viewer_Cleanup, ID
;-------------------------

  ; Get top level base info state
  SRF_Viewer_GetState, ID, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'SRF_Viewer_Cleanup'

  ; Free the top level base info state
  SRF_Viewer_FreeState, ID

END


;-------------------------------
PRO SRF_Viewer_Exit_Event, Event
;-------------------------------

  ; Get top level base info state
  SRF_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'SRF_Viewer_Exit_Event'

  ; Destroy the widget heirarchy
  WIDGET_CONTROL, Event.Top, /DESTROY

END

;-------------------------------
PRO SRF_Viewer_Open_Event, Event
;-------------------------------

  ; Get top level base info state
  SRF_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'SRF_Viewer_Open_Event'

  ; Get an SRF filename
  SRF_Filename = DIALOG_PICKFILE( TITLE = 'Select an SRF file', $
                                  FILTER = '*srf.nc', $
                                  /MUST_EXIST )

  ; Do nothing if it's not valid
  IF ( NOT Valid_String( SRF_Filename ) ) THEN RETURN

  ; Load and display the data
  SRF_Viewer_Load_File, SRF_Filename, Event.Top

END


;--------------------------------
PRO SRF_Viewer_Print_Event, Event
;--------------------------------

  ; Get top level base info state
  SRF_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'SRF_Viewer_Print_Event'

  ; Output to file
  pson, FILENAME=Info.Filename+'.channel_'+STRTRIM((*Info.SRF).Channel,2)+'.ps'
  SRF_Viewer_Display, Event.Top, $
                      FONT=1, CHARSIZE=1.5, THICK=3
  psoff

END


;----------------------------------------
PRO SRF_Viewer_ChannelSlider_Event, Event
;----------------------------------------

  ; Get top level base info state
  SRF_Viewer_GetState, Event.Top, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'SRF_Viewer_ChannelSlider_Event'

  ; Get the channel number
  WIDGET_CONTROL, Event.ID, GET_VALUE = Channel

  ; Is the selected channel present?
  Loc = WHERE( *Info.ChannelList EQ Channel, Count )
  IF ( Count EQ 0 ) THEN BEGIN
    MESSAGE, 'Channel number '+STRTRIM(Channel,2)+' is not present', $
             /INFORMATIONAL
    RETURN
  ENDIF

  ; Read the requested channel SRF
  Error_Status = Read_SRF_netCDF( Info.Full_Filename, $
                                  Channel, $
                                  *Info.SRF )

  ; Save top level base info state
  SRF_Viewer_SetState, Event.Top, Info

  ; Display the new profile data
  SRF_Viewer_Display, Event.Top

END


;--------------------------------------------
PRO SRF_Viewer_Display, ID, $
    FONT=Font, CHARSIZE=Charsize, THICK=Thick
;--------------------------------------------

  ; Get top level base info state
  SRF_Viewer_GetState, ID, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'SRF_Viewer_Display'

  ; Plot the current SRF
  PLOT, *(*Info.SRF).Frequency, *(*Info.SRF).Response, $
        TITLE='Channel '+STRTRIM((*Info.SRF).Channel,2)+' SRF for '+$
              (*Info.SRF).Platform_Name+' '+(*Info.SRF).Sensor_Name, $
        XTITLE='Frequency (cm!U-1!N)', $
        YTITLE='Relative response', $
        FONT=Font, $
        CHARSIZE=CharSize, $
        /NODATA
  OPLOT, *(*Info.SRF).Frequency, *(*Info.SRF).Response, $
         COLOR=5, $ ; Red
         THICK=Thick

END


;---------------------------------
PRO SRF_Viewer_Load_File, File, ID
;---------------------------------

  @error_codes

  ; Get top level base info state
  SRF_Viewer_GetState, ID, Info

  ; Print debug statement if required
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'SRF_Viewer_Load_File'

  WIDGET_CONTROL, /HOURGLASS

  ; Save the filename
  Info.Full_Filename = File
  FileParts = STRSPLIT( File, '/', COUNT = nParts, /EXTRACT )
  Info.Filename = FileParts[nParts-1]

  ; Inquire the new file
  Error_Status = Inquire_SRF_netCDF( File, $
                                     n_Channels = nChannels, $
                                     Channel_List = *Info.ChannelList, $
                                     Sensor_Name   = Sensor_Name, $
                                     Platform_Name = Platform_Name )

  ; Read the first channel of the new file
  Error_Status = Read_SRF_netCDF( File, $
                                  (*Info.ChannelList)[0], $
                                  *Info.SRF )

  ; Activate the slider
  WIDGET_CONTROL, Info.ChannelSlider_ID, $
                  SET_SLIDER_MIN = MIN(*Info.ChannelList), $
                  SET_SLIDER_MAX = MAX(*Info.ChannelList), $
                  SET_VALUE = MIN(*Info.ChannelList), $
                  SENSITIVE = 1
    
  ; Save top level base info state
  SRF_Viewer_SetState, ID, Info

  ; Display the new data
  SRF_Viewer_Display, ID

END



;============================
PRO SRF_Viewer, Debug = Debug
;============================

  ; ---------------
  ; Check arguments
  ; ---------------
  IF ( NOT KEYWORD_SET( Debug ) ) THEN BEGIN
    Debug = 0 
  ENDIF ELSE BEGIN
    Debug = 1
    PRINT, 'SRF_Viewer'
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
                                   TITLE = 'SRF_Viewer' )
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
                                EVENT_PRO = 'SRF_Viewer_Open_Event', $
                                UVALUE = 'Open' )
  File_Print_ID = WIDGET_BUTTON( File_Menu_ID, $
                                 VALUE = 'Print', $
                                 EVENT_PRO = 'SRF_Viewer_Print_Event', $
                                 UVALUE = 'Print' )
  File_Exit_ID = WIDGET_BUTTON( File_Menu_ID, $
                                VALUE = 'Exit', $
                                EVENT_PRO = 'SRF_Viewer_Exit_Event', $
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
  xSize = 640
  ySize = 512
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

  ; Create the channel slider
  ChannelSlider_ID = WIDGET_SLIDER( Control_Base_ID, $
                                    GROUP_LEADER = Top_Level_Base_ID, $
                                    DRAG = 0, $
                                    EVENT_PRO = 'SRF_Viewer_ChannelSlider_Event', $
                                    MINIMUM = 0, $
                                    MAXIMUM = 1, $
                                    SENSITIVE = 0, $
                                    TITLE = 'Channel number', $
                                    UVALUE = 'Channel', $
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

  Info = { Debug            : Debug, $
           PMulti           : !P.MULTI, $
           Draw_Widget_ID   : Draw_Widget_ID, $
           Draw_Window_ID   : Draw_Window_ID, $
           ChannelSlider_ID : ChannelSlider_ID, $
           Full_Filename    : ' ', $
           Filename         : ' ', $
           ChannelList      : PTR_NEW(/ALLOCATE_HEAP), $
           SRF              : PTR_NEW(/ALLOCATE_HEAP) }

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

  XMANAGER, 'SRF_Viewer', Top_Level_Base_ID, $
            CLEANUP = 'SRF_Viewer_Cleanup', $
            GROUP_LEADER = Top_Level_Base_ID

END ; SRF_Viewer
