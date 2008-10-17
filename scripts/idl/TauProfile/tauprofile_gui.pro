;=============================
PRO TauProfile_GUI_Cleanup, ID
;=============================
  TauProfile_GetState, ID, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_GUI_Cleanup'
  TauProfile_FreeState, ID
END


;===============================
PRO TauProfile_Open_Event, Event
;===============================
  TauProfile_GetState, Event.Top, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_Open_Event'

  ; Get a TauProfile filename
  TauProfile_Filename = DIALOG_PICKFILE( TITLE ='Select a TauProfile file', $
                                         FILTER='*TauProfile.nc', $
                                         /MUST_EXIST )
  IF ( NOT Valid_String( TauProfile_Filename ) ) THEN RETURN

  ; Load and display the data
  TauProfile_Load_File, TauProfile_Filename, Event.Top
END


;================================
PRO TauProfile_Print_Event, Event
;================================
  TauProfile_GetState, Event.Top, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_Print_Event'

  ; Output to PS file
  pson, Filename = 'TauProfile.ps'
  TauProfile_Display, Event.Top, FONT=1, CHARSIZE=2.0
  psoff
END


;===============================
PRO TauProfile_Exit_Event, Event
;===============================
  TauProfile_GetState, Event.Top, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_Exit_Event'

  ; Restore the plotting parameters
  !P.MULTI = Info.PMulti

  ; Destroy the widget heirarchy
  WIDGET_CONTROL, Event.Top, /DESTROY
END


;=================================
PRO TauProfile_Slider_Event, Event
;=================================
  TauProfile_GetState, Event.Top, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_Slider_Event'

  TauProfile_Display, Event.Top
END


;=========================================
PRO TauProfile_Profile_Slider_Event, Event
;=========================================
  @error_codes
  TauProfile_GetState, Event.Top, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_Profile_Slider_Event'

  ; Get the profile number
  WIDGET_CONTROL, Event.ID, GET_VALUE=m

  ; Alert user
  WIDGET_CONTROL, /HOURGLASS

  ; Read the required profile TauProfile data
  Result = Read_TauProfile_netCDF( Info.Filename, $
                                   TauProfile, PROFILE_LIST=(*Info.Profile_List)[m-1] )
  IF ( Result NE SUCCESS ) THEN MESSAGE, 'Error reading profile #'+ $
                                         STRTRIM((*Info.Profile_List)[m-1],2) + $
                                         ' from '+STRTRIM(File,2)

  ; Replace the TauProfile data in the info structure
  Result = Destroy_TauProfile( *Info.TauProfile )
  IF ( Result NE SUCCESS ) THEN MESSAGE, 'Error destroying *(Info.TauProfile)'
  PTR_FREE, Info.TauProfile
  Info.TauProfile = PTR_NEW(TauProfile)
  
  ; Save top level base info state
  TauProfile_SetState, Event.Top, Info

  ; Display the new profile
  TauProfile_Display, Event.Top
END


;========================================
PRO TauProfile_UpDown_Button_Event, Event
;========================================
  TauProfile_GetState, Event.Top, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_UpDown_Button_Event'

  ; Reverse the pressure array
  *Info.Pressure = REVERSE(*Info.Pressure)

  ; Save top level base info state
  TauProfile_SetState, Event.Top, Info

  ; Display the result
  TauProfile_Display, Event.Top

END


;========================================
PRO TauProfile_LogLin_Button_Event, Event
;========================================
  TauProfile_GetState, Event.Top, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_LogLin_Button_Event'

  ; Reset the yLog keyword
  IF ( Info.yLog ) THEN BEGIN
    Info.yLog=0 
    Info.yTickFormat = '' 
  ENDIF ELSE BEGIN
    Info.yLog=1 
    Info.yTickFormat = 'logticks' 
  ENDELSE

  ; Save top level base info state
  TauProfile_SetState, Event.Top, Info

  ; Display the result
  TauProfile_Display, Event.Top
END


;======================================
PRO TauProfile_PSym_Button_Event, Event
;======================================
  TauProfile_GetState, Event.Top, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_PSym_Button_Event'

  ; Reset the PSym keyword
  IF ( Info.PSym EQ 0 ) THEN BEGIN
    Info.PSym = -4
  ENDIF ELSE BEGIN
    Info.PSym = 0
  ENDELSE

  ; Save top level base info state
  TauProfile_SetState, Event.Top, Info

  ; Display the result
  TauProfile_Display, Event.Top

END

;======================================
PRO TauProfile_WgtFn_Button_Event, Event
;======================================
  TauProfile_GetState, Event.Top, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_WgtFn_Button_Event'

  ; Toggle the weighting function keyword
  Info.WgtFn = (Info.WgtFn EQ 0) ? 1 : 0

  ; Save top level base info state
  TauProfile_SetState, Event.Top, Info

  ; Display the result
  TauProfile_Display, Event.Top

END


;==================================
FUNCTION Molecule_Name, Molecule_ID
;==================================
  @tauprofile_parameters
  loc = WHERE( MOLECULE_SET_ID EQ Molecule_ID, count )
  IF ( count EQ 0 ) THEN MESSAGE, 'Invalid molecule set id: '+STRTRIM(Molecule_ID,2)
  RETURN, MOLECULE_SET_NAME[loc[0]]
END 


;=======================================================
PRO TauProfile_Display, ID, FONT=Font, CHARSIZE=Charsize
;=======================================================
  @tauprofile_parameters
  TauProfile_GetState, ID, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_Display'

  ; Set the plotting parameters
  !P.MULTI = [ 0, Info.iX, Info.iY ]

  ; Get the current slider values
  WIDGET_CONTROL, Info.Channel_Slider_ID, GET_VALUE=l
  WIDGET_CONTROL, Info.Angle_Slider_ID  , GET_VALUE=i
  WIDGET_CONTROL, Info.Profile_Slider_ID, GET_VALUE=m

  ; Point to the plot window if necessary
  IF ( !D.NAME EQ 'X' ) THEN WSET, Info.Draw_Window_ID

  ; Set plotting parameters
  IF ( N_ELEMENTS(Charsize) EQ 0 ) THEN Charsize=1.5
  XTitlePos = 0.5
  YTitlePos = 0.96

  ; Plot the data
  p   = *Info.Pressure
  lnp = ALOG(p)
  pRange = [ MAX(p), MIN(p)<0.01 ]
  IF ( Info.WgtFn EQ 0 ) THEN $
    xTitle = 'Transmittance' $
  ELSE $
    xTitle = 'dTau/dlnp'
  
  FOR j = 0L, (*Info.TauProfile).n_Molecule_Sets - 1L DO BEGIN

    ; Construct the plot titles
    jName = Molecule_Name((*(*Info.TauProfile).Molecule_Set)[j])
    Title = 'Molecule Set: ' + jName + '!C' + $
            'Profile: ' + STRTRIM( (*Info.Profile_List)[m-1], 2 ) + $
            '; Angle: ' + STRING( (*(*Info.TauProfile).Angle)[i-1], FORMAT='(f4.2)' ) + $ 
            '; Channel: ' + STRTRIM( (*(*Info.TauProfile).Channel)[l-1], 2 ) 
            
    ; Plot the transmittance profile
    x = (*(*Info.TauProfile).Tau)[*,l-1,i-1,0,j]
    IF ( Info.WgtFn EQ 1 ) THEN x = DERIV(lnp,x)
    PLOT, x, p, $
          XTITLE = xTitle, $
          YTITLE = 'Pressure (hPa)', $
          YRANGE = pRange, $
          YSTYLE = 1, $
          YLOG = Info.yLog, $
          YTICKFORMAT = Info.yTickFormat, $
          YMARGIN = [4,4], $
          CHARSIZE = Charsize, $
          FONT = Font, $
          PSYM = Info.PSym, $
          TITLE = Title
    OPLOT, (Info.WgtFn EQ 0) ? [1,1] : [0,0], (Info.yLog EQ 0) ? !Y.CRANGE : 10^!Y.CRANGE, $
           LINESTYLE = 2
  ENDFOR

  ; Print out the min/max values and locations
  tDim = SIZE(*(*Info.TauProfile).Tau, /DIMENSIONS)
  tMin = MIN(*(*Info.TauProfile).Tau, locMin, MAX=tMax, SUBSCRIPT_MAX=locMax)
  locMin = ARRAY_INDICES(tDim,locMin,/DIMENSIONS)
  locMax = ARRAY_INDICES(tDim,locMax,/DIMENSIONS)
  
;  channel  = (*(*Info.TauProfile).Channel)[locMin[1]]
;  angle    = (*(*Info.TauProfile).Angle)[locMin[2]]
;  molecule = Molecule_Name((*(*Info.TauProfile).Molecule_Set)[locMin[4]])
;  XYOUTS, 0.6, 0.2, $
;          STRING(tMin, channel, angle, molecule, $
;                 FORMAT='("MIN: ",e13.6,"!CChannel: ",i5,"!CAngle: ",f4.2,"!CMolecule: ",a)'), $
;          ALIGNMENT=0.0, /NORMAL, $
;          FONT=Font, CHARSIZE=Charsize
;          
;  channel  = (*(*Info.TauProfile).Channel)[locMax[1]]
;  angle    = (*(*Info.TauProfile).Angle)[locMax[2]]
;  molecule = Molecule_Name((*(*Info.TauProfile).Molecule_Set)[locMax[4]])
;  XYOUTS, 0.8, 0.2, $
;          STRING(tMax, channel, angle, molecule, $
;                 FORMAT='("MAX: ",e13.6,"!CChannel: ",i5,"!CAngle: ",f4.2,"!CMolecule: ",a)'), $
;          ALIGNMENT=0.0, /NORMAL, $
;          FONT=Font, CHARSIZE=Charsize
;          
;  XYOUTS, 0.77, 0.23, $
;          (*Info.TauProfile).Sensor_ID+' transmittance extrema for profile '+STRTRIM((*Info.Profile_List)[m-1],2), $
;          ALIGNMENT=0.5, /NORMAL, $
;          FONT=Font, CHARSIZE=Charsize

END


;=================================
PRO TauProfile_Load_File, File, ID
;=================================
  @error_codes
  TauProfile_GetState, ID, Info
  IF ( Info.Debug EQ 1 ) THEN PRINT, 'TauProfile_Load_File'

  ; Alert user
  WIDGET_CONTROL, /HOURGLASS

  ; Free the current TauProfile structure
  IF ( PTR_VALID(Info.TauProfile) ) THEN BEGIN
    Result = Destroy_TauProfile( *Info.TauProfile )
    IF ( Result NE SUCCESS ) THEN MESSAGE, 'Error destroying *(Info.TauProfile)'
    PTR_FREE, Info.TauProfile
  ENDIF
  
  ; Save the filename
  Info.Filename = File
  
  ; Inquire the file for the profile list
  Result = Inquire_TauProfile_netCDF( File, N_PROFILES=n_Profiles, PROFILE_LIST=Profile_List )
  IF ( Result NE SUCCESS ) THEN MESSAGE, 'Error inquiring '+STRTRIM(File,2)
  Info.Profile_List = PTR_NEW(Profile_List)

  ; Read the first profile TauProfile data
  Result = Read_TauProfile_netCDF( File, TauProfile, PROFILE_LIST=Profile_List[0] )
  IF ( Result NE SUCCESS ) THEN MESSAGE, 'Error in initial read of '+STRTRIM(File,2)
  Info.TauProfile = PTR_NEW(TauProfile)

  ; Copy out the pressure
  Pressure = (*(*Info.TauProfile).Level_Pressure)[0:(*Info.TauProfile).n_Layers-1]
  Info.Pressure = PTR_NEW(REVERSE(Pressure))  ; Default for upwelling
  
  ; Set the pmulti plotting parameters
  n_j = (*Info.TauProfile).n_Molecule_Sets
  Info.iX = LONG(SQRT(DOUBLE(n_j)))
  Info.iY = Info.iX
  IF ( Info.iX*Info.iY LT n_j ) THEN Info.iX = Info.iX + 1L
  IF ( Info.iX*Info.iY LT n_j ) THEN Info.iY = Info.iY + 1L

  ; Save top level base info state
  TauProfile_SetState, ID, Info

  ; Set the new slider values
  ; -------------------------
  ; Channel display
  IF ( (*Info.TauProfile).n_Channels GT 1 ) THEN BEGIN
    WIDGET_CONTROL, Info.Channel_Slider_ID, $
                    SET_SLIDER_MIN = 1, $
                    SET_SLIDER_MAX = (*Info.TauProfile).n_Channels, $
                    SET_VALUE = 1, $
                    SENSITIVE = 1
  ENDIF ELSE BEGIN
    WIDGET_CONTROL, Info.Channel_Slider_ID, $
                    SET_SLIDER_MIN = 0, $
                    SET_SLIDER_MAX = 1, $
                    SET_VALUE = 1, $
                    SENSITIVE = 0
  ENDELSE

  ; Zenith angle display
  IF ( (*Info.TauProfile).n_Angles GT 1 ) THEN BEGIN
    WIDGET_CONTROL, Info.Angle_Slider_ID, $
                    SET_SLIDER_MIN = 1, $
                    SET_SLIDER_MAX = (*Info.TauProfile).n_Angles, $
                    SET_VALUE = 1, $
                    SENSITIVE = 1
  ENDIF ELSE BEGIN
    WIDGET_CONTROL, Info.Angle_Slider_ID, $
                    SET_SLIDER_MIN = 0, $
                    SET_SLIDER_MAX = 1, $
                    SET_VALUE = 1, $
                    SENSITIVE = 0
  ENDELSE

  ; Profile display
  IF ( n_Profiles GT 1 ) THEN BEGIN
    WIDGET_CONTROL, Info.Profile_Slider_ID, $
                    SET_SLIDER_MIN = 1, $
                    SET_SLIDER_MAX = n_Profiles, $
                    SET_VALUE = 1, $
                    SENSITIVE = 1
  ENDIF ELSE BEGIN
    WIDGET_CONTROL, Info.Profile_Slider_ID, $
                    SET_SLIDER_MIN = 0, $
                    SET_SLIDER_MAX = 1, $
                    SET_VALUE = 1, $
                    SENSITIVE = 0
  ENDELSE


  ; Sensitise the appropriate buttons
  ; ---------------------------------
  ; The Up/Down-welling button
  WIDGET_CONTROL, Info.UpDown_Button_ID, $
                  SENSITIVE=1

  ; The Log/Lin pressure axis button
  WIDGET_CONTROL, Info.LogLin_Button_ID, $
                  SENSITIVE=1

  ; The plot symbol button
  WIDGET_CONTROL, Info.PSym_Button_ID, $
                  SENSITIVE=1

  ; The weighting fn button
  WIDGET_CONTROL, Info.WgtFn_Button_ID, $
                  SENSITIVE=1


  ; Display the new data
  ; --------------------
  TauProfile_Display, ID
END


;######################################
PRO TauProfile_GUI, File, Debug = Debug
;######################################

  ; Set up
  ; ------
  ; Set compilation options
  COMPILE_OPT STRICTARR
  
  ; Compile TauProfile structure routines
  dummy = {TauProfile}

  ; Check arguments
  IF ( NOT KEYWORD_SET( Debug ) ) THEN BEGIN
    Debug = 0 
  ENDIF ELSE BEGIN
    Debug = 1
    PRINT, 'TauProfile_GUI'
  ENDELSE



  ;#----------------------------------------------------------------------------#
  ;#                       -- CREATE THE WIDGET DISPLAY --                      #
  ;#----------------------------------------------------------------------------#

  Map = 1


  ; -------------------------
  ; Create the top level base
  ; -------------------------

  Top_Level_Base_ID = WIDGET_BASE( COLUMN = 1, $
                                   MAP    = 0, $
                                   MBAR   = Menu_Bar_ID, $
                                   TITLE  = 'TauProfile_GUI' )
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
                                EVENT_PRO = 'TauProfile_Open_Event', $
                                UVALUE = 'Open' )
  File_Print_ID = WIDGET_BUTTON( File_Menu_ID, $
                                 VALUE = 'Print', $
                                 EVENT_PRO = 'TauProfile_Print_Event', $
                                 UVALUE = 'Print' )
  File_Exit_ID = WIDGET_BUTTON( File_Menu_ID, $
                                VALUE = 'Exit', $
                                EVENT_PRO = 'TauProfile_Exit_Event', $
                                /SEPARATOR, $
                                UVALUE = 'Exit' )


  ; ----------------------
  ; Create the draw widget
  ; ----------------------

  xSize = 1400
  ySize = 850
  Draw_Widget_ID = WIDGET_DRAW( Top_Level_Base_ID, $
                                GROUP_LEADER = Top_Level_Base_ID, $
                                XSIZE = xSize, YSIZE = ySIZE )


  ; -----------------------
  ; Create the control base
  ; -----------------------

  Control_Base_ID = WIDGET_BASE( Top_Level_Base_ID, $
                                 GROUP_LEADER = Top_Level_Base_ID, $
                                 FRAME = 2, $
                                 MAP = Map, $
                                 ROW = 1 )

  ; ----------------------
  ; Create the slider base
  ; ----------------------

  Slider_Base_ID = WIDGET_BASE( Control_Base_ID, $
                                GROUP_LEADER = Top_Level_Base_ID, $
                                FRAME = 2, $
                                MAP = Map, $
                                COLUMN = 1 )

  ; Create the channel slider
  Channel_Slider_ID = WIDGET_SLIDER( Slider_Base_ID, $
                                     GROUP_LEADER = Top_Level_Base_ID, $
                                     DRAG = 1, $
                                     EVENT_PRO = 'TauProfile_Slider_Event', $
                                     MINIMUM = 0, $
                                     MAXIMUM = 1, $
                                     SENSITIVE = 0, $
                                     TITLE = 'Channel', $
                                     UVALUE = 'Channel', $
                                     XSIZE = xSize/2 )

  ; Create the angle slider
  Angle_Slider_ID = WIDGET_SLIDER( Slider_Base_ID, $
                                   GROUP_LEADER = Top_Level_Base_ID, $
                                   DRAG = 1, $
                                   EVENT_PRO = 'TauProfile_Slider_Event', $
                                   MINIMUM = 0, $
                                   MAXIMUM = 1, $
                                   SENSITIVE = 0, $
                                   TITLE = 'Angle index', $
                                   UVALUE = 'Angle', $
                                   XSIZE = xSize/2 )

  ; Create the profile slider
  Profile_Slider_ID = WIDGET_SLIDER( Slider_Base_ID, $
                                     GROUP_LEADER = Top_Level_Base_ID, $
                                     DRAG = 0, $
                                     EVENT_PRO = 'TauProfile_Profile_Slider_Event', $
                                     MINIMUM = 0, $
                                     MAXIMUM = 1, $
                                     SENSITIVE = 0, $
                                     TITLE = 'Profile index', $
                                     UVALUE = 'Profile', $
                                     XSIZE = xSize/2 )


  ; ----------------------------------
  ; Create the various button controls
  ; ----------------------------------

  ButtonControl_Base_ID = WIDGET_BASE( Control_Base_ID, $
                                       GROUP_LEADER = Top_Level_Base_ID, $
                                       FRAME = 2, $
                                       MAP = Map, $
                                       COLUMN = 1 )

  UpDown_Button_ID = WIDGET_BUTTON( ButtonControl_Base_ID, $
                                    EVENT_PRO = 'TauProfile_UpDown_Button_Event', $
                                    SENSITIVE = 0, $
                                    VALUE = 'Up/Down', $
                                    UVALUE = 'UpDown' )

  LogLin_Button_ID = WIDGET_BUTTON( ButtonControl_Base_ID, $
                                    EVENT_PRO = 'TauProfile_LogLin_Button_Event', $
                                    SENSITIVE = 0, $
                                    VALUE = 'Log/Linear', $
                                    UVALUE = 'LogLin' )

  PSym_Button_ID = WIDGET_BUTTON( ButtonControl_Base_ID, $
                                  EVENT_PRO = 'TauProfile_PSym_Button_Event', $
                                  SENSITIVE = 0, $
                                  VALUE = 'Symbol', $
                                  UVALUE = 'PSym' )

  WgtFn_Button_ID = WIDGET_BUTTON( ButtonControl_Base_ID, $
                                   EVENT_PRO = 'TauProfile_WgtFn_Button_Event', $
                                   SENSITIVE = 0, $
                                   VALUE = 'Tau/WgtFn', $
                                   UVALUE = 'WgtFn' )


  ; --------------------------------------------
  ; Map, update and realise the widget heirarchy
  ; --------------------------------------------

  WIDGET_CONTROL, Top_Level_Base_ID, MAP     = 1, $
                                     REALIZE = 1, $
                                     UPDATE  = 1

  ; --------------------------------
  ; Obtain the draw widget window ID
  ; --------------------------------

  WIDGET_CONTROL, Draw_Widget_ID, GET_VALUE = Draw_Window_ID



  ;#----------------------------------------------------------------------------#
  ;#                     -- LOAD THE INFORMATION STRUCTURE --                   #
  ;#----------------------------------------------------------------------------#

  Info = { Debug             : Debug, $
           PMulti            : !P.MULTI, $
           iX                : 0L, $
           iY                : 0L, $
           Draw_Widget_ID    : Draw_Widget_ID, $
           Draw_Window_ID    : Draw_Window_ID, $
           Channel_Slider_ID : Channel_Slider_ID, $
           Angle_Slider_ID   : Angle_Slider_ID, $
           Profile_Slider_ID : Profile_Slider_ID, $
           UpDown_Button_ID  : UpDown_Button_ID, $
           Pressure          : PTR_NEW(), $
           LogLin_Button_ID  : LogLin_Button_ID, $
           yLog              : 1L, $
           yTickFormat       : 'logticks', $
           PSym_Button_ID    : PSym_Button_ID, $
           pSym              : 0, $
           WgtFn_Button_ID   : WgtFn_Button_ID, $
           WgtFn             : 0, $
           Filename          : ' ', $
           Profile_List      : PTR_NEW(), $
           TauProfile        : PTR_NEW() }

  InfoPtr = PTR_NEW( Info )
  WIDGET_CONTROL, Top_Level_Base_ID, SET_UVALUE = InfoPtr



  ;#----------------------------------------------------------------------------#
  ;#                             -- DISPLAY A FILE  --                          #
  ;#----------------------------------------------------------------------------#

  IF ( Valid_String( File ) ) THEN BEGIN

    TauProfile_Load_File, File, Top_Level_Base_ID

  ENDIF ELSE BEGIN

    XYOUTS, 0.5, 0.5, $
            'Use File->Open to select a file', $
            /NORM, ALIGNMENT = 0.5, $
            CHARSIZE = 2.0

  ENDELSE



  ;#----------------------------------------------------------------------------#
  ;#                           -- START THE XMANAGER --                         #
  ;#----------------------------------------------------------------------------#

  XMANAGER, 'TauProfile_GUI', Top_Level_Base_ID, $
            CLEANUP = 'TauProfile_GUI_Cleanup', $
            GROUP_LEADER = Top_Level_Base_ID

END
