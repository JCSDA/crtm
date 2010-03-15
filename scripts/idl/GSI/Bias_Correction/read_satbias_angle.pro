;+
FUNCTION Read_SatBias_Angle, File, $
                             Display=Display
;-

  HEADER_FMTSTRING = '(i5,1x,a20,1x,i5,e15.6,:,e15.6,i5)'
  BIAS_FMTSTRING   = '(9(4x,10f7.3/))'

  maxFOVs = 90L

  Channel_Index  = 0L
  Sensor_Id      = " "
  Channel        = 0L
  LapseRate_Mean = 0.0
  Bias_Mean      = 0.0
  nFOVS          = maxFOVs
  Bias           = FLTARR( maxFOVs )

  x = { SatBias_Angle                  , $
        Channel_Index  : Channel_Index , $
        Sensor_Id      : Sensor_Id     , $
        Channel        : Channel       , $
        LapseRate_Mean : LapseRate_Mean, $
        Bias_Mean      : Bias_Mean     , $
        nFOVs          : nFOVS         , $
        Bias           : Bias            }

  MAX_N_CHANNELS = 1000L
  SatBias_Angle = REPLICATE( x, MAX_N_CHANNELS )

  GET_LUN, lun
  OPENR, lun, File, WIDTH=200

  l = -1L
  WHILE ( NOT EOF(lun) ) DO BEGIN
    l++

    ; Read the entry header
    READF, lun, FORMAT=HEADER_FMTSTRING, $
           Channel_Index , $
           Sensor_Id     , $
           Channel       , $
           LapseRate_Mean, $
           Bias_Mean     , $
           nFOVs
    ; Save the entry header
    SatBias_Angle[l].Channel_Index  = Channel_Index
    SatBias_Angle[l].Sensor_Id      = Sensor_Id
    SatBias_Angle[l].Channel        = Channel
    SatBias_Angle[l].LapseRate_Mean = LapseRate_Mean
    SatBias_Angle[l].Bias_Mean      = Bias_Mean
    SatBias_Angle[l].nFOVs          = nFOVs

    ; Read the bias data
    READF, lun, FORMAT=BIAS_FMTSTRING, $
           Bias
    SatBias_Angle[l].Bias = Bias
  ENDWHILE

  ; Close file
  FREE_LUN, lun

  ; Trim the structure array
  nChannels = l+1
  SatBias_Angle = SatBias_Angle[0:l]

  ; Display data if required
  IF ( KEYWORD_SET( Display ) ) THEN BEGIN
    FOR l = 0, nChannels - 1L DO BEGIN
      i=LINDGEN(SatBias_Angle[l].nFOVs)
      PLOT, i+1, SatBias_Angle[l].Bias[i], $
            TITLE = STRTRIM(SatBias_Angle[l].Sensor_Id, 2) + $
                    ' channel ' + STRTRIM( SatBias_Angle[l].Channel, 2 ), $
            XTITLE = 'FOV position', $
            YTITLE = 'Bias (K)', $
            /YNOZERO, PSYM=-4
      q = GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN GOTO, Done
    ENDFOR

  ENDIF

  Done:

  RETURN, SatBias_Angle

END
