;+
PRO Compare_SatBias_Angle, s1             , $ ; First  SatBias_Angle structure
                           s2             , $ ; Second SatBias_Angle structure
                           Sensor_Id      , $ ; Sensor ID to display
                           Diff=Diff      , $ ; Set this keyword to plot the s1-s2 differences
                           NoPause=NoPause    ; Set this keyword to not pause between channel plots
;-

  NoDiff=1
  IF ( KEYWORD_SET(Diff) ) THEN NoDiff=0

  Pause=1
  IF ( KEYWORD_SET(NoPause) ) THEN Pause=0
  
  RED=5
  GREEN=4

  idx1=WHERE(STRTRIM(s1.sensor_id,2) EQ sensor_id,count)
  IF(count EQ 0) THEN $
    MESSAGE, "SatBias_Angle structure #1 does not contain "+sensor_id+" entries."

  idx2=WHERE(STRTRIM(s2.sensor_id,2) EQ sensor_id,count)
  IF(count EQ 0) THEN $
    MESSAGE, "SatBias_Angle structure #2 does not contain "+sensor_id+" entries."

  nChannels=count
  i=LINDGEN(s1[idx1[0]].nFOVs)
  
  FOR l = idx1[0], idx1[nChannels-1L] DO BEGIN
    IF ( NoDiff EQ 1 ) THEN BEGIN
      yRange=[MIN(s1[l].Bias[i])<MIN(s2[l].Bias[i]), $
              MAX(s1[l].Bias[i])>MAX(s2[l].Bias[i])  ]
      PLOT, i+1, s1[l].Bias[i], $
            TITLE = STRTRIM(Sensor_Id, 2) + $
                    ' channel ' + STRTRIM( s1[l].Channel, 2 ), $
            XTITLE = 'FOV position', $
            YTITLE = 'Bias (K)', $
            YRANGE=yRange, $
            /NODATA
      OPLOT, i+1, s1[l].Bias[i], $
             PSYM=-6, $
             COLOR=RED
      OPLOT, !X.CRANGE, [s1[l].Bias_Mean,s1[l].Bias_Mean], $
             COLOR=RED, $
             LINESTYLE=1
      OPLOT, i+1, s2[l].Bias[i], $
             PSYM=-4, $
             COLOR=GREEN
      OPLOT, !X.CRANGE, [s2[l].Bias_Mean,s2[l].Bias_Mean], $
             COLOR=GREEN, $
             LINESTYLE=2
    ENDIF ELSE BEGIN
      d = s1[l].Bias[i]-s2[l].Bias[i]
      yRange=[MIN(d), MAX(d)]
      PLOT, i+1, d, $
            TITLE = STRTRIM(Sensor_Id, 2) + $
                    ' channel ' + STRTRIM( s1[l].Channel, 2 ) + $
                    ' d(bias)', $
            XTITLE = 'FOV position', $
            YTITLE = 'd(Bias) (K)', $
            YRANGE=yRange
    ENDELSE
    OPLOT, !X.CRANGE, [0,0], $
           LINESTYLE=2

    IF( Pause EQ 1 ) THEN BEGIN
      q = GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    ENDIF
  ENDFOR

END
