PRO Plot_SatBias_Angle, SatBias_Angle, sensor_id

  RED=5
  GREEN=4
  
  idx=WHERE(STRTRIM(SatBias_Angle.sensor_id,2) EQ sensor_id,count)
  IF(count EQ 0) THEN $
    MESSAGE, "SatBias_Angle structure does not contain "+sensor_id+" entries."

  nChannels=count
  i=LINDGEN(SatBias_Angle[idx[0]].nFOVs)
  
  FOR l = idx[0], idx[nChannels-1L] DO BEGIN
    yRange=[MIN(SatBias_Angle[l].Bias[i]),MAX(SatBias_Angle[l].Bias[i])]
    PLOT, i+1, SatBias_Angle[l].Bias[i], $
          TITLE = STRTRIM(Sensor_Id, 2) + $
                  ' channel ' + STRTRIM( SatBias_Angle[l].Channel, 2 ), $
          XTITLE = 'FOV position', $
          YTITLE = 'Bias (K)', $
          YRANGE=yRange, $
          /NODATA
    OPLOT, i+1, SatBias_Angle[l].Bias[i], $
           PSYM=-6, $
           COLOR=RED
    OPLOT, !X.CRANGE, [SatBias_Angle[l].Bias_Mean,SatBias_Angle[l].Bias_Mean], $
           COLOR=RED, $
           LINESTYLE=2
    OPLOT, !X.CRANGE, [0,0], $
           LINESTYLE=2
    q = GET_KBRD(1)
    IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
  ENDFOR

END
