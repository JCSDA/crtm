FUNCTION Remove_SatBias_Angle_Mean, sIn
  sOut = sIn
  i1=0
  FOR l = 0, N_ELEMENTS(sOut)-1L DO BEGIN
    i2=sOut[l].nFOVs-1L
    sOut[l].Bias[i1:i2] = sOut[l].Bias[i1:i2] - sOut[l].Bias_Mean
    sOut[l].Bias_Mean = 0.0
  ENDFOR
  RETURN, sOut
END
