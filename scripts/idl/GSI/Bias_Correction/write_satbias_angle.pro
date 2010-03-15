PRO Write_SatBias_Angle, File, SatBias, Zero = Zero

  x = SatBias

  IF ( KEYWORD_SET( Zero ) ) THEN $
    SatBias.Bias = 0.0

  nChannels = N_ELEMENTS( SatBias )

  GET_LUN, lun
  OPENW, lun, File, WIDTH = 200

  FOR l = 0, nChannels - 1L DO BEGIN
    PRINTF, lun, FORMAT = '( "channel = ",3i5,e15.6/9(4x,10f7.3/))', $
           SatBias[l].Channel_Index,      $
           SatBias[l].Instrument_ID,      $
           SatBias[l].Instrument_Channel, $
           SatBias[l].Mean_Lapse_Rate,    $
           SatBias[l].Bias              

  ENDFOR

  FREE_LUN, lun



END
