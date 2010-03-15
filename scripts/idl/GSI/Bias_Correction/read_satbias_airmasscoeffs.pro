FUNCTION Read_SatBias_AirMassCoeffs, File

  nCoeffs = 5L

  Channel_Index      = 0L
  Instrument_ID      = 0L
  Instrument_Channel = 0L
  Coefficient        = FLTARR( nCoeffs )

  x = { SatBias_AirMassCoeffs, $
        Channel_Index     : Channel_Index,      $
        Instrument_ID     : Instrument_ID,      $
        Instrument_Channel: Instrument_Channel, $
        Coefficient       : Coefficient         }

  MAX_N_CHANNELS = 1000L
  AirMassCoeffs = REPLICATE( x, MAX_N_CHANNELS )

  GET_LUN, lun
  OPENR, lun, File, WIDTH = 200

  l = -1L
  WHILE ( NOT EOF(lun) ) DO BEGIN
    l++
    READF, lun, Channel_Index,      $
                Instrument_ID,      $
                Instrument_Channel, $
                Coefficient

    AirMassCoeffs[l].Channel_Index      = Channel_Index
    AirMassCoeffs[l].Instrument_ID      = Instrument_ID
    AirMassCoeffs[l].Instrument_Channel = Instrument_Channel
    AirMassCoeffs[l].Coefficient        = Coefficient

  ENDWHILE

  FREE_LUN, lun

  nChannels = l+1
  AirMassCoeffs = AirMassCoeffs[0:l]

  RETURN, AirMassCoeffs

END
