FUNCTION Read_BCor_Stats, BCor_Stats_File, $  ; Input
                          n_Channels,      $  ; Input
                          BCor_Stats,      $  ; Output
                          Swap = Swap


  ;#------------------------------------------------------------------------------#
  ;#                         -- SET UP ERROR HANDLER --                           #  
  ;#------------------------------------------------------------------------------#

  @error_codes

  CATCH, Error_Status

  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS( FileID ) GT 0 ) THEN FREE_LUN, FileID
    RETURN, FAILURE
  ENDIF


  ;#------------------------------------------------------------------------------#
  ;#                              -- CHECK INPUT --                               #
  ;#------------------------------------------------------------------------------#
 
  n_Arguments = 3
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT
 
  IF ( NOT Valid_String( BCor_Stats_File ) ) THEN $
    MESSAGE, 'Input BCor_Stats_File argument not defined!', $
             /NONAME, /NOPRINT

  IF ( N_ELEMENTS( n_Channels ) EQ 0 ) THEN $
    MESSAGE, 'Input n_Channels argument not defined!', $
             /NONAME, /NOPRINT
 

  ;#------------------------------------------------------------------------------#
  ;#                             -- INCLUDE FILES --                              # 
  ;#------------------------------------------------------------------------------#

  @monitoring_parameters


  ;#------------------------------------------------------------------------------#
  ;#                        -- DEFINE RETURN STRUCTURE --                         #
  ;#------------------------------------------------------------------------------#
 
  BCor_Stats = { Count:      FLTARR( n_Channels, N_REGIONS ),    $
                 Penalty:    FLTARR( n_Channels, N_REGIONS ),    $
                 Component:  FLTARR( n_Channels, N_REGIONS, N_BIAS_COMPONENTS, N_STATS ) }
                                  

  ;#------------------------------------------------------------------------------#
  ;#                              -- READ THE DATA --                             #
  ;#------------------------------------------------------------------------------#

  GET_LUN, FileID
  OPENR, FileID, $
         BCor_Stats_File, $
         /F77_UNFORMATTED, $
         SWAP_ENDIAN = Swap


  ; --------------------------
  ; The count and penalty data
  ; --------------------------

  Dummy = FLTARR( n_Channels, N_REGIONS )

  READU, FileID, Dummy
  BCor_Stats.Count = Dummy

  READU, FileID, Dummy
  BCor_Stats.Penalty = Dummy


  ; -----------------------------------
  ; Read the bias correction components
  ; -----------------------------------

  FOR j = 0L, N_STATS-1L DO BEGIN
    FOR i = 0L, N_BIAS_COMPONENTS-1L DO BEGIN
      READU, FileID, Dummy
      BCor_Stats.Component[*,*,i,j] = Dummy
    ENDFOR
  ENDFOR

  FREE_LUN, FileID


  ;#------------------------------------------------------------------------------#
  ;#                                  -- DONE --                                  # 
  ;#------------------------------------------------------------------------------#

  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Read_BCor_Stats
