FUNCTION Read_BCoef_Stats, BCoef_Stats_File, $  ; Input
                           n_Channels,       $  ; Input
                           BCoef_Stats,      $  ; Output
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
  ;#                             -- INCLUDE FILES --                              # 
  ;#------------------------------------------------------------------------------#

  @monitoring_parameters


  ;#------------------------------------------------------------------------------#
  ;#                              -- CHECK INPUT --                               #
  ;#------------------------------------------------------------------------------#
 
  n_Arguments = 3
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT
 
  IF ( NOT Valid_String( BCoef_Stats_File ) ) THEN $
    MESSAGE, 'Input BCoef_Stats_File argument not defined!', $
             /NONAME, /NOPRINT

  IF ( N_ELEMENTS( n_Channels ) EQ 0 ) THEN $
    MESSAGE, 'Input n_Channels argument not defined!', $
             /NONAME, /NOPRINT
 

  ;#------------------------------------------------------------------------------#
  ;#                        -- DEFINE RETURN STRUCTURE --                         #
  ;#------------------------------------------------------------------------------#
 
  BCoef_Stats = { Penalty:      FLTARR( n_Channels ),                        $
                  Coefficients: FLTARR( n_Channels, N_AIRMASS_COEFFICIENTS ) }
                                  

  ;#------------------------------------------------------------------------------#
  ;#                              -- READ THE DATA --                             #
  ;#------------------------------------------------------------------------------#

  GET_LUN, FileID
  OPENR, FileID, $
         BCoef_Stats_File, $
         /F77_UNFORMATTED, $
         SWAP_ENDIAN = Swap


  ; ----------------
  ; The penalty data
  ; ----------------

  Dummy = FLTARR( n_Channels )

  READU, FileID, Dummy
  BCoef_Stats.Penalty = Dummy


  ; -------------------------------------
  ; Read the bias correction coefficients
  ; -------------------------------------

  FOR i = 0L, N_AIRMASS_COEFFICIENTS-1L DO BEGIN
    READU, FileID, Dummy
    BCoef_Stats.Coefficients[*,i] = Dummy
  ENDFOR

  FREE_LUN, FileID


  ;#------------------------------------------------------------------------------#
  ;#                                  -- DONE --                                  # 
  ;#------------------------------------------------------------------------------#

  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Read_BCoef_Stats
