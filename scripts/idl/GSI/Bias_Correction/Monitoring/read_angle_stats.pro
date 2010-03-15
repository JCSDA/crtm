FUNCTION Read_Angle_Stats, Angle_Stats_File, $  ; Input
                           n_FOVS,           $  ; Input
                           n_Channels,       $  ; Input
                           Angle_Stats,      $  ; Output
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
 
  n_Arguments = 4
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT
 
  IF ( NOT Valid_String( Angle_Stats_File ) ) THEN $
    MESSAGE, 'Input Angle_Stats_File argument not defined!', $
             /NONAME, /NOPRINT

  IF ( N_ELEMENTS( n_FOVS ) EQ 0 ) THEN $
    MESSAGE, 'Input n_FOVS argument not defined!', $
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
 
  Angle_Stats = { FixAng:     FLTARR( n_FOVS, n_Channels, N_ANGLE_TIMES ),    $
                  TimeAng:    FLTARR( n_FOVS, n_Channels, N_ANGLE_TIMES ),    $
                  Count:      FLTARR( n_FOVS, n_Channels, N_REGIONS, N_ANGLE_TIMES ),    $
                  Penalty:    FLTARR( n_FOVS, n_Channels, N_REGIONS, N_ANGLE_TIMES ),    $
                  OmG_NBC:    FLTARR( n_FOVS, n_Channels, N_REGIONS, N_STATS, N_ANGLE_TIMES ), $
                  Tot_Cor:    FLTARR( n_FOVS, n_Channels, N_REGIONS, N_STATS, N_ANGLE_TIMES ), $
                  OmG_BC:     FLTARR( n_FOVS, n_Channels, N_REGIONS, N_STATS, N_ANGLE_TIMES ), $
                  FixAng_Cor: FLTARR( n_FOVS, n_Channels, N_REGIONS, N_STATS, N_ANGLE_TIMES ), $
                  Lapse_Cor:  FLTARR( n_FOVS, n_Channels, N_REGIONS, N_STATS, N_ANGLE_TIMES ), $
                  Lapse2_Cor: FLTARR( n_FOVS, n_Channels, N_REGIONS, N_STATS, N_ANGLE_TIMES ), $
                  Const_Cor:  FLTARR( n_FOVS, n_Channels, N_REGIONS, N_STATS, N_ANGLE_TIMES ), $
                  ScAngl_Cor: FLTARR( n_FOVS, n_Channels, N_REGIONS, N_STATS, N_ANGLE_TIMES ), $
                  CLW_Cor:    FLTARR( n_FOVS, n_Channels, N_REGIONS, N_STATS, N_ANGLE_TIMES ) }
                                  

  ;#------------------------------------------------------------------------------#
  ;#                              -- READ THE DATA --                             #
  ;#------------------------------------------------------------------------------#

  GET_LUN, FileID
  OPENR, FileID, $
         Angle_Stats_File, $
         /F77_UNFORMATTED, $
         SWAP_ENDIAN = Swap


  ; ------------------------------
  ; Create dummy array for reading
  ; ------------------------------

  Dummy = FLTARR( n_FOVS, n_Channels )


  ; ---------------------------
  ; Begin loop over angle times
  ; ---------------------------

  FOR m = 0L, N_ANGLE_TIMES-1L DO BEGIN


    ; -----------------------
    ; FixAng and TimeAng data
    ; -----------------------

    READU, FileID, Dummy
    Angle_Stats.FixAng[*,*,m] = Dummy

    READU, FileID, Dummy
    Angle_Stats.TimeAng[*,*,m] = Dummy


    ; --------------------------
    ; The count and penalty data
    ; --------------------------

    FOR k = 0L, N_REGIONS-1L DO BEGIN
      READU, FileID, Dummy
      Angle_Stats.Count[*,*,k,m] = Dummy
    ENDFOR

    FOR k = 0L, N_REGIONS-1L DO BEGIN
      READU, FileID, Dummy
      Angle_Stats.Penalty[*,*,k,m] = Dummy
    ENDFOR

    ; --------------
    ; The statistics
    ; --------------

    FOR l = 0L, N_STATS-1L DO BEGIN
      FOR k = 0L, N_REGIONS-1L DO BEGIN
        READU, FileID, Dummy
        Angle_Stats.OmG_NBC[*,*,k,l,m] = Dummy
      ENDFOR
      FOR k = 0L, N_REGIONS-1L DO BEGIN
        READU, FileID, Dummy
        Angle_Stats.Tot_Cor[*,*,k,l,m] = Dummy
      ENDFOR
      FOR k = 0L, N_REGIONS-1L DO BEGIN
        READU, FileID, Dummy
        Angle_Stats.OmG_BC[*,*,k,l,m] = Dummy
      ENDFOR

; -- Not implemented yet
;      FOR k = 0L, N_REGIONS-1L DO BEGIN
;        READU, FileID, Dummy
;        Angle_Stats.FixAng_Cor[*,*,k,l,m] = Dummy
;      ENDFOR
;      FOR k = 0L, N_REGIONS-1L DO BEGIN
;        READU, FileID, Dummy
;        Angle_Stats.Lapse_Cor[*,*,k,l,m] = Dummy
;      ENDFOR
;      FOR k = 0L, N_REGIONS-1L DO BEGIN
;        READU, FileID, Dummy
;        Angle_Stats.Lapse2_Cor[*,*,k,l,m] = Dummy
;      ENDFOR
;      FOR k = 0L, N_REGIONS-1L DO BEGIN
;        READU, FileID, Dummy
;        Angle_Stats.Const_Cor[*,*,k,l,m] = Dummy
;      ENDFOR
;      FOR k = 0L, N_REGIONS-1L DO BEGIN
;        READU, FileID, Dummy
;        Angle_Stats.ScAngl_Cor[*,*,k,l,m] = Dummy
;      ENDFOR
;      FOR k = 0L, N_REGIONS-1L DO BEGIN
;        READU, FileID, Dummy
;        Angle_Stats.CLW_Cor[*,*,k,l,m] = Dummy
;      ENDFOR
    ENDFOR

  ENDFOR

  FREE_LUN, FileID


  ;#------------------------------------------------------------------------------#
  ;#                                  -- DONE --                                  # 
  ;#------------------------------------------------------------------------------#

  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Read_Angle_Stats
