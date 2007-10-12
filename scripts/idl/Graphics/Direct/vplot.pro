FUNCTION wticks, axis, index, value

  wavelength = 10000.0d / value

  exponent   = LONG( ALOG10( wavelength ) )

  CASE 1 OF

    ; -- Exponent is less than zero ->
    ; -- fractional ticklabel
    ( exponent LT 0 ): format = '( f' + $
                                STRTRIM( ABS( exponent ) + 2, 2 ) + $
                                '.' + $
                                STRTRIM( ABS( exponent ), 2 ) + $
                                ' )'

    ; -- Exponent is greater than or = to zero ->
    ; -- whole number ticklabel
    ( exponent GE 0 ): format = '( i' + $
                                STRTRIM( ABS( exponent ) + 1, 2 ) + $
                                ' )'

  ENDCASE

  RETURN, STRING( wavelength, FORMAT = format )

END

PRO vplot, frequency, spectrum, $
           wavelength_tickvalues, $
           XSTYLE  = xstyle, $
           XTITLE  = xtitle, $
           YMARGIN = ymargin, $
           _EXTRA  = extra

  ;#--------------------------------------------------------------------------#
  ;#                      -- Set up an error handler --                       #
  ;#--------------------------------------------------------------------------#

  @error_codes

  CATCH, error_status

  IF ( error_status NE 0 ) THEN BEGIN
    MESSAGE, !ERR_STRING, /CONTINUE
    CATCH, /CANCEL
    RETURN
  ENDIF




  IF ( N_PARAMS() NE 3 ) THEN $
    MESSAGE, 'Usage: vplot, frequency, spectrum, wavelength_ticks, [PLOT keywords]', $
             /NONAME, /NOPRINT

  n_points = N_ELEMENTS( frequency )
  IF ( N_ELEMENTS( spectrum ) NE n_points ) THEN $
    MESSAGE, 'Input FREQUENCY and SPECTRUM arrays are different sizes.', $
             /NONAME, /NOPRINT

  IF ( NOT KEYWORD_SET( xtitle ) ) THEN $
    xtitle = 'Frequency (cm!U-1!N)'

  IF ( NOT KEYWORD_SET( ymargin ) ) THEN $
    ymargin = [ 4, 4 ]

  PLOT, frequency, spectrum, $
        XSTYLE  = 8, $
        XTITLE  = xtitle, $
        YMARGIN = ymargin, $
        _EXTRA  = extra

  xticks = N_ELEMENTS( wavelength_tickvalues ) - 1L
;  IF ( MIN( wavelength_tickvalues ) LT MIN( !X.CRANGE ) )
  AXIS, XAXIS = 1, $
        XRANGE = !X.CRANGE ,$
        XTICKV = 10000.0d / wavelength_tickvalues, $
        XTICKS = N_ELEMENTS( wavelength_tickvalues ) - 1L, $
        XSTYLE = 1, xtickformat = 'wticks', $
        XTITLE = 'Wavelength (um)'

  CATCH, /CANCEL

END
