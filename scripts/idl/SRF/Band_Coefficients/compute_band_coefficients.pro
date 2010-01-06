;+
; NAME:
;       Compute_Band_Coefficients
;
; PURPOSE:
;       Compute the polychromatic band correction coefficients for an
;       input SRF structure.
;
; CALLING SEQUENCE:
;       Result = Compute_Band_Coefficients( SRF         , $  ; Input SRF structure
;                                           Coefficients, $  ; Output coefficient array
;                                           Order=Order , $  ; Input keyword
;                                           NoPlot=NoPlot )  ; Input keyword
;
; INPUT ARGUMENTS:
;       SRF:          Spectral Response Function data structure.
;                     UNITS:      N/A
;                     TYPE:       {SRF}
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT(IN)
;
; OUTPUT ARGUMENTS:
;       Coefficients: Array of polychromatic band correction coefficients
;                     that are applied to temperatures. The size of the
;                     array is Order+1 where Order is the degree of the
;                     polynomial fit. (See the Order keyword description).
;                     UNITS:      N/A
;                     TYPE:       REAL
;                     DIMENSION:  Rank-1
;                     ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORD PARAMETERS:
;       Order:        The degree of the fitting polynomial. If not specified
;                     the default fit is linear (Order=1).
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       NoPlot:       Set this keyword to suppress output plots.
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       Result:       The return value is an integer defining the error
;                     status. The error codes are defined in the error_codes
;                     include file.
;                     If == SUCCESS the computation were sucessful
;                        == FAILURE an unrecoverable error occurred
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;
; INCLUDE FILES:
;       error_codes:  Include file containing error code definitions.
;       color_db:     Include file containing colour database definitions.
;
; EXAMPLE:
;       After reading in a channel SRF from file, for example channel 5
;       of the NOAA-17 AVHRR/3,
;
;         IDL> Result = Read_SRF_netCDF('avhrr3_n17.srf.nc', 5, srf)
;         IDL> HELP, srf
;         SRF             STRUCT    = -> SRF Array[1]
;
;       the band coefficients can be computed by,
;
;         IDL> Result = Compute_Band_Coefficients(SRF,a)
;         IDL> PRINT, a
;               0.24414808
;               0.99908233
;
;       Results for a higher order fit are obtained via the Order keyword,
;
;         IDL> Result = Compute_Band_Coefficients(SRF,a,Order=2)
;         IDL> PRINT, a
;               0.36662515
;               0.99810552
;            1.8784826e-06
;
;       Given an actual temperature, T, the coefficients are used to determine
;       an effective temperature, Teff, for the broadband channel like so,
;
;         Teff = a[0] + a[1]*T + a[2]*T^2 + .... + a[N]*T^N
;
;       where N == Order+1.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION Compute_Band_Coefficients, SRF          , $  ; Input SRF structure
                                    Coefficients , $  ; Output coefficient array
                                    Order=Order  , $  ; Input keyword
                                    NoPlot=NoPlot, $  ; Input keyword
                                    f0=f0        , $  ; In/Output keyword
                                    y=y          , $  ; Output keyword
                                    fit=fit           ; Output keyword

  ; Setup
  ; -----
  ; Error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF
  
  ; Plotting setup
  @color_db
  charsize = (!D.NAME EQ 'PS') ? 2.0 : 2.0
  font     = (!D.NAME EQ 'PS') ? 1 : -1
  thick    = (!D.NAME EQ 'PS') ? 2 :  1
  Delta    = (!D.NAME EQ 'PS') ? '!9D!X' : '!4D!X'
  lcharsize = charsize/1.75

  ; Default is linear fit
  IF ( N_ELEMENTS(Order) EQ 0 ) THEN Order=1
  
  ; Define test temperatures
  tmin = 180.0d0
  tmax = 340.0d0
  nt = 17L
  t = dindgen(nt)/double(nt-1L)
  t = t*(tmax-tmin) + tmin
  
    
  ; Compute the Y(t) vector
  ; -----------------------
  ; Compute the central frequency
  IF ( N_ELEMENTS(f0) EQ 0 ) THEN $
    f0 = Integral(*SRF.Frequency,*SRF.Frequency*(*SRF.Response))/SRF.Integrated_SRF
  ; Effective temperature loop
  y = DBLARR(nt)
  FOR i = 0L, nt-1L DO BEGIN
    ; Compute the Planck radiance spectrum
    result = Planck_Radiance(*SRF.Frequency,t[i],r)
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error computing Planck radiance for '+SRF.Sensor_Id+' channel '+ $
               STRTRIM(SRF.Channel,2)+' frequencies', /NONAME, /NOPRINT
    ; Compute the convolved radiance
    ir = Integral(*SRF.Frequency,r*(*SRF.Response))/SRF.Integrated_SRF
    ; Compute the effective temperature
    result = Planck_Temperature(f0,ir,efft)
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error computing effective Planck temperature for '+SRF.Sensor_Id+' channel '+ $
               STRTRIM(SRF.Channel,2), /NONAME, /NOPRINT
    ; Save it.
    y[i] = efft
  ENDFOR


  ; Fit a polynomial to the data
  ; ----------------------------
  Coefficients = REFORM(POLY_FIT(t,y,Order,YFIT=fit))


  ; Plot some stuff
  ; ---------------
  IF ( ~KEYWORD_SET(NoPlot) ) THEN BEGIN
    psave = !P
    !P.MULTI=[0,1,3]
    ; Set default display window
    IF ( !D.NAME EQ 'X') THEN BEGIN
      xSize=800
      ySize=900
      winID = !D.WINDOW
      IF ( winID EQ -1 ) THEN winID=0
      IF ( !D.X_SIZE NE xSize OR !D.Y_SIZE NE ySize ) THEN $
        WINDOW, winID, XSIZE=xSize, YSIZE=ySize
    ENDIF
    ; The SRF itself
    PLOT, *SRF.Frequency,*SRF.Response, $
          TITLE=SRF.Sensor_Id+' channel '+STRTRIM(SRF.Channel,2)+' SRF', $
          XTITLE='Frequency (cm!U-1!N)', $
          YTITLE='Relative response', $
          CHARSIZE=charsize,FONT=font,THICK=thick
    ; The temperature data and fit
    PLOT, t, y, $
          /YNOZERO,$
          TITLE='Polynomial fit to effective temperature, order='+STRTRIM(Order,2), $
          XTITLE='Temperature (K)', $
          YTITLE='Effective!CTemperature (K)', $
          CHARSIZE=charsize,FONT=font,THICK=thick
    OPLOT, t, fit, $
           COLOR=RED,PSYM=4,THICK=thick
    mylegend, 0.75, 0.45, ['Temperature','Fitted temperature'],$
             color=[!P.COLOR,RED], PSYM=[0,4], CHARSIZE=lcharsize
    ; The fit residual
    PLOT, t, y-fit,$
          TITLE='Residual for order='+STRTRIM(Order,2)+' fit',$
          XTITLE='Temperature (K)', $
          YTITLE=Delta+'T (K)', $
          CHARSIZE=charsize,FONT=font,THICK=thick
    OPLOT, !X.CRANGE,[0,0],LINESTYLE=2,THICK=thick
    !P = psave
  ENDIF


  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS

END
