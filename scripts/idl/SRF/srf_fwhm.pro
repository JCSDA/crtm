;+
; NAME:
;       srf_fwhm
;
; PURPOSE:
;       This function returns the full-width at half-maximum (FWHM) of a
;         tabulated spectral response function (SRF).
;
; CATEGORY:
;       Spectral
;
; CALLING SEQUENCE:
;       result = srf_fwhm( x, srf, half_maximum = half_maximum, plot = plot )
;
; INPUTS:
;       x:      Independent variable of the tabulated SRF (usually wavenumber)
;       srf:    Tabulated SRF the FWHM of which is required.
;
; KEYWORD PARAMETERS:
;       half_maximum:   Set this keyword to return the abscissae coordinates of the
;                         half-maximum points.
;       plot:           Set this keyword to plot the SRF and half-maximum points.         
;
; OUTPUTS:
;       - The function returns the FWHM in units of the independent variable x.
;       - If an error occurs, an informational message is printed out and the returned
;         result is -1.
;
; SIDE EFFECTS:
;       No SRF FWHM calculation is performed if an error occurs.
;
; RESTRICTIONS:
;       Requires at least 2 points below the half-maximum value found to provide
;         data for cubic spline fit and interpolation. Linear interpolation is
;         the default
;
; PROCEDURE:
;       The points surrounding the half-maximum levels are found by a simple
;       search.  The precise x-value at the half-maximum levels are found by 
;       linear interpolation (or cubic spline optionally) to these points. The maximum 
;       SRF value is found by linear interpolation (or an optional cubic spline 
;       interpolation) about the maximum value found using the MAX function.
;
; EXAMPLE:
;       To determine the FWHM of a tabulated SRF (v, srf) type:
;
;         fwhm = srf_fwhm( v, srf )
;
;
; MODIFICATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC 11-Jul-1996
;                       paul.vandelst@ssec.wisc.edu
;
;-

FUNCTION srf_fwhm, x, srf, half_maximum = half_maximum, half_max_response=half_max_response, spline_int = spline_int, plot = plot



;------------------------------------------------------------------------------
;                             -- RCS Id keyword --
;------------------------------------------------------------------------------

  rcs_Id = '$Id: srf_fwhm.pro,v 1.6 1999/04/27 15:24:45 paulv Exp $'



;------------------------------------------------------------------------------
;                                 -- Check input --
;------------------------------------------------------------------------------

; ----------------------------------
; Make sure that input x is a vector
; ----------------------------------

  x_size_and_type = SIZE( x )

  IF ( x_size_and_type[ 0 ] NE 1 ) THEN BEGIN
    MESSAGE, 'Input X argument must be a vector', /INFO
    RETURN, -1
  ENDIF


; ------------------------------------
; Make sure that input srf is a vector
; ------------------------------------

  srf_size_and_type = SIZE( srf )

  IF ( srf_size_and_type[ 0 ] NE 1 ) THEN BEGIN
    MESSAGE, 'Input SRF argument must be a vector', /INFO
    RETURN, -1
  ENDIF


; ------------------------------------------------
; Check SRF type and flag user if input is complex
; ------------------------------------------------

  IF ( srf_size_and_type[ 2 ] EQ 6 OR $
       srf_size_and_type[ 2 ] EQ 9 ) THEN $
    MESSAGE, 'Input SRF is a COMPLEX data type. Using the REAL part only for FWHM determination', /INFO

  srf_to_use = ABS( DOUBLE( srf ) )
  

; -------------------------------------------------
; Check that X and SRF have same number of elements
; -------------------------------------------------

  n_srf_pts = srf_size_and_type[ 3 ]

  IF ( n_srf_pts NE x_size_and_type[ 3 ] ) THEN BEGIN
    MESSAGE, 'Input X and SRF vectors have inconsistent lengths', /INFO
    RETURN, -1
  ENDIF



;------------------------------------------------------------------------------
;         -- Find good representation of "true" SRF maximum value --
;------------------------------------------------------------------------------

; --------------------------------
; Define nominal maximum SRF value
; --------------------------------

  srf_max = MAX( srf_to_use, nominal_maximum_location )

 
; -------------------------------------------
; Set # of indices on either side of required
; points to include in interpolation
; -------------------------------------------
  
  offset = 1
  

; --------------------------------------------------------
; Determine actual index positions to use in interpolation
; checking that the points exist.
; --------------------------------------------------------

; -- Left side --

  x1_location = nominal_maximum_location - offset
  
  ;print, x1_location, ( N_ELEMENTS( x ) - 1 )
  IF ( x1_location LT 0 OR x1_location GT ( N_ELEMENTS( x ) - 1 ) ) THEN BEGIN    
    MESSAGE, 'Not enought points on the LEFT side of the SRF maximum!', /INFO
    RETURN, -1
  ENDIF

  x1 = x[ x1_location ]


; -- Right side --

  x2_location = nominal_maximum_location + offset

  IF ( x2_location LT 0 OR x2_location GT ( N_ELEMENTS( x ) - 1 ) ) THEN BEGIN
    MESSAGE, 'Not enought points on the RIGHT side of the SRF maximum!', /INFO
    RETURN, -1
  ENDIF

  x2 = x[ x2_location ]

 
; -----------------------------------------
; Set # of interpolation points (arbitrary)
; -----------------------------------------
 
  n_int_pts = 500L
 
 
; ---------------------------------------
; Calculate interpolation wavenumber grid
; ---------------------------------------
 
  x_int = DINDGEN( n_int_pts ) / DOUBLE( n_int_pts - 1 )
  x_int = x_int * ( x2 - x1 ) + x1
 
 
; ---------------------------------------------------
; Interpolate SRF near maximum to find "true" maximum
; ---------------------------------------------------
  IF(KEYWORD_SET(SPLINE_INT)) THEN BEGIN
    srf_int = SPLINE( x[ x1_location : x2_location ], $
                      srf_to_use[ x1_location : x2_location ], $
                      x_int, 0.001 )
  ENDIF ELSE BEGIN
    srf_int = INTERPOL( srf_to_use[ x1_location : x2_location ], $
                        x[ x1_location : x2_location ], $                      
                        x_int)
  ENDELSE
  
  ;print, srf_to_use[ x1_location : x2_location ]
  srf_max      = MAX( srf_int )
  srf_half_max = srf_max / 2.0d0



;------------------------------------------------------------------------------
;                 -- Find left side half_maximum value --
;------------------------------------------------------------------------------

; ------------------------------------------------- 
; Find first point below the left-side half-maximum
; ------------------------------------------------- 

;  index_end = nominal_maximum_location
;
;  index = WHERE( srf_to_use[ 0 : index_end ] LE srf_half_max, count )
;  IF ( count LE 0 ) THEN BEGIN
;    MESSAGE, 'SRF left-side not sufficiently defined in input SRF. Returning -1', /INFO
;    RETURN, -1
;  ENDIF

   ; find points on either side of the two srf_half_maxes
   above = WHERE(srf_to_use GT srf_half_max)
   n_pts = N_ELEMENTS(above)

   srf_left_section = srf_to_use[above[0]-1:above[0]]
   srf_right_section = srf_to_use[above[n_pts-1]:above[n_pts-1]+1]
   left_section = x[above[0]-1:above[0]]
   right_section = x[above[n_pts-1]:above[n_pts-1]+1]
;   
   IF(KEYWORD_SET(SPLINE_INT)) THEN BEGIN 
     left_hwhm = SPLINE( srf_left_section, left_section, srf_half_max )
   ENDIF ELSE BEGIN
     left_hwhm = INTERPOL( left_section, srf_left_section, srf_half_max )
   ENDELSE
;   
   IF(KEYWORD_SET(SPLINE_INT)) THEN BEGIN  
     right_hwhm = SPLINE( srf_right_section, right_section, srf_half_max )
   ENDIF ELSE BEGIN
     right_hwhm = INTERPOL( right_section, srf_right_section, srf_half_max )
   ENDELSE
   
      
; ------------------------------------
; Interpolate to the true half-maximum
; ------------------------------------
  
;  index_end   = MAX( index )
;  index_begin = index_end - offset
  ;STOP
;  IF ( index_begin LT 0 ) THEN BEGIN
;    MESSAGE, 'Not enought points on the LEFT side of the LEFT SRF half-maximum point!', /INFO
;    RETURN, -1
;  ENDIF
  
;  index_end = index_end + offset
;  IF ( index_end GT ( n_srf_pts - 1 ) ) THEN BEGIN
;    MESSAGE, 'Not enought points on the RIGHT side of the LEFT SRF half-maximum point!', /INFO
;    RETURN, -1
;  ENDIF
;  
 ; srf_section = srf_to_use[ index_begin : index_end ]
 ; x_section   = x[ index_begin : index_end ]
;   
 ; IF(KEYWORD_SET(SPLINE_INT)) THEN BEGIN 
 ;   left_hwhm = SPLINE( srf_section, x_section, srf_half_max )
 ; ENDIF ELSE BEGIN
 ;   left_hwhm = INTERPOL( x_section, srf_section, srf_half_max )
 ; ENDELSE

;------------------------------------------------------------------------------
;                 -- Find right side half_maximum value --
;------------------------------------------------------------------------------

; -------------------------------------------------- 
; Find first point below the right-side half-maximum
; -------------------------------------------------- 

 ; index_begin = nominal_maximum_location

 ; index = WHERE( srf_to_use[ index_begin : n_srf_pts - 1 ] LE srf_half_max, count ) + $
 ;         index_begin
 ; IF ( count LE 0 ) THEN BEGIN
 ;   MESSAGE, 'SRF right-side not sufficiently defined in input SRF. Returning -1', /INFO
 ;   RETURN, -1
 ; ENDIF


; ------------------------------------
; Interpolate to the true half-maximum
; ------------------------------------
  
 ; index_begin = index[ 0 ] - offset
 ; IF ( index_begin LT 0 ) THEN BEGIN
 ;   MESSAGE, 'Not enought points on the LEFT side of the RIGHT SRF half-maximum point!', /INFO
 ;   RETURN, -1
 ; ENDIF
;  
 ; index_end = index[ 0 ] + offset
 ; IF ( index_end GT ( n_srf_pts - 1 ) ) THEN BEGIN
 ;   MESSAGE, 'Not enought points on the RIGHT side of the RIGHT SRF half-maximum point!', /INFO
 ;   RETURN, -1
 ; ENDIF
  
 ; srf_section = srf_to_use[ index_begin : index_end ]
 ; x_section   = x[ index_begin : index_end ]
;   
 ; IF(KEYWORD_SET(SPLINE_INT)) THEN BEGIN  
 ;   right_hwhm = SPLINE( srf_section, x_section, srf_half_max )
 ; ENDIF ELSE BEGIN
 ;   right_hwhm = INTERPOL( x_section, srf_section, srf_half_max )
 ; ENDELSE

;------------------------------------------------------------------------------
;                       -- Plot data if required --
;------------------------------------------------------------------------------

  IF ( KEYWORD_SET( plot ) ) THEN BEGIN
    PLOT, x, srf_to_use, TITLE = 'SRF FWHM plot'
    OPLOT, [ MIN( x ), MAX( x ) ], [ srf_half_max, srf_half_max ], COLOR = 5
    OPLOT, [ left_hwhm, left_hwhm ], [ MIN( srf_to_use ), MAX( srf_to_use ) ], COLOR = 4
    OPLOT, [ right_hwhm, right_hwhm ], [ MIN( srf_to_use ), MAX( srf_to_use ) ], COLOR = 4
  ENDIF



;------------------------------------------------------------------------------
;                     -- Calculate FWHM and return --
;------------------------------------------------------------------------------

  srf_fwhm     = right_hwhm - left_hwhm
  half_maximum = [ left_hwhm, right_hwhm ]
  half_max_response = DBLARR(2)
  half_max_response[0] = srf_half_max
  half_max_response[1] = srf_half_max


  RETURN, srf_fwhm

END

