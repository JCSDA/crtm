;+
;
; NAME:
;       CrIS_ApodFunction
;
; PURPOSE:
;       Pure function to compute the CrIS apodisation function for a given
;       optical delay grid.
;
; CALLING SEQUENCE:
;       afn = CrIS_ApodFunction(band, x, apodType=apodType, nominal=nominal)
;
; INPUTS:
;       band:      CrIS band number (1, 2, or 3).
;                  If band < 1, then 1 is used.
;                     band > 3, then 3 is used.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN)
;
;       x:         Double-sided optical delay grid.
;                  UNITS:      Centimetres (cm)
;                  TYPE:       DOUBLE
;                  DIMENSION:  Rank-1
;                  ATTRIBUTES: INTENT(IN)
;
; OPTIONAL INPUTS:
;       apodType:  Set this argument to select the type of apodisation function.
;                     == 1 for Hamming apodisation [DEFAULT]
;                     == 2 for Blackman-Harris 3-term apodisation
;                     == 3 for Blackman-Harris 4-term apodisation
;                  If not specified, or any other value is supplied, then
;                  the computed apodisation uses HAMMING.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       nominal:   Set this argument to return the nominal value of the CRIS
;                  max. OPD rather than the resampled max. OPD..
;                  If NOT SET, the resampled value is returned, [DEFAULT]
;                     SET,     the nominal value is returned
;                  If not specified, the resampled value of maxX is returned.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       afn:       CrIS apodisation function.
;                  UNITS:      N/A
;                  TYPE:       DOUBLE
;                  DIMENSION:  Same as input x argument.
;
; COMMENTS:
;       The Hamming apodization function is a reasonable and efficient
;       function to use in atmospheric remote sensing applications with
;       high signal-to-noise instruments, both because its channel response
;       funciton has side-lobes less than 1% of the central lobe and because
;       it has a well behaved analytic inverse transformation which
;       satisfies retrieval models and also allows apodized radiances to
;       be returned to their unapodized values.
;
;       The Blackman-Harris apodizations produce high lobe suppression functions
;       and can be considered among the top performers in commonly used FTIR
;       apodisation filters.
;-

FUNCTION CrIS_ApodFunction, band, x, apodType=apodType, nominal=nominal
  @cris_parameters

  ; Setup
  maxX = CrIS_MaxX(band, nominal=nominal)
  xnorm = x/maxX
  ; ...Set apodisation type
  atype = N_ELEMENTS(apodType) GT 0 ? LONG(apodType[0]) : CRIS_HAMMING


  ; Compute apodisation function
  CASE atype OF

    CRIS_BLACKMANHARRIS_3: BEGIN
      a0 = 0.42323d0
      a1 = 0.49755d0
      a2 = 0.07922d0
      afn = a0 + a1*COS(PI*xnorm) + $
                 a2*COS(TWO*PI*xnorm)
    END

    CRIS_BLACKMANHARRIS_4: BEGIN
      a0 = 0.35875d0
      a1 = 0.48829d0
      a2 = 0.14128d0
      a3 = 0.01168d0
      afn = a0 + a1*COS(PI*xnorm)     + $
                 a2*COS(TWO*PI*xnorm) + $
                 a3*COS(THREE*PI*xnorm)
    END
    
    ; ...Hamming apodisation is the default
    ELSE: BEGIN
      a0 = 0.54d0
      a1 = 0.46d0
      afn = a0 + a1*COS(PI*xnorm)
    END

  ENDCASE


  ; Set out-of-bounds apodisation to zero
  tolerance = Compute_MeanDelta(x)/10.0d0
  loc = WHERE(ABS(x) GT maxX+tolerance, count)
  IF ( count GT 0 ) THEN afn[loc] = ZERO

  RETURN, afn
  
END


