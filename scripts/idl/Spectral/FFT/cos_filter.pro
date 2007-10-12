FUNCTION Cos_Filter, dF, $
                     FILTER_WIDTH=Filter_Width, $
                     FLIP        =Flip
  @fft_parameters
  
  ; The filter width
  DEFAULT_WIDTH = 10.0d0
  IF ( N_ELEMENTS(Filter_Width) EQ 0 ) THEN $
    Width = DEFAULT_WIDTH $
  ELSE $
    Width = DOUBLE(Filter_Width)
  
  ; How many points required for filter?
  n_Filter = Compute_nPoints(Width, dF)
  
  ; Construct frequency array
  f1 = ZERO
  f2 = DOUBLE(n_Filter-1L)*dF
  f  = DINDGEN(n_Filter)/DOUBLE(n_Filter-1L)
  f  = f*(f2-f1) + f1
  
  ; Initialise filter
  Filter = MAKE_ARRAY(n_Filter, VALUE=ONE)
  
  ; Compute filter
  Filter = POINT5 * (ONE + COS(f*!DPI/Width))
  IF ( KEYWORD_SET(Flip) ) THEN Filter = REVERSE(Filter)

  RETURN, Filter
  
END
