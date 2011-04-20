; docformat = 'rst'

;+
; Computes the local moments for a given window size for an array.
; 
; :Returns: 
;    the local mean
;
; :Params:
;    image : in, required, type=2D numeric array 
;       original image; will be converted to float or double
;    width : in, required, type=integer
;       size of window
;
; :Keywords:
;    double : in, optional, type=boolean 
;       set to do computations as doubles
;    sdev : out, optional, type=2D float/double array
;       local standard deviation
;    variance : out, optional, type=2D float/double array 
;       local variance
;    skewness : out, optional, type=2D float/double array 
;       local skewness
;    kurtosis : out, optional, type=2D float/double array 
;       local kurtosis
;    edge_truncate : in, optional, type=boolean 
;       compute edge values by repeating
;    edge_wrap : in, optional, type=boolean 
;       compute edge values by padding array with zeros
;    edge_zero : in, optional, type=boolean 
;       compute edge values by wrapping
;
; :Requires: 
;    IDL 6.2
;-
function mg_local_moment, image, width, double=double, $
                          sdev=sdev, variance=variance, skewness=skewness, $ 
                          kurtosis=kurtosis, edge_truncate=edge_truncate, $
                          edge_wrap=edge_wrap, edge_zero=edge_zero
  compile_opt strictarr
  on_error, 2

  ; sanity checking on arguments
  if (n_params() ne 2) then message, 'incorrect number of arguments' 
  if (size(image, /n_dimensions) ne 2) then begin
    message, 'IMAGE parameter must be a 2D array'
  endif
    
  im = keyword_set(double) ? double(image) : float(image) 
  kernel = keyword_set(double) $
             ? dblarr(width, width) + 1.0D $
             : fltarr(width, width) + 1.0
  n = width^2

  ; mean
  local_mean = convol(im, kernel, edge_truncate=edge_truncate, $
                      edge_wrap=edge_wrap, edge_zero=edge_zero) / n

  ; variance or standard deviation
  if (arg_present(sdev) $
        || arg_present(variance) $
        || arg_present(skewness) $
        || arg_present(kurtosis)) then begin
    squared = convol(im^2, kernel, edge_truncate=edge_truncate, $
                     edge_wrap=edge_wrap, edge_zero=edge_zero)
    variance = (squared  - n * local_mean^2) / (n - 1.0)
    sdev = sqrt(variance)
  endif

  ; skewness
  if (arg_present(skewness) || arg_present(kurtosis)) then begin
    cubed = convol(im^3, kernel, edge_truncate=edge_truncate, $
                   edge_wrap=edge_wrap, edge_zero=edge_zero)  
    skewness = (cubed / n - 3.0 * local_mean * squared / n + 2.0 * local_mean^3) / sdev ^ 3
  endif

  ; kurtosis
  if (arg_present(kurtosis)) then begin
    fourth = convol(im^4, kernel, edge_truncate=edge_truncate, $
                    edge_wrap=edge_wrap, edge_zero=edge_zero)
    kurtosis = (fourth / n - 4.0 * local_mean * cubed / n + 6.0 * local_mean^2 * squared / n - 3.0 * local_mean^4) / variance^2 - 3.0
  endif
  
  return, local_mean
end
