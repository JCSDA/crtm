function cmrebin, array, D1, D2, D3, D4, dimension=dims, total=total

  sz     = size(array)
  type   = sz( sz(0) + 1 )
  np     = n_params()
  if n_elements(dims) EQ 0 AND (np LT 2 OR np GT 5) then $
    message, 'ERROR: must specify >0 and <5 dimensions in DIMS or D1, D2, ...'

  if n_elements(dims) EQ 0 then begin
      case np of 
          2: dims = [d1]
          3: dims = [d1, d2]
          4: dims = [d1, d2, d3]
          5: dims = [d1, d2, d3, d4]
      endcase
  endif
  nd = n_elements(dims)

  if type GT 0 AND type LT 4 then tmpres = double(array) else tmpres = array

  case nd of
      1: tmpres = rebin(temporary(tmpres),dims(0))
      2: tmpres = rebin(temporary(tmpres),dims(0),dims(1))
      3: tmpres = rebin(temporary(tmpres),dims(0),dims(1),dims(2))
      4: tmpres = rebin(temporary(tmpres),dims(0),dims(1),dims(2),dims(3))
  endcase

  if keyword_set(total) then begin
      total1 = double(total(array))
      total2 = double(total(tmpres))
      if total2 GT 0 then tmpres = temporary(tmpres) * (total1 / total2)
  endif

  newsz = size(tmpres)
  if type EQ newsz(newsz(0)+1) then return, reform(tmpres, dims, /overwrite)

  castfns = ['UNDEF', 'BYTE', 'FIX', 'LONG', 'FLOAT', $
             'DOUBLE', 'COMPLEX', 'UNDEF', 'UNDEF', 'DCOMPLEX' ]
  if type GE 1 AND type LE 3 then $
    tmpres = call_function(castfns(type), round(temporary(tmpres))) $
  else $
    tmpres = call_function(castfns(type), temporary(tmpres))
  return, reform(tmpres, dims, /overwrite)
end
