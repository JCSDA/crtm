FUNCTION Compute_NextPO2, n
  x  = ALOG(DOUBLE(n))/ALOG(2.0d0)
  ix = LONG(x)
  IF ( 2L^ix EQ n ) THEN $
    po2 = ix $
  ELSE $
    po2 = ix+1L
  RETURN, po2
END

