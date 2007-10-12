FUNCTION Compute_nPoints, deltaA, $   ; !<- - - - - ->!
                          dA          ;    -->| |<--
  @fft_parameters
  RETURN, LONG((deltaA/dA) + ONEPOINT5)
END

