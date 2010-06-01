pro stddev, array, mean, variance, sigma

;+
; Purpose:
;    To compute the mean, variance, and standard deviation
;    (also known as RMS, root-mean-square) values for a given dataset.
;
; Usage:
;    stddev, array, mean, variance, sigma
;
; Input:
;    array     array of data values (must have more than one element)
;
; Output:
;    mean      data mean
;    variance  data variance
;    sigma     data standard deviation
;-

;- check number of elements

n = n_elements( array )
if ( n lt 1 ) then message, 'Number of elements less than 1 in STDDEV'

;- compute mean and variance

mean = total( double( array ) ) / double( n )
variance = total( double( array ) ^ 2 ) / double( n ) - mean ^ 2

;- compute sigma if precision ok, otherwise return zero

sigma = 0.0d
test = 1.0d + variance
if ( test gt 1.0d ) then sigma = sqrt( variance )

end
