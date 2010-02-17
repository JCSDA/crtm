;+
; Name:
;	weibull
; Purpose:
;	Function to return the value of the Weibull pdf.
; Example:
;	beta = 1.	; SHAPE PARAMETER
;			;   =1==> exponential distribution
;			;   >1==> wear-out type shapes whereby f(T) is zero at
;			;   T=gamma, increases as T-->T~, and then decays to 0.
;			;   =2==> Rayleigh distribution
;	gamma = 0.	; LOCATION PARAMETER
;			;   locates the distribution along the abscissa
;	eta = 1.	; SCALE PARAMETER
;			;   reciprocal of useful life
;	T = findgen(100)/10.
;	print,weibull(T,beta,gamma,eta)
; Usage:
;	pdf = weibull(T[,beta][,gamma][,eta][,/help][,/reliability][,/failurerate])
; Inputs:
;	T = abscissa coordinate, scalar or array
; Optional Inputs:
;	beta = shape parameter, > 0 (D=1==> exponential distribution)
;	eta = scale parameter, > 0 (D=1)
;	gamma = location parameter (D=0)
; Keywords:
;	help = flag to print header
;	/reliability = return only the exponential portion:
;	  R = exp(-((T-gamma)/eta)^beta)
;	  R has a value of 1 from 0<=T<=gamma 
;	  and decreases for T>gamma to zero.
;	/failurerate = return only the failure rate portion:
;	  lambda = (beta/eta)*((T-gamma)/eta)^(beta-1)
;	  lambda is infinity for 0<beta<1 and T=gamma.
; Outputs:
;	Value or values of the Weibull pdf
; Common blocks:
;	none
; Procedure:
;	If keyword help is set, call doc_library to print header.
;	See, for example, Dimitri Kececioglu, RELIABILITY ENGINEERING HANDBOOK,
;	Vol. 1, pp. 271ff.
; Modification history:
;	write, 14 Sep 92, F.K.Knight
;-
function weibull,t,beta,gamma,eta,help=help $
  ,reliability=reliability,failurerate=failurerate
;
;	=====>> HELP
;
on_error,2
if keyword_set(help) then begin & doc_library,'weibull' & return,0 & endif
;
;	=====>> SET DEFAULTS
;
case n_elements(beta) of
  0:beta = 1.		; DEFAULT
  1:if beta le 0. then message,'Shape parameter must be >= 0.'
  else:message,'Only scalar allowed for shape parameter.'
  endcase
case n_elements(gamma) of
  0:gamma = 0.		; DEFAULT
  1:
  else:message,'Only scalar allowed for location parameter.'
  endcase
case n_elements(eta) of
  0:eta = 1.		; DEFAULT
  1:if eta le 0. then message,'Scale parameter must be >= 0.'
  else:message,'Only scalar allowed for scale parameter.'
  endcase
if min(T) lt gamma then message,'All abscissa values must be >= gamma('+strtrim(gamma,2)+').'
;
;	=====>> CALCULATE VALUES
;
arg = (t-float(gamma))/eta
R = exp(-arg^beta)
lambda = (float(beta)/eta)*arg^(beta-1.)
if keyword_set(reliability) then return,R
if keyword_set(failurerate) then return,lambda
return,lambda*R
end
