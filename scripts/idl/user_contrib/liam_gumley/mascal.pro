pro mascal, band, bbc1, bbc2, bbt1, bbt2, tback, eslope, eincpt, micron = micron

;+
; Purpose:
;     Compute MAS IR band calibration slope and intercept with
;     correction for non-unit blackbody emissivity
; 
; Usage:
;     MASCAL, BAND, BBC1, BBC2, BBT1, BBT2, TBACK, ESLOPE, EINCPT
;
; Input:
;     BAND     50 channel configuration MAS band number
;     BBC1     ambient (cool) blackbody count for band ib
;     BBC2     warm blackbody count for band ib
;     BBT1     ambient (cool) blackbody temperature (K)
;     BBT2     warm blackbody temperature (K)
;     TBACK    MAS background temperature (K) (scan head temperature)
;
; Optional Keywords:
;     MICRON   If set, slope and intercept units are
;              slope (W/m2/sr/micron/count)
;              intercept (W/m2/sr/micron)
; 
; Output:
;     ESLOPE   emissivity corrected calibration slope (mW/m2/sr/cm-1/count)
;     EINCPT   emissivity corrected calibration intercept (mW/m2/sr/cm-1)
;-

;- blackbody effective emissivity for all bands

e = [ 1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0, $
  1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0, $
  0.98095, 0.98104, 0.98115, 0.98124, 0.98134, $
  0.98145, 0.98155, 0.98164, 0.98175, 0.98184, $
  0.98194, 0.98203, 0.98213, 0.98223, 0.98233, $
  0.98243, 0.94590, 0.94102, 0.93890, 0.93782, $
  0.93668, 0.93668, 0.93700, 0.93764, 0.93858 ]

;- convert bb temps to radiance

if not keyword_set( micron ) then micron = 0
rad1 = masplanck( bbt1, band, micron = micron )
rad2 = masplanck( bbt2, band, micron = micron )

;- compute emissivity corrected calibration slope

eslope = e( band - 1 ) * ( ( rad2 - rad1 ) / ( bbc2 - bbc1 ) )

;- compute emissivity corrected intercept

delrad = masplanck( tback, band, micron = micron ) - rad1
eincpt = rad1 + delrad * ( 1.0 - e( band - 1 ) ) - eslope * bbc1

end
