;+
;
; NAME:
;       Gaussian_SRF
;
; PURPOSE:
;       Function to calculate a Gaussian Spectral Response Function (SRF)
;
; CATEGORY:
;       Spectral
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       result = Gaussian_SRF( Central_Frequency, $     ; Input
;                              Bandwidth, $             ; Input
;                              Power, $                 ; Input
;                              SRF_Frequency, $         ; Input
;                              SRF, $                   ; Output
;                              plot  = plot )           ; Input keyword
;
; INPUTS:
;       Central_Frequency:  Central frequency of the required SRF in units
;                           of inverse centimetres, cm^-1.
;
;       Bandwidth:          Full Width at Half Maximum of the required SRF
;                           in units of inverse centimetres (cm^-1).
;
;       Power:              Power to which the exponent is raised.
;
;       SRF_Frequency:      Frequency grid vector, in units of inverse
;                           centimetres, cm^-1, at which the SRF is to be
;                           calculated.
;
; INPUT KEYWORD PARAMETERS:
;       plot:               Set this keyword to plot the SRF after calculation.
;
; OUTPUTS:
;       SRF:                Vector containing the SRF relative response.
;
; OUTPUT KEYWORD PARAMETERS:
;       None.
;
; FUNCTION RESULT:
;       Function returns a flag
;         result = SUCCESS calculation was successful
;                = FAILURE error occurred
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       - Both the central frequency and bandwidth must be greater than zero.
;
;       - The minimum powers allowed is 1. If the supplied value is less, the
;         value is set to 1.
;
;       - Only positive input frequencies are allowed.
;
; PROCEDURE:
;       The Gaussian SRF is calculated using:
;
;         SRF = EXP( -a * |x - x0|^n )
;
;       where a is a multiplier determined from the half Power (HWHM) points,
;
;         a =  -1 * LN( 0.5 )
;             ----------------
;                      n
;                  HWHM
;
;       and x  is the frequency in cm-1
;           x0 is the central frequency in cm-1, and
;           n  is the Gaussian power supplied by the user.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC 07-Apr-1998 
;                       paul.vandelst@ssec.wisc.edu
;
;  Copyright (C) 1998 Paul van Delst
;
;  This program is free software; you can redistribute it and/or
;  modify it under the terms of the GNU General Public License
;  as published by the Free Software Foundation; either version 2
;  of the License, or (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software
;  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;-

FUNCTION Gaussian_SRF, Input_Central_Frequency, $       ; Input
                       Input_Bandwidth, $               ; Input
                       Input_Power, $                   ; Input
                       Input_SRF_Frequency, $           ; Input
                       SRF, $                           ; Output
                       plot  = plot                     ; Input keyword



  ;#--------------------------------------------------------------------------#
  ;#                       -- SET UP ERROR HANDLER --                         #
  ;#--------------------------------------------------------------------------#

  @error_codes

  CATCH, Error_Status

  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF




  ;#--------------------------------------------------------------------------#
  ;#                     -- SET SOME CONSTANT VALUES --                       #
  ;#--------------------------------------------------------------------------#

  ; -- Floating point precision tolerance value
  Tolerance = ( MACHAR( /DOUBLE ) ).EPS

  ; -- Literal constants
  ZEROpointFIVE = 0.5d0
  ZERO          = 0.0d0
  ONE           = 1.0d0
  TWO           = 2.0d0
  THREE         = 3.0d0

  ; -- Number of FWHMs used in calculation
  n_FWHMs = THREE



  ;#--------------------------------------------------------------------------#
  ;#                        -- CHECK INPUT ARGUMENTS --                       #
  ;#--------------------------------------------------------------------------#

  ; -------------------------------------
  ; Check for correct number of arguments
  ; -------------------------------------
 
  n_Arguments = 5
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT


  ; -----------------------------------------------
  ; Check that required input arguments are defined
  ; -----------------------------------------------

  ; -- Central Frequency
  IF ( N_ELEMENTS( Input_Central_Frequency ) EQ 0 ) THEN $
    MESSAGE, 'Input Central_Frequency argument not defined!', $
             /NONAME, /NOPRINT

  ; -- Bandwidth
  IF ( N_ELEMENTS( Input_Bandwidth ) EQ 0 ) THEN $
    MESSAGE, 'Input Bandwidth argument not defined!', $
             /NONAME, /NOPRINT

  ; -- Power
  IF ( N_ELEMENTS( Input_Power ) EQ 0 ) THEN $
    MESSAGE, 'Input Power argument not defined!', $
             /NONAME, /NOPRINT

  ; -- SRF frequency grid
  IF ( N_ELEMENTS( Input_SRF_Frequency ) EQ 0 ) THEN $
    MESSAGE, 'Input SRF_Frequency argument not defined!', $
             /NONAME, /NOPRINT


  ; ---------------------------------
  ; Check for valid central Frequency
  ; ---------------------------------

  ; -- Make sure it's floating point and scalar
  Central_Frequency = DOUBLE( Input_Central_Frequency[ 0 ] )

  ; -- Is it positive and non-zero?
  IF ( Central_Frequency LT Tolerance ) THEN $
    MESSAGE, 'Invalid central frequency, < or = 0.0!', $
             /NONAME, /NOPRINT


  ; -------------------------
  ; Check for valid Bandwidth
  ; -------------------------

  ; -- Make sure it's floating point and scalar
  Bandwidth = DOUBLE( Input_Bandwidth[ 0 ] )

  ; -- Is it positive and non-zero?
  IF ( Bandwidth LT Tolerance ) THEN $
    MESSAGE, 'Invalid Bandwidth, < or = 0.0!', $
             /NONAME, /NOPRINT


  ; ---------------------
  ; Check for valid Power
  ; ---------------------

  ; -- Make sure it's floating point and scalar
  Power = DOUBLE( Input_Power[ 0 ] )

  ; -- Is it >= 1?
  Min_Power =  1.0d0
  IF ( Power LT Min_Power ) THEN BEGIN
    Power = Min_Power
    MESSAGE, 'Invalid Power. Setting to ' + $
             STRING( Power, FORMAT = '( f3.1 )' ), /INFO
  ENDIF


  ; ------------------------------
  ; Check for valid frequency grid
  ; ------------------------------

  ; -- Is it an array?
  n_Points = N_ELEMENTS( Input_SRF_Frequency )
  IF ( n_Points LT 2 ) THEN $
    MESSAGE, 'Input SRF_Frequency must be an array.', $
             /NONAME, /NOPRINT

  ; -- Make sure it's a floating point vector
  SRF_Frequency = REFORM( DOUBLE( Input_SRF_Frequency ), n_Points )

  ; -- Are any elements less than zero?
  index = WHERE( SRF_Frequency LT ZERO, count )
  IF ( count gt 0 ) THEN $
    MESSAGE, 'Input SRF_Frequency contains negative elements.', $
             /NONAME, /NOPRINT



  ;#--------------------------------------------------------------------------#
  ;#                 -- CALCULATE SRF BANDWIDTH PARAMETERS --                 #
  ;#--------------------------------------------------------------------------#

  HWHM = Bandwidth / TWO
  Exponent_Multiplier = ALOG( ZEROpointFIVE ) / ( HWHM^Power )



  ;#--------------------------------------------------------------------------#
  ;#                        -- CALCULATE GAUSSIAN SRF --                      #
  ;#--------------------------------------------------------------------------#

  Frequency_Difference = ABS( SRF_Frequency - Central_Frequency )
  SRF = EXP( Exponent_Multiplier * ( Frequency_Difference^Power ) )


  ; --------------------------------
  ; Set too small values to zero and
  ; clean up the math error buffer
  ; --------------------------------

  index = WHERE( SRF LT Tolerance, count )
  IF ( count GT 0 ) THEN SRF[ index ] = ZERO
  junk = CHECK_MATH()



  ;#--------------------------------------------------------------------------#
  ;#                 -- PLOT THE CALCULATED SRF IF REQUIRED --                #
  ;#--------------------------------------------------------------------------#

  IF ( KEYWORD_SET( plot ) ) THEN BEGIN

    PLOT, SRF_Frequency, SRF, $
          TITLE = 'Gaussian SRF, n = ' + STRING( Power, '( f3.1 )' ), $
          XTITLE = 'Frequency (cm!U-1!N)', $
          YTITLE = 'Relative response'

  ENDIF
 


  ;#--------------------------------------------------------------------------#
  ;#                               -- DONE --                                 #
  ;#--------------------------------------------------------------------------#

  CATCH, /CANCEL
  RETURN, SUCCESS

END


;-------------------------------------------------------------------------------
;                          -- MODIFICATION HISTORY --
;-------------------------------------------------------------------------------
;
; $Id$
;
; $Date: 2005/06/29 22:28:12 $
;
; $Revision$
;
; $State: Exp $
;
; $Log: gaussian_srf.pro,v $
; Revision 2.2  2005/06/29 22:28:12  paulv
; Massive commit to resync repository.
;
; Revision 2.1  2002/10/02 13:52:50  paulv
; - New version with error handling.
; - All calcs done in double precision.
;
; Revision 1.2  1999/06/18 21:20:31  paulv
; Changed function return value to a flag indicating success of calculation.
; SRF and frequency grid are returned in the argument list.
;
; Revision 1.1  1998/04/07 20:58:38  paulv
; Initial revision
;
;
