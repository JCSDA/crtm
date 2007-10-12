;+
;
; NAME:
;       axis_scale
;
; PURPOSE:
;       Procedure to provide a scaling factor and modify a string for
;       plot ranges to avoid long-string tick mark values.
;
; CATEGORY:
;       Graphics: Direct
;
; LANGUAGE:
;       IDL v5.4
;
; CALLING SEQUENCE:
;       axis_scale, axis_range,                  $  ; Input
;                   axis_title,                  $  ; Input
;                   scale_factor,                $  ; Output
;                   scale_title,                 $  ; Output
;                   max_exponent = max_exponent, $  ; Input keyword
;                   scale_check  = scale_check,  $  ; Input keyword
;                   new_line     = new_line         ; Input keyword
;
; INPUTS:
;       axis_range:  A 2-element vecot containing the range of the
;                    plot axis.
;
;       axis_title:  A string containing the plot axis title.
;
; INPUT KEYWORD PARAMETERS:
;       max_exponent:  Set this keyword to the maximum exponent that
;                      will be accepted before scaling is applied. If
;                      not specified the default value is 1.0
;
;       scale_check:   Set this keyword to the cutoff value below which
;                      scaling is not performed. If not specified the 
;                      default value is ((MACHAR(/DOUBLE)).EPS)^3
;
;       new_line:      Set this keyword to modify the separaotr between
;                      the input axis axis title and the multiplier string
;                      with a new line (i.e. the title modifier appears
;                      below the original title text). If not set the 
;                      default value is a space (i.e. the title modifier
;                      appears next to the original title text)
;
; OUTPUTS:
;       scale_factor:  The scale factor with which to multiply plot values.
;
;       scale_title:   The original axis_title with the scale factor added.
;       
; OUTPUT KEYWORD PARAMETERS:
;       None.
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; INCLUDE FILES:
;       None.
;
; SIDE EFFECTS:
;       None
;
; RESTRICTIONS:
;       None.
;
; PROCEDURE:
;       Given an input range,
;
;         axis_range = [ axis_min, axis_max ]
;
;       the maximum absolute value is determined. If the absolute value of
;       the power of ten exponent is greater than the max_exponent value,
;       the scale factor
;
;         10^(-exponent)
;
;       is returned (note the "-" sign). The axis title modifier is for
;       the scaling 10^exponent (note that the exponent is now positive.)
;
; EXAMPLE:
;       Note how the yscale output is used in both scaling the plot y-data
;       and the original y-data range.
;
;       IDL> yrange = [ -1.0e-03, 2.5e-03 ]
;
;       IDL> axis_scale, yrange, 'dT(K)',yscale,ytitle
;
;       IDL> HELP, yscale, ytitle
;       YSCALE          DOUBLE    =        1000.0000
;       YTITLE          STRING    = 'dT(K) (x1.0e-03)'
;
;       IDL> PLOT, x_data, yscale * y_data, $
;                  YRANGE = yscale * yrange, $
;                  YTITLE = ytitle
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 01-Jun-2001
;                       paul.vandelst@ssec.wisc.edu
;
;  Copyright (C) 2001 Paul van Delst, CIMSS/SSEC/UW-Madison
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

PRO axis_scale, axis_range,                  $  ; Input
                axis_title,                  $  ; Input
                scale_factor,                $  ; Output
                scale_title,                 $  ; Output
                max_exponent = max_exponent, $  ; Input keyword
                scale_check  = scale_check,  $  ; Input keyword  
                new_line     = new_line         ; Input keyword



  ;------------------------------------------------------------------------------
  ;                         -- Set up error handler --
  ;------------------------------------------------------------------------------

  CATCH, error_status

  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
  ENDIF    



  ;------------------------------------------------------------------------------
  ;                            -- Check input --
  ;------------------------------------------------------------------------------

  ; -------------------
  ; Number of arguments
  ; -------------------

  n_arguments = 4
  IF ( N_PARAMS() LT n_arguments ) THEN $
    MESSAGE, 'Invalid number of input arguments.', /NONAME, /NOPRINT


  ; ---------------------
  ; Check input arguments
  ; ---------------------

  IF ( N_ELEMENTS( axis_range ) EQ 0 ) THEN $
    MESSAGE, 'Input AXIS_RANGE argument not specified..', /NONAME, /NOPRINT

  IF ( N_ELEMENTS( axis_title ) EQ 0 ) THEN $
    MESSAGE, 'Input AXIS_TITLE argument not specified..', /NONAME, /NOPRINT

  IF ( STRLEN( axis_title ) EQ 0 ) THEN $
    MESSAGE, 'Input AXIS_TITLE argument not specified..', /NONAME, /NOPRINT


  ; --------------
  ; Check keywords
  ; --------------

  IF ( N_ELEMENTS( max_exponent ) EQ 0 ) THEN $
    max_exp = 1L $
  ELSE $
    max_exp = ABS( LONG( max_exponent ) )

  IF ( N_ELEMENTS( scale_scheck ) EQ 0 ) THEN $
    scale_chk = ( ( MACHAR( /DOUBLE ) ).EPS )^3 $
  ELSE $
    scale_chk = ABS( DOUBLE( scale_check ) )

  IF ( KEYWORD_SET( new_line ) ) THEN $
    space_char = '!C' $
  ELSE $
    space_char = ' '



  ;------------------------------------------------------------------------------
  ;                        -- Determine scale factor --
  ;------------------------------------------------------------------------------

  ; -- Default output
  scale_factor = 1.0d0
  scale_title  = axis_title

  ; -- Determine the maximum value
  max_val = MAX( ABS( axis_range ) )
  IF ( max_val GT scale_chk ) THEN BEGIN

    ; -- Determine the exponent
    exponent = FLOOR( ALOG10( max_val ) )
    IF ( ABS(exponent) GT max_exp ) THEN BEGIN

      ; -- Calculate the scale factor and a new scale title.
      scale_factor = 10.0d^( -exponent )
      scale_title = scale_title + $
                    space_char  + $
                    '(x'+STRING(10.0d^( exponent ),FORMAT='(e7.1)')+')'

    ENDIF

  ENDIF



  ;------------------------------------------------------------------------------
  ;                               -- Done --
  ;------------------------------------------------------------------------------

  CATCH, /CANCEL

END



;-------------------------------------------------------------------------------
;                          -- MODIFICATION HISTORY --
;-------------------------------------------------------------------------------
;
; $Id: axis_scale.pro,v 1.1 2002/06/28 15:13:16 paulv Exp $
;
; $Date: 2002/06/28 15:13:16 $
;
; $Revision: 1.1 $
;
; $State: Exp $
;
; $Log: axis_scale.pro,v $
; Revision 1.1  2002/06/28 15:13:16  paulv
; Initial checkin.
;
;
;
;
