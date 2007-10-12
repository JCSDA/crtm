;+
;
; NAME:
;       Routine_name
;
; PURPOSE:
;
; CATEGORY:
;
; LANGUAGE:
;       IDL v5.4
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; INPUT KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OUTPUT KEYWORD PARAMETERS:
;
; FUNCTION RESULT:
;
; CALLS:
;
; COMMON BLOCKS:
;       None.
;
; INCLUDE FILES:
;       error_codes:   Include file containing error code definitions.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, DD-MMM-YYYY
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

PRO mybarplot, Input_Y



  ;------------------------------------------------------------------------------
  ;                         -- SET UP ERROR HANDLER --
  ;------------------------------------------------------------------------------

;  CATCH, error_status
;
;  IF ( error_status NE 0 ) THEN BEGIN
;    CATCH, /CANCEL
;    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
;    RETURN
;  ENDIF    



  ;------------------------------------------------------------------------------
  ;                   -- SET FLOATING POINT PRECISION --
  ;------------------------------------------------------------------------------

  tolerance = ( MACHAR( /DOUBLE ) ).EPS




  ;------------------------------------------------------------------------------
  ;                            -- CHECK INPUT --
  ;------------------------------------------------------------------------------

  n_Arguments = 1
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments', $
             /NONAME, /NOPRINT


  ; -----------------------------------------
  ; Check that required arguments are defined
  ; -----------------------------------------

  Y = Input_Y

  IF ( N_ELEMENTS( Y ) EQ 0 ) THEN $
    MESSAGE, 'Input Y argument not defined!', $
             /NONAME, /NOPRINT



  Y_Info = SIZE( Y, /STRUCTURE )

  


  ;------------------------------------------------------------------------------
  ;                      -- CLUSTERED GROUP OF BARS --
  ;------------------------------------------------------------------------------

  ; ---------------
  ; Data dimensions
  ; ---------------

  ; -- The number of bars in a clusters
  n_DataSets = Y_Info.DIMENSIONS(0)  ; == cols

  ; -- The number of bars/bar clusters
  n_DataPoints = Y_Info.DIMENSIONS(1)  ; == rows

help, n_DataPoints, n_DataSets

colors = lindgen(n_DataSets) + 1

  !Y.RANGE = [ 0, MAX( Y ) ]

  FOR i = 0L, n_DataPoints - 1L DO BEGIN

    BAR_PLOT, Y[*,i], $
              BARWIDTH = 0.65, $
              BARSPACE = 0.0, $
              BAROFFSET = i*(1.4*n_DataSets), $
              OVERPLOT = ( i GT 0 ), $
              BASERANGE = 0.12, $
              COLORS = colors

  ENDFOR


  ;------------------------------------------------------------------------------
  ;                               -- DONE --
  ;------------------------------------------------------------------------------

  CATCH, /CANCEL

END



;-------------------------------------------------------------------------------
;                          -- MODIFICATION HISTORY --
;-------------------------------------------------------------------------------
;
; $Id:$
;
; $Date:$
;
; $Revision:$
;
; $State:$
;
; $Log:$
;
;
;
