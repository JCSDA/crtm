;+
;
; NAME:
;       myxyouts
;
; PURPOSE:
;       Procedure to output a plot legend in "PLOT NORMAL" coordinates. Plot normal
;       coordinates are defined as coordinates normalised for the plotting region
;       bounded by the plot axes, e.g. the bottom left axex vertex corresponds to
;       (0,0) and the top right one corresponds to (1,1).
;
; CATEGORY:
;       Graphics: Direct
;
; LANGUAGE:
;       IDL v5.4
;
; CALLING SEQUENCE:
;       mylegend, x_legend_pos,          $
;                 y_legend_pos,          $
;                 legend_text,           $
;                 dx_pos    = dx_pos,    $
;                 color     = color,     $
;                 psym      = psym,      $
;                 symsize   = symsize,   $
;                 linestyle = linestyle, $
;                 charsize  = charsize,  $
;                 _extra    = extra
;
; INPUTS:
;       x_legend_pos:   X-coordinate of the legend start position in "PLOT NORMAL"
;                       coordinates. Plot normal coordinates are coordinates normalised
;                       for the plotting region bounded by the plot axes.
;
;       y_legend_pos:   Y-coordinate of the legend start position in "PLOT NORMAL"
;                       coordinates. Plot normal coordinates are coordinates normalised
;                       for the plotting region bounded by the plot axes.
;
;       legend_text:    String array containing the legend text. No zero-length
;                       elements are permitted.
;
; INPUT KEYWORD PARAMETERS:
;       dx_pos:         Length of the line in the legend, in normalised units. If not
;                       specified the default value is 0.04.
;
;       color:          Array containing the color of the legend text entries.
;                       If defined, must be the same size as the LEGEND_TEXT argument.
;
;       psym:           Array containing the symbol code of the legend text entries.
;                       If defined, must be the same size as the LEGEND_TEXT argument.
;
;       symsize:        Array containing the symbol size of the legend text entries.
;                       If defined, must be the same size as the LEGEND_TEXT argument.
;
;       linestyle:      Array containing the linestyel of the legend text entries.
;                       If defined, must be the same size as the LEGEND_TEXT argument.
;
;       charsize:       Character size to use in output of the legend text. If not
;                       specirfied the default value is 1.0.
;
;       _extra:         Any other keywords accepted by both the PLOTS and XYOUTS
;                       commands.
;
; OUTPUTS:
;       None.
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
;       None.
;
; RESTRICTIONS:
;       This procedure assumes that the plotting range (transformation?) 
;       has already been established by a call to PLOT. You'll still get
;       output if you haven't but if it's not where you expect it, too bad.
;
; EXAMPLE:
;       To plot a legend in the centre of the plot,
;
;         mylegend, 0.5, 0.5, $
;                   [ 'First one', 'Second one' ]
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 04-Apr-2001
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

PRO MyXYouts, xPos,            $
              yPos,            $
              Text,            $
              NORMAL = Normal, $
              _EXTRA = Extra



  ;------------------------------------------------------------------------------
  ;                         -- Set up error handler --
  ;------------------------------------------------------------------------------

  CATCH, error_status

  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF    


  
  ;------------------------------------------------------------------------------
  ;                            -- CHECK INPUT --
  ;------------------------------------------------------------------------------

  n_arguments = 3
  IF ( N_PARAMS() LT n_arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT


  ; -----------------------------------------
  ; Check that required arguments are defined
  ; -----------------------------------------

  IF ( N_ELEMENTS( xPos ) EQ 0 ) THEN $
    MESSAGE, 'Input XPOS argument not defined!', $
             /NONAME, /NOPRINT
             
  IF ( N_ELEMENTS( yPos ) EQ 0 ) THEN $
    MESSAGE, 'Input YPOS argument not defined!', $
             /NONAME, /NOPRINT

  IF ( NOT Valid_String( Text ) ) THEN $
    MESSAGE, 'Input TEXT argument not defined!', $
             /NONAME, /NOPRINT



  ;------------------------------------------------------------------------------
  ; -- CONVERT THE INPUT "PLOT NORM" COORDINATES TO ACTUAL NORMAL COORDINATES --
  ;------------------------------------------------------------------------------

  ; -- Assign axis ranges
  x1 = !X.CRANGE[0]
  x2 = !X.CRANGE[1]

  y1 = !Y.CRANGE[0]
  y2 = !Y.CRANGE[1]

  ; -- Apply transformations. Doesn't matter if axes are logarithmic
  ; -- If they are, transformation is applied to the LOG of the range.
  xDataPos = x1 + ( ( x2 - x1 ) * xPos )
  xNormPos = !X.S[0] + ( !X.S[1] * xDataPos )

  yDataPos = y1 + ( ( y2 - y1 ) * yPos )
  yNormPos = !Y.S[0] + ( !Y.S[1] * yDataPos )



  ;------------------------------------------------------------------------------
  ;                          -- OUTPUT THE TEXT STRING --
  ;------------------------------------------------------------------------------

  XYOUTS, xNormPos, yNormPos, $
          STRTRIM( Text, 2 ), $
          /NORMAL, $
          _EXTRA = Extra



  ;------------------------------------------------------------------------------
  ;                               -- Done --
  ;------------------------------------------------------------------------------

  CATCH, /CANCEL
  RETURN

END


;-------------------------------------------------------------------------------
;                          -- MODIFICATION HISTORY --
;-------------------------------------------------------------------------------
;
; $Id: myxyouts.pro,v 1.1 2005/09/16 21:22:28 paulv Exp $
;
; $Date: 2005/09/16 21:22:28 $
;
; $Revision: 1.1 $
;
; $State: Exp $
;
; $Log: myxyouts.pro,v $
; Revision 1.1  2005/09/16 21:22:28  paulv
; Initial checkin.
;
;
;
;
