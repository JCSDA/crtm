;
;+
; NAME:
;       WOPLOT
;
; PURPOSE:
;       Overplots X,Y data in a resizeable, zoomable WPLOT window. If a call
;       to WPLOT has not yet occurred, WOPLOT calls WPLOT.
;
; CATEGORY:
;       Direct Graphics
;
; CALLING SEQUENCE:
;       WOPLOT, X, Y
;
; INPUTS:
;       X: abscissa data
;       Y: ordinate data
;
; KEYWORD PARAMETERS:
;       All the keywords allowed in the OPLOT command
;
; OUTPUTS:
;       None
;
; COMMON BLOCKS:
;       WPLOT_COMM: This common block contains the state and data pointers.
;
; SIDE EFFECTS:
;       This routine will no longer work if the command
;         PTR_FREE, PTR_VALID()
;       is used, either on the command line or in any application programs.
;
; RESTRICTIONS:
;       Cannot use the command
;         PTR_FREE, PTR_VALID()
;       anywhere when WPLOT windows are active.
;
; PROCEDURE:
;       The data displayed in the WPLOT window, and the associated PLOT keywords used, are
;       stored in a linked list for each WPLOT window. Each linked list is accessed
;       through a data pointer array. The state information for each window is stored
;       in a structure array. Both of these arrays are accessed via pointers in a
;       common block.
;
; EXAMPLE:
;       x = findgen(100)
;       y = x
;       wplot, x, y
;       woplot, x, 0.01*y^2, color = 20
;       woplot, x, 10.0*sqrt(y), color = 80
;
; MODIFICATION HISTORY:
; 	Written by:     Paul van Delst, CIMSS/SSEC, 26-Nov-1997
;
; $Author: paulv $
; $Date: 1997/11/28 17:27:52 $
; $Id: woplot.pro,v 1.4 1997/11/28 17:27:52 paulv Exp $
; $Log: woplot.pro,v $
; Revision 1.4  1997/11/28 17:27:52  paulv
; Renamed X,Y parameters to allow overplotting of only Y data.
;
; Revision 1.3  1997/11/26 17:03:03  paulv
; Added X,Y parameter checks.
;
; Revision 1.2  1997/11/26 16:45:38  paulv
; Removed WPLOT_CREATE_GUI call. The condition that would lead
; to this call should *never* occur. However, we shall see.....
; Call replaced with an error message and a STOP command.
;
; Revision 1.1  1997/11/26 16:09:42  paulv
; Initial revision
;
;
;-
 
 
 
;##############################################################################
;##############################################################################
;
;                          ## WOPLOT PROCEDURE ##
;
;
pro woplot, xp, yp, _extra = extra, $
            color = color, linestyle = linestyle, thick = thick, psym = psym
;
;##############################################################################
;##############################################################################


;------------------------------------------------------------------------------
;                      -- Declare the common block --
;------------------------------------------------------------------------------

  @wplot_common



;------------------------------------------------------------------------------
;                         -- Check the parameters --
;------------------------------------------------------------------------------
 
  if ( n_params() lt 1 ) then begin
    message, 'Incorrect number of arguments', /info
    return
  endif

  if ( n_params() eq 1 ) then begin
    y = xp
    x = findgen( n_elements( y ) )
  endif else begin
    x = xp
    y = yp
  endelse


 
 

;------------------------------------------------------------------------------
;                         -- Check the keywords --
;------------------------------------------------------------------------------

  if ( n_elements( color ) eq 0 ) then color = !p.color
  if ( n_elements( linestyle ) eq 0 ) then linestyle = !p.linestyle
  if ( n_elements( thick ) eq 0 ) then thick = !p.thick
  if ( n_elements( psym ) eq 0 ) then psym = !p.psym



;------------------------------------------------------------------------------
;                 -- Get current (or last) wplot window id --
;------------------------------------------------------------------------------

  if ( ( ptr_valid( state_ptr ) eq 0 ) ) then begin


;   ----------------------------
;   No wplot windows! Call wplot
;   ----------------------------

    wplot, x, y, _extra = extra, $
           color = color, linestyle = linestyle, thick = thick, psym = psym

    return

  endif else begin


;   ---------------------
;   Get current window ID
;   ---------------------

    win_id = !d.window


;   --------------------------------
;   Is this a valid wplot window ID?
;   --------------------------------

    loc = ( where( ( *state_ptr ).win_id eq win_id, count ) )[ 0 ]

    if ( count le 0 ) then begin                    ; No wplot windows with win_id found

      n_win = n_elements( *state_ptr )                ; How many wplot windows are there?

      if ( n_win eq 0 ) then begin                    ; No wplot windows -> how did I get here?
        message, 'Active pointers and no windows! How did this happen?', /info
        stop
      endif else begin                                ; Make the last created wplot window current
        win_id = ( *state_ptr )[ n_win - 1 ].win_id
        tlb_id = ( *state_ptr )[ n_win - 1 ].tlb_id
      endelse

      wset, win_id

    endif else $

      tlb_id = ( *state_ptr )[ loc ].tlb_id      ; Get current wplot window tlb_id

  endelse






;------------------------------------------------------------------------------
;                           -- Plot the data --
;------------------------------------------------------------------------------

  oplot, x, y, color = color, linestyle = linestyle, thick = thick, psym = psym, $
         _extra = extra



;------------------------------------------------------------------------------
;                       -- Add to the data pointer structure --
;------------------------------------------------------------------------------


; ----------------------
; Set the data structure
; ----------------------

  data = { x         : ptr_new( x ), $
           y         : ptr_new( y ), $
           color     : color, $
           linestyle : linestyle, $
           thick     : thick, $
           psym      : psym, $
           extra     : ptr_new( extra ), $
           next      : ptr_new() }


; -------------------------------------------
; Find the root index of the data linked list
; for the current wplot window
; -------------------------------------------

  loc = ( where( ( *state_ptr ).win_id eq win_id ) )[ 0 ]


; ----------------------------------
; Dereference the data pointer array
; ----------------------------------

  data_ptr_array = temporary( *data_ptr )


; ------------------------
; Traverse the linked list
; ------------------------

  current_ptr = data_ptr_array[ loc ]
  while ( ptr_valid( ( *current_ptr ).next ) ne 0 ) do current_ptr = ( *current_ptr ).next


; ------------------------------------------------------------
; Place the new data structure onto the end of the linked list
; ------------------------------------------------------------

  ( *current_ptr ).next = ptr_new( data )


; ------------------------
; Restore the data pointer
; ------------------------

  *data_ptr = temporary( data_ptr_array )

end



