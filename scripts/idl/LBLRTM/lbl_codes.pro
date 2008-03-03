;+
;
; NAME:
;       lbl_codes
;
; PURPOSE:
;       Include file containing LBLRTM read status codes.
;
; CATEGORY:
;       LBLRTM
;
; LANGUAGE:
;       IDL v5.4
;
; CALLING SEQUENCE:
;       @lbl_codes
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       Values can be changed during program execution since there is no way to
;       declare parameters in IDL.
;
; RESTRICTIONS:
;       None.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 21-Dec-2000
;                       paul.vandelst@ssec.wisc.edu
;
;  Copyright (C) 2000 Paul van Delst, CIMSS/SSEC/UW-Madison
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


  ;#----------------------------------------------------------------------------#
  ;#                          -- Define codes --                                #
  ;#----------------------------------------------------------------------------#

  ; -- Successful read return codes
  READ_TO_EOF = 0L
  READ_TO_EOL = 1L

  ; -- Return types (indexed by above codes)
  RETURN_TYPE = [ 'EOF', 'EOL' ]

