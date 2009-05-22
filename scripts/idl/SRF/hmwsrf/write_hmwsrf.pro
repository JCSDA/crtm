;+
; NAME:
;       Write_HMWSRF
;
; PURPOSE:
;       This procedure writes a spectral response function "record" in 
;       HMW (Hal M. Woolf) ASCII format.
;
; CATEGORY:
;       Spectral
;
; CALLING SEQUENCE:
;       result = Write_HMWSRF( SRF_fileNAME,        $  ; Input
;                              channel,         $  ; Input
;                              SRF_Frequency,   $  ; Input
;                              SRF,             $  ; Input
;                              header = header, $  ; Optional input
;                              append = append  )  ; Optional input
;
; INPUTS:
;       SRF_fileNAME:         The file into which the SRF record is to be written.
;
;       channel:          The channel number of the sensor for which
;                         the SRF data corresponds.
;
;       SRF_Frequency:    The frequency array, in cm-1, of the SRF abscissa.
;                         The values *must* be evenly spaced.
;
;       SRF:              The array containing the SRF data.
;
; INPUT KEYWORD PARAMETERS:
;       header:           Set this keyword to a string which is written before
;                         the SRF record. This should only be done for the
;                         *FIRST* SRF record in a HMW format SRF file. This
;                         keyword is ignored if the APPEND keyword is also set.
;
;       append:           Set this keyword to append the data to the output 
;                         SRF file. If this keyword is set, the HEADER keyword
;                         is ignored.
;
; OUTPUTS:
;       None.
;
; OUTPUT KEYWORD PARAMETERS:
;       None.
;
; FUNCTION RESULT
;       Function returns a flag
;         result = SUCCESS, SRF write was successful
;                = FAILURE, error occurred.
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; INCLUDE FILES:
;       error_codes:  Include file containing error code definitions.
;
; SIDE EFFECTS:
;       If the append keyword is not set the file specified is overwritten if it 
;       exists and is writable.
;
; RESTRICTIONS:
;       None.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC 18-Sep-1998
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

FUNCTION Write_HMWSRF, Input_SRF_fileNAME, $
                       Input_Channel, $
                       SRF_Frequency, $
                       SRF, $
                       Header = Header, $
                       Append = Append



  ;#--------------------------------------------------------------------------#
  ;#                       -- SET UP ERROR HANDLER --                         #
  ;#--------------------------------------------------------------------------#

  @error_codes

  CATCH, error_status

  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS( SRF_fileID ) NE 0 ) THEN FREE_LUN, SRF_fileID
    RETURN, FAILURE
  ENDIF    



  ;#--------------------------------------------------------------------------#
  ;#                      -- SET A TOLERANCE VALUE --                         #
  ;#--------------------------------------------------------------------------#

  TOLERANCE = 1.0d-03



  ;#--------------------------------------------------------------------------#
  ;#                         -- CHECK THE INPUT  --                           #
  ;#--------------------------------------------------------------------------#

  n_arguments = 4
  IF ( N_PARAMS() LT n_arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT


  ; ------------------
  ; Check SRF filename
  ; ------------------

  SRF_fileNAME = ( STRING( Input_SRF_fileNAME ) )[ 0 ]

  IF ( N_ELEMENTS( SRF_fileNAME ) EQ 0 ) THEN $
    MESSAGE, 'Must specify an output SRF filename.', $
             /NONAME, /NOPRINT


  ; -------------
  ; Check channel
  ; -------------

  Channel = LONG( Input_Channel[ 0 ] )

  IF ( Channel < 0 ) THEN $
    MESSAGE, 'Invalid channel specified. Must be > 0.', $
             /NONAME, /NOPRINT


  ; ----------------
  ; Check SRF arrays
  ; ----------------

  n_Points = N_ELEMENTS( SRF_Frequency )

  ; -- Must exist
  IF ( n_Points EQ 0 ) THEN $
    MESSAGE, 'Must specify the input SRF frequency grid.', $
             /NONAME, /NOPRINT

  ; -- Must be monotonically increasing
  dFrequency = SRF_Frequency[ 1:n_Points-1 ] - SRF_Frequency[ 0:n_Points-2 ]
  Index = WHERE( dFrequency LT TOLERANCE, Count )
  IF ( Count GT 0 ) THEN $
    MESSAGE, 'Input SRF frequency grid must be monotonically increasing.', $
             /NONAME, /NOPRINT

  ; -- Must have a constant spacing
  dFave = TOTAL( dFrequency ) / DOUBLE( n_Points - 1L )
  Index = WHERE( ABS( dFrequency - dFave ) GT TOLERANCE, Count )
  IF ( Count GT 0 ) THEN $
    MESSAGE, 'Input SRF frequency grid must be regular.', $
             /NONAME, /NOPRINT

  ; -- SRF data
  IF ( N_ELEMENTS( SRF ) NE n_Points ) THEN $
    MESSAGE, 'Input SRF frequency and response arrays have inconsistent size.', $
             /NONAME, /NOPRINT




  ;#--------------------------------------------------------------------------#
  ;#                 -- OPEN OUTPUT SPECTRAL RESPONSE FILE --                 #
  ;#--------------------------------------------------------------------------#

  GET_LUN, SRF_fileID

  OPENW, SRF_fileID, SRF_fileNAME, $
         APPEND = Append



  ;#--------------------------------------------------------------------------#
  ;#                   -- OUTPUT FILE HEADER IF REQUIRED --                   #
  ;#--------------------------------------------------------------------------#

  IF ( KEYWORD_SET( Header ) AND ( NOT KEYWORD_SET( Append ) ) ) THEN BEGIN
    PRINTF, SRF_fileID, STRING( Header )
  ENDIF



  ;#--------------------------------------------------------------------------#
  ;#                           -- OUTPUT SRF DATA --                          #
  ;#--------------------------------------------------------------------------#

  ; --------------------
  ; Output record header
  ; --------------------

  PRINTF, SRF_fileID, $
          FORMAT = '( i3, i5, f8.2, f8.2 )', $
          Channel, n_Points, $
          SRF_Frequency[ 0 ], $
          SRF_Frequency[ n_Points - 1 ]


  ; ---------------
  ; Output SRF data
  ; ---------------

  PRINTF, SRF_fileID, $
          FORMAT = '( 8( f9.6 ) )', $
          SRF

  FREE_LUN, SRF_fileID



  ;#--------------------------------------------------------------------------#
  ;#                                 -- DONE --                               #
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
; $Log: write_hmwsrf.pro,v $
; Revision 1.3  2005/06/29 22:28:12  paulv
; Massive commit to resync repository.
;
; Revision 1.2  1998/08/19 15:55:22  paulv
; Updated documentation header with procedure and example.
;
; Revision 1.1  1998/08/19 13:58:04  paulv
; Initial revision
;
;
;
