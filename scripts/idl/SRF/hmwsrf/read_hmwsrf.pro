;+
;
; NAME:
;       Read_HMWSRF
;
; PURPOSE:
;       This function reads a spectral response function "record" from a
;       ASCII input file in HMW (Hal M. Woolf) format.
;
; CATEGORY:
;       Spectral
;
; CALLING SEQUENCE:
;       result = Read_HMWSRF( SRF_fileNAME,   $  ; Input
;                             channel,        $  ; Input
;                             SRF_Frequency,  $  ; Output
;                             SRF,            $  ; Output
;                             quiet  = quiet, $  ; Input keyword
;                             header = header )  ; Output keyword
;
; INPUTS:
;       SRF_fileNAME:   The file, in HMW SRF format, that contains the spectral
;                       response data.
;
;       channel:        The channel of the sensor of which the spectral response
;                       is required.
;
; INPUT KEYWORD PARAMETERS:
;       quiet:          Set this keyword to suppress screen output of
;                       informational messages.
;
; OUTPUTS:
;       SRF_Frequency:  The frequency array, in cm-1, of the SRF abscissa.
;
;       SRF:            The requested SRF data.
;
; OUTPUT KEYWORD PARAMETERS:
;       header:         Set this keyword to a named variable to return the 
;                       the SRF data file header.
;
; FUNCTION RESULT
;       Function returns a flag
;         result = SUCCESS, SRF read was successful
;                = FAILURE, error occurred.
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
;       None.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst; CIMSS/SSEC 03-Sep-1996
;                       paul.vandelst@ssec.wisc.edu
;
;  Copyright (C) 1996, 2002 Paul van Delst
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


FUNCTION Read_HMWSRF, Input_SRF_fileNAME, $  ; Input
                      Input_Channel,      $  ; Input
                      SRF_Frequency,      $  ; Output
                      SRF,                $  ; Output
                      Quiet  = Quiet,     $  ; Input keyword
                      Header = Header        ; Output keyword



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
    MESSAGE, 'Must specify an input SRF filename.', $
             /NONAME, /NOPRINT


  ; -------------
  ; Check channel
  ; -------------

  Channel = LONG( Input_Channel[ 0 ] )

  IF ( Channel < 0 ) THEN $
    MESSAGE, 'Invalid channel specified. Must be > 0.', $
             /NONAME, /NOPRINT



  ;#--------------------------------------------------------------------------#
  ;#                 -- OPEN INPUT SPECTRAL RESPONSE FILE --                  #
  ;#--------------------------------------------------------------------------#

  GET_LUN, SRF_fileID
  OPENR, SRF_fileID, SRF_fileNAME



  ;#--------------------------------------------------------------------------#
  ;#                        -- READ THE FILE HEADER --                        #
  ;#--------------------------------------------------------------------------#

  ; ------------------------------
  ; Define file_header as a string
  ; ------------------------------

  Header = ' '


  ; -------------------------------
  ; Read the first line of the file
  ; -------------------------------

  READF, SRF_fileID, Header



  ;#--------------------------------------------------------------------------#
  ;#      -- LOOP OVER SRF DATA UNTIL THE REQUIRED CHANNEL IS REACHED --      #
  ;#--------------------------------------------------------------------------#

  IF ( NOT KEYWORD_SET( Quiet ) ) THEN BEGIN
    PRINT, FORMAT = '( 5x, "Searching for channel ", i4, " SRF." )', Channel
  ENDIF


  ; ---------------------------
  ; Search for required channel
  ; ---------------------------

  REPEAT BEGIN


    ; ----------------------
    ; Are we at END-OF-FILE?
    ; ----------------------

    IF ( EOF( SRF_fileID ) ) THEN BEGIN

      ; -- Make sure no spurious data is returned
      SRF_Frequency = 0B
      dummy = TEMPORARY( SRF_Frequency )
      SRF = 0B
      dummy = TEMPORARY( SRF )

      ; -- Output error message and return with EOF error code
      MESSAGE, SRF_fileNAME + ' EOF reached before channel data found.', $
               /NONAME, /NOPRINT

    ENDIF


    ; --------------------
    ; Read SRF data header
    ; --------------------

    READF, SRF_fileID, $
           FORMAT = '( i3, i5, f8.2, f8.2 )', $
           Channel_Number, $
           n_Points, $
           Begin_Frequency, $
           End_Frequency


    ; -------------------------
    ; Create array for SRF data
    ; -------------------------

    SRF = FLTARR( n_Points )


    ; -------------
    ; Read SRF data
    ; -------------

    READF, SRF_fileID, $
           FORMAT = '( 8( f9.6 ) )', $
           SRF

  ENDREP UNTIL ( ( Channel_Number - Channel ) EQ 0 )

  FREE_LUN, SRF_fileID



  ;#--------------------------------------------------------------------------#
  ;#                      -- COMPUTE FREQUENCY GRID --                        #
  ;#--------------------------------------------------------------------------#

  SRF_Frequency = FINDGEN( n_Points ) / FLOAT( n_Points - 1 )
  SRF_Frequency = Begin_Frequency + $
                  ( SRF_Frequency * ( End_Frequency - Begin_Frequency ) )



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
; $Log: read_hmwsrf.pro,v $
; Revision 2.4  2005/06/29 22:28:12  paulv
; Massive commit to resync repository.
;
; Revision 2.3  1999/10/22 14:50:05  paulv
; Added sensor name and platform name read.
;
; Revision 2.2  1999/10/11 21:40:06  paulv
; Altered read open loop to take into account floating point channels
; numbers due to ASTER having two channel 3's - a 3B and 3N which are
; represented in the .srf file as 3.1 and 3.2.
;
; Revision 2.1  1999/10/07 22:08:30  paulv
; New version.
;
; Revision 1.1  1998/04/13 21:35:05  paulv
; Initial revision
;
;
