;+
; NAME:
;       lbl_read
;
; PURPOSE:
;       This function reads a layer of data from a LBLRTM/FASCODE format file.
;
; CATEGORY:
;       LBLRTM
;
; LANGUAGE:
;       IDL v5.4
;
; CALLING SEQUENCE:
;       result = lbl_read( lbl_file, $                           ; Input
;                          lbl_file_lun, $                       ; Input
;                          lbl_spc, $                            ; Output
;                          v_spc, $                              ; Output
;                          lbl_file_type   = lbl_file_type, $    ; Input keyword
;                          no_lbl_data     = no_lbl_data, $      ; Input keyword
;                          quiet           = quiet, $            ; Input keyword
;                          dpf             = dpf, $              ; Input keyword
;                          double          = double, $           ; Input keyword
;                          lbl_file_header = lbl_file_header )   ; Output keyword
;
; INPUTS:
;       lbl_file:         File name of the LBLRTM/FASCODE format file to read.
;
;       lbl_file_lun:     Logical Unit Number of the LBLRTM/FASCODE format file to
;                         read. This is an argument to allow :
;                           1) More than 1 LBLRTM/FASCODE file to read at the
;                              same time, i.e. it's returned to be used in
;                              subsequent calls,
;                           2) Allow opening of the file within this routine.  This
;                              is done as the file MUST be opened with the
;                              f77_unformatted flag.
;
; INPUT KEYWORD PARAMETERS:
;       lbl_file_type:    Set this keyword to either 'SINGLE' or 'DOUBLE' to 
;                         explicitly specify the LBL file type. This keyword 
;                         overrides any automatic file type detection.
;
;       dpf:              Diagnostic Print Flag.  If set, diagnostic output about
;                         the file header and panel header frequency bounds and
;                         number of points read are output.
;
;       no_lbl_data:      Set this keyword to read only the current layer file
;                         header and NOT the layer data. This keyword should be
;                         used in tandem with the LBL_FILE_HEADER output keyword.
;                         Note that after reading the file header, the file pointer
;                         is positioned back to the same location it was in BEFORE
;                         the file header read so subsequent "regular" calls (i.e.
;                         those that read the LBL data panels) are not affected.
;
;       double:           Set this keyword to read the double precision format
;                         LBLRTM data files. Default is single precision.
;
; OUTPUTS:
;       lbl_spc:  Array containing the required layer spectral data. 
;                   If SINGLE panel, dimension( lbl_spc ) = [ N ],
;                     where N is the no. of points.
;                   If DOUBLE panel, dimension( lbl_spc ) = [ N, 2 ],
;                     where lbl_spc[ *, 0 ] = first item spectral data (radiance)
;                           lbl_spc[ *, 1 ] = second item spectral data (transmittance)
; 
;       v_spc:    Array of wavenumbers corresponding to the data in lbl_spc.
;
; OUTPUT KEYWORD PARAMETERS:
;       lbl_file_header:  Set this keyword to a named variable to return the file
;                         header structure for the current layer being read. The 
;                         structure is 1056 bytes long and contains the following
;                         fields :
;
;         user_id      : BYTE(80) 80 characters of user ID information.
;         secant       : DOUBLE Column amount scale factor used in the 
;                          LBL calculation for the current layer.
;                          If +ve, looking up; if -ve, looking down.
;         p_ave        : FLOAT Average layer pressure.
;         t_ave        : FLOAT Average layer temperature.
;         molecule_id  : BYTE(8,64) Character data identifying moelcule.
;         mol_col_dens : FLOAT(64) Molecular column densities.
;         broad_dens   : FLOAT Broadening gases column densities.
;         dv           : FLOAT Frequency interval.
;         v1           : DOUBLE Calculation start frequency.
;         v2           : DOUBLE Calculation end frequency.
;         t_bound      : FLOAT Boundary temperature at H2.
;         emis_bound   : FLOAT Boundary emissivity at H2.
;         LBL_id       : Structure of input flags (HIRAC, LBLF4, etc.).
;         n_mol        : LONG Number of molecules used in LBL caclulation.
;         layer        : LONG Number of current layer.
;         yi1          : FLOAT unknown
;         yid          : BYTE(8,10) Character data containing time, date and 
;                          other bits and pieces.
;
; FUNCTION RESULT:
;       The function returns a flag indicating whether or not the data read was successful.
;        -1 = Error occurred in function.
;         0 = Read "failed"
;         1 = Read successful to EOF (end-of-file)
;         2 = Read successful to EOL (end-of-layer)
;       The quotes above are included because even though a fail condition is
;       returned, all of the data *may* have been read in.  An example of this
;       is if the user calls this function in a loop and the number of loops
;       exceeds the number of layers in the file.  In that case, all of the data
;       in the file will have been read but the function will be looking for the
;       "extra" layers that don't exist - hence the return error.
;
; CONTAINS:
;       lbl_open:  Function to check if the LBL file data needs to be byte-swapped
;                    and opens the file with the SWAP_ENDIAN keyword appropriately set.
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None
;
; SIDE EFFECTS:
;       Files opened by this routine are *NOT* closed *IF* the input LBLRTM/FASCODE
;       files consist of MORE THAN ONE layer of data. Multiple reads are required
;       for multi-level files. 
;
;       Files are closed if an EOF is detected.
;
;       Radiances are automatically scaled by 1.0e+07 to convert them to
;       mW/m2.sr.cm-1 units IF THE LBL FILE TYPE IS SET TO, OR DETECTED AS,
;       DOUBLE PANELED. 
;
; RESTRICTIONS:
;       None.
;
; PROCEDURE:
;       Before any data is read in, the file is checked to determine if byte swapping
;       needs to be performed, e.g. if the file was created on a big-endian platform
;       and is being read on a little-endian platform or vice versa. This is done by
;       checking the value of the file header LBL_id.hirac flag. If the flag is not
;       within an accepted value range (0-9), the LBL file is opened with the
;       SWAP_ENDIAN keyword.
;
;       The file header is read in and then panels are read in until an end-of-layer
;       (-99) or end-of-file (0) marker is encountered.  If the former, then the next
;       layer is read until the required layer data has been read in.
;
;       Single- or double-panel files are detected by checking the LBL_id(4) value.
;       If LBL_id(4) = 0, Single panel file,
;                    = 1, Double panel file.
;
;       Explicit end-of-file checking is carried out rather than using the 
;       "while ( not eof(lun) ) do" construct.  This is because the end-of-file can
;       occur at a file header read or a panel header read depending on the way
;       LBLRTM/FASCODE was run (in my experience anyway). The data read is done in
;       an open loop so no jumping out of explicit loops is required. This does
;       require the use of GOTO statements but, hey, I didn't create the file format.
;
; EXAMPLE:
;       Read the current layer of data from an LBLRTM/FASCODE file without
;       specifying the file type and store the data in an array DATA and 
;       the frequency info in an array V :
; 
;         result = lbl_read( 'TAPE13', lun, data, v )
;
;       Read layers of data within a loop.  Set the lbl_file_type keyword
;       so that the first call will bring up the file_type widget but subsequent
;       calls will not.  Assuming 20 layers of data in a file :
;
;         n_layers = 20
;         FOR i = 0, n_layers - 1 DO BEGIN
;           IF ( lbl_read( 'TAPE13', lun, data, v ) ) NE 0 ) THEN BEGIN
;                    .
;                    .
;                process data
;                    .
;                    .
;           ENDIF ELSE BEGIN
;                    .
;                    .
;               handle error condition
;                    .
;                    .
;           ENDELSE
;         ENDFOR
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC 07-Jul-1996
;                       paul.vandelst@ssec.wisc.edu
;
;                       Paul van Delst, 25-Mar-1997
;                       - Put under RCS control.
;
;  Copyright (C) 1996, 2001 Paul van Delst, CIMSS/SSEC/UW-Madison
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

;-


;###############################################################################
;                  ## CONTAINED PROCEDURES/FUNCTIONS ##
;###############################################################################


;-----------------------------------------------------------------------------
; NAME:
;       lbl_open
;
; PURPOSE:
;       Function to check if the LBL file data needs to be byte-swapped and
;         opens the file with the SWAP_ENDIAN keyword appropriately set.
;
; CALLING SEQUENCE:
;       result = lbl_open( lbl_file, $       ; Input
;                          lbl_file_lun )    ; Input/Output
;
; INPUTS:
;       lbl_file:      File name of the LBL format file to open.
;       lbl_file_lun:  Logical Unit Number of the LBL format file to open.
;
; OUTPUTS:
;       lbl_file_lun:  If not defined on input, it is set to a valid value
;                      on output.
;
;       The function returns a flag indicating the status of the open file
;       procedure:
;         result = -1 if an error occurred
;                =  1 if the file was opened successfully
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; MODIFICATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 28-Feb-1999
;                       paul.vandelst@ssec.wisc.edu
;
;-----------------------------------------------------------------------------


FUNCTION lbl_open, lbl_file, $     ; Input
                   lbl_file_lun, $ ; Input/Output
                   double = double ; Input keyword


  ; --------------------
  ; Set up error handler
  ; --------------------

  @error_codes

  CATCH, open_error_status

  IF ( open_error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS( lbl_file_lun ) GT 0 ) THEN FREE_LUN, lbl_file_lun
    RETURN, FAILURE
  ENDIF    


  ; ------------------------------------------
  ; Determine if data needs to be byte swapped
  ; ------------------------------------------

  ; -- Open file as "direct access"
  GET_LUN, lbl_file_lun
  OPENR, lbl_file_lun, lbl_file, $
         ERROR = open_error

  IF ( open_error NE 0 ) THEN $
    MESSAGE, 'Error opening LBL file for byte-swapping test.', $
             /NONAME, /NOPRINT


  ; -- Set the file pointer position to the file_header.t_ave file header value
  ; -- The value 4 is the Fortran sequential access begin-of-record longword marker
  ; -- and the other value is the no. of bytes from file_header.user_id to
  ; -- file_header.t_ave
  byte_offset = KEYWORD_SET( double ) ? 4 + 96 : 4 + 92
  POINT_LUN, lbl_file_lun, byte_offset


  ; -- Read T_AVE variable
  t_ave = KEYWORD_SET( double ) ? 0.0d : 0.0
  READU, lbl_file_lun, t_ave

  ; -- Check T_AVE for reasonable temperature values. If the values are
  ; -- *not* reasonable, then the data needs to be byte-swapped.
  IF ( t_ave LT 50.0 OR t_ave GT 400.0 ) THEN BEGIN
    swap = 1     ; Data must be byte swapped
    MESSAGE, 'Byte-swapping set on file open', /INFO
  ENDIF ELSE BEGIN
    swap = 0     ; Data does not need byte-swapping
  ENDELSE


  ; -- Close file
  FREE_LUN, lbl_file_lun


  ; -------------------
  ; Open file correctly
  ; -------------------

  GET_LUN, lbl_file_lun
  OPENR, lbl_file_lun, lbl_file, $
         ERROR = open_error, $
         /F77_UNFORMATTED, $
         SWAP_ENDIAN = swap

  IF ( open_error NE 0 ) THEN $
    MESSAGE, 'Error opening LBL file', $
             /NONAME, /NOPRINT


  ; ----
  ; Done
  ; ----

  CATCH, /CANCEL
  RETURN, SUCCESS

END





;###############################################################################
;                    ## MAIN PROCEDURE/FUNCTION ##
;###############################################################################

FUNCTION lbl_read, lbl_file, $                           ; Input
                   lbl_file_lun, $                       ; In/Output
                   lbl_spc, $                            ; Output
                   v_spc, $                              ; Output
                   no_rad_scale    = no_rad_scale, $     ; Input keyword
                   lbl_file_type   = lbl_file_type, $    ; Input keyword
                   no_lbl_data     = no_lbl_data, $      ; Input keyword
                   double          = double, $           ; Input keyword
                   dpf             = dpf, $              ; Input keyword
                   quiet           = quiet, $            ; Input keyword
                   lbl_file_header = lbl_file_header     ; Output keyword



  ;------------------------------------------------------------------------------
  ;                         -- Set up error handler --
  ;------------------------------------------------------------------------------

  @error_codes

  CATCH, error_status

  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS( lbl_file_lun ) GT 0 ) THEN FREE_LUN, lbl_file_lun
    RETURN, FAILURE
  ENDIF    


  ; -------------------------
  ; Define LBL specific codes
  ; -------------------------

  ; -- Return codes
  @lbl_codes

  ; -- Integer defining the end of a layer
  END_OF_LAYER = KEYWORD_SET( double ) ? -99LL : -99L



  ;------------------------------------------------------------------------------
  ;                         -- Check IDL Version --
  ;
  ; Function uses v5.4 features that won't work for earlier IDL releases
  ;------------------------------------------------------------------------------

  IF ( FLOAT( !VERSION.RELEASE ) LT 5.4 ) THEN $
    MESSAGE, 'IDL v5.4 or above required', $
             /NONAME, /NOPRINT


  ;------------------------------------------------------------------------------
  ;                            -- Check input --
  ;------------------------------------------------------------------------------

  n_arguments = 4
  IF ( N_PARAMS() LT n_arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT


  ; -----------------------------------------
  ; Check that required arguments are defined
  ; -----------------------------------------

  IF ( N_ELEMENTS( lbl_file ) EQ 0 ) THEN $
    MESSAGE, 'Input LBL_FILE argument not defined!', $
             /NONAME, /NOPRINT
             
  IF ( STRLEN( lbl_file ) EQ 0 ) THEN $
    MESSAGE, 'Input LBL_FILE argument not defined!', $
             /NONAME, /NOPRINT


  ; ----------------------------
  ; Set the radiance scale value
  ; ----------------------------

  ; -- Default
  rad_scale = KEYWORD_SET( double ) ? 1.0d+07 : 1.0e+07

  ; -- Turn it off
  IF ( KEYWORD_SET( no_rad_scale ) ) THEN $
    rad_scale = KEYWORD_SET( double ) ? 1.0d : 1.0


  ; -----------------------------------------------------
  ; Check that lbl_file_type has been specified correctly
  ; -----------------------------------------------------

  IF ( KEYWORD_SET( lbl_file_type ) ) THEN BEGIN

    file_type = STRUPCASE( lbl_file_type )

    IF ( ( file_type NE 'SINGLE' ) AND $
         ( file_type NE 'DOUBLE' )     ) THEN BEGIN
      MESSAGE, 'LBL_FILE_TYPE keyword must be SINGLE or DOUBLE panel. Using auto-detect.', /INFO
      lbl_file_type = ''
    ENDIF

  ENDIF



  ;------------------------------------------------------------------------------
  ;            -- Define LBLRTM/FASCODE file and panel structures --
  ;------------------------------------------------------------------------------

  fp_zero = KEYWORD_SET( double ) ? 0.0d : 0.0
  ip_zero = KEYWORD_SET( double ) ? 0LL : 0L


  ; ---------------------
  ; LBL ID flag structure
  ; ---------------------

  LBL_id = { hirac : ip_zero, $
             lblf4 : ip_zero, $
             xscnt : ip_zero, $
             aersl : ip_zero, $
             emit  : ip_zero, $
             scan  : ip_zero, $
             plot  : ip_zero, $
             path  : ip_zero, $
             jrad  : ip_zero, $
             test  : ip_zero, $
             merge : ip_zero, $
             scnid : fp_zero, $
             hwhm  : fp_zero, $
             idabs : ip_zero, $
             atm   : ip_zero, $
             layr1 : ip_zero, $
             nlayr : ip_zero  }


  ; ---------------------
  ; File header structure
  ; ---------------------

  file_header = { user_id      : BYTARR( 80 ),                                        $
                  secant       : 0.0d,                                                $
                  p_ave        : fp_zero,                                             $
                  t_ave        : fp_zero,                                             $
                  molecule_id  : BYTARR( 8, 64 ),                                     $
                  mol_col_dens : KEYWORD_SET( double ) ? DBLARR( 64 ) : FLTARR( 64 ), $
                  broad_dens   : fp_zero,                                             $
                  dv           : fp_zero,                                             $
                  v1           : 0.0d,                                                $
                  v2           : 0.0d,                                                $
                  t_bound      : fp_zero,                                             $
                  emis_bound   : fp_zero,                                             $
                  LBL_id       : LBL_id,                                              $
                  n_mol        : ip_zero,                                             $
                  layer        : ip_zero,                                             $
                  yi1          : fp_zero,                                             $
                  yid          : BYTARR( 8, 10 )                                      }


  ; ----------------------
  ; Panel header structure
  ; ----------------------

  panel_header = { v1    : 0.0d,    $
                   v2    : 0.0d,    $
                   dv    : fp_zero, $
                   n_pts : ip_zero  }




  ;------------------------------------------------------------------------------
  ;          -- Check if file is already open.  If not, open it. --
  ;------------------------------------------------------------------------------

  ; -------------------------------------
  ; Check if unit number variable defined
  ; -------------------------------------

  IF ( N_ELEMENTS( lbl_file_lun ) EQ 0 ) THEN BEGIN

    result = lbl_open( lbl_file, lbl_file_lun, $
                       double = double )

    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'LBL file open unsuccessful', $
               /NONAME, /NOPRINT

  ENDIF


  ; -------------------------------------------
  ; Check if file is open (in case lbl_file_lun
  ; is defined but file has been closed)
  ; -------------------------------------------

  file_info = FSTAT( lbl_file_lun )

  IF ( file_info.open EQ 0 ) THEN BEGIN

    FREE_LUN, lbl_file_lun
    result = lbl_open( lbl_file, lbl_file_lun, $
                       double = double )

    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'LBL file open unsuccessful', $
               /NONAME, /NOPRINT

  ENDIF



  ;------------------------------------------------------------------------------
  ;             -- Check for end of input file (this may not --
  ;             -- be the first call to this routine)        --
  ;------------------------------------------------------------------------------

  IF ( EOF( lbl_file_lun ) ) THEN BEGIN

    ; -- Output message and close file
    MESSAGE, 'EOF on input file', /INFO
    FREE_LUN, lbl_file_lun

    ; -- Undefine input arguments
    lbl_spc = 0B & dummy = TEMPORARY( lbl_spc )
    v_spc   = 0B & dummy = TEMPORARY( v_spc   )

    ; -- Done
    CATCH, /CANCEL
    RETURN, READ_TO_EOF

  ENDIF



  ;------------------------------------------------------------------------------
  ;       -- Read file header and assign it to return keyword variable --
  ;------------------------------------------------------------------------------

  ; ----------------------------
  ; Assign current file position
  ; ----------------------------

  POINT_LUN, -1L * lbl_file_lun, file_position


  ; ----------------
  ; Read file header
  ; ----------------

  READU, lbl_file_lun, file_header

  lbl_file_header = file_header
  dv = lbl_file_header.dv


  ; ---------------------------------
  ; Return if NO_LBL_DATA keyword set
  ; ---------------------------------

  IF ( KEYWORD_SET( no_lbl_data ) ) THEN BEGIN

    ; -- Set file position to what it was before the file header read
    POINT_LUN, lbl_file_lun, file_position

    ; -- Return in "Read successful to (previous) EOL" mode
    CATCH, /CANCEL
    RETURN, READ_TO_EOL

  ENDIF



  ;------------------------------------------------------------------------------
  ;             -- Check if file is double or single panel file --
  ;------------------------------------------------------------------------------

  IF ( NOT KEYWORD_SET( lbl_file_type ) ) THEN BEGIN

    IF ( lbl_file_header.LBL_id.emit EQ 0 ) THEN $
      file_type = 'SINGLE' $
    ELSE $
      file_type = 'DOUBLE'

  ENDIF

  IF ( NOT KEYWORD_SET( quiet ) ) THEN $
    PRINT, FORMAT = '( 5x, "Reading ", a, " panel file..." )', STRLOWCASE( file_type )



  ;------------------------------------------------------------------------------
  ;     -- Estimate number of spectral points in this layer + some slack --
  ;------------------------------------------------------------------------------
    
  n_pts = LONG( ( ( file_header.v2 - file_header.v1 ) / file_header.dv ) + 5.5 )


  IF ( KEYWORD_SET( dpf ) ) THEN BEGIN

    PRINT, FORMAT = '( /5x, "Frequency limits from file header   : ", f12.6, ", ", f12.6 )', $
                     file_header.v1, file_header.v2
    PRINT, FORMAT = '(  5x, "Frequency interval from file header : ", f12.6 )', $
                     file_header.dv
    PRINT, FORMAT = '(  5x, "Total number of points to be read   : ", i10 )', $
                     n_pts

  ENDIF



  ;------------------------------------------------------------------------------
  ;                        -- Create output array --
  ;------------------------------------------------------------------------------
    
  CASE file_type OF

    'SINGLE': lbl_spc = KEYWORD_SET( double ) ? DBLARR( n_pts,    /NOZERO ) : FLTARR( n_pts,    /NOZERO ) 
    'DOUBLE': lbl_spc = KEYWORD_SET( double ) ? DBLARR( n_pts, 2, /NOZERO ) : FLTARR( n_pts, 2, /NOZERO )

  ENDCASE



  ;------------------------------------------------------------------------------
  ;                       -- Read data panel by panel --
  ;------------------------------------------------------------------------------

  bksp = MAKE_ARRAY( 5, VALUE = 8B )
  IF ( NOT KEYWORD_SET( quiet ) ) THEN $
    PRINT, FORMAT = '( 5x, "Reading panel # :      ", $ )'


  ; ------------------------
  ; Initialise some counters
  ; ------------------------

  n_panels   = 0L
  n_pts_read = 0L


  ; --------------------------
  ; Begin panel read open loop
  ; --------------------------

  WHILE 1 DO BEGIN


    IF ( NOT KEYWORD_SET( quiet ) ) THEN $
      PRINT, FORMAT = '( a, i5, $ )', STRING( bksp ), n_panels + 1


    ; ---------------------------
    ; Check for end of input file
    ; ---------------------------

    IF ( EOF( lbl_file_lun ) ) THEN BEGIN

      FREE_LUN, lbl_file_lun
      return_code = READ_TO_EOF
      BREAK

    ENDIF


    ; -------------------------------------------------
    ; Read panel header and save first point wavenumber
    ; -------------------------------------------------
 
    READU, lbl_file_lun, panel_header
    IF ( n_panels EQ 0 ) THEN v1 = panel_header.v1
    v2 = panel_header.v2


    ; ----------------------
    ; Check for end-of-layer
    ; ----------------------

    IF ( panel_header.n_pts EQ END_OF_LAYER ) THEN BEGIN

      return_code = READ_TO_EOL
      BREAK

    ENDIF


    ; -----------------------------------------------------------------
    ; Read current panel data. The method I use of concatenating arrays
    ; as they are read is not too clever for very large files since the
    ; bigger the array to concatenate the more copying that goes on in
    ; memory == data read slows down. This is noticeable in the panel
    ; counter that slows down as the panel # increases.
    ; -----------------------------------------------------------------

    CASE file_type OF


      ; -----------------
      ; Single panel file
      ; -----------------

      'SINGLE': BEGIN

        ; -- Create array for LBLRTM data
        od = KEYWORD_SET( double ) ? DBLARR( panel_header.n_pts, /NOZERO ) : FLTARR( panel_header.n_pts, /NOZERO ) 

        ; -- Read in a single panel
        READU, lbl_file_lun, od

        ; -- Calculate indices for output array storage
        index_begin = n_pts_read
        index_end   = n_pts_read + panel_header.n_pts - 1

        ; -- Make sure end index is within bounds. If not,
        ; -- tack on some extra space on the end of the array.
        IF ( index_end GT ( n_pts - 1 ) ) THEN BEGIN

          n_pts_required = index_end - n_pts + 1

;          MESSAGE, 'Expanding output array by ' + $
;                   STRTRIM( STRING( n_pts_required ), 2 ), /INFO

          n_pts   = n_pts + n_pts_required
          lbl_spc = [ TEMPORARY( lbl_spc ), $
                      KEYWORD_SET( double ) ? DBLARR( n_pts_required, /NOZERO ) : FLTARR( n_pts_required, /NOZERO ) ]
        ENDIF

        ; -- Slot data into output array
        lbl_spc[ index_begin : index_end ] = TEMPORARY( od )

      END


      ; -----------------
      ; Double panel file
      ; -----------------

      'DOUBLE': BEGIN

        ; -- Create arrays for LBLRTM data
        rad = KEYWORD_SET( double ) ? DBLARR( panel_header.n_pts, /NOZERO ) : FLTARR( panel_header.n_pts, /NOZERO ) 
        tau = KEYWORD_SET( double ) ? DBLARR( panel_header.n_pts, /NOZERO ) : FLTARR( panel_header.n_pts, /NOZERO ) 

        ; -- Read in the two panels
        READU, lbl_file_lun, rad
        READU, lbl_file_lun, tau

        ; -- Calculate indices for output array storage
        index_begin = n_pts_read
        index_end   = n_pts_read + panel_header.n_pts - 1

        ; -- Make sure end index is within bounds. If not,
        ; -- tack on some extra space on the end of the array.
        IF ( index_end GT ( n_pts - 1 ) ) THEN BEGIN

          n_pts_required = index_end - n_pts + 1

          MESSAGE, 'Expanding output array by ' + $
                   STRTRIM( STRING( n_pts_required ), 2 ), /INFO

          n_pts   = n_pts + n_pts_required
          lbl_spc = TRANSPOSE( [ [ TEMPORARY( TRANSPOSE( lbl_spc ) ) ], $
                                 [ KEYWORD_SET( double ) ? DBLARR( 2, n_pts_required, /NOZERO ) : FLTARR( 2, n_pts_required, /NOZERO ) ] ] )
        ENDIF

        ; -- Slot data into output array
        lbl_spc[ index_begin : index_end, 0 ] = rad_scale * TEMPORARY( rad )
        lbl_spc[ index_begin : index_end, 1 ] = TEMPORARY( tau )

      END

    ENDCASE
 

    ; ----------------------------------
    ; Increment point and panel counters
    ; ----------------------------------

    n_pts_read = n_pts_read + panel_header.n_pts
    n_panels   = n_panels + 1


    ; -----------------
    ; Diagnostic output
    ; -----------------

    IF ( KEYWORD_SET( dpf ) ) THEN BEGIN

      PRINT, FORMAT = '( /10x, "Panel number                         : ", i5 )', $
                       n_panels
      PRINT, FORMAT = '( 10x, "Frequency limits from panel header   : ", f12.6, ", ", f12.6 )', $
                       panel_header.v1, panel_header.v2
      PRINT, FORMAT = '( 10x, "Frequency interval from panel header : ", f12.6 )', $
                       panel_header.dv
      PRINT, FORMAT = '( 10x, "Number of points in panel            : ", i6 )', $
                       panel_header.n_pts
      PRINT, FORMAT = '( 10x, "Number of points read so far         : ", i6 )', $
                       n_pts_read

    ENDIF

  ENDWHILE   ; Panel read loop



  ;------------------------------------------------------------------------------
  ;                -- End-of-read reached. Assemble output --
  ;------------------------------------------------------------------------------
    
  ; ---------------------------------
  ; Truncate output array if required
  ; ---------------------------------

  IF ( n_pts_read LT n_pts ) THEN BEGIN

    CASE file_type OF
      'SINGLE': lbl_spc = lbl_spc[ 0 : n_pts_read - 1 ]
      'DOUBLE': lbl_spc = lbl_spc[ 0 : n_pts_read - 1, * ]
    ENDCASE

  ENDIF


  ; ---------------------------------
  ; Construct return wavenumber array
  ; ---------------------------------

  v2 = v1 + ( DOUBLE( n_pts_read - 1 ) * DOUBLE( dv ) )

  v_spc = DINDGEN( n_pts_read ) / DOUBLE( n_pts_read - 1 )
  v_spc = v_spc * ( v2 - v1 ) + v1


  ; ----------------------------
  ; Output some info if required
  ; ----------------------------

  IF ( NOT KEYWORD_SET( quiet ) ) THEN BEGIN
    PRINT, FORMAT = '( /5x, a, " on input file after panel : ", i5 )', $
                    RETURN_TYPE[ return_code ], $
                    n_panels
    PRINT, FORMAT = '(  5x, "Number of points in layer     : ", i8 )', $
                    n_pts_read
  ENDIF


  ;------------------------------------------------------------------------------
  ;                                  -- Done --
  ;------------------------------------------------------------------------------

  CATCH, /CANCEL
  RETURN, return_code

END
