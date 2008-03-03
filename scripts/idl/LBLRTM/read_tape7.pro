;+
; NAME:
;       read_tape7
;
; PURPOSE:
;       Procedure to read LBLRTM/FASCODE TAPE7 format files.
;
; CATEGORY:
;       LBL
;
; CALLING SEQUENCE:
;       read_tape7, tape7_file, tape7_data
;
; INPUTS:
;       tape7_file:  TAPE7 data filename.
;
; OUTPUTS:
;       tape7_data:  Structure containing the TAPE7 data.  The output can have
;                    two forms depending on the input format. If there is only 
;                    molecular information in the TAPE7 input, then the returned
;                    structure has the following tag names:
;                      tape7_data = { i_form n_layers n_mols secnt0 h1 h2 ang len
;                                     pressure temperature moldens p_top p_bot t_top
;                                     t_bot z_top z_bot }
;                    If, however, CFC data is present, the returned structure has
;                    the following tag names, each of which is a structure:
;                      tape7_data = { atm_data cfc_data }
;                    The cfc_data structure is identical to that of atm_data except
;                    for the tag cfc_hdr.
;
; SIDE EFFECTS:
;        None known.
;
; RESTRICTIONS:
;        Not fully tested, so unknown.
;
; EXAMPLE:
;        read_tape7, 'TAPE7', tape7_structure
;        print, tag_names( tape7_structure )
;
; MODIFICATION HISTORY:
; 	Written by:      Paul van Delst, CIMSS/SSEC 27-Mar-1997
;
;-

pro read_data, lun_data, data_structure, $
               cfc_hdr = cfc_hdr



;------------------------------------------------------------------------------
;                              -- Read data --
;------------------------------------------------------------------------------

  c_hdr =  ' '
  readf, lun_data, c_hdr

  i_form   = long( strmid( c_hdr, 0, 2 ) )
  n_layers = long( strmid( c_hdr, 2, 3 ) )
  n_mols   = long( strmid( c_hdr, 8, 2 ) )
  secnt0   = double( strmid( c_hdr, 10, 10 ) )
  h1       = float( strmid( c_hdr, 40, 8 ) )
  h2       = float( strmid( c_hdr, 52, 8 ) )
  ang      = float( strmid( c_hdr, 65, 8 ) )
  len      = long( strmid( c_hdr, 78, 2 ) )


; ----------------
; Loop over layers
; ----------------

; -- Set up arrays --

  pressure    = fltarr( n_layers )
  temperature = fltarr( n_layers )

  moldens       = fltarr( n_mols, n_layers )
  moldens_tmp   = fltarr( n_mols + 1 > 8 )
  if ( n_mols le 7 ) then $
    moldens_loc = lindgen( n_mols ) $
  else $
    moldens_loc = [ lindgen( 7 ), lindgen( n_mols - 7 ) + 8 ]

  path    = intarr( n_layers )
  p_top   = fltarr( n_layers )
  p_bot   = fltarr( n_layers )
  t_top   = fltarr( n_layers )
  t_bot   = fltarr( n_layers )
  z_top   = fltarr( n_layers )
  z_bot   = fltarr( n_layers )
  broaddens = fltarr( n_layers )


; -- Read and parse first layer...format is a bit different --

  c_buf = ' '
  readf, lun_data, c_buf
  pressure( 0 )    = float( strmid( c_buf, 0, 15 ) )
  temperature( 0 ) = float( strmid( c_buf, 15, 10 ) )
  path( 0 )  = fix( strmid( c_buf, 39, 1 ) )
  z_bot( 0 ) = float( strmid( c_buf, 41, 7 ) )
  p_bot( 0 ) = float( strmid( c_buf, 48, 8 ) )
  t_bot( 0 ) = float( strmid( c_buf, 56, 7 ) )
  z_top( 0 ) = float( strmid( c_buf, 63, 7 ) )
  p_top( 0 ) = float( strmid( c_buf, 70, 8 ) ) 
  t_top( 0 ) = float( strmid( c_buf, 78, 7 ) )

  readf, lun_data, moldens_tmp
  moldens( *, 0 ) = moldens_tmp( moldens_loc )
  broaddens( 0 )  = moldens_tmp( 7 )



; -- Read remaining layers --

  for i = 1, n_layers - 1 do begin

    readf, lun_data, c_buf
    pressure( i )    = float( strmid( c_buf, 0, 15 ) )
    temperature( i ) = float( strmid( c_buf, 15, 10 ) )
    path( i )  = fix( strmid( c_buf, 39, 1 ) )
    z_top( i ) = float( strmid( c_buf, 63, 7 ) )
    p_top( i ) = float( strmid( c_buf, 70, 8 ) ) 
    t_top( i ) = float( strmid( c_buf, 78, 7 ) )
    p_bot( i ) = p_top( i - 1 )
    t_bot( i ) = t_top( i - 1 )
    z_bot( i ) = z_top( i - 1 )

    readf, lun_data, moldens_tmp
    moldens( *, i ) = moldens_tmp( moldens_loc )
    broaddens( i )  = moldens_tmp( 7 )

  endfor



;------------------------------------------------------------------------------
;                      -- Load data structure --
;------------------------------------------------------------------------------

  if ( keyword_set( cfc_hdr ) ) then $
    data_structure = { cfc_hdr     : cfc_hdr, $
                       i_form      : i_form, $
                       n_layers    : n_layers, $
                       n_mols      : n_mols, $
                       secnt0      : secnt0, $
                       h1          : h1, $
                       h2          : h2, $
                       ang         : ang, $
                       len         : len, $
                       pressure    : pressure, $
                       temperature : temperature, $
                       moldens     : moldens, $
                       path        : path, $
                       p_top       : p_top, $
                       p_bot       : p_bot, $
                       t_top       : t_top, $
                       t_bot       : t_bot, $
                       z_top       : z_top, $
                       z_bot       : z_bot, $
                       broaddens   : broaddens } $
  else $
    data_structure = { i_form      : i_form, $
                       n_layers    : n_layers, $
                       n_mols      : n_mols, $
                       secnt0      : secnt0, $
                       h1          : h1, $
                       h2          : h2, $
                       ang         : ang, $
                       len         : len, $
                       pressure    : pressure, $
                       temperature : temperature, $
                       moldens     : moldens, $
                       path        : path, $
                       p_top       : p_top, $
                       p_bot       : p_bot, $
                       t_top       : t_top, $
                       t_bot       : t_bot, $
                       z_top       : z_top, $
                       z_bot       : z_bot, $
                       broaddens   : broaddens }

  return

end






pro read_tape7, tape7_file, tape7_data, $
                water_vapor_mixing_ratio = water_vapor_mixing_ratio


;------------------------------------------------------------------------------
;                              -- Read data --
;------------------------------------------------------------------------------

; ---------
; Open file
; ---------

  OPENR, lun_tape7, tape7_file, /GET_LUN


; ---------------------
; Read atmospheric data
; ---------------------

  read_data, lun_tape7, atm_data


; ---------------------------------------------
; Save the atmospheric molecular data for water
; vapour mixing ratio calculation
; ---------------------------------------------

  n_layers   = atm_data.n_layers
  n_mols     = atm_data.n_mols
  moldens    = atm_data.moldens
  broaddens  = atm_data.broaddens


; ---------------------
; Check for CFC data...
; ---------------------

  IF ( EOF( lun_tape7 ) eq 0 ) THEN BEGIN

    cfc_hdr1 = ' '
    READF, lun_tape7, cfc_hdr1
    
    IF ( STRPOS( cfc_hdr1, 'CROSS-SECTIONS' ) NE -1 ) THEN BEGIN
      PRINT, FORMAT = '( /5x, "CFC data detected..." )'

      n_cfc_mols = LONG( STRMID( cfc_hdr1, 0, 5 ) )
      ixsbin     = LONG( STRMID( cfc_hdr1, 10, 5 ) )

      cfc_hdr2 = ' '
      READF, lun_tape7, cfc_hdr2
      cfc_names  = (STR_SEP( STRTRIM( STRCOMPRESS( cfc_hdr2 ), 2 ), ' ' ))( 0 : n_cfc_mols - 1 )


      PRINT, FORMAT = '( 10x, "CFC present : ", a )', cfc_names

      read_data, lun_tape7, cfc_data, $
                 cfc_hdr = [ cfc_hdr1, cfc_hdr2 ]

      tape7_data = { atm_data : atm_data, cfc_data : cfc_data }

    ENDIF

  ENDIF ELSE $

    tape7_data = TEMPORARY( atm_data )


;------------------------------------------------------------------------------
;                      -- Close TAPE7 input file --
;------------------------------------------------------------------------------

  FREE_LUN, lun_tape7



;------------------------------------------------------------------------------
;                --  Calculate water vapour mixing ratio --
;------------------------------------------------------------------------------

; --------------------------------------------------------
; Define acceleration due to gravity and Avagadro's number
; --------------------------------------------------------

  gravity      = 9.80665
  avagadros_no = 6.022045e+23


; ------------------------------------------
; Define molecular weights for all molecules
; ------------------------------------------

  molecular_weight = [ 18.0152, 44.0098, 47.9982, 44.0128, 28.0104, 16.0426, $
                       31.9988, 30.0061, 64.0588, 46.0055, 17.0304, 63.0128, $
                       17.0073, 20.0063, 36.4609, 80.9119,127.9124, 51.4524, $
                       60.0704, 30.0262, 52.4603, 28.0134, 27.0256, 50.4877, $
                       34.0146, 26.0378, 30.0694, 33.9975, 66.0072,146.0504, $
                       34.0758, 42.0256 ]


; -------------------------------------
; Compute column density for each layer
; -------------------------------------

  molecule_weight = FLTARR( n_mols, n_layers )

  FOR i = 0, n_mols - 1 DO $
    molecule_weight[ i, * ] = moldens[ i, * ] * molecular_weight[ i ] / avagadros_no

  mass_of_dry_air = TOTAL( molecule_weight[ 1 : n_mols - 1, * ], 1 )


; -----------------------------------------------------------
; Include broadening gases only if the number of molecules is
; less than 22 - this assumes that the "other" gases are
; composed mostly of nitrogen
; -----------------------------------------------------------

  IF ( n_mols LT 22 ) THEN $
    mass_of_dry_air = mass_of_dry_air + ( broaddens * molecular_weight[ 21 ] / avagadros_no )
 

; -------------------------------------------
; Calculate water vapour mixing ratio in g/kg
; -------------------------------------------
 
  water_vapor_mixing_ratio = 1000.0 * REFORM( molecule_weight[ 0, * ] ) / mass_of_dry_air
 


;------------------------------------------------------------------------------
;                               --  Done --
;------------------------------------------------------------------------------

  RETURN

END

