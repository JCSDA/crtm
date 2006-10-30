PRO check_airs_srf_interpolation, ylog = ylog



  ;#--------------------------------------------------------------------------#
  ;#                        -- SET UP AN ERROR HANDLER --                     #
  ;#--------------------------------------------------------------------------#

  @error_codes

  CATCH, error_status

  IF ( error_status NE 0 ) THEN BEGIN
    MESSAGE, !ERR_STRING, /CONTINUE
    CATCH, /CANCEL
    RETURN
  ENDIF



  ;#--------------------------------------------------------------------------#
  ;#                            -- CHECK INPUT --                             #
  ;#--------------------------------------------------------------------------#

  ; ---------------------------------
  ; Plotting range based on plot type
  ; ---------------------------------

  IF ( KEYWORD_SET( ylog ) ) THEN $
    yrange = [ 0.0001, 1.0 ] $
  ELSE $
    yrange = [ 0.0, 1.0 ]



  ;#--------------------------------------------------------------------------#
  ;#                      -- READ THE UMBC AIRS SRF FILE --                   #
  ;#--------------------------------------------------------------------------#

  UMBC_SRF_file = 'airs_srf.nc'

  PRINT, FORMAT = '( /5x, "Reading the UMBC AIRS SRF file...." )'

  result = read_ncdf( UMBC_SRF_file, SRF, /quiet )
  IF ( result NE SUCCESS ) THEN $
    MESSAGE, 'Error reading ' + UMBC_SRF_file, $
             /NOPRINT, /NONAME


  ; -------------------------------------------
  ; Construct the UMBC AIRS SRF frequency array
  ; -------------------------------------------

  n_AIRS_Channels = N_ELEMENTS( SRF.chanid )
  n_AIRS_SRF_Points = N_ELEMENTS( SRF.fwgrid )

  SRF_Frequency = ( SRF.fwgrid # SRF.width ) + REBIN( SRF.freq, n_AIRS_SRF_Points, n_AIRS_Channels )



  ;#--------------------------------------------------------------------------#
  ;#            -- LOOP OVER INTERPOLATED AIRS MODULE SRF FILES --            #
  ;#--------------------------------------------------------------------------#

  ; --------------------------------------------
  ; Set the number of channels at a time to plot 
  ; --------------------------------------------

  n_channels_to_plot = 20


  ; ----------------------------------------------
  ; Obtain the list of interpolated AIRS SRF files
  ; ----------------------------------------------

  iSRF_files = FINDFILE( 'airs*_aqua.srf.nc' )
  n_files = N_ELEMENTS( iSRF_files )


  ; -------------------
  ; Begin the file loop
  ; -------------------

  FOR i = 0L, n_files - 1 DO BEGIN


    ; -----------------------------------
    ; Read the interpolated SRF data file
    ; -----------------------------------

    PRINT, FORMAT = '( 5x, "Reading the interpolated AIRS SRF module file ", a, "...." )', $
           STRTRIM( iSRF_files[ i ], 2 )

    result = read_ncdf( iSRF_files[ i ], iSRF, /quiet )
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error reading ' + iSRF_files[ i ], $
               /NOPRINT, /NONAME


    ; ----------------------------------------------------
    ; Determine the number of channels in this module file
    ; ----------------------------------------------------

    n_channels = N_ELEMENTS( iSRF.channel_list )


    ; -----------------------------------------------------
    ; Get the field names of the current module's structure
    ; -----------------------------------------------------

    fields = TAG_NAMES( iSRF )


    ; -------------------------------------------------
    ; Loop over all channels in the current module file
    ; -------------------------------------------------

    FOR l = 0, n_channels - 1, n_channels_to_plot DO BEGIN


      ; ------------------------------------------------------
      ; Determine the channel range for the current "plot set"
      ; This is required to ensure that l2 doesn't exceed the
      ; array boundaries
      ; ------------------------------------------------------

      l1 = l
      l2 = ( l1 + n_channels_to_plot - 1 ) < ( n_channels - 1 )


      ; -------------------------------------------------
      ; Determine the frequency range for the current set
      ; and plot the boundaries
      ; -------------------------------------------------

      min_v = MIN( iSRF.begin_frequency[l1:l2] )
      max_v = MAX( iSRF.end_frequency[l1:l2]   )
      wPLOT, [ min_v, max_v ], yrange, $
            TITLE = iSRF_files[i], $
            YLOG = ylog, $
            /NODATA


      ; -------------------------------------------------
      ; Loop over every channel in the current "plot set"
      ; -------------------------------------------------

      FOR ln = l1, l2 DO BEGIN

        ; -- Get the position of the current interpolated
        ; -- SRF array in the structure
        varname = 'channel_' + STRTRIM( iSRF.channel_list( ln ), 2 ) + '_response'
        fieldloc = WHERE( STRUPCASE( varname ) EQ STRUPCASE( fields ), count )
        IF ( count EQ 0 ) THEN CONTINUE
        j = fieldloc[ 0 ]

        ; -- Construct the interpolated SRF frequency grid 
        niSRF = N_ELEMENTS( iSRF.(j) )
        iSRF_frequency = DINDGEN( niSRF ) / DOUBLE( niSRF-1 )
        iSRF_frequency = iSRF_frequency * ( iSRF.end_frequency[ln] - iSRF.begin_frequency[ln] ) + iSRF.begin_frequency[ln]

        ; -- Plot the SRFs
        wOPLOT, iSRF_frequency, iSRF.(j), COLOR = 5
        wOPLOT, SRF_Frequency[ *, iSRF.Channel_List[ln]-1 ], $
               SRF.srfval[ *, iSRF.Channel_List[ln]-1 ], $
               COLOR = 4

      ENDFOR

      ql = GET_KBRD( 1 )
      IF ( STRUPCASE( ql ) EQ 'Q' ) THEN RETURN
      IF ( STRUPCASE( ql ) EQ 'N' ) THEN BREAK


    ENDFOR

    IF ( STRUPCASE( ql ) NE 'N' ) THEN BEGIN
      q = GET_KBRD( 1 )
      IF ( STRUPCASE( q ) EQ 'Q' ) THEN RETURN
    ENDIF

  ENDFOR

END




;-------------------------------------------------------------------------------
;                          -- MODIFICATION HISTORY --
;-------------------------------------------------------------------------------
;
; $Id: check_airs_srf_interpolation.pro,v 1.1 2002/09/26 15:31:16 paulv Exp $
;
; $Date: 2002/09/26 15:31:16 $
;
; $Revision: 1.1 $
;
; $State: Exp $
;
; $Log: check_airs_srf_interpolation.pro,v $
; Revision 1.1  2002/09/26 15:31:16  paulv
; Initial checkin.
;
;
;
;
;
