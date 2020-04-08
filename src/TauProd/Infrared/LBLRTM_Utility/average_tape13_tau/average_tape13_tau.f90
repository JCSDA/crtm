PROGRAM average_tape13_tau

  USE type_kinds
  USE error_handler
  USE netcdf
  USE netcdf_utility
  USE lblrtm_utility
  USE spectral

  IMPLICIT NONE


  CHARACTER( LEN = 128 ) :: lblrtm_file

  REAL( Double ) :: v1_LBL, v2_LBL
  REAL( Double ) :: v1_Avg, v2_Avg
  REAL( Double ) :: v_Min,  v_Max

  REAL( Single ) :: dv_LBL, dv_Avg
  REAL( Single ) :: real_max_n_Avg

  INTEGER( Long ) :: max_n_LBL, max_n_Avg
  INTEGER( Long ) :: n_LBL,     n_Avg
  INTEGER( Long ) :: panel_type
  INTEGER( Long ) :: max_n_layers
  INTEGER( Long ) :: error_status
  INTEGER( Long ) :: file_id
  INTEGER( Long ) :: layer
  INTEGER( Long ) :: eof
  INTEGER( Long ) :: allocate_status
  INTEGER( Long ) :: ncdf_id
  INTEGER( Long ) :: n_points_dim_id, n_layers_dim_id
  INTEGER( Long ) :: begin_frequency_var_id, end_frequency_var_id, delta_frequency_var_id
  INTEGER( Long ) :: transmittance_var_id

  REAL( Double ), ALLOCATABLE, DIMENSION( : ) :: v_LBL,   v_Avg
  REAL( Single ), ALLOCATABLE, DIMENSION( : ) :: tau_LBL, tau_Avg

  CHARACTER( LEN = 80 ) :: outfile
  CHARACTER( LEN = 80 ) :: message
  CHARACTER( LEN = 7  ) :: output_action


! -- Id

  CHARACTER( LEN = 18 ),  PARAMETER :: program_name = 'AVERAGE_TAPE13_TAU'
  CHARACTER( LEN = 128 ), PARAMETER :: rcs_Id = &



!-------------------------------------------------------------------------------
!                          -- Program header --
!-------------------------------------------------------------------------------

  WRITE( *, '( 2/, 1x, a )' ) program_name
  WRITE( *, '( 1x, a )' ) rcs_Id



!-------------------------------------------------------------------------------
!                       -- Enter the LBLRTM filename --
!-------------------------------------------------------------------------------

  WRITE( *, '( /5x, "Enter LBLRTM input file : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) lblrtm_file



!-------------------------------------------------------------------------------
!                      -- Inquire the LBLRTM file --
!-------------------------------------------------------------------------------

  WRITE( *, '( /5x, "Inquiring LBLRTM file..." )' )

  error_status = lblrtm_inquire( lblrtm_file, &
                                 begin_frequency    = v1_LBL,       &
                                 end_frequency      = v2_LBL,       &
                                 frequency_interval = dv_LBL,       &
                                 panel_type         = panel_type,   &
                                 max_n_points       = max_n_LBL,    &
                                 max_n_layers       = max_n_layers, &
                                 first_only         = 1  )

  IF ( error_status /= success_state ) THEN
    CALL display_message( program_name, &
                          'Error inquiring LBLRTM input file', &
                          failure_state )
    STOP ' '
  END IF

  WRITE( *, '( 10x, "Begin frequency [v1_LBL]                : ", f12.6, &
             &/10x, "End frequency   [v2_LBL]                : ", f12.6, &
             &/10x, "Frequency interval [dv_LBL]             : ", f12.6, &
             &/10x, "Panel type [1=single,2=double]          : ", i8, &
             &/10x, "Max. no. of spectral points [max_n_LBL] : ", i8, &
             &/10x, "Max. no. of layers [max_n_layers]       : ", i8 )' ) &
            v1_LBL,       &
            v2_LBL,       &
            dv_LBL,       &
            panel_type,   &
            max_n_LBL,    &
            max_n_layers


  ! ------------------------------------
  ! Check that this is a file containing
  ! transmittance data
  ! ------------------------------------

  IF ( panel_type /= 2 ) THEN
    WRITE( message, '( a, " is not a double-panel file." )' ) &
                    TRIM( lblrtm_file )
    CALL display_message( program_name, &
                          message, &
                          failure_state )
    STOP ' '
  END IF



!-------------------------------------------------------------------------------
!                   -- Get spectral averaging parameters --
!-------------------------------------------------------------------------------

  ! --------------------------
  ! Get the averaging interval
  ! --------------------------

  ! -- Read the averaging frequency interval
  WRITE( *, '( /5x, "Enter the averaging frequency interval [>2x dv_LBL] : " )', &
            ADVANCE = 'NO' )
  READ( *, * ) dv_Avg

  ! -- Make sure it's at least 2x greater than the LBLRTM interval
  IF ( dv_Avg < ( 2.0 * dv_LBL ) ) THEN
    CALL display_message( program_name, &
                          'Averaging DV must be > twice the input LBLRTM DV', &
                          failure_state )
    STOP ' '
  END IF


  ! ----------------------------------
  ! Get the averaging frequency bounds
  ! ----------------------------------

  ! -- Read the averaging frequency bounds
  WRITE( *, '( 5x, "Enter begin and end frequencies over which ", &
                  &"to perform the boxcar average [v1,v2] : " )', &
            ADVANCE = 'NO' )
  READ( *, * ) v1_Avg, v2_Avg

  ! -- Make sure the values are in the correct order
  v_Min = MIN( v1_Avg, v2_Avg )
  v_Max = MAX( v1_Avg, v2_Avg )

  v1_Avg = v_Min; v2_Avg = v_Max


  ! -- Make sure values are within input data range - taking 
  ! -- the averaging kernel into account
  v_Min = v1_LBL + REAL( dv_Avg / 2.0, Double )
  v_Max = v2_LBL - REAL( dv_Avg / 2.0, Double )
  IF ( ( v1_Avg < v_Min ) .OR. &
       ( v2_Avg > v_Max ) ) THEN
    WRITE( message, '( "Invalid averaging frequency range. ", &
                      &"Valid range is [",f12.6,",",f12.6,"]")' ) &
                    v_Min, v_Max
    CALL display_message( program_name, &
                          message, &
                          failure_state )
    STOP ' '
  END IF


  ! -----------------------------------------------
  ! Estimate the maximum number of averaged points
  ! (The + 6.5 is to give 5 extra points of slop)
  ! The calculation is done in two steps since some
  ! compilers complain (i.e. they issue "CAUTION"
  ! messages) when a REAL division occurs in an
  ! expression being converted to INTEGER.
  ! -----------------------------------------------

  real_max_n_Avg = ( REAL( v2_Avg - v1_Avg, Single ) / dv_Avg ) + 6.5
  max_n_Avg      = INT( real_max_n_Avg, Long )
  WRITE( *, '( 10x, "Max. no. of averaged points [max_n_Avg] : ", i8 )' ) &
            max_n_Avg



!-------------------------------------------------------------------------------
!                      -- Open the LBLRTM file --
!-------------------------------------------------------------------------------

  error_status = lblrtm_open_file( TRIM( lblrtm_file ), &
                                   file_id )

  IF ( error_status /= success_state ) THEN
    CALL display_message( program_name, &
                          'Error opening LBLRTM input file', &
                          failure_state )
    STOP ' '
  END IF



!-------------------------------------------------------------------------------
!                       -- Get the output filename --
!-------------------------------------------------------------------------------

  WRITE( *, '( /5x, "Enter the output netCDF filename : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a ) ' ) outfile



!-------------------------------------------------------------------------------
!                     -- Allocate the data arrays --
!-------------------------------------------------------------------------------

  ! ---------------------------
  ! High resolution, LBL arrays
  ! ---------------------------

  ALLOCATE( tau_LBL( max_n_LBL ), &
            v_LBL(   max_n_LBL ), &
            STAT = allocate_status )

  IF ( allocate_status /= 0 ) THEN
    CALL display_message( program_name, &
                          'Error allocating LBL data arrays', &
                          failure_state )
    STOP ' '
  END IF
  

  ! ----------------------------
  ! Lower resolution, Avg arrays
  ! ----------------------------

  ALLOCATE( tau_Avg( max_n_Avg ), &
            v_Avg(   max_n_Avg ), &
            STAT = allocate_status )

  IF ( allocate_status /= 0 ) THEN
    CALL display_message( program_name, &
                          'Error allocating Avg data arrays', &
                          failure_state )
    STOP ' '
  END IF



!-------------------------------------------------------------------------------
!                        -- Loop over layers --
!-------------------------------------------------------------------------------

  WRITE( *, '(/)' )

  layer_loop: DO layer = 1, max_n_layers

    WRITE( *, '( 5x, "Averaging layer# ", i3, " of ", i3, "..." )' ) &
              layer, max_n_layers


    ! --------------------
    ! Read a layer of data
    ! --------------------

    error_status = lblrtm_read_layer( lblrtm_file, &
                                      file_id, &
                                      n_LBL, &
                                      eof, &
                                      frequency = v_LBL, &
                                      transmittance = tau_LBL )

    IF ( error_status /= success_state ) THEN
      WRITE( *, '( "Error reading layer ", i3, " of ", a )' ) &
                layer, TRIM( lblrtm_file )
      CALL display_message( program_name, &
                            message, &
                            failure_state )
      STOP ' '
    END IF

    WRITE( *, '( 10x, "No. of data points read : ", i8 )' ) n_LBL


    ! -----------------------
    ! Boxcar average the data
    ! -----------------------

    error_status = boxcar_average( tau_LBL, v_LBL,  n_LBL, &
                                   v1_Avg,  v2_Avg, dv_Avg, &
                                   tau_Avg, v_Avg,  n_Avg )

    IF ( error_status /= success_state ) THEN
      WRITE( *, '( "Error averaging layer ", i3, " of ", a )' ) &
                layer, TRIM( lblrtm_file )
      CALL display_message( program_name, &
                            message, &
                            failure_state )
      STOP ' '
    END IF

    WRITE( *, '( 10x, "No. of averaged points  : ", i8 )' ) n_Avg



    ! --------------------------
    ! Create the NetCDF data set
    ! --------------------------

    create_ncdf_file: IF ( layer == 1 ) THEN


      ! --------------------
      ! Create the data file
      ! --------------------

      error_status = NF90_CREATE( outfile,      &
                                  NF90_CLOBBER, &  ! Overwrite existing dataset
                                  ncdf_id       )

      IF ( error_status /= NF90_NOERR ) THEN
        CALL display_message( program_name, &
                              TRIM( NF90_STRERROR( error_status ) ), &
                              failure_state )
        STOP ' '
      END IF


      ! ---------------------
      ! Define the dimensions
      ! ---------------------

      ! -- Number of spectral points dimension
      error_status = NF90_DEF_DIM( ncdf_id,        &
                                   'n_points',     &
                                   n_Avg,          &
                                   n_points_dim_id )

      IF ( error_status /= NF90_NOERR ) THEN
        CALL display_message( program_name, &
                              'n_points def dim: '//TRIM( NF90_STRERROR( error_status ) ), &
                              failure_state )
        STOP ' '
      END IF


      ! -- Number of layers dimension
      error_status = NF90_DEF_DIM( ncdf_id,     &
                                   'n_layers',  &
                                   max_n_layers,       &
                                   n_layers_dim_id )

      IF ( error_status /= NF90_NOERR ) THEN
        CALL display_message( program_name, &
                              'n_layers def dim: '//TRIM( NF90_STRERROR( error_status ) ), &
                              failure_state )
        STOP ' '
      END IF


      ! --------------------
      ! Define the variables
      ! --------------------

      ! -- Define the begin frequency variable
      error_status = NF90_DEF_VAR( ncdf_id,           &
                                   'begin_frequency', &
                                   NF90_DOUBLE,       &
                                   begin_frequency_var_id )

      IF ( error_status /= NF90_NOERR ) THEN
        CALL display_message( program_name, &
                              'begin_frequency def var: '//TRIM( NF90_STRERROR( error_status ) ), &
                              failure_state )
        STOP ' '
      END IF

      error_status = nf90_put_def_atts( ncdf_id, &
                                        'begin_frequency', &
                                        long_name      = 'Calculation begin frequency', &
                                        units          = 'inverse centimeters (cm^-1)', &
                                        scale_factor   = 1.0d0, &
                                        add_offset     = 0.0d0, &
                                        valid_min      = v1_Avg, &
                                        valid_max      = v1_Avg, &
                                        fill_value     = -999.0d0, &
                                        missing_value  = -999.0d0, &
                                        fortran_format = '( f12.6 )' )

      IF ( error_status /= success_state ) THEN
        CALL display_message( program_name, &
                              'Error writing default attributes for begin_frequency variable', &
                              failure_state )
        STOP ' '
      END IF


      ! -- Define the end frequency variable
      error_status = NF90_DEF_VAR( ncdf_id,           &
                                   'end_frequency', &
                                   NF90_DOUBLE,       &
                                   end_frequency_var_id )

      IF ( error_status /= NF90_NOERR ) THEN
        CALL display_message( program_name, &
                              'end_frequency def var: '//TRIM( NF90_STRERROR( error_status ) ), &
                              failure_state )
        STOP ' '
      END IF

      error_status = nf90_put_def_atts( ncdf_id, &
                                        'end_frequency', &
                                        long_name      = 'Calculation end frequency  ', &
                                        units          = 'inverse centimeters (cm^-1)', &
                                        scale_factor   = 1.0d0, &
                                        add_offset     = 0.0d0, &
                                        valid_min      = v2_Avg, &
                                        valid_max      = v2_Avg, &
                                        fill_value     = -999.0d0, &
                                        missing_value  = -999.0d0, &
                                        fortran_format = '( f12.6 )' )

      IF ( error_status /= success_state ) THEN
        CALL display_message( program_name, &
                              'Error writing default attributes for end_frequency variable', &
                              failure_state )
        STOP ' '
      END IF


      ! -- Define the frequency interval variable
      error_status = NF90_DEF_VAR( ncdf_id,           &
                                   'delta_frequency', &
                                   NF90_FLOAT,       &
                                   delta_frequency_var_id )

      IF ( error_status /= NF90_NOERR ) THEN
        CALL display_message( program_name, &
                              'delta_frequency def var: '//TRIM( NF90_STRERROR( error_status ) ), &
                              failure_state )
        STOP ' '
      END IF

      error_status = nf90_put_def_atts( ncdf_id, &
                                        'delta_frequency', &
                                        long_name      = 'Calculation delta frequency', &
                                        units          = 'inverse centimeters (cm^-1)', &
                                        scale_factor   = 1.0, &
                                        add_offset     = 0.0, &
                                        valid_min      = dv_Avg, &
                                        valid_max      = dv_Avg, &
                                        fill_value     = -999.0, &
                                        missing_value  = -999.0, &
                                        fortran_format = '( f12.6 )' )

      IF ( error_status /= success_state ) THEN
        CALL display_message( program_name, &
                              'Error writing default attributes for delta_frequency variable', &
                              failure_state )
        STOP ' '
      END IF


      ! -- Define the transmittance variable and attributes
      error_status = NF90_DEF_VAR( ncdf_id,                                &
                                   'transmittance',                        &
                                   NF90_FLOAT,                             &
                                   (/ n_points_dim_id, n_layers_dim_id /), &
                                   transmittance_var_id                    )

      IF ( error_status /= NF90_NOERR ) THEN
        CALL display_message( program_name, &
                              'transmittance def var: '//TRIM( NF90_STRERROR( error_status ) ), &
                              failure_state )
        STOP ' '
      END IF

      error_status = nf90_put_def_atts( ncdf_id, &
                                        'transmittance', &
                                        long_name      = 'Spectrally averaged transmittance', &
                                        units          = 'None', &
                                        scale_factor   = 1.0, &
                                        add_offset     = 0.0, &
                                        valid_min      = 0.0, &
                                        valid_max      = 1.0, &
                                        fill_value     = -999.0, &
                                        missing_value  = -999.0, &
                                        fortran_format = '( f7.4 )' )

      IF ( error_status /= success_state ) THEN
        CALL display_message( program_name, &
                              'Error writing default attributes for transmittance variable', &
                              failure_state )
        STOP ' '
      END IF


      ! ----------------------------   
      ! Put the dataset in data mode
      ! ----------------------------   

      error_status = NF90_ENDDEF( ncdf_id )

      IF ( error_status /= NF90_NOERR ) THEN
        CALL display_message( program_name, &
                              'enddef: '//TRIM( NF90_STRERROR( error_status ) ), &
                              failure_state )
        STOP ' '
      END IF


      ! ------------------------
      ! Write the frequency data
      ! ------------------------

      error_status = NF90_PUT_VAR( ncdf_id,           &
                                   begin_frequency_var_id,  &
                                   v1_Avg )

      IF ( error_status /= NF90_NOERR ) THEN
        CALL display_message( program_name, &
                              'begin_frequency put var: '//TRIM( NF90_STRERROR( error_status ) ), &
                              failure_state )
        STOP ' '
      END IF

      error_status = NF90_PUT_VAR( ncdf_id,           &
                                   end_frequency_var_id,  &
                                   v2_Avg )

      IF ( error_status /= NF90_NOERR ) THEN
        CALL display_message( program_name, &
                              'end_frequency put var: '//TRIM( NF90_STRERROR( error_status ) ), &
                              failure_state )
        STOP ' '
      END IF

      error_status = NF90_PUT_VAR( ncdf_id,           &
                                   delta_frequency_var_id,  &
                                   dv_Avg )

      IF ( error_status /= NF90_NOERR ) THEN
        CALL display_message( program_name, &
                              'delta_frequency put var: '//TRIM( NF90_STRERROR( error_status ) ), &
                              failure_state )
        STOP ' '
      END IF

    END IF create_ncdf_file



    ! ----------------------------------------------------
    ! Write the current layers averaged transmittance data
    ! ----------------------------------------------------

    error_status = NF90_PUT_VAR( ncdf_id,                &
                                 transmittance_var_id,   &
                                 tau_Avg( 1 : n_Avg ),   &
                                 start = (/ 1, layer /), &
                                 count = (/ n_Avg, 1 /)  )

    IF ( error_status /= NF90_NOERR ) THEN
      CALL display_message( program_name, &
                            'transmittance put var: '//TRIM( NF90_STRERROR( error_status ) ), &
                            failure_state )
      STOP ' '
    END IF

  END DO layer_loop



!-------------------------------------------------------------------------------
!                      -- Deallocate the data arrays --
!-------------------------------------------------------------------------------

  DEALLOCATE( tau_LBL, tau_Avg, &
              v_LBL,   v_Avg,   &
              STAT = allocate_status )

  IF ( allocate_status /= 0 ) THEN
    CALL display_message( program_name, &
                          'Error deallocating data arrays', &
                          failure_state )
  END IF



!-------------------------------------------------------------------------------
!                      -- Close the output file --
!-------------------------------------------------------------------------------

  error_status = NF90_CLOSE( ncdf_id )

  IF ( error_status /= NF90_NOERR ) THEN
    CALL display_message( program_name, &
                          TRIM( NF90_STRERROR( error_status ) ), &
                          failure_state )
  END IF



!-------------------------------------------------------------------------------
!                      -- Close the LBLRTM file --
!-------------------------------------------------------------------------------

  error_status = lblrtm_close_file( TRIM( lblrtm_file ), &
                                    file_id )

  IF ( error_status /= success_state ) THEN
    CALL display_message( program_name, &
                          'Error closing LBLRTM input file', &
                          failure_state )
  END IF



!-------------------------------------------------------------------------------
!                        -- Regular end of program --
!-------------------------------------------------------------------------------

  WRITE( *, '( /5x, "Done.", / )' )


END PROGRAM average_tape13_tau


!-------------------------------------------------------------------------------
! CVS/RCS modification history
!
! $Log: average_tape13_tau.f90,v $
! Revision 1.6  2000/07/13 23:19:58  paulv
! - Replaced output of the frequency grid with output of the begin, end,
!   and delta frequency scalars.
!
! Revision 1.5  2000/06/30 22:25:45  paulv
! - Altered the way that real expressions involving divisions are converted
!   to real type. The IRIX f90 compiler was issuing "CAUTION" messages for
!   doing the following:
!     int_value = INT( real_value1 / real_value2 )
!   So, the above was changed to:
!     real_value = real_value1 / real_value2
!     int_value  = INT( real_value )
!
! Revision 1.4  2000/06/30 21:37:32  paulv
! - Changed call to netCDF default attribute write function to reflect
!   changes in the function interface.
!
! Revision 1.3  2000/06/30 15:54:55  paulv
! - Output file is now written in netCDF format using the Fortran-90
!   interface.
! - Module netcdf_utility is used to write default variables attributes.
!
! Revision 1.2  2000/06/29 17:24:37  paulv
! - Output filename now specified by user
! - Number of points read in and number of averaged points written per
!   layer output to standard output.
!
!
!-------------------------------------------------------------------------------


