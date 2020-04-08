!
! Create_Solar
!
! Program to read the ASCII format Kurucz solar source spectrum and
! write it and a blackbody equivalent source function to a netCDF
! format file.
!
!
! FILES ACCESSED:
!       Input:  ASCII data file containing the regularly spaced Kurucz solar
!               irradiance data.
!
!       Output: netCDF format file containing the Kurucz solar source spectrum
!               and a blackbody equivalent source function.
!
!
! PROCEDURE:
!       The input data from the ASCII file is read in and simply written
!       out after being scaled to mW/m^2.cm-1. The frequency grid of this
!       data is used to calculate an equivalent blackbody source function.
!
!       First the Planck radiance, Bsolar, using an equivalent blackbody
!       temperature, Tsolar, is calculated. This radiance spectrum is 
!       converted to irradiance by assuming an isotropic source:
!
!         Fsolar = !PI * Bsolar
!
!       The solar irradiance at the top of the Earth's atmosphere is
!       then estimated by multiplying by the ratio of the areas of the
!       Sun and the sphere described by the mean Earth-Sun distance:
!
!                               [    mean_solar_radius    ]2
!         Fsolar@TOA = Fsolar * [-------------------------]
!                               [ mean_earth_sun_distance ]
!
!       Note the 4pi factors in the area formulae cancel. The values
!       used in the geometric scaling are:
!
!         mean_solar_radius       = 6.599e+08m (visible disk, or photosphere)
!         mean_earth_sun_distance = 1.495979e+11m
!
!       from the Solar_Define module.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 13-Oct-2000
!                       paul.vandelst@noaa.gov
!

PROGRAM Create_Solar

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE File_Utility         , ONLY: Get_Lun
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, Display_Message, Program_Message
  USE Fundamental_Constants, ONLY: PI
  USE Interpolate_Utility  , ONLY: Polynomial_Interpolate
  USE Average_Utility      , ONLY: Boxcar_Average
  USE Planck_Functions     , ONLY: Planck_Radiance
  USE Solar_Define         , ONLY: Solar_type      , &
                                   Solar_Associated, &
                                   Solar_Create    , &
                                   Solar_Destroy   , &
                                   Solar_Frequency
  USE Solar_IO             , ONLY: Solar_WriteFile
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_Solar'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  ! The input ASCII filename and data attributes
  CHARACTER(*), PARAMETER :: INFILE     = 'Kurucz_solar_extracted_490.0-26000cm-1.asc'
  CHARACTER(*), PARAMETER :: TITLE      = 'Kurucz synthetic and blackbody extraterrestrial solar source functions.'
  CHARACTER(*), PARAMETER :: HISTORY    = '; AER extract_solar.f'
  CHARACTER(*), PARAMETER :: COMMENT    = '; Data extracted from AER solar.kurucz.rad.mono.full_disk.bin file.'
  CHARACTER(*), PARAMETER :: SOURCE     = 'Solar spectrum computed with a version of the model atmosphere program ATLAS'
  CHARACTER(*), PARAMETER :: REFERENCES = 'Kurucz, R.L., Synthetic infrared spectra, in Infrared Solar Physics, '//&
                                          'IAU Symp. 154, edited by D.M. Rabin, J.T. Jefferies, and C. Lindsey, '//&
                                          'Kluwer, Acad., Norwell, MA, 1992.'
  ! Solar source function interpolation parameters
  INTEGER     , PARAMETER :: N_DF = 3
  REAL(fp)    , PARAMETER :: DF(N_DF) = (/  0.001_fp, 0.0025_fp,     0.1_fp /)
  REAL(fp)    , PARAMETER :: F1(N_DF) = (/  500.0_fp,  500.0_fp,   500.0_fp /)
  REAL(fp)    , PARAMETER :: F2(N_DF) = (/ 3500.0_fp, 3500.0_fp, 25900.0_fp /)
  CHARACTER(*), PARAMETER :: CF(N_DF) = (/ 'IR only', 'IR only',  'IR+VIS ' /)


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  CHARACTER(256) :: outfile
  CHARACTER(256) :: process_history, process_comment 
  CHARACTER(256) :: io_msg
  INTEGER :: fid
  INTEGER :: io_stat
  INTEGER :: err_stat
  INTEGER :: alloc_stat
  INTEGER :: i, n, idf, n_frequencies
  REAL(fp) :: f1_in, f2_in, df_in
  REAL(fp) :: omega
  REAL(fp), ALLOCATABLE :: f_in(:), h_in(:)
  TYPE(Solar_type) :: solar


  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read the ASCII format Kurucz solar source '//&
                        'spectrum and write it and a blackbody equivalent source '//&
                        'function to a netCDF format file.', &
                        '$Revision$' )


  ! Read the ASCII solar data file
  WRITE( *,'(/2x,"Reading ASCII Solar data file...")' )
  ! ...Open it
  fid = Get_Lun()
  IF ( fid < 0 ) THEN
    msg = 'Error obtaining file unit number for '//INFILE//' read.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  OPEN( fid, &
        FILE   = INFILE      , &
        STATUS = 'OLD'       , &
        ACCESS = 'SEQUENTIAL', &
        FORM   = 'FORMATTED' , &
        ACTION = 'READ'      , &
        IOSTAT = io_stat     , &
        IOMSG  = io_msg        )
  IF ( io_stat /= 0 ) THEN
    msg = 'Error opening '//INFILE//' - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...Read the file header
  READ( fid, &
        FMT    = *      , &
        IOSTAT = io_stat, &
        IOMSG  = io_msg   ) f1_in, f2_in, df_in, n
  IF ( io_stat /= 0 ) THEN
    msg = 'Error reading '//INFILE//' header - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...Allocate the input data arrays
  ALLOCATE( f_in(n), h_in(n), &
            STAT = alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    WRITE( msg,'("Error allocating input solar data arrays. STAT = ",i0)' ) alloc_stat
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...Read the data
  DO i = 1, n
    READ( fid, &
          FMT    = *      , &
          IOSTAT = io_stat, &
          IOMSG  = io_msg   ) f_in(i), h_in(i)
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading point number ",i0,&
                  &" irradiance value from ",a,&
                  &" - ",a)' ) i, INFILE, TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
  END DO
  CLOSE( fid )
  ! ...Output some info
  WRITE( *, '( 4x, "Input begin frequency : ", f12.6, " cm-1", &
             &/4x, "Input end frequency   : ", f12.6, " cm-1", &
             &/4x, "Input number of values: ", i0 )' ) &
            f_in(1), f_in(n), n


  ! Loop over the solar source function interpolation outputs
  Process_Loop: DO idf = 1, N_DF
  
    WRITE( *,'(/2x,"Processing data for df=",f6.4,"cm^-1 output...")' ) DF(idf)
    
    
    ! Ensure the input data spans the required frequencies
    IF ( MINVAL(f_in) > F1(idf) .OR. MAXVAL(f_in) < F2(idf) ) THEN
      msg = 'Input data does not span interpolation frequency range.'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Allocate the solar data object
    n_frequencies = NINT((F2(idf) - F1(idf)) / DF(idf)) + 1
    CALL Solar_Create( solar, n_frequencies )
    IF ( .NOT. Solar_Associated( solar ) ) THEN
      msg = 'Error allocating Solar structure'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
  
  
    ! Interpolate or average the solar source data
    ! ...Create the interpolation frequency grid
    CALL Solar_Frequency( solar, F1(idf), F2(idf) )
    ! ...Perform the interpolation/averaging
    SELECT CASE (TRIM(CF(idf)))
      CASE ('IR only')
        process_comment = '4-pt polynomial interpolation of original data'
        err_stat = Polynomial_Interpolate( &
          f_in, h_in, &
          solar%Frequency, &
          solar%Irradiance, &
          Order=3, &
          RCS_Id=process_history )
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error interpolating the solar data'
          CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
        END IF
      CASE ('IR+VIS')
        process_comment = 'Boxcar average of original data'
        err_stat = Boxcar_Average( &
          f_in, h_in, &
          F1(idf), F2(idf), DF(idf), &
          solar%Irradiance, i, &
          RCS_Id=process_history )
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error averaging the solar data'
          CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
        END IF

    END SELECT
    WRITE( *,'( 4x, "Output begin frequency   : ",f12.6," cm-1", &
              &/4x, "Output end frequency     : ",f12.6," cm-1", &
              &/4x, "Output frequency interval: ",es13.6," cm-1", &
              &/4x, "Output number of values  : ",i0)' ) &
             solar%f1, solar%f2, DF(idf), n_frequencies
!             F1(idf), F2(idf), DF(idf), n_frequencies


    ! Compute the blackbody solar source irradiance
    ! ...Calculate a blackbody radiance spectrum at the solar temperature
    err_stat = Planck_Radiance( &
      solar%Frequency, &
      solar%Blackbody_Temperature, &
      solar%Blackbody_Irradiance )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error calculating solar blackbody radiance spectrum'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Convert the radiance to irradiance
    omega = PI * ( solar%Radius / solar%Earth_Sun_Distance )**2
    solar%Blackbody_Irradiance = omega * solar%Blackbody_Irradiance


    ! Write the output file
    WRITE( outfile,'("dF_",f6.4,".Solar.nc")' ) DF(idf)
    err_stat = Solar_WriteFile( &
      outfile, &
      solar  , &
      Title      = TITLE, &
      History    = PROGRAM_VERSION_ID//'; '//TRIM(process_history)//'; '//HISTORY, &
      Comment    = 'Data for use with '//TRIM(CF(idf))//' channel SRFs; '//&
                   TRIM(process_comment)//'; '//COMMENT, &
      Source     = SOURCE, &
      References = REFERENCES )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing Solar file '//TRIM(outfile)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF

  END DO Process_Loop
  
  
  ! Clean up
  CALL Solar_Destroy( solar )
  DEALLOCATE( f_in, h_in, STAT=alloc_stat )

END PROGRAM Create_Solar
