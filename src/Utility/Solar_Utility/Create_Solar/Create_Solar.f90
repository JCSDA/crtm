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
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, &
                                   Display_Message, Program_Message
  USE Fundamental_Constants, ONLY: PI
  USE Interpolate_Utility  , ONLY: Polynomial_Interpolate
  USE Average_Utility      , ONLY: Boxcar_Average
  USE Planck_Functions     , ONLY: Planck_Radiance
  USE Solar_Define         , ONLY: Solar_type    , &
                                   Allocate_Solar, &
                                   Destroy_Solar , &
                                   Frequency_Solar
  USE Solar_netCDF_IO      , ONLY: Write_Solar_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_Solar'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  ! The input ASCII filename
  CHARACTER(*), PARAMETER :: INFILE = 'Kurucz_solar_extracted_490.0-26000cm-1.asc'
  ! Solar source function interpolation parameters
  INTEGER     , PARAMETER :: N_DF = 3
  REAL(fp)    , PARAMETER :: DF(N_DF) = (/  0.001_fp, 0.0025_fp,     0.1_fp /)
  REAL(fp)    , PARAMETER :: F1(N_DF) = (/  500.0_fp,  500.0_fp,   500.0_fp /)
  REAL(fp)    , PARAMETER :: F2(N_DF) = (/ 3500.0_fp, 3500.0_fp, 25000.0_fp /)
  CHARACTER(*), PARAMETER :: CF(N_DF) = (/ 'IR only', 'IR only',  'IR+VIS ' /)


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Outfile
  CHARACTER(256) :: Process_RCS_Id, Process_Comment 
  INTEGER :: FileID
  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: i, n, idf
  REAL(fp) :: f1_in, f2_in, df_in
  REAL(fp) :: Omega
  REAL(fp), ALLOCATABLE :: f_in(:), irrad_in(:)
  TYPE(Solar_type) :: Solar


  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read the ASCII format Kurucz solar source '//&
                        'spectrum and write it and a blackbody equivalent source '//&
                        'function to a netCDF format file.', &
                        '$Revision$' )

  ! Get user frequency interval input
  WRITE( *,'(/5x,"Select frequency interval")' )
  DO i = 1, N_DF
    WRITE( *,'(10x,i1,") ",f6.4,3x,"(for ",f7.1,"-",f7.1,"cm-1)")' ) i, DF(i), F1(i), f2(i)
  END DO
  WRITE( *,'(5x,"Enter choice : ")',ADVANCE='NO' )
  READ( *,* ) idf
  

  ! Read the ASCII solar data file
  WRITE( *,'(/5x,"Reading ASCII Solar data file...")' )
  ! ..Open it
  FileID = Get_Lun()
  IF ( FileID < 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error obtaining file unit number for '//INFILE//' read.', &
                          FAILURE )
    STOP
  END IF
  OPEN( FileID, FILE  =INFILE, &
                STATUS='OLD', &
                ACCESS='SEQUENTIAL', &
                FORM  ='FORMATTED', &
                ACTION='READ', &
                IOSTAT=IO_Status )
  IF ( IO_Status /= 0 ) THEN
    WRITE( Message,'("Error opening ",a,". IOSTAT = ",i0)' ) &
                    INFILE, IO_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF
  ! ..Read the file header
  READ( FileID,*,IOSTAT=IO_Status ) f1_in, f2_in, df_in, n
  IF ( IO_Status /= 0 ) THEN
    WRITE( Message,'("Error reading ",a," header. IOSTAT = ",i0)' ) &
                    INFILE, IO_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    CLOSE( FileID )
    STOP
  END IF
  ! ..Allocate the input data arrays
  ALLOCATE( f_in(n), irrad_in(n), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'( "Error allocating input solar data arrays. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    CLOSE( FileID )
    STOP
  END IF
  ! ..Read the data
  DO i = 1, n
    READ( FileID,*,IOSTAT=IO_Status ) f_in(i), irrad_in(i)
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading point number ",i0,&
                      &" irradiance value from ",a,&
                      &". IOSTAT = ",i5)' ) &
                      i, INFILE, IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      CLOSE( FileID )
      STOP
    END IF
  END DO
  CLOSE( FileID )
  WRITE( *, '( 10x, "Input begin frequency:     ", f12.6, " cm-1", &
             &/10x, "Input end frequency:       ", f12.6, " cm-1", &
             &/10x, "Input number of values:    ", i0 )' ) &
            f_in(1), f_in(n), n
  ! ..Ensure the input data spans the required frequencies
  IF ( MINVAL(f_in) > F1(idf) .OR. MAXVAL(f_in) < F2(idf) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input data does not span interpolation frequency range.', &
                          FAILURE )
    STOP
  END IF

  ! Interpolate or average the solar source data
  WRITE( *,'(/5x,"Processing the input Solar data...")' )
  ! ..Determine the number of interpolated points
  n = NINT((F2(idf)-F1(idf))/DF(idf)) + 1
  ! ..Allocate the Solar data structure
  Error_Status = Allocate_Solar( n, Solar )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Solar structure.', &
                          FAILURE )
    STOP
  END IF
  Solar%Begin_Frequency    = F1(idf)
  Solar%End_Frequency      = F2(idf)
  Solar%Frequency_Interval = DF(idf)
  ! ..Create the interpolation frequency grid
  Error_Status = Frequency_Solar( Solar )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing the Solar interpolated frequency grid.', &
                          FAILURE )
    STOP
  END IF
  ! ..Perform the interpolation/averaging
  SELECT CASE (TRIM(CF(idf)))
    CASE ('IR only')
      WRITE( *,'(/5x,"Interpolating the input Solar data...")' )
      Process_Comment = '4-pt polynomial interpolation of original Kurucz data. '
      Error_Status = Polynomial_Interpolate( f_in, irrad_in, &
                                             Solar%Frequency, &
                                             Solar%Irradiance, &
                                             Order=3, &
                                             RCS_Id=Process_RCS_Id )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error interpolating the solar data.', &
                              Error_Status )
        STOP
      END IF
    CASE ('IR+VIS')
      WRITE( *,'(/5x,"Averaging the input Solar data...")' )
      Process_Comment = 'Boxcar average of original Kurucz data. '
      Error_Status = Boxcar_Average( f_in, irrad_in, &
                                     F1(idf), F2(idf), DF(idf), &
                                     Solar%Irradiance, i, &
                                     RCS_Id=Process_RCS_Id )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error averaging the solar data.', &
                              Error_Status )
        STOP
      END IF
  END SELECT
  WRITE( *,'( 10x, "Output begin frequency:    ",f12.6," cm-1", &
            &/10x, "Output end frequency:      ",f12.6," cm-1", &
            &/10x, "Output frequency interval: ",es13.6," cm-1", &
            &/10x, "Output number of values:   ",i0)' ) &
           F1(idf), F2(idf), DF(idf), n


  ! Compute the blackbody solar source irradiance
  WRITE( *,'(/5x,"Calculating the blackbody solar source irradiance...")' )
  ! ..Calculate a blackbody radiance spectrum at the solar temperature
  Error_Status = Planck_Radiance( Solar%Frequency, &
                                  Solar%Blackbody_Temperature, &
                                  Solar%Blackbody_Irradiance )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error calculating solar blackbody radiance spectrum.', &
                          Error_Status )
    STOP
  END IF
  ! ..Compute the geometry factor
  Omega = PI * ( Solar%Radius / Solar%Earth_Sun_Distance )**2
  ! ..Compute the irradiance
  Solar%Blackbody_Irradiance = Omega * Solar%Blackbody_Irradiance


  ! Write the output netCDF file
  WRITE( *,'(/5x,"Writing the output netCDF Solar data file...")' )
  WRITE( Outfile,'("dF_",f6.4,".Solar.nc")' ) DF(idf)
  Error_Status = Write_Solar_netCDF( Outfile, &
                                     Solar, &
                   Title = 'Kurucz synthetic and blackbody extraterrestrial solar source functions.', &
                   History = PROGRAM_RCS_ID//'; '//&
                             TRIM(Process_RCS_Id)//'; '//&
                             'AER extract_solar.f', &
                   Comment = 'Data for use with '//TRIM(CF(idf))//' channel SRFs. '//&
                             TRIM(Process_Comment)//&
                             ' Data extracted from AER solar.kurucz.rad.mono.full_disk.bin file.', &
                   Source = 'Solar spectrum computed with a version of the model atmosphere program ATLAS', &
                   References = 'Kurucz, R.L., Synthetic infrared spectra, in Infrared Solar Physics, '//&
                                'IAU Symp. 154, edited by D.M. Rabin, J.T. Jefferies, and C. Lindsey, '//&
                                'Kluwer, Acad., Norwell, MA, 1992.' )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing netCDF Solar file '//&
                          TRIM(Outfile), &
                          Error_Status )
    STOP
  END IF


  ! Clean up
  ! ..Destroy the solar structure
  Error_Status = Destroy_Solar( Solar )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Solar data structure.', &
                          WARNING )
  END IF
  ! ..Deallocate input data arrays
  DEALLOCATE( f_in, irrad_in, STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating input data arrays. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          WARNING )
  END IF

END PROGRAM Create_Solar
