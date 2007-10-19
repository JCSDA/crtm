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

  ! Solar source function interpolation parameters
  REAL(fp), PARAMETER :: F1 = 500.0_fp
  REAL(fp), PARAMETER :: F2 = 3500.0_fp
  INTEGER , PARAMETER :: N_DF = 2
  REAL(fp), PARAMETER :: DF(N_DF) = (/ 0.001_fp, 0.0025_fp /)


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Filename
  CHARACTER(256) :: Interpolation_RCS_Id 
  INTEGER        :: FileID
  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: i, n, idf
  REAL(fp) :: f1_in, f2_in, df_in
  REAL(fp) :: Omega
  REAL(fp), ALLOCATABLE :: f_in(:), irrad_in(:)
  TYPE(Solar_type) :: Solar


  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read the ASCII format Kurucz solar source '//&
                        'spectrum and write it and a blackbody equivalent source '//&
                        'function to a netCDF format file.', &
                        '$Revision$' )

  ! Get user input
  ! --------------
  ! Input ASCII filename
  WRITE( *,'(/5x,"Enter the ASCII input filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Filename
  
  ! The interpolation interval
  WRITE( *,'(/5x,"Select interpolation frequency interval")' )
  DO i = 1, N_DF
    WRITE( *,'(10x,i1,") ",f6.4)' ) i, DF(i)
  END DO
  WRITE( *,'(5x,"Enter choice : ")',ADVANCE='NO' )
  READ( *,* ) idf
  

  ! Read the ASCII solar data file
  ! ------------------------------
  WRITE( *,'(/5x,"Reading ASCII Solar data file...")' )
  
  ! Open it
  FileID = Get_Lun()
  IF ( FileID < 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error obtaining file unit number for '//&
                          TRIM(Filename)//' read.', &
                          FAILURE )
    STOP
  END IF

  OPEN( FileID, FILE  =TRIM(Filename), &
                STATUS='OLD', &
                ACCESS='SEQUENTIAL', &
                FORM  ='FORMATTED', &
                ACTION='READ', &
                IOSTAT=IO_Status )
  IF ( IO_Status /= 0 ) THEN
    WRITE( Message,'("Error opening ",a,". IOSTAT = ",i0)' ) &
                    TRIM(Filename), IO_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF

  ! Read the file header
  READ( FileID,*,IOSTAT=IO_Status ) f1_in, f2_in, df_in, n
  IF ( IO_Status /= 0 ) THEN
    WRITE( Message,'("Error reading ",a," header. IOSTAT = ",i0)' ) &
                    TRIM(Filename), IO_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    CLOSE( FileID )
    STOP
  END IF

  ! Allocate the input data arrays
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

  ! Read the data
  DO i = 1, n
    READ( FileID,*,IOSTAT=IO_Status ) f_in(i), irrad_in(i)
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading point number ",i0,&
                      &" irradiance value from ",a,&
                      &". IOSTAT = ",i5)' ) &
                      i, TRIM(Filename), IO_Status
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


  ! Interpolate the solar source data
  ! ---------------------------------
  WRITE( *,'(/5x,"Interpolating the input Solar data...")' )

  ! Determine the number of interpolated points
  n = NINT((F2-F1)/DF(idf)) + 1

  ! Allocate the Solar structure
  Error_Status = Allocate_Solar( n, Solar )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Solar structure.', &
                          FAILURE )
    STOP
  END IF

  ! Assign the scalar frequency members
  ! of the Solar data structure
  Solar%Begin_Frequency    = F1
  Solar%End_Frequency      = F2
  Solar%Frequency_Interval = DF(idf)
  
  ! Create the interpolation frequency grid
  Error_Status = Frequency_Solar( Solar )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing the Solar interpolated frequency grid.', &
                          FAILURE )
    STOP
  END IF
  
  ! Perform the interpolation.
  Error_Status = Polynomial_Interpolate( f_in, irrad_in, &
                                         Solar%Frequency, &
                                         Solar%Irradiance, &
                                         Order =3, &
                                         RCS_Id=Interpolation_RCS_Id )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error interpolating the solar data.', &
                          Error_Status )
    STOP
  END IF

  WRITE( *,'( 10x, "Output begin frequency:    ",f12.6," cm-1", &
            &/10x, "Output end frequency:      ",f12.6," cm-1", &
            &/10x, "Output frequency interval: ",es13.6," cm-1", &
            &/10x, "Output number of values:   ",i0)' ) &
           F1, F2, DF(idf), n


  ! Compute the blackbody solar source irradiance
  ! ---------------------------------------------
  WRITE( *,'(/5x,"Calculating the blackbody solar source irradiance...")' )

  ! Calculate a blackbody radiance
  ! spectrum at the solar temperature
  Error_Status = Planck_Radiance( Solar%Frequency, &
                                  Solar%Blackbody_Temperature, &
                                  Solar%Blackbody_Irradiance )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error calculating solar blackbody radiance spectrum.', &
                          Error_Status )
    STOP
  END IF

  ! COmpute the geometry factor
  Omega = PI * ( Solar%Radius / Solar%Earth_Sun_Distance )**2

  ! Compute the irradiance
  Solar%Blackbody_Irradiance = Omega * Solar%Blackbody_Irradiance



  ! Write the output netCDF file
  ! ----------------------------
  WRITE( *,'(/5x,"Writing the output netCDF Solar data file...")' )

  WRITE( Filename,'("dF_",f6.4,".Solar.nc")' ) DF(idf)
  Error_Status = Write_Solar_netCDF( Filename, &
                                     Solar, &
                   Title = 'Kurucz synthetic and blackbody extraterrestrial solar source functions.', &
                   History = PROGRAM_RCS_ID//'; '//&
                             TRIM(Interpolation_RCS_Id)//'; '//&
                             'AER extract_solar.f', &
                   Comment = '4-pt Lagrangian interpolated from original Kurucz data.', &
                   Source = 'Solar spectrum computed with a version of the model atmosphere program ATLAS', &
                   References = 'Kurucz, R.L., Synthetic infrared spectra, in Infrared Solar Physics, '//&
                                'IAU Symp. 154, edited by D.M. Rabin and J.T. Jefferies, Kluwer, Acad., '//&
                                'Norwell, MA, 1992.' )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing netCDF Solar file '//&
                          TRIM(Filename), &
                          Error_Status )
    STOP
  END IF


  ! Clean up
  ! --------
  ! Destroy the solar structure
  Error_Status = Destroy_Solar( Solar )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Solar data structure.', &
                          WARNING )
  END IF
  
  ! Deallocate input data arrays
  DEALLOCATE( f_in, irrad_in, STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating input data arrays. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          WARNING )
  END IF

END PROGRAM Create_Solar
