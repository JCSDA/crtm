!
! Create_MW_SpcCoeff
!
! Program to create the microwave spectral coefficient (SpcCoeff)
! data files for use with the CRTM.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Jul-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM AntCorr_Impact

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds             , ONLY: fp
  USE File_Utility           , ONLY: Get_Lun, File_Exists
  USE Message_Handler        , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                     Display_Message, Program_Message
  USE Sensor_Planck_Functions, ONLY: Sensor_Radiance   , &
                                     Sensor_Temperature, &
                                     Sensor_dBdT       , &
                                     Sensor_dTdB
  USE SpcCoeff_Define        , ONLY: SpcCoeff_type
  USE SpcCoeff_netCDF_IO     , ONLY: Read_SpcCoeff_netCDF
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'AntCorr_Impact'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! A list of the instruments for which we may have antenna correction data
  INTEGER,      PARAMETER :: N_AC_SENSORS = 10
  CHARACTER(*), PARAMETER :: AC_SENSOR_ID(N_AC_SENSORS) = &
  (/ 'amsua_n15    ', &
     'amsua_n16    ', &
     'amsua_n17    ', &
     'amsua_n18    ', &
     'amsua_metop-a', &
     'amsub_n15    ', &
     'amsub_n16    ', &
     'amsub_n17    ', &
     'mhs_n18      ', &
     'mhs_metop-a  ' /)
     
  ! The different versions of SpcCoeff+AntCorr data
  INTEGER,      PARAMETER :: N_AC_VERSIONS = 4
  CHARACTER(*), PARAMETER :: AC_VERSION_NAME(N_AC_VERSIONS) = &
  (/ 'R1V1','R1V2','R1V3','R1V4'/) 
  CHARACTER(*), PARAMETER :: AC_VERSION_ORIG(N_AC_VERSIONS) = &
  (/ 'NESDIS    ','AAPP      ','AAPP(halw)','AAPP(v6.4)'/) 

  ! The number of temperatures to test
  INTEGER,  PARAMETER :: N_TEMPERATURES = 5
  REAL(fp), PARAMETER :: TEARTH(N_TEMPERATURES) = &
  (/ 250.0_fp, 265.0_fp, 280.0_fp, 295.0_fp, 310.0_fp /)
  
  ! Temperature of space.
  REAL(fp), PARAMETER :: TSPACE = 2.7253_fp
  
  
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: SpcCoeff_Filename
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: IO_Status
  INTEGER :: FileID
  INTEGER :: n_Versions
  INTEGER :: i, j, k, l, n
  LOGICAL :: First_Pass
  INTEGER :: vIdx(N_AC_VERSIONS)
  REAL(fp) :: Radiance
  REAL(fp), ALLOCATABLE :: Ta(:), Ra(:), Ta_Ra(:)
  TYPE(SpcCoeff_type) :: SC


  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the impact of different antenna correction data '//&
                        'upon antenna/brightness temperature calculations.', &
                        '$Revision$' )


  ! Loop over AC sensors
  ! --------------------
  Sensor_Loop: DO n = 1, N_AC_SENSORS
  
    WRITE(*,'(/5x,"Assessing data for ",a)') TRIM(AC_SENSOR_ID(n))


    ! Count the available versions for the current sensor
    ! ---------------------------------------------------
    n_Versions = 0
    VersionCount_Loop: DO i = 1, N_AC_VERSIONS
    
      ! Construct the SpcCoeff filename
      SpcCoeff_Filename = 'Data/'//TRIM(AC_VERSION_NAME(i))//'/'//&
                          TRIM(AC_SENSOR_ID(n))//'.SpcCoeff.nc'
      
      ! Check if it exists
      IF ( .NOT. File_Exists(SpcCoeff_Filename) ) CYCLE VersionCount_Loop
      
      ! Increment counter and set index
      n_Versions = n_Versions + 1
      vIdx(n_Versions) = i
      
    END DO VersionCount_Loop
      

    ! Loop over data versions
    ! -----------------------
    First_Pass = .TRUE.
    Version_Loop: DO i = 1, n_Versions

      ! Construct the SpcCoeff filename
      SpcCoeff_Filename =  'Data/'//TRIM(AC_VERSION_NAME(vIdx(i)))//'/'//&
                           TRIM(AC_SENSOR_ID(n))//'.SpcCoeff.nc'

      ! Read the SpcCoeff data
      Error_Status = Read_SpcCoeff_netCDF( SpcCoeff_Filename, SC )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading SpcCoeff file '//&
                              TRIM(SpcCoeff_Filename), &
                              FAILURE )
        STOP
      END IF
      
      ! Set up stuff for first pass
      IF ( First_Pass ) THEN
        ! Allocate the antenna temperature and radiance arrays
        ALLOCATE( Ta(SC%AC%n_FOVs), Ra(SC%AC%n_FOVs), Ta_Ra(SC%AC%n_FOVs), STAT=Allocate_Status)
        IF ( Allocate_Status /= 0 ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error allocating Ta and Ra for '//TRIM(AC_SENSOR_ID(n)), &
                                FAILURE )
          STOP
        END IF
        ! Open output file
        FileId = Get_Lun()
        OPEN( FileID, FILE   = TRIM(AC_SENSOR_ID(n))//'.dTa.bin', &
                      STATUS = 'REPLACE', &
                      FORM   = 'UNFORMATTED', &
                      ACCESS = 'SEQUENTIAL', &
                      IOSTAT = IO_Status )
        IF ( IO_Status /= 0 ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error opening output file for '//TRIM(AC_SENSOR_ID(n)), &
                                FAILURE )
          STOP
        END IF
        ! Write dimensions
        WRITE(FileID) n_Versions, N_TEMPERATURES, SC%n_Channels, SC%AC%n_FOVs
        ! Write data identifiers
        WRITE(FileID) LEN(AC_VERSION_ORIG(1))
        WRITE(FileID) AC_VERSION_ORIG(vIdx(1:n_Versions))
        ! Write dimension data
        WRITE(FileID) TEARTH, SC%Sensor_Channel
        ! Toggle switch
        First_Pass = .FALSE.
      END IF
      
      
      ! Loop over temperatures
      ! ----------------------
      Temperature_Loop: DO j = 1, N_TEMPERATURES
      
      
        ! Loop over channel
        ! -----------------
        Channel_Loop: DO l = 1, SC%n_Channels 
        
          ! Compute the sensor radiance
          Error_Status = Sensor_Radiance( SC, SC%Sensor_Channel(l), TEARTH(j), &
                                          Radiance )
          IF ( Error_Status /= SUCCESS ) THEN
            Write(Message,'("Error computing sensor radiance for ch.",i0," at ",f5.1,"K.")') &
                          SC%Sensor_Channel(l), TEARTH(j)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  FAILURE )
            STOP
          END IF
          
          
          ! Apply the antenna correction...
          ! ...to the temperature.
          CALL Apply_AntCorr( SC, l, TEARTH(j), TSPACE, Ta )
          ! ...to the radiance.
          CALL Apply_AntCorr( SC, l, Radiance,  SC%Cosmic_Background_Radiance(l), Ra )

          ! Convert the "antenna radiance" to temperature
          FOV_Loop: DO k =1, SC%AC%n_FOVs
            Error_Status = Sensor_Temperature( SC, SC%Sensor_Channel(l), Ra(k), &
                                               Ta_Ra(k) )
            IF ( Error_Status /= SUCCESS ) THEN
              Write(Message,'("Error computing sensor temperature for FOV#",i0," ch.",i0," at ",f5.1,"K.")') &
                            k, SC%Sensor_Channel(l), TEARTH(j)
              CALL Display_Message( PROGRAM_NAME, &
                                    TRIM(Message), &
                                    FAILURE )
              STOP
            END IF
          END DO FOV_Loop
          
          ! Output data
          WRITE(FileID) Ta, Ta_Ra
          
        END DO Channel_Loop
      END DO Temperature_Loop
    END DO Version_Loop

    ! Close output file
    CLOSE(FileID)
    
    ! Deallocate the antenna correction arrays
    DEALLOCATE( Ta, Ra, Ta_Ra, STAT=Allocate_Status)
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error edallocating Ta and Ra for '//TRIM(AC_SENSOR_ID(n)), &
                            FAILURE )
      STOP
    END IF
  
  END DO Sensor_Loop


CONTAINS

  SUBROUTINE Apply_AntCorr( SC, ChIdx, Te, Ts, Ta )
    TYPE(SpcCoeff_type), INTENT(IN)  :: SC
    INTEGER            , INTENT(IN)  :: ChIdx
    REAL(fp)           , INTENT(IN)  :: Te, Ts
    REAL(fp)           , INTENT(OUT) :: Ta(:)
    INTEGER :: k
    DO k = 1, SIZE(Ta)
      Ta(k) = SC%AC%A_earth(   k,ChIdx)*Te + &
              SC%AC%A_platform(k,ChIdx)*Te + &
              SC%AC%A_space(   k,ChIdx)*Ts
    END DO
  END SUBROUTINE Apply_AntCorr
  
END PROGRAM AntCorr_Impact

