!
! NESDIS_AMSUA_AntCorr_ASC2NC
!
! Program to read the NESDIS AMSU-A antenna correction ASCII files
! and output the normalised antenna efficiencies to a netCDF
! format AntCorr file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Jun-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM NESDIS_AMSUA_AntCorr_ASC2NC

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, &
                               Display_Message, Program_Message
  USE File_Utility     , ONLY: Get_Lun
  USE AntCorr_Define   , ONLY: AntCorr_type, &
                               Allocate_AntCorr, &
                               Destroy_AntCorr
  USE AntCorr_netCDF_IO, ONLY: Write_AntCorr_netCDF
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'NESDIS_AMSUA_AntCorr_ASC2NC'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
  ! Structure dimensions
  INTEGER, PARAMETER :: N_FOVS     = 30
  INTEGER, PARAMETER :: N_CHANNELS = 15
  INTEGER, PARAMETER :: N_IN_CHANNELS = 10
  ! The sensors
  INTEGER, PARAMETER :: N_SENSORS = 5
  CHARACTER(*), PARAMETER :: SENSOR_ID(N_SENSORS) = &
    (/ 'amsua_n15    ', &
       'amsua_n16    ', &
       'amsua_n17    ', &
       'amsua_n18    ', &
       'amsua_metop-a' /)
  INTEGER,PARAMETER :: WMO_SATELLITE_ID(N_SENSORS) = &
    (/ 206, 207, 208, 209, 4 /)
  INTEGER,PARAMETER :: WMO_SENSOR_ID = 570
  CHARACTER(*), PARAMETER :: PLATFORM_NAME(N_SENSORS) = &
    (/ 'NOAA-15', &
       'NOAA-16', &
       'NOAA-17', &
       'NOAA-18', &
       'MetOp-A' /)
  CHARACTER(*), PARAMETER :: REFERENCE = &
    'T. Mo, (1999), "AMSU-A Antenna Pattern Corrections", IEEE Transactions '//&
    'on Geoscience and Remote Sensing, Vol.37, pp.103-112'


  ! ---------  
  ! Variables
  ! ---------  
  CHARACTER(256) :: Message
  CHARACTER(256) :: ASC_Filename, NC_Filename
  CHARACTER(500) :: Buffer
  INTEGER :: Error_Status, IO_Status
  INTEGER :: i, l, n, FileID
  INTEGER :: k_earth, k_space, k_platform
  REAL(fp) :: Input_Fdata((N_IN_CHANNELS*3) + 2, N_FOVS)
  REAL(fp) :: Input_Eta(N_IN_CHANNELS), Eta(N_CHANNELS), Neta
  TYPE(AntCorr_type) :: AC
  
  
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read the NESDIS AMSU-A antenna correction '//&
                        'ASCII files and output the normalised antenna efficiencies '//&
                        'to a netCDF format AntCorr file.', &
                        '$Revision$' )


  ! Allocate AntCorr structure
  ! --------------------------
  Error_Status = Allocate_AntCorr( N_FOVS, N_CHANNELS, AC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating AntCorr structure', &
                          FAILURE )
    STOP
  END IF
  
  ! Assign sensor channel data
  AC%Sensor_Channel = (/(l,l=1,N_CHANNELS)/)


  ! Begin loop over sensors
  ! -----------------------
  Sensor_Loop: DO n = 1, N_SENSORS

    WRITE(*,'(/5x,"Converting antenna correction data for ",a)') TRIM(SENSOR_ID(n))
    
    ! Construct filenames
    ASC_Filename = TRIM(SENSOR_ID(n))//'.AntCorr.txt'
    NC_Filename  = TRIM(SENSOR_ID(n))//'.AntCorr.nc'

    ! Read the ASCII data
    ! -------------------
    ! Open ASCII file
    FileID = Get_Lun()
    OPEN( FileID, FILE   = ASC_Filename, &
                  STATUS = 'OLD', &
                  FORM   = 'FORMATTED', &
                  ACCESS = 'SEQUENTIAL', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error opening ASCII file '//TRIM(ASC_Filename), &
                            FAILURE )
      STOP
    END IF

    ! Read initial comments
    DO i = 1, 6
      READ(FileID,'(a)') Buffer
    END DO
    
    ! Read the antenna efficiencies
    READ(FileID,*) Input_Fdata
    
    ! More comments
    DO i = 1, 3
      READ(FileID,'(a)') Buffer
    END DO

    ! Read the correction factor
    READ(FileID,*) Input_Eta

    CLOSE(FileID)


    ! Transfer data to AntCorr structure
    ! ----------------------------------
    ! Sensor id info
    AC%Sensor_Id        = TRIM(SENSOR_ID(n))
    AC%WMO_Satellite_Id = WMO_SATELLITE_ID(n)
    AC%WMO_Sensor_Id    = WMO_SENSOR_ID
    
    ! The first nine channels
    k_space = 2
    DO l = 1, 9
      k_earth    = k_space    + 1
      k_platform = k_earth    + 1
      k_space    = k_platform + 1
      AC%A_earth(:,l)    = Input_Fdata(k_earth,   :)
      AC%A_platform(:,l) = Input_Fdata(k_platform,:)
      AC%A_space(:,l)    = Input_Fdata(k_space   ,:)
      Eta(l) = Input_Eta(l)
    END DO
    
    ! Replicate ch9 into ch10-14
    AC%A_earth(:,10:14)    = SPREAD( AC%A_earth(:,9)   , DIM=2, NCOPIES=5 )
    AC%A_platform(:,10:14) = SPREAD( AC%A_platform(:,9), DIM=2, NCOPIES=5 )
    AC%A_space(:,10:14)    = SPREAD( AC%A_space(:,9)   , DIM=2, NCOPIES=5 )
    Eta(10:14) = Input_Eta(9)

    ! Channel 15
    k_earth    = k_space    + 1
    k_platform = k_earth    + 1
    k_space    = k_platform + 1
    AC%A_earth(:,15)    = Input_Fdata(k_earth,   :)
    AC%A_platform(:,15) = Input_Fdata(k_platform,:)
    AC%A_space(:,15)    = Input_Fdata(k_space   ,:)
    Eta(15) = Input_Eta(10)
    
    
    ! Normalise the data
    ! ------------------
    DO l = 1, N_CHANNELS
      DO i = 1, N_FOVS
        Neta = AC%A_earth(i,l) + AC%A_space(i,l) + Eta(l)*AC%A_platform(i,l)
        AC%A_earth(i,l)    = AC%A_earth(i,l) / Neta
        AC%A_space(i,l)    = AC%A_space(i,l) / Neta
        AC%A_platform(i,l) = Eta(l)*AC%A_platform(i,l) / Neta
      END DO
    END DO
    
    
    ! Write the netCDF file
    ! ---------------------
    Error_Status = Write_AntCorr_netCDF( NC_Filename, &
                                         AC, &
                                         Title = 'Normalised antenna efficiencies for '//&
                                                 PLATFORM_NAME(n)//' AMSU-A', &
                                         History = PROGRAM_RCS_ID, &
                                         Comment = 'Reference: '//REFERENCE//&
                                                   '; Original data normalised. Far->near '//&
                                                   'field effect correction factor included.')
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing to '//TRIM(NC_Filename), &
                            FAILURE )
      STOP
    END IF
    
  END DO Sensor_Loop
  
END PROGRAM NESDIS_AMSUA_AntCorr_ASC2NC
