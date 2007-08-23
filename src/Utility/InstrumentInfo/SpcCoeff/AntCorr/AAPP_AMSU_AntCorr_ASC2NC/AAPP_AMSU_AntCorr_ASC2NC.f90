!
! AAPP_AMSU_AntCorr_ASC2NC
!
! Program to read the AAPP AMSU-A/AMSU-B/MHS antenna correction
! ASCII files and output the normalised antenna efficiencies to
! a netCDF format AntCorr file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Jun-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM AAPP_AMSU_AntCorr_ASC2NC

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
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'AAPP_AMSU_AntCorr_ASC2NC'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'
  ! Structure dimensions
  INTEGER, PARAMETER :: AMSUA_N_FOVS     = 30
  INTEGER, PARAMETER :: AMSUA_N_CHANNELS = 15
  INTEGER, PARAMETER :: AMSUBMHS_N_FOVS     = 90
  INTEGER, PARAMETER :: AMSUBMHS_N_CHANNELS = 5
  ! The sensors
  INTEGER, PARAMETER :: N_SENSORS = 10
  CHARACTER(*), PARAMETER :: SENSOR_ID(N_SENSORS) = &
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
  INTEGER, PARAMETER :: WMO_SATELLITE_ID(N_SENSORS) = &
    (/ 206, 207, 208, 209, 4, 206, 207, 208, 209, 4 /)
  INTEGER, PARAMETER :: AMSUA_WMO_SENSOR_ID = 570
  INTEGER, PARAMETER :: AMSUB_WMO_SENSOR_ID = 574
  INTEGER, PARAMETER :: MHS_WMO_SENSOR_ID   = 203
  INTEGER, PARAMETER :: WMO_SENSOR_ID(N_SENSORS) = &
    (/ AMSUA_WMO_SENSOR_ID, &
       AMSUA_WMO_SENSOR_ID, &
       AMSUA_WMO_SENSOR_ID, &
       AMSUA_WMO_SENSOR_ID, &
       AMSUA_WMO_SENSOR_ID, &
       AMSUB_WMO_SENSOR_ID, &
       AMSUB_WMO_SENSOR_ID, &
       AMSUB_WMO_SENSOR_ID, &
       MHS_WMO_SENSOR_ID  , &
       MHS_WMO_SENSOR_ID   /)
  CHARACTER(*), PARAMETER :: PLATFORM_NAME(N_SENSORS) = &
    (/ 'NOAA-15', &
       'NOAA-16', &
       'NOAA-17', &
       'NOAA-18', &
       'MetOp-A', &
       'NOAA-15', &
       'NOAA-16', &
       'NOAA-17', &
       'NOAA-18', &
       'MetOp-A' /)
  CHARACTER(*), PARAMETER :: AMSUA_REFERENCE = &
    'T. Mo, (1999), "AMSU-A Antenna Pattern Corrections", IEEE Transactions '//&
    'on Geoscience and Remote Sensing, Vol.37, pp.103-112'
  CHARACTER(*), PARAMETER :: AMSUBMHS_REFERENCE = &
    'Hewison, T.J. and R.Saunders (1996), "Measurements of the AMSU-B '//&
    'antenna pattern", IEEE Transactions on Geoscience and Remote Sensing, Vol.34, pp.405-412'


  ! ---------  
  ! Variables
  ! ---------  
  CHARACTER(256) :: Message
  CHARACTER(256) :: ASC_Filename, NC_Filename
  CHARACTER(500) :: Title
  CHARACTER(500) :: Comment
  CHARACTER(500) :: Reference
  CHARACTER(256) :: FDF_Id
  INTEGER :: Error_Status, IO_Status
  INTEGER :: Version
  INTEGER :: i, l, n, FileID
  INTEGER :: n_FOVs, n_Channels
  TYPE(AntCorr_type) :: AC
  
  
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read the AAPP AMSU-A/AMSU-B/MHS antenna correction '//&
                        'ASCII files and output the normalised antenna efficiencies '//&
                        'to a netCDF format AntCorr file.', &
                        '$Revision$' )


  ! Enter the AAPP data file name
  ! -----------------------------
  WRITE( *,'(/5x,"Enter the AAPP fdf identifier: ")', ADVANCE='NO' )
  READ( *,'(a)' ) FDF_Id
  FDF_Id = ADJUSTL(FDF_Id)
  
  ! Assign a data version number based on the FDF id
  ! (NOTE: NESDIS original data is considered v1.)
  SELECT CASE (TRIM(FDF_Id))
    CASE ('')
      Version = 2
    CASE ('_halw')
      Version = 3
    CASE ('_v6.4')
      Version = 4
    CASE DEFAULT
      CALL Display_Message( PROGRAM_NAME, &
                            'Unrecognised FDF Id, '//TRIM(FDF_Id), &
                            FAILURE )
      STOP
  END SELECT
    
  
  ! Enter an output comment attribute
  ! ---------------------------------
  WRITE( *,'(/5x,"Enter a file comment: ")' )
  READ( *,'(a)' ) Comment
  Comment = ADJUSTL(Comment)
  

  ! Begin loop over sensors
  ! -----------------------
  Sensor_Loop: DO n = 1, N_SENSORS

    WRITE(*,'(/5x,"Converting antenna correction data for ",a)') TRIM(SENSOR_ID(n))
    
    ! Construct filenames
    ASC_Filename = TRIM(SENSOR_ID(n))//'.fdf'//TRIM(FDF_Id)
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

    ! Read initial comment
    READ( FileID,'(a)' ) Title
    Title = Title(2:)
    
    ! Read the dimensions
    READ( FileID,'(2i5)' ) n_FOVs, n_Channels
    
    ! Allocate the AntCorr structure
    Error_Status = Allocate_AntCorr( n_FOVs, n_Channels, AC )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error allocating AntCorr structure for '//TRIM(SENSOR_ID(n)), &
                            FAILURE )
      STOP
    END IF

    ! Assign the version
    AC%Version = Version
    
    ! Begin channel loop
    DO l = 1, n_Channels
    
      ! Read the channel number
      READ( FileID,'(i5)' ) AC%Sensor_Channel(l)
      
      ! Read the antenna efficiencies
      READ( FileID,'(10f8.6)' ) AC%A_earth(:,l)
      READ( FileID,'(10f8.6)' ) AC%A_platform(:,l)
      READ( FileID,'(10f8.6)' ) AC%A_space(:,l)
    END DO
    
    
    ! Fill the Sensor id info
    ! -----------------------
    AC%Sensor_Id        = TRIM(SENSOR_ID(n))
    AC%WMO_Satellite_Id = WMO_SATELLITE_ID(n)
    AC%WMO_Sensor_Id    = WMO_SENSOR_ID(n)

    CLOSE(FileID)


    ! Set the data reference
    ! ----------------------
    IF ( WMO_SENSOR_ID(n) == AMSUA_WMO_SENSOR_ID ) THEN
      Reference = AMSUA_REFERENCE
    ELSE
      Reference = AMSUBMHS_REFERENCE
    END IF
    
    
    ! Write the netCDF file
    ! ---------------------
    Error_Status = Write_AntCorr_netCDF( NC_Filename, &
                                         AC, &
                                         Title = TRIM(Title), &
                                         History = PROGRAM_RCS_ID, &
                                         Comment = TRIM(Comment)//&
                                                   '; Reference: '//TRIM(Reference) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing to '//TRIM(NC_Filename), &
                            FAILURE )
      STOP
    END IF
    
    
    ! Destroy the AntCorr structure
    ! -----------------------------
    Error_Status = Destroy_AntCorr( AC )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying AntCorr structure used for '//TRIM(SENSOR_ID(n)), &
                            FAILURE )
      STOP
    END IF
    
  END DO Sensor_Loop
  
END PROGRAM AAPP_AMSU_AntCorr_ASC2NC
