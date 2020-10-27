!
! SpcCoeff_Edit
!
! Program to allow simple editing of some contents of a CRTM Binary format SpcCoeff file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 22-Jul-2013
!                       paul.vandelst@noaa.gov
!

PROGRAM SpcCoeff_Edit

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility         , ONLY: File_Exists
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, Program_Message, Display_Message
  USE SensorInfo_Parameters, ONLY: INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID   
  USE SpcCoeff_Define      , ONLY: SpcCoeff_type, SpcCoeff_Destroy, &
                                   SpcCoeff_Inspect
  USE SpcCoeff_Binary_IO   , ONLY: SpcCoeff_Binary_ReadFile, &
                                   SpcCoeff_Binary_WriteFile
  USE ACCoeff_Define       , ONLY: ACCoeff_Associated
  USE NLTECoeff_Define     , ONLY: NLTECoeff_Associated
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_Edit'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  INTEGER     , PARAMETER :: N_EDITABLE_COMPONENTS = 4
  CHARACTER(*), PARAMETER :: EDITABLE_COMPONENT(N_EDITABLE_COMPONENTS) = &
    (/'Sensor Id       ', &
      'WMO Satellite Id', &
      'WMO Sensor Id   ', &
      'Version         '/)


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat, io_stat
  INTEGER :: i, iedit
  CHARACTER(256) :: filename, msg, io_msg
  TYPE(SpcCoeff_type) :: sc
  CHARACTER(256) :: sensor_id
  INTEGER :: wmo_sensor_id, wmo_satellite_id
  INTEGER :: version

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to allow simple editing of some contents '//&
                        'of a CRTM Binary format SpcCoeff file.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the Binary SpcCoeff filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Read the binary data file
  err_stat = SpcCoeff_Binary_ReadFile( filename, sc )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading Binary SpcCoeff file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! The main editing loop
  Edit_Loop: DO
  
    ! What to edit?
    WRITE(*,'(/5x,"Select item to edit")')
    DO i = 1, N_EDITABLE_COMPONENTS
      WRITE(*,'(7x,i0,") ",a)') i, TRIM(EDITABLE_COMPONENT(i))
    END DO
    WRITE( *,'(5x,"Enter selection [-ve to quit]: ")',ADVANCE='NO' )
    READ(*,*,IOSTAT=io_stat,IOMSG=io_msg) iedit
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading input! - '//TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    
    IF ( iedit < 1 ) EXIT Edit_Loop
    IF ( iedit > N_EDITABLE_COMPONENTS ) CYCLE Edit_Loop
    
    WRITE(*,'(/5x,"Editing the ",a)') TRIM(EDITABLE_COMPONENT(iedit))

    
    ! Make the changes
    Edit_Case: SELECT CASE (iedit)

      ! Sensor id
      CASE(1)
        WRITE(*,'(7x,"Current value  : ",a)') sc%Sensor_Id
        sensor_id_loop: DO
          WRITE(*,'(7x,"Enter new value: ")',ADVANCE='NO' )
          READ(*,'(a)') sensor_id
          IF ( LEN_TRIM(sensor_id) == 0 ) THEN
            msg = 'No zero length strings!'
            CALL Display_Message( PROGRAM_NAME, msg, WARNING )
            CYCLE sensor_id_loop
          ELSE
            EXIT sensor_id_loop
          END IF
        END DO sensor_id_loop
        ! ...The main structure
        sc%Sensor_Id = ADJUSTL(sensor_id)
        ! ...The substructures
        IF ( ACCoeff_Associated(sc%AC) ) THEN
          sc%AC%Sensor_Id = sc%Sensor_Id
        END IF
        IF ( NLTECoeff_Associated(sc%NC) ) THEN
          sc%NC%Sensor_Id = sc%Sensor_Id
        END IF
         
      ! WMO Satellite Id
      CASE(2)
        WRITE(*,'(7x,"Current value  : ",i0)') sc%WMO_Satellite_Id
        wmo_satellite_id_loop: DO
          WRITE(*,'(7x,"Enter new value: ")',ADVANCE='NO' )
          READ(*,*,IOSTAT=io_stat,IOMSG=io_msg) wmo_satellite_id
          IF ( io_stat /= 0 ) THEN
            msg = 'Error reading input! - '//TRIM(io_msg)
            CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
          END IF
          IF ( wmo_satellite_id < 1 .OR. wmo_satellite_id > INVALID_WMO_SATELLITE_ID ) THEN
            WRITE(msg,'("Valid values are between 1 and ",i0," inclusive")') INVALID_WMO_SATELLITE_ID
            CALL Display_Message( PROGRAM_NAME, msg, WARNING )
            CYCLE wmo_satellite_id_loop
          ELSE
            EXIT wmo_satellite_id_loop
          END IF
        END DO wmo_satellite_id_loop
        ! ...The main structure
        sc%WMO_Satellite_Id = wmo_satellite_id
        ! ...The substructures
        IF ( ACCoeff_Associated(sc%AC) ) THEN
          sc%AC%WMO_Satellite_Id = sc%WMO_Satellite_Id
        END IF
        IF ( NLTECoeff_Associated(sc%NC) ) THEN
          sc%NC%WMO_Satellite_Id = sc%WMO_Satellite_Id
        END IF
           
      ! WMO Sensor Id
      CASE(3)
        WRITE(*,'(7x,"Current value  : ",i0)') sc%WMO_Sensor_Id
        wmo_sensor_id_loop: DO
          WRITE(*,'(7x,"Enter new value: ")',ADVANCE='NO' )
          READ(*,*,IOSTAT=io_stat,IOMSG=io_msg) wmo_sensor_id
          IF ( io_stat /= 0 ) THEN
            msg = 'Error reading input! - '//TRIM(io_msg)
            CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
          END IF
          IF ( wmo_sensor_id < 1 .OR. wmo_sensor_id > INVALID_WMO_SENSOR_ID ) THEN
            WRITE(msg,'("Valid values are between 1 and ",i0," inclusive")') INVALID_WMO_SENSOR_ID
            CALL Display_Message( PROGRAM_NAME, msg, WARNING )
            CYCLE wmo_sensor_id_loop
          ELSE
            EXIT wmo_sensor_id_loop
          END IF
        END DO wmo_sensor_id_loop
        ! ...The main structure
        sc%WMO_Sensor_Id = wmo_sensor_id
        ! ...The substructures
        IF ( ACCoeff_Associated(sc%AC) ) THEN
          sc%AC%WMO_Sensor_Id = sc%WMO_Sensor_Id
        END IF
        IF ( NLTECoeff_Associated(sc%NC) ) THEN
          sc%NC%WMO_Sensor_Id = sc%WMO_Sensor_Id
        END IF
    
      ! Version
      CASE(4)
        WRITE(*,'(7x,"Current value  : ",i0)') sc%Version
        version_loop: DO
          WRITE(*,'(7x,"Enter new value: ")',ADVANCE='NO' )
          READ(*,*,IOSTAT=io_stat,IOMSG=io_msg) version
          IF ( io_stat /= 0 ) THEN
            msg = 'Error reading input! - '//TRIM(io_msg)
            CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
          END IF
          IF ( version < 1 ) THEN
            msg = 'Only +ve version numbers!'
            CALL Display_Message( PROGRAM_NAME, msg, WARNING )
            CYCLE version_loop
          ELSE
            EXIT version_loop
          END IF
        END DO version_loop
        sc%Version = version
    
      ! ...This should never happen
      CASE DEFAULT
         msg = 'Invalid component selection! How did that happen? Try again.'
         CALL Display_Message( PROGRAM_NAME, msg, WARNING )
    
    END SELECT Edit_Case
    
  END DO Edit_Loop

  ! Display the contents
  CALL SpcCoeff_Inspect( sc )
  
  ! Write the binary data file
  err_stat = SpcCoeff_Binary_WriteFile( filename, sc )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error writing Binary SpcCoeff file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Clean up
  CALL SpcCoeff_Destroy( sc )

END PROGRAM SpcCoeff_Edit
