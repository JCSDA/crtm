!
! NLTECoeff_OLD2NEW
!
! Program to convert netCDF format NLTECoeff files from an OLD release 
! format to a NEW one.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 18-Feb-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM NLTECoeff_OLD2NEW

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds         , ONLY: fp
  USE File_Utility       , ONLY: Get_Lun, File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                 Display_Message, Program_Message
  USE NLTECoeff_Define   , ONLY: NLTECoeff_type, &
                                 NLTECoeff_Associated, &
                                 NLTECoeff_Create    , &
                                 NLTECoeff_Destroy
  USE NLTECoeff_netCDF_IO, ONLY: NLTECoeff_netCDF_WriteFile
  USE NLTE_Define        , ONLY: NLTE_type, &
                                 NLTE_Create, &
                                 NLTE_Destroy, &
                                 Pt
  USE NLTE_Binary_IO     , ONLY: Read_NLTE_Binary
  USE SensorInfo_Define
  USE SensorInfo_IO
  USE SensorInfo_LinkedList
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'NLTECoeff_OLD2NEW'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  CHARACTER(*), PARAMETER :: SENSORINFO_FILE = 'SensorInfo'
  

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256)  :: msg
  CHARACTER(256)  :: old_filename, filename
  INTEGER :: err_stat
  INTEGER :: n
  TYPE(SensorInfo_List_type) :: sinfo_list
  TYPE(SensorInfo_type)      :: sinfo
  TYPE(NLTECoeff_type) :: NLTECoeff
  TYPE(NLTE_type)      :: old_NLTECoeff


  ! Program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to convert NLTECoeff files from '//&
                       'an OLD release Binary format to a NEW netCDF one.', &
                       '$Revision$' )

  ! Read the SensorInfo file
  err_stat = Read_SensorInfo( SENSORINFO_FILE, sinfo_list, Quiet = 1 )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading SensorInfo file'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  
  
  ! Get user inputs
  WRITE( *,FMT='(/5x,"Enter the OLD format NLTECoeff Binary filename: ")',ADVANCE='NO' )
  READ( *,FMT='(a)' ) old_filename
  old_filename = ADJUSTL(old_filename)
  IF ( .NOT. File_Exists( old_filename ) ) THEN
    msg = 'oSRF file '//TRIM(old_filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  filename = TRIM(old_filename)//'.nc'


  ! Read the old release netCDF NLTECoeff file
  err_stat = Read_NLTE_Binary( &
    old_filename, &
    old_NLTECoeff )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading old release binary NLTECoeff file '//TRIM(old_filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Get the matching SensorInfo list node
  Node_Loop: DO n = 1, Count_SensorInfo_Nodes( sinfo_list )
    err_stat = GetFrom_SensorInfo_List( sinfo_list, n, sinfo )
    IF ( err_stat /= SUCCESS ) THEN
      WRITE( msg,'("Error retrieving node #",i0," from SensorInfo file")' ) n
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    IF ( TRIM(sinfo%Sensor_Id) == TRIM(old_NLTECoeff%Sensor_Id) ) EXIT Node_Loop
    err_stat = Destroy_SensorInfo( sinfo ) 
  END DO Node_Loop
  ! ...Check node is valid
  IF ( .NOT. Associated_SensorInfo( sinfo ) ) THEN
    msg = 'Error finding SensorInfo node for '//TRIM(old_NLTECoeff%Sensor_Id)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  
  
  ! Allocate the new NLTECoeff structure
  CALL NLTECoeff_Create( &
    NLTECoeff, &
    old_NLTECoeff%n_Predictors    , &
    old_NLTECoeff%n_Sensor_ZAngles, &
    old_NLTECoeff%n_Solar_ZAngles , &
    old_NLTECoeff%n_NLTE_Channels , &
    old_NLTECoeff%n_Channels        )
  IF ( .NOT. NLTECoeff_Associated( NLTECoeff ) ) THEN   
    msg = 'Error allocating new NLTECoeff strructure'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Copy over the unchanged data
  NLTECoeff%Sensor_Id            = sinfo%Sensor_Id
  NLTECoeff%WMO_Satellite_ID     = sinfo%WMO_Satellite_ID
  NLTECoeff%WMO_Sensor_ID        = sinfo%WMO_Sensor_ID
  NLTECoeff%Sensor_Channel       = sinfo%Sensor_Channel
  NLTECoeff%Upper_Plevel         = Pt(1,:)
  NLTECoeff%Lower_Plevel         = Pt(2,:)
  NLTECoeff%Min_Tm               = old_NLTECoeff%Tm(1,:)
  NLTECoeff%Max_Tm               = old_NLTECoeff%Tm(2,:)
  NLTECoeff%Mean_Tm              = old_NLTECoeff%Tm(3,:)
  NLTECoeff%C_Index              = old_NLTECoeff%NLTE_Channel_Index
  NLTECoeff%C                    = old_NLTECoeff%C
  NLTECoeff%Secant_Sensor_Zenith = old_NLTECoeff%Sensor_Zangle
  NLTECoeff%Secant_Solar_Zenith  = old_NLTECoeff%Solar_Zangle
  NLTECoeff%NLTE_Channel         = old_NLTECoeff%NLTE_Channel
  WHERE(old_NLTECoeff%NLTE_Channel_Index > 0) NLTECoeff%Is_NLTE_Channel = .TRUE.


  ! Write the new netCDF NLTECoeff file
  err_stat = NLTECoeff_netCDF_WriteFile( &
    filename, &
    NLTECoeff, &
    Title   = 'NLTE-correction coefficients for '//TRIM(NLTECoeff%Sensor_Id), &
    History = PROGRAM_VERSION_ID, &
    Comment = 'Created from old version datafiles for testing.' )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error writing new release netCDF NLTECoeff file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  
  
  ! Clean up
  err_stat = Destroy_SensorInfo( sinfo )
  err_stat = Destroy_SensorInfo_List( sinfo_list, Quiet = 1 )
  CALL NLTECoeff_Destroy( NLTECoeff )
  CALL NLTE_Destroy( old_NLTECoeff )

END PROGRAM NLTECoeff_OLD2NEW
