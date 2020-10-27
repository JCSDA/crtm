!---------------------------------------------------------------------------
!  Program to concatenate two training set files along the given dimention
!    Inputs:
!       (1) the first netCDF training set filename
!       (2) thsecond netCDF training set filename
!       (3) output (combined) netCDF TrainSet filename
!       (4) dimension index at which the concatenation is performed
!          1 - along profile
!          2 - along sensor zenith angle
!          3 - along solar senith angle
!
!    Written by Yong Han, August 2010
!
PROGRAM Cat_TrainSet

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE DeltaRad_TrainSet_Define, ONLY: TrainSet_Concatenate, &
                                      TrainSet_type
  USE DeltaRadTrainSet_netCDF_IO, ONLY: Read_TrainSet_netCDF, &
                                        Write_TrainSet_netCDF, &
                                        Inquire_TrainSet_netCDF 
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Cat_TrainSet'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &

  
  TYPE(TrainSet_type) :: TrainSet1, TrainSet2, TrainSet_out
  CHARACTER(256)      :: fname1, fname2, fname_out
  INTEGER             :: dimIndex
  INTEGER             :: Error_Status
  CHARACTER(256)  :: ID_Tag, Title
  CHARACTER(1024) :: History, Comment

  
  Error_Status = SUCCESS
    
  WRITE(*, FMT='(/5x,"Enter the first netCDF TrainSet filename: ")', ADVANCE='NO')
  READ(*,'(a)') fname1
  fname1 = ADJUSTL(fname1)
  WRITE(*, FMT='(/5x,"Enter the second netCDF TrainSet filename: ")', ADVANCE='NO')
  READ(*,'(a)') fname2
  fname2 = ADJUSTL(fname2)
  
  WRITE(*, FMT='(/5x,"Enter the output (combined) netCDF TrainSet filename: ")', ADVANCE='NO')
  READ(*,'(a)') fname_out
  fname_out = ADJUSTL(fname_out)
  
  WRITE(*, FMT='(/5x,"Enter the CAT dimension index[1 - along profile; 2 - along sensor angle; 3 - along sun angle]: ")', ADVANCE='NO')
  READ(*,'(i3)') dimIndex

  Error_Status = Read_TrainSet_netCDF( fname1, &
                                       TrainSet1 )
   IF( Error_Status /= SUCCESS )THEN
    CALL Display_Message( PROGRAM_NAME, &                                                              
                          "Error: reading data from file: "//TRIM(fname1), &  
                          FAILURE)                                                                     
    STOP                                                                                               
  END IF

  Error_Status = Read_TrainSet_netCDF( fname2, &
                                       TrainSet2 )
   IF( Error_Status /= SUCCESS )THEN
    CALL Display_Message( PROGRAM_NAME, &                                                              
                          "Error: reading data from file: "//TRIM(fname2), &  
                          FAILURE)                                                                     
    STOP                                                                                               
  END IF
                                       
  Error_Status = TrainSet_Concatenate( TrainSet1, &
                                       TrainSet2, &
                                       DimIndex,  &
                                       TrainSet_out)
   IF( Error_Status /= SUCCESS )THEN
    CALL Display_Message( PROGRAM_NAME, &                                                              
                          "Error: Concatenating TrainSet", &  
                          FAILURE)                                                                     
    STOP                                                                                               
  END IF

  Error_Status = Inquire_TrainSet_netCDF( fname1, &
                                          ID_Tag = ID_Tag, & 
                                          Title =  Title, &
                                          History = History, &
                                          Comment = Comment )
   IF( Error_Status /= SUCCESS )THEN
    CALL Display_Message( PROGRAM_NAME, &                                                              
                          "Error: calling function Inquire_TrainSet_netCDF", &  
                          FAILURE)                                                                     
    STOP                                                                                               
  END IF
           
  ! Write out a netCDF file 
  Error_Status = Write_TrainSet_netCDF(      &
                 fname_out,                  &
                 TrainSet_out%Radiance_nlte,     &
                 TrainSet_out%Radiance_lte,      &
                 TrainSet_out%Sensor_Angle,      &
                 TrainSet_out%Sun_Angle,         &
                 TrainSet_out%Level_Pressure,    &
                 TrainSet_out%Level_Temperature, &
                 TrainSet_out%Level_CO2,         &
                 TrainSet_out%Channel,           &
                 Release          = TrainSet_out%Release,           &
                 Version          = TrainSet_out%Version,           &
                 Sensor_ID        = TrainSet_out%Sensor_ID,         &
                 WMO_Satellite_ID = TrainSet_out%WMO_Satellite_ID,  &
                 WMO_Sensor_ID    = TrainSet_out%WMO_Sensor_ID,     &
                 ID_Tag           = TRIM(ID_Tag),            &
                 Title            = TRIM(Title),             &
                 History          = TRIM(History),           &
                 Comment          = TRIM(Comment) )
   IF( Error_Status /= SUCCESS )THEN
    CALL Display_Message( PROGRAM_NAME, &                                                              
                          "Error: writing data into a netCDF file", &  
                          FAILURE)                                                                     
    STOP                                                                                               
  END IF

END PROGRAM Cat_TrainSet
