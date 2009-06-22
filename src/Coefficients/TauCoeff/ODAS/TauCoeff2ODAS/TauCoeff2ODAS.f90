!
! TauCoeff2ODAS
!
! Program to convert ODAS (Optical Depth Absorber Space) netCDF files
! from the old TauCoeff format to the new ODAS format for use with the
! multiple-algorithm form of the CRTM.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 12-Jun-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM TauCoeff2ODAS

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility      , ONLY: File_Exists
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                Program_Message, Display_Message
  USE TauCoeff_Define   , ONLY: TauCoeff_type, Destroy_TauCoeff
  USE TauCoeff_netCDF_IO, ONLY: Read_TauCoeff_netCDF
  USE ODAS_Define       , ONLY: N_SENSOR_TYPES, SENSOR_TYPE_NAME, &
                                ODAS_type, Allocate_ODAS, Destroy_ODAS
  USE ODAS_netCDF_IO    , ONLY: Write_ODAS_netCDF
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'TauCoeff2ODAS'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: TauCoeff_Filename
  CHARACTER(256) :: ODAS_Filename
  CHARACTER(20)  :: Sensor_Id
  INTEGER        :: Sensor_Type
  CHARACTER(256)  :: Title         
  CHARACTER(5000) :: History       
  CHARACTER(5000) :: Comment       
  CHARACTER(2000) :: Profile_ID_Tag  
  TYPE(TauCoeff_type) :: TauCoeff
  TYPE(ODAS_type) :: ODAS
  INTEGER :: i


  ! Output prgram header
  ! --------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert ODAS (Optical Depth Absorber Space) netCDF '//&
                        'files from the old TauCoeff format to the new ODAS format for '//&
                        'use with the multiple-algorithm form of the CRTM.', &
                        '$Revision$' )

  ! Enter a sensor Id and type
  ! --------------------------
  WRITE( *,FMT='(/5x,"Enter a sensor ID: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Sensor_Id
  
  WRITE( *,'(/5x,"Enter a sensor type")')
  DO i = 1, N_SENSOR_TYPES
    WRITE( *,'(7x,i2,") ",a)') i, SENSOR_TYPE_NAME(i)
  END DO
  WRITE( *,FMT='(/5x,"Select choice: ")',ADVANCE='NO' )
  READ( *,'(i10)' ) Sensor_Type
  

  ! Construct the filenames
  ! -----------------------
  TauCoeff_Filename = TRIM(Sensor_Id)//'.TauCoeff.nc'  
  ODAS_Filename     = TRIM(Sensor_Id)//'.ODAS.TauCoeff.nc'
  
  
  ! Read the TauCoeff data
  ! ----------------------
  WRITE( *,'(/5x,"Reading TauCoeff file ",a,"...")' ) TRIM(TauCoeff_Filename)
  Error_Status = Read_TauCoeff_netCDF( TauCoeff_Filename, &
                                       TauCoeff, &
                                       History = History       , &
                                       Comment = Comment       , &
                                       ID_Tag  = Profile_ID_Tag  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF TauCoeff file '//&
                          TRIM(TauCoeff_Filename), &
                          Error_Status )
    STOP
  END IF


  ! Allocate the ODAS structure
  ! ---------------------------
  Error_Status = Allocate_ODAS( TauCoeff%n_Orders    , &
                                TauCoeff%n_Predictors, &
                                TauCoeff%n_Absorbers , &
                                TauCoeff%n_Channels  , &
                                ODAS                   )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'ODAS allocation failed', &
                          Error_Status )
    STOP
  END IF


  ! Copy over the data
  ! ------------------
  ODAS%Release          = TauCoeff%Release + 1
  ODAS%Version          = TauCoeff%Version

  ODAS%Sensor_Id        = Sensor_Id
  ODAS%WMO_Satellite_ID = TauCoeff%WMO_Satellite_ID(1)
  ODAS%WMO_Sensor_ID    = TauCoeff%WMO_Sensor_ID(1)   
  ODAS%Sensor_Type      = Sensor_Type     
  ODAS%Sensor_Channel   = TauCoeff%Sensor_Channel  
  ODAS%Absorber_ID      = TauCoeff%Absorber_ID     
  ODAS%Alpha            = TauCoeff%Alpha           
  ODAS%Alpha_C1         = TauCoeff%Alpha_C1        
  ODAS%Alpha_C2         = TauCoeff%Alpha_C2        
  ODAS%Order_Index      = TauCoeff%Order_Index     
  ODAS%Predictor_Index  = TauCoeff%Predictor_Index 
  ODAS%C                = TauCoeff%C               
  

  ! Write the new datafile
  ! ----------------------
  Title = 'ODAS upwelling transmittance coefficients for '//TRIM(Sensor_Id)
  Error_Status = Write_ODAS_netCDF( ODAS_Filename, &
                                    ODAS, &
                                    Title         =TRIM(Title)         , &
                                    History       =PROGRAM_RCS_ID//'; '//&
                                                   TRIM(History)       , &
                                    Comment       =TRIM(Comment)       , &
                                    Profile_Set_ID=TRIM(Profile_ID_Tag)  )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing netCDF ODAS file '//&
                          TRIM(ODAS_Filename), &
                          Error_Status )
    STOP
  END IF


  ! Clean up
  ! --------
  Error_Status = Destroy_TauCoeff( TauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff structure', &
                          WARNING )
  END IF
  Error_Status = Destroy_ODAS( ODAS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODAS structure', &
                          WARNING )
  END IF

END PROGRAM TauCoeff2ODAS
