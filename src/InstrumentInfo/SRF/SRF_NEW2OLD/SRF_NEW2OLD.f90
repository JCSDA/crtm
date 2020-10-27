PROGRAM SRF_NEW2OLD

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE CRTM_PARAMETERS

  USE SRF_Define,    ONLY : SRF_type_new => SRF_type, &
                            Destroy_SRF_new => Destroy_SRF
  USE SRF_Define_old,    ONLY : SRF_type_old => SRF_type,  &
                            Allocate_SRF_old => Allocate_SRF, &
                            Destroy_SRF_old => Destroy_SRF
  USE SRF_netCDF_IO, ONLY : Read_SRF_netCDF_new => Read_SRF_netCDF
  USE SRF_netCDF_IO_old, ONLY : Read_SRF_netCDF_old => Read_SRF_netCDF, &
                                Write_SRF_netCDF_old => Write_SRF_netCDF, &
                                Create_SRF_netCDF_old => Create_SRF_netCDF

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'SRF_NEW2OLD'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'



  ! ---------
  ! Variables
  ! ---------


  INTEGER :: Error_Status

  CHARACTER(*), PARAMETER :: INPUT_SRF_FILENAME = 'avhrr3_n19.srf.nc'
  CHARACTER(*), PARAMETER :: OUTPUT_SRF_FILENAME = 'avhrr3_n19.srf.old.nc'
  
  TYPE( SRF_type_new ) :: SRF_New
  TYPE( SRF_type_old ) :: SRF_Old
  
  INTEGER :: n_Points, i
  INTEGER, PARAMETER, DIMENSION(3) :: Channel_List=[3,4,5]
  INTEGER, PARAMETER :: WMO_SATELLITE_ID = 223
  INTEGER, PARAMETER :: WMO_SENSOR_ID = 591 
  CHARACTER(*), PARAMETER :: PLATFORM_NAME = 'NOAA-19'
  CHARACTER(*), PARAMETER :: SENSOR_NAME = 'AVHRR/3'
  CHARACTER(*), PARAMETER :: TITLE = 'Infrared channel avhrr3_n19 Spectral Response Functions'
  CHARACTER(*), PARAMETER :: COMMENT = 'Channel 5 frequency cutoffs:   787.0:  876.0; Channel 5 no.'//&
                                       'of -ve values: 63(Zeroed); Channel 4 frequency cutoffs:' // &
                                        '860.0: 1003.0; Channel 4 no. of -ve values: 85(Zeroed); ' // &
                                        'Channel 3B frequency cutoffs:  2416.0: 3041.0; Channel 3B no. ' // &
                                        'of -ve values: 35(Zeroed); SRFs interpolated to 0.1cm^-1 spacing via spline ' //&
                                        'with tension 5.0e+00; Data obtained from Xiangqian Wu at ' // &
                                        'http://www.star.nesdis.noaa.gov/smcd/spb/fwu/solar_cal/spec_resp_func'

  
  

  Error_Status = Create_SRF_netCDF_old( OUTPUT_SRF_FILENAME,                   &
                                        Channel_List,                          &
                                        WMO_Satellite_ID=WMO_SATELLITE_ID,     &
                                        WMO_Sensor_ID=WMO_SENSOR_ID,           &
                                        Title=TITLE,                           &
                                        Comment=COMMENT,                       &
                                        Platform_Name=PLATFORM_NAME,           &
                                        Sensor_Name=SENSOR_NAME,               &
                                        History=PROGRAM_RCS_ID,                &
                                        NCEP_Sensor_ID=-1                      )
  IF (Error_Status /= SUCCESS) THEN
    PRINT *, 'Error creating old srf file'
  END IF                                      
                                        
                                        
  DO i = 3, 5 


    ! read new srf
     WRITE( *, '( /5x, "Reading NEW FORMAT netCDF SRF data ..." )' )

     Error_Status = Read_SRF_netCDF_new( INPUT_SRF_FILENAME , &
                                               i,                    &
                                               SRF_New               )

     IF ( Error_Status /= SUCCESS ) THEN
       CALL display_message( PROGRAM_NAME, &
                             'Error reading new format SRF file '//&
                             TRIM( INPUT_SRF_FILENAME ), &
                             Error_Status )
       STOP
     END IF
   
     n_Points = SRF_New%npts_Band(1)

     ! Allocate for old srf
     Error_Status = Allocate_SRF_old( n_Points     ,  &  ! Input
                                       SRF_Old          ) ! Output 
      
      IF ( Error_Status /= SUCCESS) THEN
        print *, 'Error allocating old srf structure'
      END IF
     
      !print *, SRF_New%Response
      ! Copy new fields into old 
      SRF_Old%NCEP_Sensor_Id = -1
      SRF_Old%Sensor_Name = PLATFORM_NAME
      SRF_Old%Platform_Name = SENSOR_NAME
      SRF_Old%WMO_Satellite_Id = 223
      SRF_Old%WMO_Sensor_Id = SRF_New%WMO_Sensor_Id
      SRF_Old%End_Frequency = SRF_New%f2_Band(1)
      SRF_Old%Begin_Frequency = SRF_New%f1_Band(1)
      SRF_Old%Integrated_SRF = SRF_New%Integrated_SRF
      SRF_Old%Summation_SRF = SRF_New%Summation_SRF
      SRF_Old%Channel = SRF_New%Channel
      SRF_Old%Frequency = SRF_New%Frequency
      SRF_Old%Response = SRF_New%Response

      print*, i
      ! Write an old netcdf srf file

      WRITE( *, '( /5x, "Writing the NEW FORMAT netCDF SpcCoeff data ..." )' )

      Error_Status = Write_SRF_netCDF_old(  Output_SRF_Filename , &
                                            SRF_Old                     )
                                            

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error writing old format srf nc file '//&
                              TRIM( Output_SRF_Filename ), &
                              Error_Status )
        STOP
      END IF
   
  

      !#------------------------------------------------------------------------#
      !#        -- DESTROY THE CURRENT SENSOR SRF DATA STRUCTURES --            #
      !#------------------------------------------------------------------------#

      Error_Status = Destroy_SRF_New( SRF_New )

      IF ( Error_Status /= SUCCESS ) THEN 
        PRINT*, 'Error destroying SRF data structure for '
      END IF


      Error_Status = Destroy_SRF_old( SRF_Old )

      IF ( Error_Status /= SUCCESS ) THEN  
        PRINT*, 'Error destroying SRF_old data structure for '
      END IF

  END DO



 
END PROGRAM SRF_NEW2OLD


