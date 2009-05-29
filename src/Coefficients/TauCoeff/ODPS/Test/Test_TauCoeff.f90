
PROGRAM Test_TauCoeff

  USE ODPS_TauCoeff     , ONLY: Load_TauCoeff, &
                                Destroy_TauCoeff, &
                                TC
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, WARNING, Display_Message

  ! Disable all implicit typing
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_TauCoeff'
  INTEGER,      PARAMETER :: N_SENSORS = 2
  CHARACTER(*), PARAMETER :: Sensor_ID(N_SENSORS) = (/&
                                          'airsM10_aqua', &
                                          'airsM11_aqua'/)
  INTEGER :: i, Error_Status
  INTEGER :: j1, j2, m_Layers

  Error_Status = Load_TauCoeff(Sensor_ID)
  IF( Error_Status /= SUCCESS )THEN                                               
    Error_Status = FAILURE                                                     
    CALL Display_Message( PROGRAM_NAME, &                                      
                          "Failed to read in TauCoeff data", &               
                          Error_Status )                                      
    STOP                                                                     
  END IF                                                                       

  DO i = 1, N_SENSORS

    Write(*, '("Sensor ID = ", a)')Sensor_ID(i)
    Write(*, '("Group ID = ", i0)')TC(i)%Group_ID
    Write(*, '("Sensor ID, WMO_Satellite ID and WMO_Sensor_ID = ", a, 1x, 2i5)')&
                        TRIM(TC(i)%Sensor_Id), TC(i)%WMO_Satellite_ID, TC(i)%WMO_Sensor_ID
    Write(*, '("Sensor Type = ", i0)')TC(i)%Sensor_type

    Write(*, '("Algorithm ID = ", i0)')TC(i)%Algorithm

    Write(*, '("n_Layers, n_Components, n_Absorbers, n_Channels, n_Coeffs = ", 4i5, i8)')&
       TC(i)%n_Layers,TC(i)%n_Components,TC(i)%n_Absorbers,TC(i)%n_Channels,TC(i)%n_Coeffs    

    Write(*, '("Sensor channel = ", 10i4)')TC(i)%Sensor_Channel
    Write(*, '("Component ID = ", 10i4)')TC(i)%Component_ID
    Write(*, '("Absorber ID = ", 10i4)')TC(i)%Absorber_ID

  END DO

  Write(*, '("------------------------")')  
  Write(*, '(2i5)')TC(1)%n_Predictors(3, 100), TC(2)%n_Predictors(1, 1)
  m_Layers = 90
  j1 = TC(2)%Pos_index(1,1) + (m_Layers-1) * TC(2)%n_Predictors(1, 1) 
  j2 = j1 + TC(2)%n_Predictors(1, 1) -1
  print *, 'TC(2) Coeff' 
  write(*, *)TC(2)%C(j1:j2)

  j1 = TC(1)%Pos_index(3,100) + (m_Layers-1) * TC(1)%n_Predictors(3, 100) 
  j2 = j1 + TC(1)%n_Predictors(3, 100) -1

  print *, 'TC(1) Coeff' 
  write(*, *)TC(1)%C(j1:j2)

END PROGRAM Test_TauCoeff
