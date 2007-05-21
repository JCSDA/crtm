!
! Compare_Jacobians
!
! Program to compare atmospheric Jacobians using the CRTM forward,
! tangent-linear and K-matrix functions.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-May-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Compare_Jacobians

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE CRTM_Module
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_Test_Utility        , ONLY: TEST_ANGLE, Write_AtmSfc_TestFile
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Compare_Jacobians'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: $'
  ! Use climatology profile data
  CHARACTER(*), PARAMETER :: ATMDATA_FILENAME = 'Model.Atmosphere.bin'
  INTEGER,      PARAMETER :: N_PROFILES = 6
  ! Use the NOAA-18 suite of instruments
  INTEGER,      PARAMETER :: N_SENSORS  = 3
  INTEGER,      PARAMETER :: N_CHANNELS = 39
  CHARACTER(*), PARAMETER :: SENSOR_ID( N_SENSORS ) = &
    (/ 'hirs4_n18', &
       'amsua_n18', &
       'mhs_n18  ' /)


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: k, l
  INTEGER :: H2O_Idx, O3_Idx
  TYPE(CRTM_ChannelInfo_type) , DIMENSION(N_SENSORS)  :: chInfo
  TYPE(CRTM_Atmosphere_type)  , DIMENSION(N_PROFILES) :: Atm, Atm_TL
  TYPE(CRTM_Surface_type)     , DIMENSION(N_PROFILES) :: Sfc, Sfc_TL
  TYPE(CRTM_GeometryInfo_type), DIMENSION(N_PROFILES) :: gInfo
  TYPE(CRTM_Atmosphere_type)  , DIMENSION(N_CHANNELS,N_PROFILES) :: Atm_K, Atm_TL_K
  TYPE(CRTM_Surface_type)     , DIMENSION(N_CHANNELS,N_PROFILES) :: Sfc_K, Sfc_TL_K
  TYPE(CRTM_RTSolution_type)  , DIMENSION(N_CHANNELS,N_PROFILES) :: RTSol
  TYPE(CRTM_RTSolution_type)  , DIMENSION(N_CHANNELS,N_PROFILES) :: RTSol_K, RTSol_TL

  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to compute atmospheric Jacobians using the '//&
                        'CRTM forward, tangent-linear and K-matrix functions.', &
                        '$Revision: $')

  ! Read the atmosphere data files
  ! ------------------------------
  WRITE( *, '( /5x, "Reading the model Atmosphere structure file..." )' )
  Error_Status = CRTM_Read_Atmosphere_Binary( ATMDATA_FILENAME, &
                                              Atm )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Atmosphere structure file '//&
                           ATMDATA_FILENAME, & 
                           Error_Status )
   STOP
  END IF


  ! Use the default water surface values
  ! ------------------------------------
  Sfc%Water_Coverage = ONE
  

  ! Initialise the CRTM
  ! -------------------
  WRITE( *, '( /5x, "Initializing the CRTM..." )' )
  Error_Status = CRTM_Init( chInfo, &
                            SensorId=SENSOR_ID )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error initializing CRTM', & 
                            Error_Status)  
   STOP
  END IF


  ! Allocate the RTSolution structures
  ! ----------------------------------
  Error_Status = CRTM_Allocate_RTSolution( Atm(1)%n_Layers, RTSol )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating RTSol', & 
                           Error_Status)  
    STOP
  END IF
  Error_Status = CRTM_Allocate_RTSolution( Atm(1)%n_Layers, RTSol_K )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating RTSol_K', & 
                           Error_Status)  
    STOP
  END IF


  ! Set the adjoint values
  ! ----------------------
  ! Copy the atmosphere and surface structures
  ! (inefficient, but 'eh'.)
  DO l = 1, N_CHANNELS
    ! The tangent-linear structures
    Error_Status = CRTM_Assign_Atmosphere( Atm, Atm_TL )
    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                            'Error copying Atmosphere structure array to TL.', &
                            Error_Status )
      STOP
    END IF
    Error_Status = CRTM_Assign_Surface( Sfc, Sfc_TL )
    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                            'Error copying Surface structure array to TL.', &
                            Error_Status )
      STOP
    END IF
    ! The tangent-linear K-matrix structures
    Error_Status = CRTM_Assign_Atmosphere( Atm, Atm_TL_K(l,:) )
    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                            'Error copying Atmosphere structure array to TL_K.', &
                            Error_Status )
      STOP
    END IF
    Error_Status = CRTM_Assign_Surface( Sfc, Sfc_TL_K(l,:) )
    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                            'Error copying Surface structure array to TL_K.', &
                            Error_Status )
      STOP
    END IF
    ! The K-matrix structures
    Error_Status = CRTM_Assign_Atmosphere( Atm, Atm_K(l,:) )
    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                            'Error copying Atmosphere structure array.', &
                            Error_Status )
      STOP
    END IF
    Error_Status = CRTM_Assign_Surface( Sfc, Sfc_K(l,:) )
    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                            'Error copying Surface structure array.', &
                            Error_Status )
      STOP
    END IF
  END DO
  ! Zero the tangent-linear structures
  CALL CRTM_Zero_Atmosphere( Atm_TL )
  CALL CRTM_Zero_Surface( Sfc_TL )
  CALL CRTM_Zero_Atmosphere( Atm_TL_K )
  CALL CRTM_Zero_Surface( Sfc_TL_K )
  ! Zero the K-matrix structures
  CALL CRTM_Zero_Atmosphere( Atm_K )
  CALL CRTM_Zero_Surface( Sfc_K )
  
  ! The K_matrix results are all dTb/dx...
  RTSol_K%Brightness_Temperature = ONE


  ! Assign some values
  ! ------------------
  gInfo%Sensor_Zenith_Angle = TEST_ANGLE


  ! Get the water vapour and ozone indices
  ! --------------------------------------
  H2O_Idx = CRTM_Get_AbsorberIdx(Atm(1), H2O_ID)
  O3_Idx  = CRTM_Get_AbsorberIdx(Atm(1), O3_ID)
  
  
  ! Call the CRTM Tangent_Linear model
  ! ----------------------------------
  WRITE(*,'(/5x,"Calling the tangent-linear function....")')
  DO k = 1, Atm(1)%n_Layers
    WRITE(*,'(7x,"Layer #",i0,1x)',ADVANCE='NO') k

    ! Temperature Jacobian
    ! --------------------
    WRITE(*,'("temperature...")',ADVANCE='NO')
    ! Perturb the atmospheric temperature profile
    CALL CRTM_Zero_Atmosphere( Atm_TL )
    CALL Perturb_Temperature()
    ! Call the model
    Error_Status = CRTM_Tangent_Linear( Atm     , &
                                        Sfc     , &
                                        Atm_TL  , &
                                        Sfc_TL  , &
                                        gInfo   , &
                                        chInfo  , &
                                        RTSol   , &
                                        RTSol_TL  )
    IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( PROGRAM_NAME, &
                             'Error in CRTM Tangent-Linear Model for temperature TL', &
                              Error_Status)  
     STOP
    END IF
    ! Save the result
    CALL Save_Temperature()
    
 
    ! Water vapour Jacobian
    ! ----------------------
    WRITE(*,'("water vapour...")',ADVANCE='NO')
    ! Perturb the water vapour profile
    CALL CRTM_Zero_Atmosphere( Atm_TL )
    CALL Perturb_Absorber(H2O_Idx)
    ! Call the model
    Error_Status = CRTM_Tangent_Linear( Atm     , &
                                        Sfc     , &
                                        Atm_TL  , &
                                        Sfc_TL  , &
                                        gInfo   , &
                                        chInfo  , &
                                        RTSol   , &
                                        RTSol_TL  )
    IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( PROGRAM_NAME, &
                             'Error in CRTM Tangent-Linear Model for water vapour TL', &
                              Error_Status)  
     STOP
    END IF
    ! Save the result
    CALL Save_Absorber(H2O_Idx)


    ! Ozone Jacobian
    ! --------------
    WRITE(*,'("ozone...")')
    ! Perturb the ozone profile
    CALL CRTM_Zero_Atmosphere( Atm_TL )
    CALL Perturb_Absorber(O3_Idx)
    ! Call the model
    Error_Status = CRTM_Tangent_Linear( Atm     , &
                                        Sfc     , &
                                        Atm_TL  , &
                                        Sfc_TL  , &
                                        gInfo   , &
                                        chInfo  , &
                                        RTSol   , &
                                        RTSol_TL  )
    IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( PROGRAM_NAME, &
                             'Error in CRTM Tangent-Linear Model for ozone TL', &
                              Error_Status)  
     STOP
    END IF
    ! Save the result
    CALL Save_Absorber(O3_Idx)
  
  END DO
  
  ! Output the results
  CALL Write_AtmSfc_TestFile( '.TL-K_Matrix', chInfo, Atm_TL_K, Sfc_TL_K )
    

  ! Call the CRTM K-matrix model
  ! ----------------------------
  WRITE(*,'(/5x,"Calling the K-matrix function....")')
  Error_Status = CRTM_K_Matrix( Atm    , &  
                                Sfc    , &  
                                RTSol_K, &  
                                gInfo  , &  
                                chInfo , &  
                                Atm_K  , &  
                                Sfc_K  , &  
                                RTSol    )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error in CRTM K_Matrix Model', & 
                            Error_Status)  
   STOP
  END IF

  ! Output some results
  CALL Write_AtmSfc_TestFile( '.K_Matrix', chInfo, Atm_K, Sfc_K )
    
  
  ! Destroy the CRTM
  ! ----------------
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( chInfo )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                           Error_Status )
  END IF


  ! --------
  ! Clean up
  ! --------
  Error_Status = CRTM_Destroy_RTSolution(RTSol_K)
  Error_Status = CRTM_Destroy_RTSolution(RTSol)
  Error_Status = CRTM_Destroy_Surface(Sfc_K)
  Error_Status = CRTM_Destroy_Atmosphere(Atm_K)
  Error_Status = CRTM_Destroy_Surface(Sfc)
  Error_Status = CRTM_Destroy_Atmosphere(Atm)

CONTAINS

  SUBROUTINE Perturb_Temperature()
    INTEGER :: m
    DO m = 1, N_PROFILES
      Atm_TL(m)%Temperature(k) = ONE
    END DO
  END SUBROUTINE Perturb_Temperature

  SUBROUTINE Perturb_Absorber(j)
    INTEGER, INTENT(IN) :: j
    INTEGER :: m
    DO m = 1, N_PROFILES
      Atm_TL(m)%Absorber(k,j) = ONE
    END DO
  END SUBROUTINE Perturb_Absorber

  SUBROUTINE Save_Temperature()
    INTEGER :: l, m
    DO l = 1, N_CHANNELS
      DO m = 1, N_PROFILES
        Atm_TL_K(l,m)%Temperature(k) = RTSol_TL(l,m)%Brightness_Temperature
      END DO
    END DO
  END SUBROUTINE Save_Temperature

  SUBROUTINE Save_Absorber(j)
    INTEGER, INTENT(IN) :: j
    INTEGER :: l, m
    DO l = 1, N_CHANNELS
      DO m = 1, N_PROFILES
        Atm_TL_K(l,m)%Absorber(k,j) = RTSol_TL(l,m)%Brightness_Temperature
      END DO
    END DO
  END SUBROUTINE Save_Absorber

END PROGRAM Compare_Jacobians
