!
! test_TL_convergence
!
! Program to provide a (relatively) simple example of how
! to test the CRTM tangent-linear function.
! This code checks the convergence between the tangent-linear
! operator and the nonlinear CRTM Forward function when 
! the magnitude of the atmospheric input state perturbation
! is step-wise reduced.
! The convergence should be monotonous, which it isn't right.
! For this reason the test in its current state will fai. 
!
! Copyright Patrick Stegmann, 2020
!
! Modified by Isaac Moradi Isaac.Moradi@NASA.GOV
!             Nov-30-2021
!             Modified to work with active sensor module
!

PROGRAM test_TL_convergence

  ! ============================================================================
  ! **** ENVIRONMENT SETUP FOR RTM USAGE ****
  !
  ! Module usage
  USE CRTM_Module
  !USE UnitTest_Define, ONLY: UnitTest_IsEqualWithin
  ! Disable all implicit typing
  IMPLICIT NONE
  ! ============================================================================


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'test_TL_convergence'
  CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/unit/'
  CHARACTER(*), PARAMETER :: COEFFICIENTS_PATH = './testinput/'
  
  ! ============================================================================
  ! 0. **** SOME SET UP PARAMETERS FOR THIS EXAMPLE ****
  !
  ! Profile dimensions...
  INTEGER, PARAMETER :: N_PROFILES  = 2
  INTEGER, PARAMETER :: N_LAYERS    = 92
  INTEGER, PARAMETER :: N_ABSORBERS = 2
  INTEGER, PARAMETER :: N_CLOUDS    = 1
  INTEGER, PARAMETER :: N_AEROSOLS  = 0
  ! ...but only ONE Sensor at a time
  INTEGER, PARAMETER :: N_SENSORS = 1

  ! Test GeometryInfo angles. The test scan angle is based
  ! on the default Re (earth radius) and h (satellite height)
  REAL(fp), PARAMETER :: ZENITH_ANGLE = 30.0_fp
  REAL(fp), PARAMETER :: SCAN_ANGLE   = 26.37293341421_fp
  ! ============================================================================


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Version
  CHARACTER(256) :: Sensor_Id
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: n_Channels
  INTEGER :: l, m
  INTEGER :: ii, jj, isign, sign, ilev, iprof, ichan
  INTEGER, PARAMETER:: nsign=2
  INTEGER :: testresult 
  ! Declarations for Jacobian comparisons
  INTEGER :: n_la, n_ma
  INTEGER :: n_ls, n_ms
  CHARACTER(256) :: atmk_File, sfck_File
  REAL(fp) :: Perturbation
  REAL(16) :: Ratio_new(nsign), Ratio_old(nsign)
  REAL(fp), PARAMETER :: TOLERANCE = 0.1_fp
  REAL(fp) :: Reflectivity_Prtb(N_LAYERS), Reflectivity(N_LAYERS), Reflectivity_TL(N_LAYERS)

  ! ============================================================================
  ! 1. **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  TYPE(CRTM_ChannelInfo_type)             :: ChannelInfo(N_SENSORS)
  TYPE(CRTM_Geometry_type)                :: Geometry(N_PROFILES)

  ! Define the FORWARD variables
  TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES), Atm_Prtb(N_PROFILES)
  TYPE(CRTM_Surface_type)                 :: Sfc(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_Perturb(:,:)

  ! Define the Tangent-Linear variables
  TYPE(CRTM_Atmosphere_type) :: Atmosphere_TL(N_PROFILES)
  !TYPE(CRTM_Atmosphere_type) :: Perturbation
  TYPE(CRTM_Surface_type)    :: Surface_TL(N_PROFILES)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution_TL(:,:)
  ! ============================================================================

  ! Directory location of coefficients
! #ifdef LITTLE_ENDIAN
  CHARACTER(*), PARAMETER :: ENDIAN_TYPE='little_endian'
! #else
!  CHARACTER(*), PARAMETER :: ENDIAN_TYPE='big_endian'
!#endif
  CHARACTER(*), PARAMETER :: COEFFICIENT_PATH='coefficients/'//ENDIAN_TYPE//'/'
  CHARACTER(*), PARAMETER :: NC_COEFFICIENT_PATH='coefficients/netcdf/'

  ! Aerosol/Cloud coefficient format
  !CHARACTER(*), PARAMETER :: Coeff_Format = 'Binary'
  CHARACTER(*), PARAMETER :: Coeff_Format = 'netCDF'

  ! Aerosol/Cloud coefficient scheme
  CHARACTER(*), PARAMETER :: Aerosol_Model = 'CRTM'
  !CHARACTER(*), PARAMETER :: Aerosol_Model = 'CMAQ'
  !CHARACTER(*), PARAMETER :: Aerosol_Model = 'GOCART-GEOS5'
  !CHARACTER(*), PARAMETER :: Aerosol_Model = 'NAAPS'
  CHARACTER(*), PARAMETER :: Cloud_Model   = 'CRTM'

  CHARACTER(256) :: AerosolCoeff_File
  CHARACTER(256) :: AerosolCoeff_Format
  CHARACTER(256) :: CloudCoeff_File
  CHARACTER(256) :: CloudCoeff_Format

  ! Program header
  ! --------------
  CALL CRTM_Version( Version )
  CALL Program_Message( PROGRAM_NAME, &
    'Program to provide a (relatively) simple example of how '//&
    'to call the CRTM K-Matrix function.', &
    'CRTM Version: '//TRIM(Version) )


  ! Get sensor id from user
  ! -----------------------
  !WRITE( *,'(/5x,"Enter sensor id [hirs4_n18, amsua_metop-a, or mhs_n18]: ")',ADVANCE='NO' )
  !READ( *,'(a)' ) Sensor_Id
  Sensor_Id = 'atms_npp'
  Sensor_Id = ADJUSTL(Sensor_Id)
  WRITE( *,'(//5x,"Running CRTM for ",a," sensor...")' ) TRIM(Sensor_Id)

  ! ============================================================================
  ! STEP 4. **** INITIALIZE THE CRTM ****
  !
  ! 4a. Initialise all the sensors at once
  ! --------------------------------------
  !.. Cloud coefficient information
  IF ( Coeff_Format == 'Binary' ) THEN
    CloudCoeff_Format   = 'Binary'
    CloudCoeff_File     = 'CloudCoeff.bin'
  ! if netCDF I/O
  ELSE IF ( Coeff_Format == 'netCDF' ) THEN
    CloudCoeff_Format   = 'netCDF'
    CloudCoeff_File     = 'CloudCoeff_DDA_ARTS.nc4'
  ELSE
    message = 'Aerosol/Cloud coefficient format is not supported'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF

  !.....Aerosol
  IF ( Aerosol_Model == 'CRTM' ) THEN
    IF ( Coeff_Format == 'Binary' ) THEN
      AerosolCoeff_Format = 'Binary'
      AerosolCoeff_File   = 'AerosolCoeff.bin'
    ELSE IF ( Coeff_Format == 'netCDF' ) THEN
      AerosolCoeff_Format = 'netCDF'
      AerosolCoeff_File   = 'AerosolCoeff.nc4'
    ELSE
      message = 'Aerosol coefficient format is not supported'
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF
  ELSEIF ( Aerosol_Model == 'CMAQ' ) THEN
    IF ( Coeff_Format == 'Binary' ) THEN
      AerosolCoeff_Format = 'Binary'
      AerosolCoeff_File   = 'AerosolCoeff.CMAQ.bin'
    ELSE IF ( Coeff_Format == 'netCDF' ) THEN
      AerosolCoeff_Format = 'netCDF'
      AerosolCoeff_File   = 'AerosolCoeff.CMAQ.nc4'
    ELSE
      message = 'Aerosol coefficient format is not supported'
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF
  ELSEIF ( Aerosol_Model == 'GOCART-GEOS5' ) THEN
    IF ( Coeff_Format == 'Binary' ) THEN
      AerosolCoeff_Format = 'Binary'
      AerosolCoeff_File   = 'AerosolCoeff.GOCART-GEOS5.bin'
    ELSE IF ( Coeff_Format == 'netCDF' ) THEN
      AerosolCoeff_Format = 'netCDF'
      AerosolCoeff_File   = 'AerosolCoeff.GOCART-GEOS5.nc4'
    ELSE
      message = 'Aerosol coefficient format is not supported'
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF
  ELSEIF ( Aerosol_Model == 'NAAPS' ) THEN
    IF ( Coeff_Format == 'Binary' ) THEN
      AerosolCoeff_Format = 'Binary'
      AerosolCoeff_File   = 'AerosolCoeff.NAAPS.bin'
    ELSE IF ( Coeff_Format == 'netCDF' ) THEN
      AerosolCoeff_Format = 'netCDF'
      AerosolCoeff_File   = 'AerosolCoeff.NAAPS.nc4'
    ELSE
      message = 'Aerosol coefficient format is not supported'
      CALL Display_Message( PROGRAM_NAME, message, FAILURE )
      STOP
    END IF
  END IF

  ! ============================================================================
  ! 2. **** INITIALIZE THE CRTM ****
  !
  ! 2a. This initializes the CRTM for the sensors
  !     predefined in the example SENSOR_ID parameter.
  !     NOTE: The coefficient data file path is hard-
  !           wired for this example.
  ! --------------------------------------------------
  WRITE( *,'(/5x,"Initializing the CRTM...")' )

  error_status = CRTM_Init( (/ sensor_ID /), &
                        channelInfo, &
                        Aerosol_Model, &
                        AerosolCoeff_Format, &
                        AerosolCoeff_File, &
                        Cloud_Model, &
                        CloudCoeff_Format, &
                        CloudCoeff_File, &
                        File_Path=COEFFICIENT_PATH, &
                        NC_File_Path=NC_COEFFICIENT_PATH, &
                        Quiet=.TRUE.)


  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error initializing CRTM'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

  ! 2b. Determine the total number of channels
  !     for which the CRTM was initialized
  ! ------------------------------------------
  n_Channels = SUM(CRTM_ChannelInfo_n_Channels(ChannelInfo))
  ! ============================================================================




  ! ============================================================================
  ! 3. **** ALLOCATE STRUCTURE ARRAYS ****
  !
  ! 3a. Allocate the ARRAYS
  ! -----------------------
  ! Note that only those structure arrays with a channel
  ! dimension are allocated here because we've parameterized
  ! the number of profiles in the N_PROFILES parameter.
  !
  ! Users can make the number of profiles dynamic also, but
  ! then the INPUT arrays (Atmosphere, Surface) will also have to be allocated.
  ALLOCATE( RTSolution( n_Channels, N_PROFILES ), &
            RTSolution_Perturb( n_Channels, N_PROFILES ), &
            RTSolution_TL( n_Channels, N_PROFILES ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Message = 'Error allocating structure arrays'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

  ! 3b. Allocate the STRUCTURES
  ! ---------------------------
  ! The input FORWARD structure
  CALL CRTM_Atmosphere_Create( Atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  Atm%Add_Extra_Layers = .FALSE.

  CALL CRTM_Atmosphere_Create( Atm_Prtb, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm_Prtb)) ) THEN
    Message = 'Error allocating CRTM Atmosphere structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  Atm_Prtb%Add_Extra_Layers = .FALSE.
  
  ! The input TL structure
  CALL CRTM_Atmosphere_Create( Atmosphere_TL, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
  IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atmosphere_TL)) ) THEN
    Message = 'Error allocating CRTM Atmosphere_TL structure'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF

CALL CRTM_RTSolution_Create(RTSolution,N_LAYERS)
CALL CRTM_RTSolution_Create(RTSolution_TL,N_LAYERS)
CALL CRTM_RTSolution_Create(RTSolution_Perturb,N_LAYERS)

  ! ============================================================================




  ! ============================================================================
  ! 4. **** ASSIGN INPUT DATA ****
  !
  ! Fill the Atmosphere structure array.
  ! NOTE: This is an example program for illustrative purposes only.
  !       Typically, one would not assign the data as shown below,
  !       but rather read it from file

  ! 4a. Atmosphere and Surface input
  ! --------------------------------
  CALL Load_Atm_Data()
  CALL Load_Sfc_Data()


    Do ii=1,2
    Atmosphere_TL(ii)%Climatology = atm(ii)%Climatology 
    Atmosphere_TL(ii)%Absorber_Id = atm(ii)%Absorber_Id
    Atmosphere_TL(ii)%Absorber_Units = atm(ii)%Absorber_Units
    atm(ii)%Height = Calculate_Height(Atm(ii))
    Atmosphere_TL(ii)%Height = atm(ii)%Height 
    Atm_Prtb(ii) = Atm(ii)
    Atm(ii)%Cloud(1)%Water_Content = 5
    END DO

  ! 4b. GeometryInfo input
  ! ----------------------
  ! All profiles are given the same value
  !  The Sensor_Scan_Angle is optional.
  CALL CRTM_Geometry_SetValue( Geometry, &
                               Sensor_Zenith_Angle = ZENITH_ANGLE, &
                               Sensor_Scan_Angle   = SCAN_ANGLE )
  ! ============================================================================

  


  ! ============================================================================
  ! 5. **** INITIALIZE THE TL ARGUMENTS ****
  !
  ! 5a. Zero the LT INPUT structures
  ! ---------------------------------------
  CALL CRTM_Atmosphere_Zero( Atmosphere_TL )
  CALL CRTM_Surface_Zero( Surface_TL )

  ! ============================================================================
  ! 6. **** CALL THE CRTM FORWARD MODEL ****
  !
  Error_Status = CRTM_Forward( Atm         , &
                                  Sfc         , &
                                  Geometry    , &
                                  ChannelInfo , &
                                  RTSolution  )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error in CRTM Forward Model'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
    
  
  
  testresult = 0
  Ratio_old = 100.0_fp 

  ilev = 70
  iprof = 1
  ichan = 3 !16
  

  OPEN(500,FILE='convergence.txt',STATUS='UNKNOWN')
  write(500, '(A)')  'Ratio should approach ZERO because it is ONE - [(R_Prtb - R) / R_TL)]'
  write(500,'(2A)')  'Iter  Refl (R)    R_Prt - R        R_TL              WC_Prtb          Ratio', &
                    '        --    R         R_Prt - R        R_TL             WC_Prtb          Ratio'
    
  convergence_loop: DO ii = 1, 20
  sign = -1  ! we want to make sure both positive and negative signs converge
  sign_loop: DO isign = 1, nsign
     sign = -1 * sign

    Perturbation = Atm(1)%Cloud(1)%Water_Content(ilev)*(sign * 1.0_fp)/(2.0_fp**ii)
    Atmosphere_TL(iprof)%Cloud(1)%Water_Content(ilev) = Perturbation
    ! ============================================================================

    !Atm(1)%Temperature(92) = Atm(1)%Temperature(92) + Perturbation
    Atm_Prtb(iprof)%Cloud(1)%Water_Content(ilev) = Atm(iprof)%Cloud(1)%Water_Content(ilev) + Perturbation
    
    ! ============================================================================
    ! 6. **** CALL THE PERTURBED CRTM FORWARD MODEL ****
    !
    Error_Status = CRTM_Forward( Atm_Prtb         , &
                                  Sfc         , &
                                  Geometry    , &
                                  ChannelInfo , &
                                  RTSolution_Perturb  )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error in perturbed CRTM Forward Model'
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
      STOP 1
    END IF


    ! ============================================================================
    ! 6. **** CALL THE CRTM TANGENT-LINEAR MODEL ****
    !
    Error_Status = CRTM_Tangent_Linear( Atm , &
                                      Sfc , &
                                      Atmosphere_TL , &
                                      Surface_TL , &
                                      Geometry , &
                                      ChannelInfo , &
                                      RTSolution , &
                                      RTSolution_TL  )
    IF ( Error_Status /= SUCCESS ) THEN
     Message = 'Error in CRTM Tangent-linear Model'
     CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
     STOP 1
    END IF

    !WRITE(*,*) 'Tangent-linear Result: ', RTSolution_TL(ichan,iprof)%Reflectivity(ilev)

    ! ============================================================================

    ! ============================================================================


    ! ============================================================================
    Reflectivity_Prtb = RTSolution_Perturb(ichan,iprof)%Reflectivity
    Reflectivity = RTSolution(ichan,iprof)%Reflectivity
    Reflectivity_TL = RTSolution_TL(ichan,iprof)%Reflectivity
    

     Ratio_new(isign) = ONE - ((Reflectivity_Prtb(ilev) - Reflectivity(ilev)) / Reflectivity_TL(ilev))
 
    
    !do jj=70,80
    jj = ilev
    write(500,'(I5, F8.3, 5F17.11)', advance='no') ii, Reflectivity(jj), Reflectivity_Prtb(jj) - Reflectivity(jj), Reflectivity_TL(jj), Perturbation,       Ratio_new(isign)
    !ENDDO


    ! Reassign Ratio for next iteration.
    Ratio_old(isign) = Ratio_new(isign) 
    
    END DO sign_loop
    write(500,*)   ! new line


  END DO convergence_loop
 
  ! ============================================================================
  ! 8. **** DESTROY THE CRTM ****
  !
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error destroying CRTM'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
    STOP 1
  END IF
  ! ============================================================================


  CLOSE(500)

  

  ! ============================================================================
  ! 10. **** CLEAN UP ****
  !
  ! 10a. Deallocate the structures.
  !      These are the explicitly allocated structures.
  !      Note that in some cases other structures, such as the Sfc
  !      and RTSolution structures, will also be allocated and thus
  !      should also be deallocated here.
  ! ---------------------------------------------------------------
  CALL CRTM_Atmosphere_Destroy(Atmosphere_TL)
  CALL CRTM_Atmosphere_Destroy(Atm)

  ! 10b. Deallocate the arrays
  ! --------------------------
  DEALLOCATE(RTSolution, RTSolution_TL, &
             STAT = Allocate_Status)
  ! ============================================================================
  IF( ( testresult == 0 ) .AND. ( Ratio_old(1) < TOLERANCE ) .AND. ( Ratio_old(2) < TOLERANCE ) ) THEN
    testresult = 0
    STOP 0
  ELSE
    testresult = 1
    STOP 1
  END IF


CONTAINS

  INCLUDE 'Load_Atm_Data.inc'
  INCLUDE 'Load_Sfc_Data.inc'

  SUBROUTINE Integrate_Reflectivity(Re, R)
      IMPLICIT NONE
      Real(fp), INTENT(IN) :: Re(:)
      Real(fp), INTENT(OUT) :: R(:)
      REAL(fp) :: Rs(SIZE(Re))
      REAL(fp), PARAMETER :: M3_to_MM6overM3 = 1.0d18
      

      WHERE (Re > -100)
          Rs = 10.0_fp**(Re / 10.0_fp) / M3_to_MM6overM3
      ELSEWHERE
          Rs = 0.0_fp
      END WHERE
      
      R = Rs      
      !R = SUM(Rs)
      
  END SUBROUTINE Integrate_Reflectivity

END PROGRAM test_TL_convergence


