!
! CRTM_IRSnowEM
!
! Module containing function to invoke the CRTM Infrared
! Snow Emissivity Model.
!
!
! CREATION HISTORY:
!       Written by:   Cheng Dang, 31-May-2022
!                     dangch@ucar.edu
!

MODULE CRTM_IRSnowEM

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,          ONLY: fp
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,     ONLY: ZERO, ONE, DEGREES_TO_RADIANS
  USE CRTM_Interpolation,  ONLY: NPTS, &
                                 LPoly, &
                                 LPoly_type, &
                                 Clear_LPoly, &
                                 Find_Index, &
                                 Interp_3D, &
                                 Interp_4D, &
                                 LPoly_TL, &
                                 Interp_3D_TL, &
                                 Interp_4D_TL, &
                                 LPoly_AD, &
                                 Interp_3D_AD, &
                                 Interp_4D_AD
  USE IRsnowCoeff_Define, ONLY: IRsnowCoeff_type
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Derived type
  PUBLIC :: iVar_type
  ! Procedures
  PUBLIC :: CRTM_Compute_IRSnowEM
  PUBLIC :: CRTM_Compute_IRSnowEM_TL
  PUBLIC :: CRTM_Compute_IRSnowEM_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! -------------------------------
  ! Structure definition to hold
  ! forward interpolating variables
  ! across fwd, tl and adjoint
  ! -------------------------------
  ! The interpolation routine structure
  TYPE :: Einterp_type
    ! The dimensions
    INTEGER :: n_Angles = 0
    INTEGER :: n_Pts    = 0
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! The interpolating polynomials
    TYPE(LPoly_type), ALLOCATABLE :: wlp(:)  ! Angle
    TYPE(LPoly_type)              :: xlp     ! Frequency
    TYPE(LPoly_type)              :: ylp     ! Snow Grain Size
    TYPE(LPoly_type)              :: zlp     ! Snow Temperature
    ! The LUT interpolation indices
    INTEGER, ALLOCATABLE :: i1(:) , i2(:)    ! Angle
    INTEGER              :: j1    , j2       ! Frequency
    INTEGER              :: k1    , k2       ! Snow Grain Size
    INTEGER              :: l1    , l2       ! Snow Temperature
    ! The LUT interpolation boundary check
    LOGICAL, ALLOCATABLE :: a_outbound(:)    ! Angle
    LOGICAL              :: f_outbound       ! Frequency
    LOGICAL              :: r_outbound       ! Snow Grain Size
    LOGICAL              :: t_outbound       ! Snow Temperature
    ! The interpolation input
    REAL(fp), ALLOCATABLE :: a_int(:)        ! Angle
    REAL(fp)              :: f_int           ! Frequency
    REAL(fp)              :: r_int           ! Snow Grain Size
    REAL(fp)              :: t_int           ! Snow Temperature
    ! The data to be interpolated
    REAL(fp), ALLOCATABLE :: a(:,:)          ! Angle
    REAL(fp), ALLOCATABLE :: f(:)            ! Frequency
    REAL(fp), ALLOCATABLE :: r(:)            ! Snow Grain Size
    REAL(fp), ALLOCATABLE :: t(:)            ! Snow Temperature
  END TYPE Einterp_type

  ! The main internal variable structure
  TYPE :: iVar_type
    PRIVATE
    ! The interpolation data
    TYPE(Einterp_type) :: ei
  END TYPE iVar_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_IRSnowEM
!
! PURPOSE:
!       Function to compute the CRTM infrared snow surface emissivity
!       for input temperature, grain size, frequency, and angles.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSnowEM(IRsnowCoeff      , &
!                                            Snow_Temperature , &
!                                            Snow_Grain_Size  , &
!                                            Frequency        , &
!                                            Angle            , &
!                                            iVar             , &
!                                            Emissivity         )
!
! INPUTS:
!         IRsnowCoeff:  Infrared snow emissivity model coefficient object.
!                       Load the object with the coefficients for the emissivity
!                       model to use.
!                       UNITS:      N/A
!                       TYPE:       IRsnowCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   Snow_Temperature:   Snow temperature.
!                       UNITS:      Kelvin (K)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!   Snow_Grain_Size:    Snow grain size.
!                       UNITS:      microns (um)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!         Frequency:    Infrared frequency.
!                       UNITS:      inverse centimetres (cm^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!            Angle:     Surface zenith angle.
!                       UNITS:      Degrees
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!             iVar:     Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of this module.
!                       UNITS:      N/A
!                       TYPE:       iVar_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
!       Emissivity:     Snow surface emissivities for the
!                       requested Grain Size, frequency, and angles.
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Same as input ANGLE argument.
!                       ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the computation was successful.
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSnowEM( &
    IRsnowCoeff        , &  ! Input model coefficients
    Snow_Temperature   , &  ! Input
    Snow_Grain_Size    , &  ! Input
    Frequency          , &  ! Input
    Angle              , &  ! Input
    iVar               , &  ! Internal variable output
    Emissivity         ) &  ! Output
  RESULT( err_stat )
    ! Arguments
    TYPE(IRsnowCoeff_type) , INTENT(IN)  :: IRsnowCoeff
    REAL(fp)               , INTENT(IN)  :: Snow_Temperature  ! t
    REAL(fp)               , INTENT(IN)  :: Snow_Grain_Size   ! r
    REAL(fp)               , INTENT(IN)  :: Frequency         ! f
    REAL(fp)               , INTENT(IN)  :: Angle(:)          ! a
    TYPE(iVar_type)        , INTENT(OUT) :: iVar
    REAL(fp)               , INTENT(OUT) :: Emissivity(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRsnowEM'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n_Angles, i

    ! Set up
    err_stat = SUCCESS
    ! ...Check dimensions
    n_Angles = SIZE(Angle)
    IF ( SIZE(Emissivity) /= n_Angles ) THEN
      err_stat = FAILURE
      msg = 'Input Angle and output Emissivity array dimensions inconsistent.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...Allocate interpolation variable structure
    CALL Einterp_Create( iVar%ei, NPTS, n_Angles )
    IF ( .NOT. Einterp_Associated( iVar%ei ) ) THEN
      err_stat = FAILURE
      msg = 'Error allocating interpolation variable structure.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Compute the snow temperature interpolating polynomial
    ! ...Find the LUT indices and check if input is out of bounds
    iVar%ei%t_int = Snow_Temperature
    CALL find_index(IRsnowCoeff%Temperature, &
                    iVar%ei%t_int, iVar%ei%l1, iVar%ei%l2, iVar%ei%t_outbound)
    iVar%ei%t = IRsnowCoeff%Temperature(iVar%ei%l1:iVar%ei%l2)
    ! ...Compute the polynomial
    CALL LPoly( iVar%ei%t    , & ! Input
                iVar%ei%t_int, & ! Input
                iVar%ei%zlp    ) ! Output


    ! Compute the grain size interpolating polynomial
    ! ...Find the LUT indices and check if input is out of bounds
    iVar%ei%r_int = Snow_Grain_Size
    CALL find_index(IRsnowCoeff%Grain_Size, &
                    iVar%ei%r_int, iVar%ei%k1, iVar%ei%k2, iVar%ei%r_outbound)
    iVar%ei%r = IRsnowCoeff%Grain_Size(iVar%ei%k1:iVar%ei%k2)
    ! ...Compute the polynomial
    CALL LPoly( iVar%ei%r    , & ! Input
                iVar%ei%r_int, & ! Input
                iVar%ei%ylp    ) ! Output


    ! Compute the frequency interpolating polynomial
    ! ...Find the LUT indices and check if input is out of bounds
    iVar%ei%f_int = Frequency
    CALL find_index(IRsnowCoeff%Frequency, &
                    iVar%ei%f_int, iVar%ei%j1, iVar%ei%j2, iVar%ei%f_outbound)
    iVar%ei%f = IRsnowCoeff%Frequency(iVar%ei%j1:iVar%ei%j2)
    ! ...Compute the polynomial
    CALL LPoly( iVar%ei%f    , & ! Input
                iVar%ei%f_int, & ! Input
                iVar%ei%xlp    ) ! Output


    ! Compute the angle interpolating polynomials
    DO i = 1, n_Angles

      ! ...Find the LUT indices and check if input is out of bounds
      iVar%ei%a_int(i) = ABS(Angle(i))
      CALL find_index(IRsnowCoeff%Angle, &
                      iVar%ei%a_int(i), iVar%ei%i1(i), iVar%ei%i2(i), iVar%ei%a_outbound(i))
      iVar%ei%a(:,i) = IRsnowCoeff%Angle(iVar%ei%i1(i):iVar%ei%i2(i))

      ! ...Compute the polynomial
      CALL LPoly( iVar%ei%a(:,i)  , & ! Input
                  iVar%ei%a_int(i), & ! Input
                  iVar%ei%wlp(i)    ) ! Output

      ! Compute the interpolated emissivity
      CALL Interp_4D( IRsnowCoeff%Emissivity( iVar%ei%i1(i):iVar%ei%i2(i) , &
                                               iVar%ei%j1   :iVar%ei%j2   , &
                                               iVar%ei%k1   :iVar%ei%k2   , &
                                               iVar%ei%l1   :iVar%ei%l2  ), & ! Input
                      iVar%ei%wlp(i), & ! Input
                      iVar%ei%xlp   , & ! Input
                      iVar%ei%ylp   , & ! Input
                      iVar%ei%zlp   , & ! Input
                      Emissivity(i)   ) ! Output

    END DO

  END FUNCTION CRTM_Compute_IRSnowEM


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_IRSnowEM_TL
!
! PURPOSE:
!       Function to compute the tangent-linear CRTM infrared snow
!       emissivity for input temperature, grain size, frequency,
!       and angles.
!
!       This function must be called *after* the forward model function,
!       CRTM_Compute_IRSnowEM, has been called. The forward model function
!       populates the internal variable structure argument, iVar.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSnowEM_TL( IRsnowCoeff         , &
!                                              Snow_Temperature_TL , &
!                                              Snow_Grain_Size_TL  , &
!                                              iVar                , &
!                                              Emissivity_TL         )
! INPUTS:
!        IRsnowCoeff:   Infrared snow emissivity model coefficient object.
!                       Load the object with the coefficients for the emissivity
!                       model to use.
!                       UNITS:      N/A
!                       TYPE:       IRsnowCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! Snow_Temperature_TL:  The tangent-linear snow temperature.
!                       UNITS:      Kelvin (K)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!  Snow_Grain_Size_TL:  The tangent-linear Grain Size.
!                       UNITS:      metres per second (m.s^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!               iVar:   Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of this module.
!                       UNITS:      N/A
!                       TYPE:       iVar_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Emissivity_TL:  Tangent-linear snow surface emissivity.
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the computation was successful.
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSnowEM_TL( &
    IRsnowCoeff          , &  ! Input model coefficients
    Snow_Temperature_TL  , &  ! Input
    Snow_Grain_Size_TL   , &  ! Input
    iVar                 , &  ! Internal variable input
    Emissivity_TL        ) &  ! Output
  RESULT( err_stat )
    ! Arguments
    TYPE(IRsnowCoeff_type) , INTENT(IN)  :: IRsnowCoeff
    REAL(fp)               , INTENT(IN)  :: Snow_Temperature_TL
    REAL(fp)               , INTENT(IN)  :: Snow_Grain_Size_TL
    TYPE(iVar_type)        , INTENT(IN)  :: iVar
    REAL(fp)               , INTENT(OUT) :: Emissivity_TL(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRsnowEM_TL'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER  :: i
    REAL(fp) :: r_TL(NPTS), t_TL(NPTS)
    REAL(fp) :: e_TL_3D(NPTS,NPTS,NPTS), e_TL_4D(NPTS,NPTS,NPTS,NPTS)
    TYPE(LPoly_Type) :: ylp_TL, xlp_TL, wlp_TL, zlp_TL

    ! Set up
    err_stat = SUCCESS
    ! ...Check internal variable allocation
    IF ( .NOT. Einterp_Associated( iVar%ei ) ) THEN
      err_stat = FAILURE
      msg = 'Internal structure ei is not allocated'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...Check dimensions
    IF ( SIZE( Emissivity_TL ) /= iVar%ei%n_Angles ) THEN
      err_stat = FAILURE
      msg = 'Input Emissivity_TL array dimensions inconsistent with number of angles.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...No TL if snow temperature is out of bounds
    IF (iVar%ei%t_outbound) THEN
      Emissivity_TL = ZERO
      RETURN
    END IF
    ! ...No TL if snow grain size is out of bounds
    IF ( iVar%ei%r_outbound ) THEN
      Emissivity_TL = ZERO
      RETURN
    END IF
    ! ...Initialise local TL variables
    t_TL = ZERO
    r_TL = ZERO
    e_TL_3D = ZERO
    e_TL_4D = ZERO
    CALL Clear_LPoly(wlp_TL)
    CALL Clear_LPoly(xlp_TL)


    ! Calculate the TL interpolating
    ! polynomials for snow temperature
    CALL LPoly_TL( iVar%ei%t, iVar%ei%t_int,   & ! FWD Input
                   iVar%ei%zlp,                & ! FWD Input
                   t_TL, Snow_Temperature_TL,  & ! TL  Input
                   zlp_TL                      ) ! TL  Output
    ! polynomials for snow grain size
    CALL LPoly_TL( iVar%ei%r, iVar%ei%r_int,   & ! FWD Input
                   iVar%ei%ylp,                & ! FWD Input
                   r_TL, Snow_Grain_Size_TL,   & ! TL  Input
                   ylp_TL                      ) ! TL  Output


    ! Begin loop over angles
    DO i = 1, iVar%ei%n_Angles

      ! Perform interpolation
      CALL interp_4D_TL(IRsnowCoeff%Emissivity(iVar%ei%i1(i) :iVar%ei%i2(i), &
                                                iVar%ei%j1   :iVar%ei%j2   , &
                                                iVar%ei%k1   :iVar%ei%k2   , &
                                                iVar%ei%l1   :iVar%ei%l2  ), & ! FWD Emissivity input
                        iVar%ei%wlp(i), & ! FWD polynomial input
                        iVar%ei%xlp   , & ! FWD polynomial input
                        iVar%ei%ylp   , & ! FWD polynomial input
                        iVar%ei%zlp   , & ! FWD polynomial input
                        e_TL_4D, wlp_TL, xlp_TL, ylp_TL, zlp_TL, & ! TL input
                        Emissivity_TL(i)              ) ! Output

    END DO

  END FUNCTION CRTM_Compute_IRSnowEM_TL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_IRSnowEM_AD
!
! PURPOSE:
!       Function to compute the adjoint of the CRTM infrared snow
!       emissivity for input grain size, frequency, and angles.
!
!       This function must be called *after* the forward model function,
!       CRTM_Compute_IRSnowEM, has been called. The forward model function
!       populates the internal variable structure argument, iVar.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSnowEM_AD(IRsnowCoeff         , &
!                                               Emissivity_AD       , &
!                                               iVar                , &
!                                               Snow_Grain_Size_AD  , &
!                                               Snow_Temperature_AD  )
!
! INPUTS:
!       IRsnowCoeff:   Infrared snow emissivity model coefficient object.
!                       Load the object with the coefficients for the emissivity
!                       model to use.
!                       UNITS:      N/A
!                       TYPE:       IRsnowCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Emissivity_AD:  Adjoint snow surface emissivity.
!                       *** SET TO ZERO ON EXIT ***
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
!               iVar:   Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of this module.
!                       UNITS:      N/A
!                       TYPE:       iVar_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!   Snow_Grain_Size_AD:  Adjoint snow grain size.
!                        *** MUST HAVE VALUE ON ENTRY ***
!                        UNITS:      per microns (um)^-1
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!  Snow_Temperature_AD:  Adjoint snow temperature.
!                        *** MUST HAVE VALUE ON ENTRY ***
!                        UNITS:      per Kelvin, (K)^-1
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the computation was successful.
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSnowEM_AD( &
    IRsnowCoeff          , &  ! Input model coefficients
    Emissivity_AD        , &  ! Input
    iVar                 , &  ! Internal Variable Input
    Snow_Grain_Size_AD   , &  ! Output
    Snow_Temperature_AD  ) &  ! Output
  RESULT( err_stat )
    ! Arguments
    TYPE(IRsnowCoeff_type) , INTENT(IN)     :: IRsnowCoeff
    REAL(fp)               , INTENT(IN OUT) :: Emissivity_AD(:)
    TYPE(iVar_type)        , INTENT(IN)     :: iVar
    REAL(fp)               , INTENT(IN OUT) :: Snow_Grain_Size_AD
    REAL(fp)               , INTENT(IN OUT) :: Snow_Temperature_AD
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRsnowEM_AD'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER  :: i
    REAL(fp) :: e_AD_3D(NPTS,NPTS,NPTS), e_AD_4D(NPTS,NPTS,NPTS,NPTS)
    REAL(fp) :: r_AD(NPTS), t_AD(NPTS)
    TYPE(LPoly_Type) :: wlp_AD, xlp_AD, ylp_AD, zlp_AD

    ! Set Up
    err_stat = SUCCESS
    e_AD_3D = ZERO
    e_AD_4D = ZERO
    r_AD = ZERO
    t_AD = ZERO
    ! ...Check internal variable allocation
    IF ( .NOT. Einterp_Associated( iVar%ei ) ) THEN
      err_stat = FAILURE
      msg = 'Internal structure ei is not allocated'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...Check dimensions
    IF ( SIZE(Emissivity_AD) /= iVar%ei%n_Angles ) THEN
      err_stat = FAILURE
      msg = 'Input Emissivity_AD array dimensions inconsistent with number of angles.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...No AD if snow temperature is out of bounds
    IF (iVar%ei%t_outbound) RETURN
    ! ...No AD if snow grain size is out of bounds
    IF ( iVar%ei%r_outbound ) RETURN
    ! ...Initialize local variables
    CALL Clear_LPoly(wlp_AD)
    CALL Clear_LPoly(xlp_AD)
    CALL Clear_LPoly(ylp_AD)
    CALL Clear_LPoly(zlp_AD)

    ! Loop over emissivity calculation angles
    DO i = 1, iVar%ei%n_Angles

      ! Get the adjoint interpoalting polynomial for snow grain size and snow temperature
      CALL Interp_4D_AD(IRsnowCoeff%Emissivity( iVar%ei%i1(i) :iVar%ei%i2(i), &
                                                 iVar%ei%j1   :iVar%ei%j2   , &
                                                 iVar%ei%k1   :iVar%ei%k2   , &
                                                 iVar%ei%l1   :iVar%ei%l2  ), & ! FWD Input
                        iVar%ei%wlp(i)  , & ! FWD Input
                        iVar%ei%xlp     , & ! FWD Input
                        iVar%ei%ylp     , & ! FWD Input
                        iVar%ei%zlp     , & ! FWD Input
                        Emissivity_AD(i), & ! AD Input
                        e_AD_4D, wlp_AD, xlp_AD, ylp_AD, zlp_AD ) ! AD Output
      ! Set adjoint emissivity to zero
      Emissivity_AD(i) = ZERO

    END DO

    ! Compute the snow grain size adjoint
    CALL Lpoly_AD(iVar%ei%r          , & ! FWD Input
                  iVar%ei%r_int      , & ! FWD Input
                  iVar%ei%ylp        , & ! FWD Input
                  ylp_AD             , & ! AD  Input
                  r_AD               , & ! AD  Output
                  Snow_Grain_Size_AD   ) ! AD  Output

    ! Compute the snow temperature adjoint
    CALL Lpoly_AD(iVar%ei%t           , & ! FWD Input
                  iVar%ei%t_int       , & ! FWD Input
                  iVar%ei%zlp         , & ! FWD Input
                  zlp_AD              , & ! AD  Input
                  t_AD                , & ! AD  Output
                  Snow_Temperature_AD   ) ! AD  Output

  END FUNCTION CRTM_Compute_IRSnowEM_AD


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! ----------------------------------------------
  ! Procedures to manipulate the Einterp structure
  ! ----------------------------------------------
  ELEMENTAL FUNCTION Einterp_Associated( ei ) RESULT( Status )
    TYPE(Einterp_type), INTENT(IN) :: ei
    LOGICAL :: Status
    Status = ei%Is_Allocated
  END FUNCTION Einterp_Associated

  ELEMENTAL SUBROUTINE Einterp_Create( ei, n_Pts, n_Angles )
    TYPE(Einterp_type), INTENT(OUT) :: ei
    INTEGER,            INTENT(IN)  :: n_Pts
    INTEGER,            INTENT(IN)  :: n_Angles
    INTEGER :: alloc_stat
    IF ( n_Pts < 1 .OR. n_Angles < 1 ) RETURN
    ALLOCATE( ei%wlp(n_Angles)       , &
              ei%i1(n_Angles)        , &
              ei%i2(n_Angles)        , &
              ei%a_outbound(n_Angles), &
              ei%a_int(n_Angles)     , &
              ei%a(n_Pts,n_Angles)   , &
              ei%f(n_Pts)            , &
              ei%r(n_Pts)            , &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN
    ei%n_Angles = n_Angles
    ei%n_Pts    = n_Pts
    ei%Is_Allocated = .TRUE.
  END SUBROUTINE Einterp_Create

END MODULE CRTM_IRSnowEM
