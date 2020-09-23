!
! Retrieve_Emissivity_Module
!
! Module containing the procedures to retrieve and interpolate emissivities from
! a look-up table (LUT)
!
!
! CREATION HISTORY:
!       Written by:  Paul van Delst, February 2012
!                    paul.vandelst@noaa.gov
!

MODULE Retrieve_Emissivity_Module

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, &
        ONLY: fp
  USE MWwaterLUT_Define, &
        ONLY: MWwaterLUT_type, &
              MWwaterLUT_Associated
  USE CRTM_Interpolation, &
        ONLY: NPTS        , &
              LPoly_type  , &
              find_index  , &
              interp_4D   , &
              interp_4D_TL, &
              interp_4D_AD, &
              Clear_LPoly , &
              LPoly       , &
              LPoly_TL    , &
              LPoly_AD
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Retrieve_Emissivity
  PUBLIC :: Retrieve_Emissivity_TL
  PUBLIC :: Retrieve_Emissivity_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &

  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp
  ! Number of look-up table dimensions
  INTEGER, PARAMETER :: N_LUTDIMS = 4


  ! --------------------------------------
  ! Structure definition to hold internal
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  ! The interpolation routine structure
  TYPE :: eiInterp_type
    TYPE(LPoly_type) :: lp        ! The interpolating polynomial
    INTEGER          :: i1, i2    ! The LUT interpolation indices
    LOGICAL          :: outbound  ! The LUT interpolation boundary check
    REAL(fp)         :: xint      ! The interpolation point
    REAL(fp)         :: x(NPTS)   ! The data to be interpolated
  END TYPE eiInterp_type


  TYPE :: iVar_type
    PRIVATE
    ! Direct inputs
    REAL(fp) :: wind_speed
    ! Look-up table interpolation data
    TYPE(eiInterp_type) :: ei(N_LUTDIMS)
  END TYPE iVar_type


CONTAINS


  ! ============================================================
  ! Procedures to retrieve the emissivity from the look-up table
  ! ============================================================
  ! Forward model
  SUBROUTINE Retrieve_Emissivity( &
    LUT         , &  ! Input
    Zenith_Angle, &  ! Input
    Frequency   , &  ! Input
    Temperature , &  ! Input
    Wind_Speed  , &  ! Input
    ev          , &  ! Output
    eh          , &  ! Output
    iVar          )  ! Internal variable output
    ! Arguments
    TYPE(MWwaterLUT_type), TARGET, INTENT(IN)     :: LUT
    REAL(fp),                      INTENT(IN)     :: Zenith_Angle
    REAL(fp),                      INTENT(IN)     :: Frequency
    REAL(fp),                      INTENT(IN)     :: Temperature
    REAL(fp),                      INTENT(IN)     :: Wind_Speed
    REAL(fp),                      INTENT(OUT)    :: ev
    REAL(fp),                      INTENT(OUT)    :: eh
    TYPE(iVar_type),               INTENT(IN OUT) :: iVar
    ! Local variables
    INTEGER :: i
    REAL(fp), POINTER :: z(:,:,:,:) => NULL()

    ! Setup
    IF ( .NOT. MWwaterLUT_Associated(LUT) ) THEN
      ev = ZERO
      eh = ZERO
      RETURN
    END IF


    ! Find the indices for interpolation
    ! ...Zenith angle
    iVar%ei(1)%xint = MAX(MIN(LUT%Angle(LUT%n_Angles),Zenith_Angle),LUT%Angle(1))
    CALL find_index(LUT%Angle,iVar%ei(1)%xint,iVar%ei(1)%i1,iVar%ei(1)%i2, iVar%ei(1)%outbound)
    iVar%ei(1)%x = LUT%Angle(iVar%ei(1)%i1:iVar%ei(1)%i2)
    ! ...Frequency
    iVar%ei(2)%xint = MAX(MIN(LUT%Frequency(LUT%n_Frequencies),Frequency),LUT%Frequency(1))
    CALL find_index(LUT%Frequency,iVar%ei(2)%xint,iVar%ei(2)%i1,iVar%ei(2)%i2, iVar%ei(2)%outbound)
    iVar%ei(2)%x = LUT%Frequency(iVar%ei(2)%i1:iVar%ei(2)%i2)
    ! ...Temperature
    iVar%ei(3)%xint = MAX(MIN(LUT%Temperature(LUT%n_Temperatures),Temperature),LUT%Temperature(1))
    CALL find_index(LUT%Temperature,iVar%ei(3)%xint,iVar%ei(3)%i1,iVar%ei(3)%i2, iVar%ei(3)%outbound)
    iVar%ei(3)%x = LUT%Temperature(iVar%ei(3)%i1:iVar%ei(3)%i2)
    ! ...Wind speed
    iVar%ei(4)%xint = MAX(MIN(LUT%Wind_Speed(LUT%n_Wind_Speeds),Wind_Speed),LUT%Wind_Speed(1))
    CALL find_index(LUT%Wind_Speed,iVar%ei(4)%xint,iVar%ei(4)%i1,iVar%ei(4)%i2, iVar%ei(4)%outbound)
    iVar%ei(4)%x = LUT%Wind_Speed(iVar%ei(4)%i1:iVar%ei(4)%i2)


    ! Calculate all the interpolating polynomials
    DO i = 1, N_LUTDIMS
      CALL LPoly( iVar%ei(i)%x   , &  ! Input
                  iVar%ei(i)%xint, &  ! Input
                  iVar%ei(i)%lp    )  ! Output
    END DO


    ! Perform the interpolation
    ! ...Vertical polarisation
    z => LUT%ev(iVar%ei(1)%i1:iVar%ei(1)%i2, &
                iVar%ei(2)%i1:iVar%ei(2)%i2, &
                iVar%ei(3)%i1:iVar%ei(3)%i2, &
                iVar%ei(4)%i1:iVar%ei(4)%i2  )
    CALL interp_4D( z, iVar%ei(1)%lp, iVar%ei(2)%lp, iVar%ei(3)%lp, iVar%ei(4)%lp, ev )
    ! ...Horizontal polarisation
    z => LUT%eh(iVar%ei(1)%i1:iVar%ei(1)%i2, &
                iVar%ei(2)%i1:iVar%ei(2)%i2, &
                iVar%ei(3)%i1:iVar%ei(3)%i2, &
                iVar%ei(4)%i1:iVar%ei(4)%i2  )
    CALL interp_4D( z, iVar%ei(1)%lp, iVar%ei(2)%lp, iVar%ei(3)%lp, iVar%ei(4)%lp, eh )
    z => NULL()

  END SUBROUTINE Retrieve_Emissivity


  ! Tangent-linear model
  SUBROUTINE Retrieve_Emissivity_TL( &
    LUT           , &  ! Input
    Temperature_TL, &  ! Input
    Wind_Speed_TL , &  ! Input
    ev_TL         , &  ! Output
    eh_TL         , &  ! Output
    iVar            )  ! Internal variable input
    ! Arguments
    TYPE(MWwaterLUT_type), TARGET, INTENT(IN)  :: LUT
    REAL(fp),                      INTENT(IN)  :: Temperature_TL
    REAL(fp),                      INTENT(IN)  :: Wind_Speed_TL
    REAL(fp),                      INTENT(OUT) :: ev_TL
    REAL(fp),                      INTENT(OUT) :: eh_TL
    TYPE(iVar_type),               INTENT(IN)  :: iVar
    ! Local variables
    INTEGER :: i
    TYPE(eiInterp_type) :: ei_TL(N_LUTDIMS)
    REAL(fp), POINTER :: z(:,:,:,:) => NULL()
    REAL(fp) :: z_TL(NPTS,NPTS,NPTS,NPTS)

    ! Setup
    ! ...No TL output if "perturbable" dimensions are outside LUT bounds
    IF ( iVar%ei(3)%outbound .AND. &        ! Temperature
         iVar%ei(4)%outbound       ) THEN   ! Wind speed
      ev_TL = ZERO
      eh_TL = ZERO
      RETURN
    END IF
    ! ...The TL inputs
    ei_TL%xint = ZERO
    ei_TL(3)%xint = Temperature_TL
    ei_TL(4)%xint = Wind_Speed_TL
    DO i = 1, N_LUTDIMS
      ei_TL(i)%x = ZERO
    END DO
    z_TL = ZERO


    ! Calculate all the TL interpolating polynomials
    DO i = 1, N_LUTDIMS
      CALL LPoly_TL( iVar%ei(i)%x   , &  ! FWD Input
                     iVar%ei(i)%xint, &  ! FWD Input
                     iVar%ei(i)%lp  , &  ! FWD Input
                     ei_TL(i)%x     , &  ! TL  Input
                     ei_TL(i)%xint  , &  ! TL  Input
                     ei_TL(i)%lp      )  ! TL  Output
    END DO


    ! Perform the interpolation
    ! ...Vertical polarisation
    z => LUT%ev(iVar%ei(1)%i1:iVar%ei(1)%i2, &
                iVar%ei(2)%i1:iVar%ei(2)%i2, &
                iVar%ei(3)%i1:iVar%ei(3)%i2, &
                iVar%ei(4)%i1:iVar%ei(4)%i2  )
    CALL interp_4D_TL( z   , iVar%ei(1)%lp, iVar%ei(2)%lp, iVar%ei(3)%lp, iVar%ei(4)%lp, &
                       z_TL,   ei_TL(1)%lp,   ei_TL(2)%lp,   ei_TL(3)%lp,   ei_TL(4)%lp, &
                       ev_TL )
    ! ...Horizontal polarisation
    z => LUT%eh(iVar%ei(1)%i1:iVar%ei(1)%i2, &
                iVar%ei(2)%i1:iVar%ei(2)%i2, &
                iVar%ei(3)%i1:iVar%ei(3)%i2, &
                iVar%ei(4)%i1:iVar%ei(4)%i2  )
    CALL interp_4D_TL( z   , iVar%ei(1)%lp, iVar%ei(2)%lp, iVar%ei(3)%lp, iVar%ei(4)%lp, &
                       z_TL,   ei_TL(1)%lp,   ei_TL(2)%lp,   ei_TL(3)%lp,   ei_TL(4)%lp, &
                       eh_TL )
    z => NULL()

  END SUBROUTINE Retrieve_Emissivity_TL


  ! Adjoint model
  SUBROUTINE Retrieve_Emissivity_AD( &
    LUT           , &  ! Input
    ev_AD         , &  ! Input
    eh_AD         , &  ! Input
    Temperature_AD, &  ! Output
    Wind_Speed_AD , &  ! Output
    iVar            )  ! Internal variable input
    ! Arguments
    TYPE(MWwaterLUT_type), TARGET, INTENT(IN)     :: LUT
    REAL(fp),                      INTENT(IN OUT) :: ev_AD
    REAL(fp),                      INTENT(IN OUT) :: eh_AD
    REAL(fp),                      INTENT(IN OUT) :: Temperature_AD
    REAL(fp),                      INTENT(IN OUT) :: Wind_Speed_AD
    TYPE(iVar_type),               INTENT(IN)     :: iVar
    ! Local variables
    INTEGER :: i
    TYPE(eiInterp_type) :: ei_AD(N_LUTDIMS)
    REAL(fp), POINTER :: z(:,:,:,:) => NULL()
    REAL(fp) :: z_AD(NPTS,NPTS,NPTS,NPTS)

  END SUBROUTINE Retrieve_Emissivity_AD

END MODULE Retrieve_Emissivity_Module
