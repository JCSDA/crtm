!
! CRTM_Interpolation
!
! Module containing the interpolation routines used in the CRTM
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Feb-2007
!                       paul.vandelst@ssec.wisc.edu
!
MODULE CRTM_Interpolation

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds, ONLY: fp
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Parameters
  PUBLIC :: ORDER
  PUBLIC :: NPTS
  ! Derived types and associated procedures
  PUBLIC :: LPoly_type
  PUBLIC :: Clear_LPoly
!  PUBLIC :: Set_LPoly
!  PUBLIC :: Get_LPoly
  ! Procedures
  PUBLIC :: Interp_1D
  PUBLIC :: Interp_2D
  PUBLIC :: Interp_3D
  PUBLIC :: Interp_1D_TL
  PUBLIC :: Interp_2D_TL
  PUBLIC :: Interp_3D_TL
  PUBLIC :: Interp_1D_AD
  PUBLIC :: Interp_2D_AD
  PUBLIC :: Interp_3D_AD
  PUBLIC :: Find_Index
  PUBLIC :: LPoly
  PUBLIC :: LPoly_TL
  PUBLIC :: LPoly_AD


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE Find_Index
    MODULE PROCEDURE Find_Regular_Index
    MODULE PROCEDURE Find_Random_Index
  END INTERFACE Find_Index


  ! -----------------  
  ! Module parameters
  ! -----------------  
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID=&
  '$Id$'
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  INTEGER,  PARAMETER :: ORDER     = 2            ! Quadratic
  INTEGER,  PARAMETER :: NPOLY_PTS = ORDER+1      ! No. of points in each polynomial
  INTEGER,  PARAMETER :: NPTS      = NPOLY_PTS+1  ! No. of points total


  ! -----------------------
  ! Derived type definition
  ! -----------------------
  TYPE :: LPoly_type
!    PRIVATE
    INTEGER :: Order=ORDER
    INTEGER :: nPts =NPOLY_PTS
    ! Left and right side polynomials
    REAL(fp) :: lp_left(NPOLY_PTS)  = ZERO
    REAL(fp) :: lp_right(NPOLY_PTS) = ZERO
    ! Left and right side weighting factors
    REAL(fp) :: w_left  = ZERO
    REAL(fp) :: w_right = ZERO
    ! Polynomial numerator differences
    REAL(fp) :: dxi1 = ZERO
    REAL(fp) :: dxi2 = ZERO
    REAL(fp) :: dxi3 = ZERO
    REAL(fp) :: dxi4 = ZERO
    ! Polynomial denominator differences
    REAL(fp) :: dx12 = ZERO
    REAL(fp) :: dx13 = ZERO
    REAL(fp) :: dx23 = ZERO
    REAL(fp) :: dx24 = ZERO
    REAL(fp) :: dx34 = ZERO
  END TYPE LPoly_type


CONTAINS


  ! -------------------------------------------
  ! Some utility routines to access the PRIVATE
  ! internals of the derived data type. These
  ! routines are only used in testing.
  ! -------------------------------------------
  SUBROUTINE Clear_LPoly(p)
    TYPE(LPoly_type), INTENT(OUT) :: p
    p%lp_left  = ZERO
    p%lp_right = ZERO
    p%w_left  = ZERO
    p%w_right = ZERO
    p%dxi1 = ZERO
    p%dxi2 = ZERO
    p%dxi3 = ZERO
    p%dxi4 = ZERO
    p%dx12 = ZERO
    p%dx13 = ZERO
    p%dx23 = ZERO
    p%dx24 = ZERO
    p%dx34 = ZERO
  END SUBROUTINE Clear_LPoly
  
  
  ! ------------------------------------
  ! Forward model interpolation routines
  ! ------------------------------------
  ! 1-D routine
  SUBROUTINE Interp_1D(y, xlp, &  ! Input
                       y_int   )  ! Output
    ! Arguments
    REAL(fp),         INTENT(IN)  :: y(:)
    TYPE(LPoly_type), INTENT(IN)  :: xlp
    REAL(fp),         INTENT(OUT) :: y_int
    ! Local variables
    INTEGER :: i
    ! Perform interpolation
    y_int = ZERO
    DO i = 1, NPOLY_PTS
      y_int = y_int + (xlp%w_left *xlp%lp_left(i) *y(i)  ) + &
                      (xlp%w_right*xlp%lp_right(i)*y(i+1))
    END DO
  END SUBROUTINE Interp_1D

  ! 2-D routine
  SUBROUTINE Interp_2D(z, xlp, ylp, &  ! Input
                       z_int        )  ! Output
    ! Arguments
    REAL(fp),         INTENT(IN)  :: z(:,:)
    TYPE(LPoly_type), INTENT(IN)  :: xlp, ylp
    REAL(fp),         INTENT(OUT) :: z_int
    ! Local variables
    INTEGER  :: i
    REAL(fp) :: a(NPTS)
    ! Interpolate z in x dimension for all y
    DO i = 1, NPTS
      CALL Interp_1D(z(:,i),xlp,a(i))
    END DO
    ! Interpolate z in y dimension
    CALL Interp_1D(a,ylp,z_int)
  END SUBROUTINE Interp_2D

  ! 3-D routine
  SUBROUTINE Interp_3D(z, wlp, xlp, ylp, &  ! Input
                       z_int             )  ! Output
    ! Arguments
    REAL(fp),         INTENT(IN)  :: z(:,:,:)
    TYPE(LPoly_type), INTENT(IN)  :: wlp, xlp, ylp
    REAL(fp),         INTENT(OUT) :: z_int
    ! Local variables
    INTEGER  :: i
    REAL(fp) :: a(NPTS)
    ! Interpolate z in w,x dimension for all y
    DO i = 1, NPTS
      CALL Interp_2D(z(:,:,i),wlp,xlp,a(i))
    END DO
    ! Interpolate a in y dimension
    CALL Interp_1D(a,ylp,z_int)
  END SUBROUTINE Interp_3D


  ! -------------------------------------------
  ! Tangent-linear model interpolation routines
  ! -------------------------------------------
  ! 1-D routine
  SUBROUTINE Interp_1D_TL( y   , xlp   , &  ! FWD Input
                           y_TL, xlp_TL, &  ! TL  Input
                           y_int_TL      )  ! TL  Output
    ! Arguments
    REAL(fp),         INTENT(IN) :: y(:)
    TYPE(LPoly_type), INTENT(IN) :: xlp
    REAL(fp),         INTENT(IN) :: y_TL(:)
    TYPE(LPoly_type), INTENT(IN) :: xlp_TL
    REAL(fp),         INTENT(OUT):: y_int_TL
    ! Local variables
    INTEGER  :: i
    ! Perform TL interpolation
    y_int_TL = ZERO
    DO i = 1, NPOLY_PTS
      y_int_TL = y_int_TL + ( xlp%w_left    * xlp%lp_left(i)    * y_TL(i) ) + &
                            ( xlp%w_left    * xlp_TL%lp_left(i) * y(i)    ) + &
                            ( xlp_TL%w_left * xlp%lp_left(i)    * y(i)    ) + &
                            ( xlp%w_right    * xlp%lp_right(i)    * y_TL(i+1) ) + &
                            ( xlp%w_right    * xlp_TL%lp_right(i) * y(i+1)    ) + &
                            ( xlp_TL%w_right * xlp%lp_right(i)    * y(i+1)    )
    END DO
  END SUBROUTINE Interp_1D_TL
  
  ! 2-D routine
  SUBROUTINE Interp_2D_TL( z,    xlp   , ylp   , &  ! FWD Input
                           z_TL, xlp_TL, ylp_TL, &  ! TL  Input
                           z_int_TL              )  ! TL  Output
    REAL(fp),         INTENT(IN)  :: z(:,:)
    TYPE(LPoly_type), INTENT(IN)  :: xlp, ylp
    REAL(fp),         INTENT(IN)  :: z_TL(:,:)
    TYPE(LPoly_type), INTENT(IN)  :: xlp_TL, ylp_TL
    REAL(fp),         INTENT(OUT) :: z_int_TL
    ! Local variables
    INTEGER  :: i
    REAL(fp) :: a(NPTS), a_TL(NPTS)
    ! Interpolate z in x dimension for all y
    DO i = 1, NPTS
      CALL Interp_1D(z(:,i),xlp,a(i))
      CALL Interp_1D_TL(z(:,i),xlp,z_TL(:,i),xlp_TL,a_TL(i))
    END DO
    ! Interpolate z in y dimension
    CALL Interp_1D_TL(a,ylp,a_TL,ylp_TL,z_int_TL)
  END SUBROUTINE Interp_2D_TL
  
  ! 3-D routine
  SUBROUTINE Interp_3D_TL( z   , wlp   , xlp   , ylp   , &  ! FWD Input
                           z_TL, wlp_TL, xlp_TL, ylp_TL, &  ! TL  Input
                           z_int_TL                      )  ! TL  Output
    ! Arguments
    REAL(fp),         INTENT(IN)  :: z(:,:,:)
    TYPE(LPoly_type), INTENT(IN)  :: wlp, xlp, ylp
    REAL(fp),         INTENT(IN)  :: z_TL(:,:,:)
    TYPE(LPoly_type), INTENT(IN)  :: wlp_TL, xlp_TL, ylp_TL
    REAL(fp),         INTENT(OUT) :: z_int_TL
    ! Local variables
    INTEGER  :: i
    REAL(fp) :: a(NPTS), a_TL(NPTS)
    ! Interpolate z in w,x dimension for all y
    DO i = 1, NPTS
      CALL Interp_2D(z(:,:,i),wlp,xlp,a(i))
      CALL Interp_2D_TL(z(:,:,i),wlp,xlp,z_TL(:,:,i),wlp_TL,xlp_TL,a_TL(i))
    END DO
    ! Interpolate a in y dimension
    CALL Interp_1D_TL(a,ylp,a_TL,ylp_TL,z_int_TL)
  END SUBROUTINE Interp_3D_TL
  

  ! ------------------------------------
  ! Adjoint model interpolation routines
  ! ------------------------------------
  ! 1-D routine
  SUBROUTINE Interp_1D_AD( y   , xlp   , &  ! FWD Input
                           y_int_AD    , &  ! AD  Input
                           y_AD, xlp_AD  )  ! AD  Output
    ! Arguments
    REAL(fp),         INTENT(IN)     :: y(:)
    TYPE(LPoly_type), INTENT(IN)     :: xlp
    REAL(fp),         INTENT(IN OUT) :: y_int_AD
    REAL(fp),         INTENT(IN OUT) :: y_AD(:)
    TYPE(LPoly_type), INTENT(IN OUT) :: xlp_AD
    ! Local variables
    INTEGER  :: i
    ! Perform adjoint interpolation
    DO i = 1, NPOLY_PTS
      ! "Right" side
      xlp_AD%w_right     = xlp_AD%w_right     + ( xlp%lp_right(i) * y(i+1)          * y_int_AD )
      xlp_AD%lp_right(i) = xlp_AD%lp_right(i) + ( xlp%w_right     * y(i+1)          * y_int_AD )
      y_AD(i+1)          = y_AD(i+1)          + ( xlp%w_right     * xlp%lp_right(i) * y_int_AD )
      ! "Left" side
      xlp_AD%w_left     = xlp_AD%w_left     + ( xlp%lp_left(i) * y(i)           * y_int_AD )
      xlp_AD%lp_left(i) = xlp_AD%lp_left(i) + ( xlp%w_left     * y(i)           * y_int_AD )
      y_AD(i)           = y_AD(i)           + ( xlp%w_left     * xlp%lp_left(i) * y_int_AD )
    END DO
  END SUBROUTINE Interp_1D_AD

  ! 2-D routine
  SUBROUTINE Interp_2D_AD( z   , xlp   , ylp   , &  ! FWD Input
                           z_int_AD            , &  ! AD  Input
                           z_AD, xlp_AD, ylp_AD  )  ! AD  Output
    ! Arguments
    REAL(fp),         INTENT(IN)     :: z(:,:)
    TYPE(LPoly_type), INTENT(IN)     :: xlp, ylp
    REAL(fp),         INTENT(IN OUT) :: z_int_AD
    REAL(fp),         INTENT(IN OUT) :: z_AD(:,:)
    TYPE(LPoly_type), INTENT(IN OUT) :: xlp_AD, ylp_AD
    ! Local variables
    INTEGER  :: i
    REAL(fp) :: a(NPTS), a_AD(NPTS)
    ! Forward calculations
    ! Interpolate z in x dimension for all y
    DO i = 1, NPTS
      CALL Interp_1D(z(:,i),xlp,a(i))
    END DO
    ! Adjoint calculations
    ! Initialize local AD variables
    a_AD = ZERO
    ! Adjoint of z interpolation in y dimension
    CALL Interp_1D_AD(a,ylp,z_int_AD,a_AD,ylp_AD)
    ! Adjoint of z interpolation in x dimension for all y
    DO i = 1, NPTS
      CALL Interp_1D_AD(z(:,i),xlp,a_AD(i),z_AD(:,i),xlp_AD)
    END DO
  END SUBROUTINE Interp_2D_AD


  ! 3-D routine
  SUBROUTINE Interp_3D_AD( z   , wlp   , xlp   , ylp   , &  ! FWD Input
                           z_int_AD                    , &  ! AD  Input
                           z_AD, wlp_AD, xlp_AD, ylp_AD  )  ! AD  Output
    ! Arguments
    REAL(fp),         INTENT(IN)     :: z(:,:,:)
    TYPE(LPoly_type), INTENT(IN)     :: wlp, xlp, ylp
    REAL(fp),         INTENT(IN OUT) :: z_int_AD
    REAL(fp),         INTENT(IN OUT) :: z_AD(:,:,:)
    TYPE(LPoly_type), INTENT(IN OUT) :: wlp_AD, xlp_AD, ylp_AD
    ! Local variables
    INTEGER  :: i
    REAL(fp) :: a(NPTS), a_AD(NPTS)

    ! Forward calculations
    ! Interpolate z in w and x dimension for all y
    DO i = 1, NPTS
      CALL Interp_2D(z(:,:,i),wlp,xlp,a(i))
    END DO
    
    ! Adjoint calculations
    ! Initialize local AD variables
    a_AD = ZERO
    ! Adjoint of a interpolation in y dimension
    CALL Interp_1D_AD(a,ylp,z_int_AD,a_AD,ylp_AD)
    ! Adjoint of z interpolation in w and x dimension for all y
    DO i = 1, NPTS
      CALL Interp_2D_AD(z(:,:,i),wlp,xlp,a_AD(i),z_AD(:,:,i),wlp_AD,xlp_AD)
    END DO
  END SUBROUTINE Interp_3D_AD
       
       
  ! --------------------
  ! Indexing subroutines
  ! --------------------
  ! Find lower index for regular spacing
  SUBROUTINE Find_Regular_Index(x, dx, x_int, i1, i2)
    REAL(fp), INTENT(IN)  :: x(:)
    REAL(fp), INTENT(IN)  :: dx, x_int
    INTEGER , INTENT(OUT) :: i1, i2
    INTEGER :: n
    n = SIZE(x)
    i1 = FLOOR((x_int-x(1))/dx)+1-(NPOLY_PTS/2)
    i1 = MIN(MAX(i1,1),n-NPOLY_PTS)
    i2 = i1 + NPOLY_PTS
  END SUBROUTINE Find_Regular_Index
  
  ! Find lower index for random spacing.
  ! Assumption is that x(1) <= xInt <= x(n)
  ! (despite the MIN/MAX test)
  SUBROUTINE Find_Random_Index(x, x_int, i1, i2)
    REAL(fp), INTENT(IN)  :: x(:)
    REAL(fp), INTENT(IN)  :: x_int
    INTEGER , INTENT(OUT) :: i1, i2
    INTEGER :: k, n
    n = SIZE(x)
    DO k=1,n
      IF (x_int <= x(k) ) EXIT
    END DO
    i1 = MIN(MAX(1,k-1-(NPOLY_PTS/2)),n-NPOLY_PTS)
    i2 = i1 + NPOLY_PTS
  END SUBROUTINE Find_Random_Index


  ! --------------------
  ! Polynomial functions
  ! --------------------
  ! Forward model
  SUBROUTINE LPoly(x, x_int, p)
    REAL(fp),         INTENT(IN)  :: x(:)  ! Input
    REAL(fp),         INTENT(IN)  :: x_int ! Input
    TYPE(LPoly_type), INTENT(OUT) :: p     ! Input
    ! Compute the differences
    CALL Compute_dx(x,p)
    CALL Compute_dxi(x,x_int,p)
    ! "Left" side quadratic
    p%lp_left(1)  = p%dxi2 * p%dxi3 / ( p%dx12 * p%dx13 )
    p%lp_left(2)  = p%dxi1 * p%dxi3 / (-p%dx12 * p%dx23 )
    p%lp_left(3)  = p%dxi1 * p%dxi2 / ( p%dx13 * p%dx23 )
    ! "Right" side quadratic
    p%lp_right(1) = p%dxi3 * p%dxi4 / ( p%dx23 * p%dx24 )
    p%lp_right(2) = p%dxi2 * p%dxi4 / (-p%dx23 * p%dx34 )
    p%lp_right(3) = p%dxi2 * p%dxi3 / ( p%dx24 * p%dx34 )
    ! Polynomial weights
    IF ( x_int < x(2) ) THEN
      p%w_right = ZERO
      p%w_left  = ONE
    ELSE IF ( x_int > x(3) ) THEN
      p%w_right = ONE
      p%w_left  = ZERO
    ELSE
      p%w_right = p%dxi2 / (-p%dx23)
      p%w_left  = ONE - p%w_right
    END IF
  END SUBROUTINE LPoly
  
  ! Tangent-linear model
  SUBROUTINE LPoly_TL(x   , x_int   , p , &
                      x_TL, x_int_TL, p_TL)
    REAL(fp),         INTENT(IN)  :: x(:)      ! FWD Input
    REAL(fp),         INTENT(IN)  :: x_int     ! FWD Input
    TYPE(LPoly_type), INTENT(IN)  :: p         ! FWD Input
    REAL(fp),         INTENT(IN)  :: x_TL(:)   ! TL  Input
    REAL(fp),         INTENT(IN)  :: x_int_TL  ! TL  Input
    TYPE(LPoly_type), INTENT(OUT) :: p_TL      ! TL  Output
    ! Compute the tangent-linear differences
    CALL Compute_dx_TL(x_TL,p_TL)
    CALL Compute_dxi_TL(x_TL,x_int_TL,p_TL)
    ! "Left" side quadratic
    p_TL%lp_left(1) = p%lp_left(1) * ( (p_TL%dxi2 / p%dxi2) + &
                                       (p_TL%dxi3 / p%dxi3) - &
                                       (p_TL%dx12 / p%dx12) - &
                                       (p_TL%dx13 / p%dx13)   )

    p_TL%lp_left(2) = p%lp_left(2) * ( (p_TL%dxi1 / p%dxi1) + &
                                       (p_TL%dxi3 / p%dxi3) - &
                                       (p_TL%dx12 / p%dx12) - &
                                       (p_TL%dx23 / p%dx23)   )

    p_TL%lp_left(3) = p%lp_left(3) * ( (p_TL%dxi1 / p%dxi1) + &
                                       (p_TL%dxi2 / p%dxi2) - &
                                       (p_TL%dx13 / p%dx13) - &
                                       (p_TL%dx23 / p%dx23)   )
    ! "Right" side quadratic
    p_TL%lp_right(1) = p%lp_right(1) * ( (p_TL%dxi3 / p%dxi3) + &
                                         (p_TL%dxi4 / p%dxi4) - &
                                         (p_TL%dx23 / p%dx23) - &
                                         (p_TL%dx24 / p%dx24)   )

    p_TL%lp_right(2) = p%lp_right(2) * ( (p_TL%dxi2 / p%dxi2) + &
                                         (p_TL%dxi4 / p%dxi4) - &
                                         (p_TL%dx23 / p%dx23) - &
                                         (p_TL%dx34 / p%dx34)   )

    p_TL%lp_right(3) = p%lp_right(3) * ( (p_TL%dxi2 / p%dxi2) + &
                                         (p_TL%dxi3 / p%dxi3) - &
                                         (p_TL%dx24 / p%dx24) - &
                                         (p_TL%dx34 / p%dx34)   )
    ! Polynomial weights
    IF ( x_int < x(2) .OR. x_int > x(3) ) THEN
      p_TL%w_right = ZERO
      p_TL%w_left  = ZERO
    ELSE
      p_TL%w_right = p%w_right * ( (p_TL%dxi2/p%dxi2) - (p_TL%dx23/p%dx23) )
      p_TL%w_left  = -p_TL%w_right
    END IF
  END SUBROUTINE LPoly_TL
  
  ! Adjoint model
  SUBROUTINE LPoly_AD(x   , x_int   , p , &
                      p_AD, x_AD, x_int_AD)
    REAL(fp),         INTENT(IN)     :: x(:)      ! FWD Input
    REAL(fp),         INTENT(IN)     :: x_int     ! FWD Input
    TYPE(LPoly_type), INTENT(IN)     :: p         ! FWD Input
    TYPE(LPoly_type), INTENT(IN OUT) :: p_AD      ! AD  Input
    REAL(fp),         INTENT(IN OUT) :: x_AD(:)   ! AD  Output
    REAL(fp),         INTENT(IN OUT) :: x_int_AD  ! AD  Output
    ! Polynomial weights
    IF ( x_int < x(2) .OR. x_int > x(3) ) THEN
      p_AD%w_right = ZERO
      p_AD%w_left  = ZERO
    ELSE
      p_AD%w_right = p_AD%w_right - p_AD%w_left
      p_AD%w_left  = ZERO
      p_AD%dx23 = p_AD%dx23 - ( p%w_right*p_AD%w_right/p%dx23 )
      p_AD%dxi2 = p_AD%dxi2 + ( p%w_right*p_AD%w_right/p%dxi2 )
      p_AD%w_right = ZERO
    END IF

    ! "Right" side quadratic
    p_AD%dxi2 = p_AD%dxi2 + ( p%lp_right(3)*p_AD%lp_right(3)/p%dxi2 )
    p_AD%dxi3 = p_AD%dxi3 + ( p%lp_right(3)*p_AD%lp_right(3)/p%dxi3 )
    p_AD%dx24 = p_AD%dx24 - ( p%lp_right(3)*p_AD%lp_right(3)/p%dx24 )
    p_AD%dx34 = p_AD%dx34 - ( p%lp_right(3)*p_AD%lp_right(3)/p%dx34 )
    p_AD%lp_right(3) = ZERO

    p_AD%dxi2 = p_AD%dxi2 + ( p%lp_right(2)*p_AD%lp_right(2)/p%dxi2 )
    p_AD%dxi4 = p_AD%dxi4 + ( p%lp_right(2)*p_AD%lp_right(2)/p%dxi4 )
    p_AD%dx23 = p_AD%dx23 - ( p%lp_right(2)*p_AD%lp_right(2)/p%dx23 )
    p_AD%dx34 = p_AD%dx34 - ( p%lp_right(2)*p_AD%lp_right(2)/p%dx34 )
    p_AD%lp_right(2) = ZERO

    p_AD%dxi3 = p_AD%dxi3 + ( p%lp_right(1)*p_AD%lp_right(1)/p%dxi3 )
    p_AD%dxi4 = p_AD%dxi4 + ( p%lp_right(1)*p_AD%lp_right(1)/p%dxi4 )
    p_AD%dx23 = p_AD%dx23 - ( p%lp_right(1)*p_AD%lp_right(1)/p%dx23 )
    p_AD%dx24 = p_AD%dx24 - ( p%lp_right(1)*p_AD%lp_right(1)/p%dx24 )
    p_AD%lp_right(1) = ZERO

    ! "Left" side quadratic
    p_AD%dxi1 = p_AD%dxi1 + ( p%lp_left(3)*p_AD%lp_left(3)/p%dxi1 )
    p_AD%dxi2 = p_AD%dxi2 + ( p%lp_left(3)*p_AD%lp_left(3)/p%dxi2 )
    p_AD%dx13 = p_AD%dx13 - ( p%lp_left(3)*p_AD%lp_left(3)/p%dx13 )
    p_AD%dx23 = p_AD%dx23 - ( p%lp_left(3)*p_AD%lp_left(3)/p%dx23 )
    p_AD%lp_left(3) = ZERO
    
    p_AD%dxi1 = p_AD%dxi1 + ( p%lp_left(2)*p_AD%lp_left(2)/p%dxi1 )
    p_AD%dxi3 = p_AD%dxi3 + ( p%lp_left(2)*p_AD%lp_left(2)/p%dxi3 )
    p_AD%dx12 = p_AD%dx12 - ( p%lp_left(2)*p_AD%lp_left(2)/p%dx12 )
    p_AD%dx23 = p_AD%dx23 - ( p%lp_left(2)*p_AD%lp_left(2)/p%dx23 )
    p_AD%lp_left(2) = ZERO
    
    p_AD%dxi2 = p_AD%dxi2 + ( p%lp_left(1)*p_AD%lp_left(1)/p%dxi2 )
    p_AD%dxi3 = p_AD%dxi3 + ( p%lp_left(1)*p_AD%lp_left(1)/p%dxi3 )
    p_AD%dx12 = p_AD%dx12 - ( p%lp_left(1)*p_AD%lp_left(1)/p%dx12 )
    p_AD%dx13 = p_AD%dx13 - ( p%lp_left(1)*p_AD%lp_left(1)/p%dx13 )
    p_AD%lp_left(1) = ZERO

    ! Compute the adjoint differences
    CALL Compute_dxi_AD(p_AD,x_AD,x_int_AD)
    CALL Compute_dx_AD(p_AD,x_AD)
  END SUBROUTINE LPoly_AD
  
  
  
  ! -------------------------------------------------------------
  ! Subroutines to compute the polynomial denominator differences
  ! -------------------------------------------------------------
  ! Forward model
  SUBROUTINE Compute_dx(x,p)
    REAL(fp),         INTENT(IN)     :: x(:)  ! Input
    TYPE(LPoly_type), INTENT(IN OUT) :: p     ! Output
    p%dx12 = x(1)-x(2)
    p%dx13 = x(1)-x(3)
    p%dx23 = x(2)-x(3)
    p%dx24 = x(2)-x(4)
    p%dx34 = x(3)-x(4)
  END SUBROUTINE Compute_dx
  
  ! Tangent-linear model
  SUBROUTINE Compute_dx_TL(x_TL,p_TL)
    REAL(fp),         INTENT(IN)     :: x_TL(:)  ! TL Input
    TYPE(LPoly_type), INTENT(IN OUT) :: p_TL     ! TL Output
    p_TL%dx12 = x_TL(1)-x_TL(2)
    p_TL%dx13 = x_TL(1)-x_TL(3)
    p_TL%dx23 = x_TL(2)-x_TL(3)
    p_TL%dx24 = x_TL(2)-x_TL(4)
    p_TL%dx34 = x_TL(3)-x_TL(4)
  END SUBROUTINE Compute_dx_TL
  
  ! Adjoint model
  SUBROUTINE Compute_dx_AD(p_AD,x_AD)
    TYPE(LPoly_type), INTENT(IN OUT) :: p_AD     ! AD Input
    REAL(fp),         INTENT(IN OUT) :: x_AD(:)  ! AD Output
    x_AD(2) = x_AD(2) - p_AD%dx12
    x_AD(1) = x_AD(1) + p_AD%dx12
    p_AD%dx12 = ZERO
    x_AD(3) = x_AD(3) - p_AD%dx13
    x_AD(1) = x_AD(1) + p_AD%dx13
    p_AD%dx13 = ZERO
    x_AD(3) = x_AD(3) - p_AD%dx23
    x_AD(2) = x_AD(2) + p_AD%dx23
    p_AD%dx23 = ZERO
    x_AD(4) = x_AD(4) - p_AD%dx24
    x_AD(2) = x_AD(2) + p_AD%dx24
    p_AD%dx24 = ZERO
    x_AD(4) = x_AD(4) - p_AD%dx34
    x_AD(3) = x_AD(3) + p_AD%dx34
    p_AD%dx34 = ZERO
  END SUBROUTINE Compute_dx_AD


  ! -----------------------------------------------------------
  ! Subroutines to compute the polynomial numerator differences
  ! -----------------------------------------------------------
  ! Forward model
  SUBROUTINE Compute_dxi(x,x_int,p)
    REAL(fp),         INTENT(IN)     :: x(:)   ! Input
    REAL(fp),         INTENT(IN)     :: x_int  ! Input
    TYPE(LPoly_type), INTENT(IN OUT) :: p      ! Output
    p%dxi1 = x_int-x(1)
    p%dxi2 = x_int-x(2)
    p%dxi3 = x_int-x(3)
    p%dxi4 = x_int-x(4)
  END SUBROUTINE Compute_dxi
  
  ! Tangent-linear model
  SUBROUTINE Compute_dxi_TL(x_TL,x_int_TL,p_TL)
    REAL(fp),         INTENT(IN)     :: x_TL(:)   ! TL Input
    REAL(fp),         INTENT(IN)     :: x_int_TL  ! TL Input
    TYPE(LPoly_type), INTENT(IN OUT) :: p_TL      ! TL Output
    p_TL%dxi1 = x_int_TL-x_TL(1)
    p_TL%dxi2 = x_int_TL-x_TL(2)
    p_TL%dxi3 = x_int_TL-x_TL(3)
    p_TL%dxi4 = x_int_TL-x_TL(4)
  END SUBROUTINE Compute_dxi_TL
  
  ! Adjoint model
  SUBROUTINE Compute_dxi_AD(p_AD,x_AD,x_int_AD)
    TYPE(LPoly_type), INTENT(IN OUT) :: p_AD      ! AD Input
    REAL(fp),         INTENT(IN OUT) :: x_AD(:)   ! AD Output
    REAL(fp),         INTENT(IN OUT) :: x_int_AD  ! AD Output
    x_AD(1)  = x_AD(1)  - p_AD%dxi1
    x_int_AD = x_int_AD + p_AD%dxi1
    p_AD%dxi1 = ZERO
    x_AD(2)  = x_AD(2)  - p_AD%dxi2
    x_int_AD = x_int_AD + p_AD%dxi2
    p_AD%dxi2 = ZERO
    x_AD(3)  = x_AD(3)  - p_AD%dxi3
    x_int_AD = x_int_AD + p_AD%dxi3
    p_AD%dxi3 = ZERO
    x_AD(4)  = x_AD(4)  - p_AD%dxi4
    x_int_AD = x_int_AD + p_AD%dxi4
    p_AD%dxi4 = ZERO
  END SUBROUTINE Compute_dxi_AD

END MODULE CRTM_Interpolation

