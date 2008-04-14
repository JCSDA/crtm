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
    REAL(fp) :: dxi_left(NPOLY_PTS)  = ZERO
    REAL(fp) :: dxi_right(NPOLY_PTS) = ZERO
    ! Polynomial denominator differences
    REAL(fp) :: dx_left(NPOLY_PTS)  = ZERO
    REAL(fp) :: dx_right(NPOLY_PTS) = ZERO
  END TYPE LPoly_type


CONTAINS


  ! -------------------------------------------
  ! Some utility routines to access the PRIVATE
  ! internals of the derived data type. These
  ! routines are only used in testing.
  ! -------------------------------------------
  SUBROUTINE Clear_LPoly(p)
    TYPE(LPoly_type), INTENT(IN OUT) :: p
    p%Order = ORDER
    p%nPts  = NPOLY_PTS
    p%lp_left   = ZERO
    p%lp_right  = ZERO
    p%w_left    = ZERO
    p%w_right   = ZERO
    p%dxi_left  = ZERO
    p%dxi_right = ZERO
    p%dx_left   = ZERO
    p%dx_right  = ZERO
  END SUBROUTINE Clear_LPoly
  
  
  ! ------------------------------------
  ! Forward model interpolation routines
  ! ------------------------------------
  ! 1-D routine
  SUBROUTINE Interp_1D(z, wlp, &  ! Input
                       z_int   )  ! Output
    ! Arguments
    REAL(fp),         INTENT(IN)  :: z(:)
    TYPE(LPoly_type), INTENT(IN)  :: wlp
    REAL(fp),         INTENT(IN OUT) :: z_int
!    REAL(fp),         INTENT(OUT) :: z_int
    ! Local variables
!    INTEGER :: i
!    ! Perform interpolation
!    z_int = ZERO
!    DO i = 1, NPOLY_PTS
!      z_int = z_int + (wlp%w_left *wlp%lp_left(i) *z(i)  ) + &
!                      (wlp%w_right*wlp%lp_right(i)*z(i+1))
!    END DO
!    z_int = ( wlp%w_left  * DOT_PRODUCT(wlp%lp_left ,z(1:3)) ) + &
!            ( wlp%w_right * DOT_PRODUCT(wlp%lp_right,z(2:4)) )
    z_int = ( wlp%w_left  * ( wlp%lp_left(1) *z(1) + &
                              wlp%lp_left(2) *z(2) + &
                              wlp%lp_left(3) *z(3) ) ) + &
            ( wlp%w_right * ( wlp%lp_right(1)*z(2) + &
                              wlp%lp_right(2)*z(3) + &
                              wlp%lp_right(3)*z(4) ) )
  END SUBROUTINE Interp_1D

  ! 2-D routine
  SUBROUTINE Interp_2D(z, wlp, xlp, &  ! Input
                       z_int        )  ! Output
    ! Arguments
    REAL(fp),         INTENT(IN)  :: z(:,:)
    TYPE(LPoly_type), INTENT(IN)  :: wlp, xlp
    REAL(fp),         INTENT(IN OUT) :: z_int
!    REAL(fp),         INTENT(OUT) :: z_int
    ! Local variables
    INTEGER  :: i
    REAL(fp) :: a(NPTS)
    ! Interpolate z in w dimension for all x
    DO i = 1, NPTS
      CALL Interp_1D(z(:,i),wlp,a(i))
    END DO
!    CALL Interp_1D(z(:,1),wlp,a(1))
!    CALL Interp_1D(z(:,2),wlp,a(2))
!    CALL Interp_1D(z(:,3),wlp,a(3))
!    CALL Interp_1D(z(:,4),wlp,a(4))
    ! Interpolate z in y dimension
    CALL Interp_1D(a,xlp,z_int)
  END SUBROUTINE Interp_2D

  ! 3-D routine
  SUBROUTINE Interp_3D(z, wlp, xlp, ylp, &  ! Input
                       z_int             )  ! Output
    ! Arguments
    REAL(fp),         INTENT(IN)  :: z(:,:,:)
    TYPE(LPoly_type), INTENT(IN)  :: wlp, xlp, ylp
    REAL(fp),         INTENT(IN OUT) :: z_int
!    REAL(fp),         INTENT(OUT) :: z_int
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
  SUBROUTINE Interp_1D_TL( x   , wlp   , &  ! FWD Input
                           x_TL, wlp_TL, &  ! TL  Input
                           x_int_TL      )  ! TL  Output
    ! Arguments
    REAL(fp),         INTENT(IN) :: x(:)
    TYPE(LPoly_type), INTENT(IN) :: wlp
    REAL(fp),         INTENT(IN) :: x_TL(:)
    TYPE(LPoly_type), INTENT(IN) :: wlp_TL
    REAL(fp),         INTENT(IN OUT):: x_int_TL
!    REAL(fp),         INTENT(OUT):: x_int_TL
    ! Local variables
!    INTEGER  :: i
!    ! Perform TL interpolation
!    x_int_TL = ZERO
!    DO i = 1, NPOLY_PTS
!      x_int_TL = x_int_TL + ( wlp%w_left    * wlp%lp_left(i)    * x_TL(i) ) + &
!                            ( wlp%w_left    * wlp_TL%lp_left(i) * x(i)    ) + &
!                            ( wlp_TL%w_left * wlp%lp_left(i)    * x(i)    ) + &
!                            ( wlp%w_right    * wlp%lp_right(i)    * x_TL(i+1) ) + &
!                            ( wlp%w_right    * wlp_TL%lp_right(i) * x(i+1)    ) + &
!                            ( wlp_TL%w_right * wlp%lp_right(i)    * x(i+1)    )
!    END DO

!    x_int_TL = wlp%w_left    * wlp%lp_left(1)    * x_TL(1) + &
!               wlp%w_left    * wlp_TL%lp_left(1) * x(1)    + &
!               wlp_TL%w_left * wlp%lp_left(1)    * x(1)    + &
!               wlp%w_left    * wlp%lp_left(2)    * x_TL(2) + &
!               wlp%w_left    * wlp_TL%lp_left(2) * x(2)    + &
!               wlp_TL%w_left * wlp%lp_left(2)    * x(2)    + &
!               wlp%w_left    * wlp%lp_left(3)    * x_TL(3) + &
!               wlp%w_left    * wlp_TL%lp_left(3) * x(3)    + &
!               wlp_TL%w_left * wlp%lp_left(3)    * x(3)    + &
!               
!               wlp%w_right    * wlp%lp_right(1)    * x_TL(2) + &
!               wlp%w_right    * wlp_TL%lp_right(1) * x(2)    + &
!               wlp_TL%w_right * wlp%lp_right(1)    * x(2)    + &
!               wlp%w_right    * wlp%lp_right(2)    * x_TL(3) + &
!               wlp%w_right    * wlp_TL%lp_right(2) * x(3)    + &
!               wlp_TL%w_right * wlp%lp_right(2)    * x(3)    + &
!               wlp%w_right    * wlp%lp_right(3)    * x_TL(4) + &
!               wlp%w_right    * wlp_TL%lp_right(3) * x(4)    + &
!               wlp_TL%w_right * wlp%lp_right(3)    * x(4)

    x_int_TL = ( wlp%w_left    * ( wlp%lp_left(1)    * x_TL(1) + &
                                   wlp_TL%lp_left(1) * x(1)    + &
                                   wlp%lp_left(2)    * x_TL(2) + &
                                   wlp_TL%lp_left(2) * x(2)    + &
                                   wlp%lp_left(3)    * x_TL(3) + &
                                   wlp_TL%lp_left(3) * x(3)    ) ) + &
               ( wlp_TL%w_left * ( wlp%lp_left(1)    * x(1)    + &
                                   wlp%lp_left(2)    * x(2)    + &
                                   wlp%lp_left(3)    * x(3)    ) ) + &
               
               ( wlp%w_right    * ( wlp%lp_right(1)    * x_TL(2) + &
                                    wlp_TL%lp_right(1) * x(2)    + &
                                    wlp%lp_right(2)    * x_TL(3) + &
                                    wlp_TL%lp_right(2) * x(3)    + &
                                    wlp%lp_right(3)    * x_TL(4) + &
                                    wlp_TL%lp_right(3) * x(4)    ) ) + &
               ( wlp_TL%w_right * ( wlp%lp_right(1)    * x(2)    + &
                                    wlp%lp_right(2)    * x(3)    + &
                                    wlp%lp_right(3)    * x(4)    ) )

  END SUBROUTINE Interp_1D_TL
  
  ! 2-D routine
  SUBROUTINE Interp_2D_TL( z,    wlp   , xlp   , &  ! FWD Input
                           z_TL, wlp_TL, xlp_TL, &  ! TL  Input
                           z_int_TL              )  ! TL  Output
    REAL(fp),         INTENT(IN)  :: z(:,:)
    TYPE(LPoly_type), INTENT(IN)  :: wlp, xlp
    REAL(fp),         INTENT(IN)  :: z_TL(:,:)
    TYPE(LPoly_type), INTENT(IN)  :: wlp_TL, xlp_TL
    REAL(fp),         INTENT(IN OUT) :: z_int_TL
!    REAL(fp),         INTENT(OUT) :: z_int_TL
    ! Local variables
    INTEGER  :: i
    REAL(fp) :: a(NPTS), a_TL(NPTS)
    ! Interpolate z in x dimension for all y
    DO i = 1, NPTS
      CALL Interp_1D(z(:,i),wlp,a(i))
      CALL Interp_1D_TL(z(:,i),wlp,z_TL(:,i),wlp_TL,a_TL(i))
    END DO
    ! Interpolate z in y dimension
    CALL Interp_1D_TL(a,xlp,a_TL,xlp_TL,z_int_TL)
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
    REAL(fp),         INTENT(IN OUT) :: z_int_TL
!    REAL(fp),         INTENT(OUT) :: z_int_TL
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
    REAL(fp) :: wl_y_int_AD, wr_y_int_AD
!    INTEGER  :: i
!    ! Perform adjoint interpolation
!    DO i = 1, NPOLY_PTS
!      ! "Right" side
!      xlp_AD%w_right     = xlp_AD%w_right     + ( xlp%lp_right(i) * y(i+1)          * y_int_AD )
!      xlp_AD%lp_right(i) = xlp_AD%lp_right(i) + ( xlp%w_right     * y(i+1)          * y_int_AD )
!      y_AD(i+1)          = y_AD(i+1)          + ( xlp%w_right     * xlp%lp_right(i) * y_int_AD )
!      ! "Left" side
!      xlp_AD%w_left     = xlp_AD%w_left     + ( xlp%lp_left(i) * y(i)           * y_int_AD )
!      xlp_AD%lp_left(i) = xlp_AD%lp_left(i) + ( xlp%w_left     * y(i)           * y_int_AD )
!      y_AD(i)           = y_AD(i)           + ( xlp%w_left     * xlp%lp_left(i) * y_int_AD )
!    END DO
    
!    ! "Right" side
!    xlp_AD%w_right     = xlp_AD%w_right     + ( xlp%lp_right(1) * y(2)            * y_int_AD )
!    xlp_AD%lp_right(1) = xlp_AD%lp_right(1) + ( xlp%w_right     * y(2)            * y_int_AD )
!    y_AD(2)            = y_AD(2)            + ( xlp%w_right     * xlp%lp_right(1) * y_int_AD )
!    ! "Left" side
!    xlp_AD%w_left     = xlp_AD%w_left     + ( xlp%lp_left(1) * y(1)           * y_int_AD )
!    xlp_AD%lp_left(1) = xlp_AD%lp_left(1) + ( xlp%w_left     * y(1)           * y_int_AD )
!    y_AD(1)           = y_AD(1)           + ( xlp%w_left     * xlp%lp_left(1) * y_int_AD )
!
!
!    ! "Right" side
!    xlp_AD%w_right     = xlp_AD%w_right     + ( xlp%lp_right(2) * y(3)            * y_int_AD )
!    xlp_AD%lp_right(2) = xlp_AD%lp_right(2) + ( xlp%w_right     * y(3)            * y_int_AD )
!    y_AD(3)            = y_AD(3)            + ( xlp%w_right     * xlp%lp_right(2) * y_int_AD )
!    ! "Left" side
!    xlp_AD%w_left     = xlp_AD%w_left     + ( xlp%lp_left(2) * y(2)           * y_int_AD )
!    xlp_AD%lp_left(2) = xlp_AD%lp_left(2) + ( xlp%w_left     * y(2)           * y_int_AD )
!    y_AD(2)           = y_AD(2)           + ( xlp%w_left     * xlp%lp_left(2) * y_int_AD )
!
!    ! "Right" side
!    xlp_AD%w_right     = xlp_AD%w_right     + ( xlp%lp_right(3) * y(4)            * y_int_AD )
!    xlp_AD%lp_right(3) = xlp_AD%lp_right(3) + ( xlp%w_right     * y(4)            * y_int_AD )
!    y_AD(4)            = y_AD(4)            + ( xlp%w_right     * xlp%lp_right(3) * y_int_AD )
!    ! "Left" side
!    xlp_AD%w_left     = xlp_AD%w_left     + ( xlp%lp_left(3) * y(3)           * y_int_AD )
!    xlp_AD%lp_left(3) = xlp_AD%lp_left(3) + ( xlp%w_left     * y(3)           * y_int_AD )
!    y_AD(3)           = y_AD(3)           + ( xlp%w_left     * xlp%lp_left(3) * y_int_AD )
!

    xlp_AD%w_right     = xlp_AD%w_right + &
                         ( y_int_AD * ( ( xlp%lp_right(1) * y(2) ) + &
                                        ( xlp%lp_right(2) * y(3) ) + &
                                        ( xlp%lp_right(3) * y(4) ) ) )

    xlp_AD%w_left     = xlp_AD%w_left + &
                        ( y_int_AD * ( ( xlp%lp_left(1) * y(1) ) + &
                                       ( xlp%lp_left(2) * y(2) ) + &
                                       ( xlp%lp_left(3) * y(3) ) ) )

    wr_y_int_AD = xlp%w_right * y_int_AD
    xlp_AD%lp_right(1) = xlp_AD%lp_right(1) + ( wr_y_int_AD * y(2) )
    xlp_AD%lp_right(2) = xlp_AD%lp_right(2) + ( wr_y_int_AD * y(3) )
    xlp_AD%lp_right(3) = xlp_AD%lp_right(3) + ( wr_y_int_AD * y(4) )

    wl_y_int_AD = xlp%w_left * y_int_AD
    xlp_AD%lp_left(1) = xlp_AD%lp_left(1) + ( wl_y_int_AD * y(1) )
    xlp_AD%lp_left(2) = xlp_AD%lp_left(2) + ( wl_y_int_AD * y(2) )
    xlp_AD%lp_left(3) = xlp_AD%lp_left(3) + ( wl_y_int_AD * y(3) )

    y_AD(1) = y_AD(1) + ( wl_y_int_AD * xlp%lp_left(1) )
    
    y_AD(2) = y_AD(2) + ( wr_y_int_AD * xlp%lp_right(1) ) + &
                        ( wl_y_int_AD * xlp%lp_left(2)  )
                        
    y_AD(3) = y_AD(3) + ( wr_y_int_AD * xlp%lp_right(2) ) + &
                        ( wl_y_int_AD * xlp%lp_left(3)  )
                        
    y_AD(4) = y_AD(4) + ( wr_y_int_AD * xlp%lp_right(3) )


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
    TYPE(LPoly_type), INTENT(IN OUT) :: p     ! Input
!    TYPE(LPoly_type), INTENT(OUT) :: p     ! Input
    ! Compute the numerator differences
    CALL Compute_dxi(x(1:3),x_int,p%dxi_left)
    CALL Compute_dxi(x(2:4),x_int,p%dxi_right)

    ! Compute the denominator differences
    CALL Compute_dx(x(1:3),p%dx_left)
    CALL Compute_dx(x(2:4),p%dx_right)
    
    ! Compute the quadratic polynomials
    CALL Compute_QPoly(p%dxi_left , p%dx_left , p%lp_left)
    CALL Compute_QPoly(p%dxi_right, p%dx_right, p%lp_right)

    ! Polynomial weights
    IF ( x_int < x(2) ) THEN
      p%w_right = ZERO
      p%w_left  = ONE
    ELSE IF ( x_int > x(3) ) THEN
      p%w_right = ONE
      p%w_left  = ZERO
    ELSE
      p%w_right = p%dxi_left(2) / (-p%dx_left(3))
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
    TYPE(LPoly_type), INTENT(IN OUT) :: p_TL      ! TL  Output
!    TYPE(LPoly_type), INTENT(OUT) :: p_TL      ! TL  Output
    ! Compute the tangent-linear numerator differences
    CALL Compute_dxi_TL(x_TL(1:3),x_int_TL,p_TL%dxi_left)
    CALL Compute_dxi_TL(x_TL(2:4),x_int_TL,p_TL%dxi_right)

    ! Compute the tangent-linear denominator differences
    CALL Compute_dx_TL(x_TL(1:3),p_TL%dx_left)
    CALL Compute_dx_TL(x_TL(2:4),p_TL%dx_right)
    
    ! Compute the tangent-linear quadratic polynomials
    CALL Compute_QPoly_TL(p%dxi_left   , p%dx_left    , p%lp_left,  &
                          p_TL%dxi_left, p_TL%dx_left , p_TL%lp_left)
    CALL Compute_QPoly_TL(p%dxi_right   , p%dx_right   , p%lp_right,  &
                          p_TL%dxi_right, p_TL%dx_right, p_TL%lp_right)

    ! Polynomial weights
    IF ( x_int < x(2) .OR. x_int > x(3) ) THEN
      p_TL%w_right = ZERO
      p_TL%w_left  = ZERO
    ELSE
      p_TL%w_right = -( p_TL%dxi_left(2) + (p%w_right*p_TL%dx_left(3)) ) / p%dx_left(3)
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
      p_AD%dx_left(3)  = p_AD%dx_left(3)  - (p%w_right*p_AD%w_right/p%dx_left(3))
      p_AD%dxi_left(2) = p_AD%dxi_left(2) - (p_AD%w_right/p%dx_left(3))
      p_AD%w_right = ZERO
    END IF

    ! "Right" side quadratic
    CALL Compute_QPoly_AD(p%dxi_right   , p%dx_right,  &
                          p%lp_right    , &
                          p_AD%lp_right , &
                          p_AD%dxi_right, p_AD%dx_right)
                          
    ! "Left" side quadratic
    CALL Compute_QPoly_AD(p%dxi_left   , p%dx_left,  &
                          p%lp_left    , &
                          p_AD%lp_left , &
                          p_AD%dxi_left, p_AD%dx_left)

    ! Compute the adjoint denominator differences
    CALL Compute_dx_AD(p_AD%dx_right,x_AD(2:4))
    CALL Compute_dx_AD(p_AD%dx_left ,x_AD(1:3))

    ! Compute the adjoint numerator differences
    CALL Compute_dxi_AD(p_AD%dxi_right,x_AD(2:4),x_int_AD)
    CALL Compute_dxi_AD(p_AD%dxi_left ,x_AD(1:3),x_int_AD)
  END SUBROUTINE LPoly_AD


  ! ------------------------------------------------
  ! Subroutines to compute the quadratic polynomials
  ! ------------------------------------------------
  ! Forward model
  SUBROUTINE Compute_QPoly(dxi, dx, lp)
    REAL(fp), INTENT(IN)     :: dxi(:)  ! Input
    REAL(fp), INTENT(IN)     :: dx(:)   ! Input
    REAL(fp), INTENT(IN OUT) :: lp(:)   ! Output
!    REAL(fp), INTENT(OUT) :: lp(:)   ! Output
    lp(1) = dxi(2)*dxi(3) / ( dx(1)*dx(2))
    lp(2) = dxi(1)*dxi(3) / (-dx(1)*dx(3))
    lp(3) = dxi(1)*dxi(2) / ( dx(2)*dx(3))
  END SUBROUTINE Compute_QPoly
  
  ! Tangent-linear model
  SUBROUTINE Compute_QPoly_TL(dxi   , dx   , lp,  &
                              dxi_TL, dx_TL, lp_TL)
    REAL(fp), INTENT(IN)     :: dxi(:)     ! FWD Input
    REAL(fp), INTENT(IN)     :: dx(:)      ! FWD Input
    REAL(fp), INTENT(IN)     :: lp(:)      ! FWD Input
    REAL(fp), INTENT(IN)     :: dxi_TL(:)  ! TL  Input
    REAL(fp), INTENT(IN)     :: dx_TL(:)   ! TL  Input
    REAL(fp), INTENT(IN OUT) :: lp_TL(:)   ! TL  Output
!    REAL(fp), INTENT(OUT) :: lp_TL(:)   ! TL  Output
    lp_TL(1) = ( (dxi(3)*dxi_TL(2)      ) + &
                 (dxi(2)*dxi_TL(3)      ) - &
                 (dx(2) *dx_TL(1) *lp(1)) - &
                 (dx(1) *dx_TL(2) *lp(1))   ) / (dx(1)*dx(2))

    lp_TL(2) = ( (dxi(3)*dxi_TL(1)      ) + &
                 (dxi(1)*dxi_TL(3)      ) + &
                 (dx(3) *dx_TL(1) *lp(2)) + &
                 (dx(1) *dx_TL(3) *lp(2))   ) / (-dx(1)*dx(3))

    lp_TL(3) = ( (dxi(2)*dxi_TL(1)      ) + &
                 (dxi(1)*dxi_TL(2)      ) - &
                 (dx(3) *dx_TL(2) *lp(3)) - &
                 (dx(2) *dx_TL(3) *lp(3))   ) / (dx(2)*dx(3))
  END SUBROUTINE Compute_QPoly_TL
  
  ! Adjoint model
  SUBROUTINE Compute_QPoly_AD(dxi   , dx, &
                              lp    , &
                              lp_AD , &
                              dxi_AD, dx_AD)
    REAL(fp), INTENT(IN)     :: dxi(:)     ! FWD Input
    REAL(fp), INTENT(IN)     :: dx(:)      ! FWD Input
    REAL(fp), INTENT(IN)     :: lp(:)      ! FWD Input
    REAL(fp), INTENT(IN OUT) :: lp_AD(:)   ! AD  Input
    REAL(fp), INTENT(IN OUT) :: dxi_AD(:)  ! AD  Output
    REAL(fp), INTENT(IN OUT) :: dx_AD(:)   ! AD  Output
    REAL(fp) :: d
    ! Adjoint of lp(3)
    d = lp_AD(3)/(dx(2)*dx(3))
    dxi_AD(1) = dxi_AD(1) + d*dxi(2)
    dxi_AD(2) = dxi_AD(2) + d*dxi(1)
    dx_AD(2)  = dx_AD(2)  - d*dx(3)*lp(3)
    dx_AD(3)  = dx_AD(3)  - d*dx(2)*lp(3)
    lp_AD(3) = ZERO
    ! Adjoint of lp(2)
    d = lp_AD(2)/(-dx(1)*dx(3))
    dxi_AD(1) = dxi_AD(1) + d*dxi(3)
    dxi_AD(3) = dxi_AD(3) + d*dxi(1)
    dx_AD(2)  = dx_AD(2)  + d*dx(3)*lp(2)
    dx_AD(3)  = dx_AD(3)  + d*dx(2)*lp(2)
    lp_AD(2) = ZERO
    ! Adjoint of lp(1)
    d = lp_AD(1)/(dx(1)*dx(2))
    dxi_AD(2) = dxi_AD(2) + d*dxi(3)
    dxi_AD(3) = dxi_AD(3) + d*dxi(2)
    dx_AD(1)  = dx_AD(1)  - d*dx(2)*lp(1)
    dx_AD(2)  = dx_AD(2)  - d*dx(1)*lp(1)
    lp_AD(1) = ZERO
  END SUBROUTINE Compute_QPoly_AD
  

  
  ! -------------------------------------------------------------
  ! Subroutines to compute the polynomial denominator differences
  ! -------------------------------------------------------------
  ! Forward model
  SUBROUTINE Compute_dx(x,dx)
    REAL(fp), INTENT(IN)     :: x(:)   ! Input
    REAL(fp), INTENT(IN OUT) :: dx(:)  ! Output
!    REAL(fp), INTENT(OUT) :: dx(:)  ! Output
    dx(1) = x(1)-x(2)
    dx(2) = x(1)-x(3)
    dx(3) = x(2)-x(3)
  END SUBROUTINE Compute_dx
  
  ! Tangent-linear model
  SUBROUTINE Compute_dx_TL(x_TL,dx_TL)
    REAL(fp), INTENT(IN)     :: x_TL(:)   ! TL Input
    REAL(fp), INTENT(IN OUT) :: dx_TL(:)  ! TL Output
!    REAL(fp), INTENT(OUT) :: dx_TL(:)  ! TL Output
    dx_TL(1) = x_TL(1)-x_TL(2)
    dx_TL(2) = x_TL(1)-x_TL(3)
    dx_TL(3) = x_TL(2)-x_TL(3)
  END SUBROUTINE Compute_dx_TL
  
  ! Adjoint model
  SUBROUTINE Compute_dx_AD(dx_AD,x_AD)
    REAL(fp), INTENT(IN OUT) :: dx_AD(:)  ! AD Input
    REAL(fp), INTENT(IN OUT) :: x_AD(:)   ! AD Output
    x_AD(3) = x_AD(3) - dx_AD(3)
    x_AD(2) = x_AD(2) + dx_AD(3)
    dx_AD(3) = ZERO
    x_AD(3) = x_AD(3) - dx_AD(2)
    x_AD(1) = x_AD(1) + dx_AD(2)
    dx_AD(2) = ZERO
    x_AD(2) = x_AD(2) - dx_AD(1)
    x_AD(1) = x_AD(1) + dx_AD(1)
    dx_AD(1) = ZERO
  END SUBROUTINE Compute_dx_AD


  ! -----------------------------------------------------------
  ! Subroutines to compute the polynomial numerator differences
  ! -----------------------------------------------------------
  ! Forward model
  SUBROUTINE Compute_dxi(x,xi,dxi)
    REAL(fp), INTENT(IN)     :: x(:)    ! Input
    REAL(fp), INTENT(IN)     :: xi      ! Input
    REAL(fp), INTENT(IN OUT) :: dxi(:)  ! Output
!    REAL(fp), INTENT(OUT) :: dxi(:)  ! Output
!    INTEGER :: i
!    DO i = 1, NPOLY_PTS
!      dxi(i) = xi-x(i)
!    END DO
    dxi(1) = xi - x(1)
    dxi(2) = xi - x(2)
    dxi(3) = xi - x(3)
  END SUBROUTINE Compute_dxi
  
  ! Tangent-linear model
  SUBROUTINE Compute_dxi_TL(x_TL,xi_TL,dxi_TL)
    REAL(fp), INTENT(IN)     :: x_TL(:)    ! TL Input
    REAL(fp), INTENT(IN)     :: xi_TL      ! TL Input
    REAL(fp), INTENT(IN OUT) :: dxi_TL(:)  ! TL Output
!    REAL(fp), INTENT(OUT) :: dxi_TL(:)  ! TL Output
!    INTEGER :: i
!    DO i = 1, NPOLY_PTS
!      dxi_TL(i) = xi_TL-x_TL(i)
!    END DO
    dxi_TL(1) = xi_TL - x_TL(1)
    dxi_TL(2) = xi_TL - x_TL(2)
    dxi_TL(3) = xi_TL - x_TL(3)
  END SUBROUTINE Compute_dxi_TL
  
  ! Adjoint model
  SUBROUTINE Compute_dxi_AD(dxi_AD,x_AD,xi_AD)
    REAL(fp), INTENT(IN OUT) :: dxi_AD(:)  ! AD Input
    REAL(fp), INTENT(IN OUT) :: x_AD(:)    ! AD Output
    REAL(fp), INTENT(IN OUT) :: xi_AD      ! AD Output
!    INTEGER :: i
!    DO i = 1, NPOLY_PTS
!      x_AD(i)   = x_AD(i) - dxi_AD(i)
!      xi_AD     = xi_AD   + dxi_AD(i)
!      dxi_AD(i) = ZERO
!    END DO
    x_AD(1)   = x_AD(1) - dxi_AD(1)
    xi_AD     = xi_AD   + dxi_AD(1)
    dxi_AD(1) = ZERO
    x_AD(2)   = x_AD(2) - dxi_AD(2)
    xi_AD     = xi_AD   + dxi_AD(2)
    dxi_AD(2) = ZERO
    x_AD(3)   = x_AD(3) - dxi_AD(3)
    xi_AD     = xi_AD   + dxi_AD(3)
    dxi_AD(3) = ZERO
  END SUBROUTINE Compute_dxi_AD

END MODULE CRTM_Interpolation

