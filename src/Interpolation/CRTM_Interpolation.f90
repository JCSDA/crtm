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
  PUBLIC :: Set_LPoly
  PUBLIC :: Get_LPoly
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
  INTEGER,  PARAMETER :: ORDER = 1
  INTEGER,  PARAMETER :: NPTS  = ORDER+1


  ! -----------------------
  ! Derived type definition
  ! -----------------------
  TYPE :: LPoly_type
    PRIVATE
    INTEGER :: Order=ORDER
    INTEGER :: nPts =NPTS
    REAL(fp) :: ilp(0:NPTS,NPTS) = ZERO
    REAL(fp) :: lp(NPTS)         = ZERO
  END TYPE LPoly_type


CONTAINS


  ! -------------------------------------------
  ! Some utility routines to access the PRIVATE
  ! internals of the derived data type. These
  ! routines are only used in testing.
  ! -------------------------------------------
  SUBROUTINE Clear_LPoly(p)
    TYPE(LPoly_type), INTENT(OUT) :: p
    p%ilp = ZERO
    p%lp  = ZERO
  END SUBROUTINE Clear_LPoly
  
  SUBROUTINE Set_LPoly(p,i,value)
    TYPE(LPoly_type), INTENT(IN OUT) :: p
    INTEGER,          INTENT(IN)     :: i
    REAL(fp),         INTENT(IN)     :: value
    p%lp(i) = value
  END SUBROUTINE Set_LPoly
  
  SUBROUTINE Get_LPoly(p,i,value)
    TYPE(LPoly_type), INTENT(IN)  :: p
    INTEGER,          INTENT(IN)  :: i
    REAL(fp),         INTENT(OUT) :: value
    value = p%lp(i)
  END SUBROUTINE Get_LPoly
  
  
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
    INTEGER  :: i
    ! Perform interpolation
    y_int = ZERO
    DO i = 1,NPTS
      y_int = y_int + xlp%lp(i)*y(i)
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
    DO i = 1,NPTS
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
    DO i = 1,NPTS
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
    DO i = 1,NPTS
      y_int_TL = y_int_TL + xlp%lp(i)*y_TL(i) + xlp_TL%lp(i)*y(i)
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
    DO i = 1,NPTS
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
    DO i = 1,NPTS
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
    DO i = 1,NPTS
      xlp_AD%lp(i) = xlp_AD%lp(i) + y(i)     *y_int_AD
      y_AD(i)      = y_AD(i)      + xlp%lp(i)*y_int_AD
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
    DO i = 1,NPTS
      CALL Interp_1D(z(:,i),xlp,a(i))
    END DO
    ! Adjoint calculations
    ! Initialize local AD variables
    a_AD = ZERO
    ! Adjoint of z interpolation in y dimension
    CALL Interp_1D_AD(a,ylp,z_int_AD,a_AD,ylp_AD)
    ! Adjoint of z interpolation in x dimension for all y
    DO i = 1,NPTS
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
    DO i = 1,NPTS
      CALL Interp_2D(z(:,:,i),wlp,xlp,a(i))
    END DO
    
    ! Adjoint calculations
    ! Initialize local AD variables
    a_AD = ZERO
    ! Adjoint of a interpolation in y dimension
    CALL Interp_1D_AD(a,ylp,z_int_AD,a_AD,ylp_AD)
    ! Adjoint of z interpolation in w and x dimension for all y
    DO i = 1,NPTS
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
    i1 = FLOOR((x_int-x(1))/dx)+1-(ORDER/2)
    i1 = MIN(MAX(i1,1),n-ORDER)
    i2 = i1 + ORDER
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
    i1 = MIN(MAX(1,k-1-(ORDER/2)),n-ORDER)
    i2 = i1 + ORDER
  END SUBROUTINE Find_Random_Index

  
  ! --------------------
  ! Polynomial functions
  ! --------------------
  ! Forward model
  SUBROUTINE LPoly(x, x_int, p)
    REAL(fp),         INTENT(IN)  :: x(:)
    REAL(fp),         INTENT(IN)  :: x_int
    TYPE(LPoly_type), INTENT(OUT) :: p
    INTEGER :: i, j, n
    n = SIZE(x)
    Order_Loop: DO j = 1, n
      p%ilp(0,j) = ONE
      Product_Loop: DO i = 1, n
        IF ( i == j ) THEN
          p%ilp(i,j) = p%ilp(i-1,j)
          CYCLE Product_Loop
        END IF
        p%ilp(i,j) = p%ilp(i-1,j)*(x_int-x(i))/(x(j)-x(i))
      END DO Product_Loop
      p%lp(j) = p%ilp(n,j)
    END DO Order_Loop
  END SUBROUTINE LPoly

  ! Tangent-linear model
  SUBROUTINE LPoly_TL(x   , x_int   , &  ! FWD Input
                      p   , &            ! FWD Input
                      x_TL, x_int_TL, &  ! TL  Input
                      p_TL)              ! TL  Output
    REAL(fp),         INTENT(IN)  :: x(:)
    REAL(fp),         INTENT(IN)  :: x_int
    TYPE(LPoly_type), INTENT(IN)  :: p
    REAL(fp),         INTENT(IN)  :: x_TL(:)
    REAL(fp),         INTENT(IN)  :: x_int_TL
    TYPE(LPoly_type), INTENT(OUT) :: p_TL
    INTEGER  :: i, j, n
    REAL(fp) :: c
    n = SIZE(x)
    Order_Loop: DO j = 1, n
      p_TL%ilp(0,j) = ZERO
      Product_Loop: DO i = 1, n
        IF ( i == j ) THEN
          p_TL%ilp(i,j) = p_TL%ilp(i-1,j)
          CYCLE Product_Loop
        END IF
        c = ONE/(x(j)-x(i))
        p_TL%ilp(i,j) = c * ( (x_int-x(i))*p_TL%ilp(i-1,j) + &
                              p%ilp(i-1,j)*x_int_TL + &
                              p%ilp(i-1,j)*(x_int-x(j))*c*x_TL(i) - &
                              p%ilp(i-1,j)*(x_int-x(i))*c*x_TL(j)   )
      END DO Product_Loop
      p_TL%lp(j) = p_TL%ilp(n,j)
    END DO Order_Loop
  END SUBROUTINE LPoly_TL
  
  ! Adjoint model
  SUBROUTINE LPoly_AD(x   , x_int   , &  ! FWD Input
                      p   , &            ! FWD Input
                      p_AD, &            ! AD  Input
                      x_AD, x_int_AD  )  ! AD  Output
    REAL(fp),         INTENT(IN)     :: x(:)
    REAL(fp),         INTENT(IN)     :: x_int
    TYPE(LPoly_type), INTENT(IN)     :: p
    TYPE(LPoly_type), INTENT(IN OUT) :: p_AD
    REAL(fp),         INTENT(IN OUT) :: x_AD(:)
    REAL(fp),         INTENT(IN OUT) :: x_int_AD
    INTEGER  :: i, j, n
    REAL(fp) :: c, c2
    n = SIZE(x)
    Order_Loop: DO j = 1, n
      p_AD%ilp(n,j) = p_AD%ilp(n,j) + p_AD%lp(j)
      p_AD%lp(j)    = ZERO
      Product_Loop: DO i = n, 1, -1
        IF ( i == j ) THEN
          p_AD%ilp(i-1,j) = p_AD%ilp(i-1,j) + p_AD%ilp(i,j)
          p_AD%ilp(i  ,j) = ZERO
          CYCLE Product_Loop
        END IF
        c  = ONE/(x(j)-x(i))
        c2 = c*c
        x_AD(j)  = x_AD(j)  - c2*p%ilp(i-1,j)*(x_int-x(i))*p_AD%ilp(i,j)
        x_AD(i)  = x_AD(i)  + c2*p%ilp(i-1,j)*(x_int-x(j))*p_AD%ilp(i,j)
        x_int_AD = x_int_AD + c *p%ilp(i-1,j)*             p_AD%ilp(i,j)
        p_AD%ilp(i-1,j) = p_AD%ilp(i-1,j) + c*(x_int-x(i))*p_AD%ilp(i,j)
        p_AD%ilp(i  ,j) = ZERO
      END DO Product_Loop
      p_AD%ilp(0,j) = ZERO
    END DO Order_Loop
  END SUBROUTINE LPoly_AD

END MODULE CRTM_Interpolation

