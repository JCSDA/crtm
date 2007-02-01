MODULE CRTM_Interpolation

  USE Type_Kinds, ONLY: fp=>fp_kind
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: interp_1D
  PUBLIC :: interp_2D
  PUBLIC :: interp_3D
  PUBLIC :: interp_1D_TL
  PUBLIC :: interp_2D_TL
  PUBLIC :: interp_3D_TL
  PUBLIC :: interp_1D_AD
  PUBLIC :: interp_2D_AD
  PUBLIC :: interp_3D_AD
  PUBLIC :: find_index
  
  INTERFACE find_index
    MODULE PROCEDURE find_regular_index
    MODULE PROCEDURE find_random_index
  END INTERFACE find_index
  
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID=&
  '$Id: $'
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  INTEGER,  PARAMETER :: ORDER   = 3
  INTEGER,  PARAMETER :: NPTS    = ORDER+1
  INTEGER,  PARAMETER :: NMINMAX = NPTS/2


CONTAINS


  !#########################
  !#   PUBLIC PROCEDURES   #
  !#########################
  
  ! -------------------------
  ! Interpolation subroutines
  ! -------------------------
  ! 1-D routine
  SUBROUTINE interp_1D(x, y , &  ! Input
                       x_int, &  ! Input
                       y_int  )  ! Output
    REAL(fp), INTENT(IN)  :: x(:), y(:)
    REAL(fp), INTENT(IN)  :: x_int
    REAL(fp), INTENT(OUT) :: y_int
    REAL(fp) :: lp(NPTS)
    ! Compute the Lagrangian polynomials
    lp = lpoly(x, x_int)
    ! Compute the interpolate
    CALL interp_1D_lp(y, lp, y_int)
  END SUBROUTINE interp_1D	   


  ! 2-D routine
  SUBROUTINE interp_2D(x, y, z     , &  ! Input
                       x_int, y_int, &  ! Input
                       z_int         )  ! Output
    REAL(fp), INTENT(IN)  :: x(:), y(:), z(:,:)
    REAL(fp), INTENT(IN)  :: x_int, y_int
    REAL(fp), INTENT(OUT) :: z_int
    INTEGER  :: i
    REAL(fp) :: lp(NPTS)
    REAL(fp) :: a(NPTS)
    ! Interpolate z in x dimension for all y
    lp = lpoly(x, x_int)
    DO i = 1,NPTS
      CALL interp_1D_lp(z(:,i),lp,a(i))
    END DO
    ! Interpolate z in y dimension
    CALL interp_1D(y,a,y_int,z_int)
  END SUBROUTINE interp_2D


  ! 3-D routine
  SUBROUTINE interp_3D(w, x, y, z         , &  ! Input
                       w_int, x_int, y_int, &  ! Input
                       z_int                )  ! Output
    REAL(fp), INTENT(IN)  :: w(:), x(:), y(:), z(:,:,:)
    REAL(fp), INTENT(IN)  :: w_int, x_int, y_int
    REAL(fp), INTENT(OUT) :: z_int
    INTEGER  :: i, j
    REAL(fp) :: wlp(NPTS), xlp(NPTS)
    REAL(fp) :: a(NPTS), b(NPTS)
    ! Compute the interpolating polynomials
    wlp = lpoly(w,w_int)
    xlp = lpoly(x,x_int)
    ! Interpolate a in x dimension for all y
    DO j = 1,NPTS
      ! Interpolate z in w dimension for all x and y
      DO i = 1,NPTS
        CALL interp_1D_lp(z(:,i,j),wlp,a(i))
      END DO
      CALL interp_1D_lp(a,xlp,b(j))
    END DO
    ! Interpolate b in y dimension
    CALL interp_1D(y,b,y_int,z_int)
  END SUBROUTINE interp_3D


  ! ----------------------------------------
  ! Tangent-linear interpolation subroutines
  ! ----------------------------------------
  ! 1-D routine
  SUBROUTINE interp_1D_TL( x, y    , &  ! Input
                           x_int   , &  ! FWD Input
                           x_int_TL, &  ! TL  Input
                           y_int_TL  )  ! TL  Output
    REAL(fp), INTENT(IN) :: x(:), y(:)
    REAL(fp), INTENT(IN) :: x_int
    REAL(fp), INTENT(IN) :: x_int_TL
    REAL(fp), INTENT(OUT):: y_int_TL
    REAL(fp) :: dlp(NPTS)
    ! Compute the derivatives of the Lagrangian polynomials
    dlp = dlpoly(x, x_int)
    ! Compute the tangent-linear interpolate
    CALL interp_1D_lp_TL(y, dlp, x_int_TL, y_int_TL)
  END SUBROUTINE interp_1D_TL


  ! 2-D routine
  SUBROUTINE interp_2D_TL( x, y, z           , &  ! Input
                           x_int   , y_int   , &  ! FWD Input
                           x_int_TL, y_int_TL, &  ! TL  Input
                           z_int_TL            )  ! TL  Output
    REAL(fp), INTENT(IN)  :: x(:), y(:), z(:,:)
    REAL(fp), INTENT(IN)  :: x_int   , y_int
    REAL(fp), INTENT(IN)  :: x_int_TL, y_int_TL
    REAL(fp), INTENT(OUT) :: z_int_TL
    INTEGER  :: i
    REAL(fp) :: a(NPTS)
    REAL(fp) :: a_TL(NPTS)
    REAL(fp) :: lp(NPTS)
    REAL(fp) :: dlp(NPTS)
    REAL(fp) :: a_int_TL, a_TL_int
    ! Compute the interpolating polynomials
    lp  = lpoly(x, x_int)
    dlp = dlpoly(x, x_int)
    ! Interpolate z in x dimension for all y
    DO i = 1,NPTS
      CALL interp_1D_lp(z(:,i),lp,a(i))
      CALL interp_1D_lp_TL(z(:,i),dlp,x_int_TL,a_TL(i))
    END DO
    ! Interpolate z in y dimension
    CALL interp_1D_TL(y,a,y_int,y_int_TL,a_int_TL)
    CALL interp_1D(y,a_TL,y_int,a_TL_int)
    z_int_TL = a_int_TL + a_TL_int
  END SUBROUTINE interp_2D_TL


  ! 3-D routine
  SUBROUTINE interp_3D_TL( w, x, y, z                  , &  ! Input
                           w_int   , x_int   , y_int   , &  ! FWD Input
                           w_int_TL, x_int_TL, y_int_TL, &  ! TL  Input
                           z_int_TL                      )  ! TL  Output
    REAL(fp), INTENT(IN)  :: w(:), x(:), y(:), z(:,:,:)
    REAL(fp), INTENT(IN)  :: w_int   , x_int   , y_int
    REAL(fp), INTENT(IN)  :: w_int_TL, x_int_TL, y_int_TL
    REAL(fp), INTENT(OUT) :: z_int_TL
    INTEGER  :: i, j
    REAL(fp) :: wlp(NPTS), xlp(NPTS)
    REAL(fp) :: wdlp(NPTS), xdlp(NPTS)
    REAL(fp) :: a(NPTS)   , b(NPTS)
    REAL(fp) :: a_TL(NPTS), b_TL(NPTS)
    REAL(fp) :: a_int_TL, a_TL_int
    REAL(fp) :: b_int_TL, b_TL_int
    ! Compute the interpolating polynomials
    wlp = lpoly(w,w_int)
    xlp = lpoly(x,x_int)
    wdlp = dlpoly(w,w_int)
    xdlp = dlpoly(x,x_int)
    ! Interpolate a in x dimension for all y
    DO j = 1,NPTS
      ! Interpolate z in w dimension for all x and y
      DO i = 1,NPTS
        CALL interp_1D_lp(z(:,i,j),wlp,a(i))
        CALL interp_1D_lp_TL(z(:,i,j),wdlp,w_int_TL,a_TL(i))
      END DO
      CALL interp_1D_lp(a,xlp,b(j))
      CALL interp_1D_lp_TL(a,xdlp,x_int_TL,a_int_TL)
      CALL interp_1D_lp(a_TL,xlp,a_TL_int)
      b_TL(j) = a_int_TL + a_TL_int
    END DO
    ! Interpolate b in y dimension
    CALL interp_1D_TL(y,b,y_int,y_int_TL,b_int_TL)
    CALL interp_1D(y,b_TL,y_int,b_TL_int)
    z_int_TL = b_int_TL + b_TL_int
  END SUBROUTINE interp_3D_TL


  ! ---------------------------------
  ! Adjoint interpolation subroutines
  ! ---------------------------------
  ! 1-D routine
  SUBROUTINE interp_1D_AD( x, y    , &  ! Input
                           x_int   , &  ! FWD Input
                           y_int_AD, &  ! AD  Input
                           x_int_AD  )  ! AD  Output
    REAL(fp), INTENT(IN)     :: x(:), y(:)
    REAL(fp), INTENT(IN)     :: x_int
    REAL(fp), INTENT(IN OUT) :: y_int_AD
    REAL(fp), INTENT(IN OUT) :: x_int_AD
    REAL(fp) :: dlp(NPTS)
    REAL(fp) :: lp_AD(NPTS)
    INTEGER  :: i
    ! Compute the derivatives of the Lagrangian polynomials
    dlp = dlpoly(x, x_int)
    ! Compute the adjoint x interpolate
    DO i = 1,NPTS
      x_int_AD = x_int_AD + y_int_AD*dlp(i)*y(i)
    END DO
    y_int_AD = ZERO
  END SUBROUTINE interp_1D_AD


  ! 1-D routine for adjoint of FWD
  ! interpolation of TL quantities
  SUBROUTINE interp_1D_FWD_AD(x, x_int, &  ! Input
                              y_int_AD, &  ! Input
                              y_AD      )  ! Output
    REAL(fp), INTENT(IN)     :: x(:), x_int
    REAL(fp), INTENT(IN OUT) :: y_int_AD
    REAL(fp), INTENT(IN OUT) :: y_AD(:)
    REAL(fp) :: lp(NPTS)
    INTEGER  :: i
    ! Compute the Lagrangian polynomials
    lp = lpoly(x, x_int)
    ! Compute the adjoint interpolate
    DO i = 1,NPTS
      y_AD(i) = y_AD(i) + lp(i)*y_int_AD
    END DO
    y_int_AD = ZERO
  END SUBROUTINE interp_1D_FWD_AD
  
  
  ! 2-D routine
  SUBROUTINE interp_2D_AD( x, y, z           , &  ! Input
                           x_int   , y_int   , &  ! FWD Input
                           z_int_AD,           &  ! AD  Input
                           x_int_AD, y_int_AD  )  ! AD  Output
    REAL(fp), INTENT(IN)     :: x(:), y(:), z(:,:)
    REAL(fp), INTENT(IN)     :: x_int   , y_int
    REAL(fp), INTENT(IN OUT) :: z_int_AD
    REAL(fp), INTENT(IN OUT) :: x_int_AD, y_int_AD
    INTEGER  :: i
    REAL(fp) :: xlp(NPTS), xdlp(NPTS)
    REAL(fp) :: a(NPTS)
    REAL(fp) :: a_AD(NPTS)
    REAL(fp) :: a_AD_int

    ! Forward calculations
    !
    ! Interpolate z in x dimension for all y
    xlp = lpoly(x,x_int)
    DO i = 1,NPTS
      CALL interp_1D_lp(z(:,i),xlp,a(i))
    END DO

    ! Adjoint calculations
    !
    ! Initialize local AD variables
    a_AD     = ZERO
    a_AD_int = ONE
    ! Adjoint of z interpolation in y dimension
    ! The first part provides y_int_AD (dz/dy)
    CALL interp_1D_AD(y,a,y_int,z_int_AD,y_int_AD)
    CALL interp_1D_FWD_AD(y,y_int,a_AD_int,a_AD)
    ! Adjoint of z interpolation in x dimension for all y
    ! This provides x_int_AD (dz/dx)
    xdlp = dlpoly(x,x_int)
    DO i = 1,NPTS
      CALL interp_1D_lp_AD(z(:,i),xdlp,a_AD(i),x_int_AD)
    END DO
  END SUBROUTINE interp_2D_AD


  ! 3-D routine
  SUBROUTINE interp_3D_AD( w, x, y, z                  , &  ! Input
                           w_int   , x_int   , y_int   , &  ! FWD Input
                           z_int_AD,                     &  ! AD  Input
                           w_int_AD, x_int_AD, y_int_AD  )  ! AD  Output
    REAL(fp), INTENT(IN)     :: w(:), x(:), y(:), z(:,:,:)
    REAL(fp), INTENT(IN)     :: w_int   , x_int   , y_int
    REAL(fp), INTENT(IN OUT) :: z_int_AD
    REAL(fp), INTENT(IN OUT) :: w_int_AD, x_int_AD, y_int_AD
    INTEGER  :: i, j
    REAL(fp) :: wlp(NPTS) , xlp(NPTS)
    REAL(fp) :: wdlp(NPTS), xdlp(NPTS)
    REAL(fp) :: a(NPTS)   , b(NPTS)
    REAL(fp) :: a_AD(NPTS), b_AD(NPTS)
    REAL(fp) :: a_AD_int
    REAL(fp) :: b_AD_int
    
    ! Forward calculations
    !
    ! Compute the interpolating polynomials
    wlp = lpoly(w,w_int)
    xlp = lpoly(x,x_int)
    ! Interpolate a in x dimension for all y
    DO j = 1,NPTS
      ! Interpolate z in w dimension for all x and y
      DO i = 1,NPTS
        CALL interp_1D_lp(z(:,i,j),wlp,a(i))
      END DO
      CALL interp_1D_lp(a,xlp,b(j))
    END DO

    ! Adjoint calculations
    !
    ! Compute the derivatives of the 
    ! interpolating polynomials
    xdlp = dlpoly(x,x_int)
    wdlp = dlpoly(w,w_int)
    ! Initialize local AD variables
    b_AD = ZERO
    a_AD = ZERO
    ! Adjoint of b interpolation in y direction
    ! The first part provides y_int_AD (dz/dy)
    b_AD_int = z_int_AD
    CALL interp_1D_AD(y,b,y_int,z_int_AD,y_int_AD)
    CALL interp_1D_FWD_AD(y,y_int,b_AD_int,b_AD)
    ! Adjoint of a interpolation in x dimension for all y
    ! The first part provides x_int_AD (dz/dx)
    DO j = 1,NPTS
      a_AD_int = b_AD(j)
      CALL interp_1D_lp_AD(a,xdlp,b_AD(j),x_int_AD)
      CALL interp_1D_lp_FWD_AD(xlp,a_AD_int,a_AD)
      ! Adjoint of z interpolation in w dimension for all x and y
      ! This provides w_int_AD (dz/dw)
      DO i = 1,NPTS
        CALL interp_1D_lp_AD(z(:,i,j),wdlp,a_AD(i),w_int_AD)
      END DO
    END DO
  END SUBROUTINE interp_3D_AD
  
  
  ! ------------------
  ! Indexing functions
  ! ------------------
  ! Find lower index for regular spacing
  SUBROUTINE find_regular_index(x, dx, x_int, i1, i2)
    REAL(fp), INTENT(IN)  :: x(:)
    REAL(fp), INTENT(IN)  :: dx, x_int
    INTEGER , INTENT(OUT) :: i1, i2
    INTEGER :: n
    n = SIZE(x)
    i1 = FLOOR((x_int-x(1))/dx)+1-(ORDER/2)
    i1 = MIN(MAX(i1,1),n-ORDER)
    i2 = i1 + ORDER
  END SUBROUTINE find_regular_index
  
  ! Find lower index for random spacing.
  ! Assumption is that x(1) <= xInt <= x(n)
  ! (despite the MIN/MAX test)
  SUBROUTINE find_random_index(x, x_int, i1, i2)
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
  END SUBROUTINE find_random_index


  !##########################
  !#   PRIVATE PROCEDURES   #
  !##########################

  ! 1-D interpolation routine that accepts
  ! the Lagrangian interpolating polynomials
  ! as input rather than calculating them
  ! in place
  SUBROUTINE interp_1D_lp(y, lp, &  ! Input
                          y_int  )  ! Output
    REAL(fp), INTENT(IN)  :: y(:), lp(:)
    REAL(fp), INTENT(OUT) :: y_int
    INTEGER  :: i
    y_int = ZERO
    DO i = 1,NPTS
      y_int = y_int + lp(i)*y(i)
    END DO
  END SUBROUTINE interp_1D_lp

  
  ! 1-D tangent-linear interpolation routine
  ! that accepts the derivatives of the Lagrangian
  ! interpolating polynomials as input rather
  ! than calculating them in place
  SUBROUTINE interp_1D_lp_TL( y, dlp  , &  ! Input
                              x_int_TL, &  ! TL  Input
                              y_int_TL  )  ! TL  Output
    REAL(fp), INTENT(IN) :: y(:), dlp(:)
    REAL(fp), INTENT(IN) :: x_int_TL
    REAL(fp), INTENT(OUT):: y_int_TL
    INTEGER  :: i
    y_int_TL = ZERO
    DO i = 1,NPTS
      y_int_TL = y_int_TL + x_int_TL*dlp(i)*y(i)
    END DO
  END SUBROUTINE interp_1D_lp_TL


  ! 1-D adjoint routine interpolation routine
  ! that accepts the derivatives of the Lagrangian
  ! interpolating polynomials as input rather
  ! than calculating them in place
  SUBROUTINE interp_1D_lp_AD( y, dlp  , &  ! Input
                              y_int_AD, &  ! AD  Input
                              x_int_AD  )  ! AD  Output
    REAL(fp), INTENT(IN)     :: y(:), dlp(:)
    REAL(fp), INTENT(IN OUT) :: y_int_AD
    REAL(fp), INTENT(IN OUT) :: x_int_AD
    INTEGER  :: i
    DO i = 1,NPTS
      x_int_AD = x_int_AD + y_int_AD*dlp(i)*y(i)
    END DO
    y_int_AD = ZERO
  END SUBROUTINE interp_1D_lp_AD

  
  ! 1-D routine for adjoint of FWD interpolation
  ! of TL quantities that accepts the Lagrangian
  ! interpolating polynomials as input rather
  ! than calculating them in place
  SUBROUTINE interp_1D_lp_FWD_AD(lp      , &  ! Input
                                 y_int_AD, &  ! Input
                                 y_AD      )  ! Output
    REAL(fp), INTENT(IN)     :: lp(:)
    REAL(fp), INTENT(IN OUT) :: y_int_AD
    REAL(fp), INTENT(IN OUT) :: y_AD(:)
    INTEGER  :: i
    DO i = 1,NPTS
      y_AD(i) = y_AD(i) + lp(i)*y_int_AD
    END DO
    y_int_AD = ZERO
  END SUBROUTINE interp_1D_lp_FWD_AD
  
  
  ! Function to compute the Lagrangian
  ! polynomials for interpolation
  FUNCTION lpoly(x, x_int) RESULT(lp)
    REAL(fp), INTENT(IN)  :: x(:)
    REAL(fp), INTENT(IN)  :: x_int
    REAL(fp) :: lp(SIZE(x))

    lp(1) =  (x_int-x(2))*(x_int-x(3))*(x_int-x(4))  / &
            ((x(1) -x(2))*(x(1) -x(3))*(x(1) -x(4)))

    lp(2) =  (x_int-x(1))*(x_int-x(3))*(x_int-x(4))  / &
            ((x(2) -x(1))*(x(2) -x(3))*(x(2) -x(4)))
    
    lp(3) =  (x_int-x(1))*(x_int-x(2))*(x_int-x(4))  / &
            ((x(3) -x(1))*(x(3) -x(2))*(x(3) -x(4)))
    
    lp(4) =  (x_int-x(1))*(x_int-x(2))*(x_int-x(3))  / &
            ((x(4) -x(1))*(x(4) -x(2))*(x(4) -x(3)))
  END FUNCTION lpoly

  
  ! Function to compute the derivatives
  ! of the Lagrangian polynomials for
  ! interpolation with the interpolation
  ! point
  FUNCTION dlpoly(x, x_int) RESULT(dlp)
    REAL(fp), INTENT(IN) :: x(:)
    REAL(fp), INTENT(IN) :: x_int
    REAL(fp) :: dlp(SIZE(x))
    
    dlp(1) = ((x_int-x(2))*(x_int-x(3)) + &
              (x_int-x(2))*(x_int-x(4)) + &
              (x_int-x(3))*(x_int-x(4))) / &
             ((x(1)-x(2))*(x(1)-x(3))*(x(1)-x(4)))

    dlp(2) = ((x_int-x(1))*(x_int-x(3)) + &
              (x_int-x(1))*(x_int-x(4)) + &
              (x_int-x(3))*(x_int-x(4))) / &
             ((x(2)-x(1))*(x(2)-x(3))*(x(2)-x(4)))

    dlp(3) = ((x_int-x(1))*(x_int-x(2)) + &
              (x_int-x(2))*(x_int-x(4)) + &
              (x_int-x(1))*(x_int-x(4))) / &
             ((x(3)-x(1))*(x(3)-x(2))*(x(3)-x(4)))
    
    dlp(4) = ((x_int-x(2))*(x_int-x(3)) + &
              (x_int-x(1))*(x_int-x(2)) + &
              (x_int-x(1))*(x_int-x(3))) / &
             ((x(4)-x(1))*(x(4)-x(2))*(x(4)-x(3)))
  END FUNCTION dlpoly
  
END MODULE CRTM_Interpolation
