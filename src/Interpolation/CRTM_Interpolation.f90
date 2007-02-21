MODULE CRTM_Interpolation

  USE Type_Kinds, ONLY: fp=>fp_kind
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: NPTS
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
  PUBLIC :: lpoly
  PUBLIC :: dlpoly
  
  INTERFACE interp_2D_TL
    MODULE PROCEDURE interp_2D_2D_TL
    MODULE PROCEDURE interp_2D_1D_TL
  END INTERFACE interp_2D_TL
  
  INTERFACE interp_3D_TL
    MODULE PROCEDURE interp_3D_3D_TL
    MODULE PROCEDURE interp_3D_2D_TL
    MODULE PROCEDURE interp_3D_1D_TL
  END INTERFACE interp_3D_TL
  
  INTERFACE interp_2D_AD
    MODULE PROCEDURE interp_2D_2D_AD
    MODULE PROCEDURE interp_2D_1D_AD
  END INTERFACE interp_2D_AD
    
  INTERFACE interp_3D_AD
    MODULE PROCEDURE interp_3D_3D_AD
    MODULE PROCEDURE interp_3D_2D_AD
    MODULE PROCEDURE interp_3D_1D_AD
  END INTERFACE interp_3D_AD

    
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
  
  
  ! 1-D interpolation routine that accepts
  ! the Lagrangian interpolating polynomials
  SUBROUTINE interp_1D(y, xlp, &  ! Input
                       y_int  )  ! Output
    REAL(fp), INTENT(IN)  :: y(:), xlp(:)
    REAL(fp), INTENT(OUT) :: y_int
    INTEGER  :: i
    y_int = ZERO
    DO i = 1,NPTS
      y_int = y_int + xlp(i)*y(i)
    END DO
  END SUBROUTINE interp_1D
  
  ! 2-D routine
  SUBROUTINE interp_2D(z, xlp, ylp, &  ! Input
                       z_int        )  ! Output
    REAL(fp), INTENT(IN)  :: z(:,:)
    REAL(fp), INTENT(OUT) :: z_int
    INTEGER  :: i
    REAL(fp), INTENT(IN) :: xlp(:)
    REAL(fp), INTENT(IN) :: ylp(:)
    REAL(fp) :: a(NPTS)
    ! Interpolate z in x dimension for all y
    DO i = 1,NPTS
      CALL interp_1D(z(:,i),xlp,a(i))
    END DO
    ! Interpolate z in y dimension
    CALL interp_1D(a,ylp,z_int)
  END SUBROUTINE interp_2D

  ! 3-D routine
  SUBROUTINE interp_3D(z, wlp, xlp, ylp, &  ! Input
                       z_int             )  ! Output
    REAL(fp), INTENT(IN)  :: z(:,:,:)
    REAL(fp), INTENT(IN)  :: wlp(:), xlp(:), ylp(:)
    REAL(fp), INTENT(OUT) :: z_int
    INTEGER  :: i, j
    REAL(fp) :: a(NPTS), b(NPTS)
    ! Interpolate a in x dimension for all y
    DO j = 1,NPTS
      ! Interpolate z in w dimension for all x and y
      DO i = 1,NPTS
        CALL interp_1D(z(:,i,j),wlp,a(i))
      END DO
      CALL interp_1D(a,xlp,b(j))
    END DO
    ! Interpolate b in y dimension
    CALL interp_1D(b,ylp,z_int)
  END SUBROUTINE interp_3D

  ! 1-D tangent-linear interpolation routine
  ! that accepts the derivatives of the Lagrangian
  ! interpolating polynomials 
  SUBROUTINE interp_1D_TL( y, xdlp , &  ! Input
                           x_int_TL, &  ! TL  Input
                           y_int_TL  )  ! TL  Output
    REAL(fp), INTENT(IN) :: y(:), xdlp(:)
    REAL(fp), INTENT(IN) :: x_int_TL
    REAL(fp), INTENT(OUT):: y_int_TL
    INTEGER  :: i
    y_int_TL = ZERO
    DO i = 1,NPTS
      y_int_TL = y_int_TL + x_int_TL*xdlp(i)*y(i)
    END DO
  END SUBROUTINE interp_1D_TL
  
  ! ------------
  ! 2-D routines
  ! ------------
  ! 2-D with 2 dimensions perturbed
  SUBROUTINE interp_2D_2D_TL( z                 , &  ! Input
                              xlp, ylp          , &  ! FWD Input
                              xdlp, ydlp        , &  ! TL Input
                              x_int_TL, y_int_TL, &  ! TL  Input
                              z_int_TL            )  ! TL  Output
    REAL(fp), INTENT(IN)  :: z(:,:)
    REAL(fp), INTENT(IN)  :: xlp(:), ylp(:)
    REAL(fp), INTENT(IN)  :: xdlp(:), ydlp(:)
    REAL(fp), INTENT(IN)  :: x_int_TL, y_int_TL
    REAL(fp), INTENT(OUT) :: z_int_TL
    INTEGER  :: i
    REAL(fp) :: a(NPTS)
    REAL(fp) :: a_TL(NPTS)
    REAL(fp) :: a_int_TL, a_TL_int
    ! Interpolate z in x dimension for all y
    DO i = 1,NPTS
      CALL interp_1D(z(:,i),xlp,a(i))
      CALL interp_1D_TL(z(:,i),xdlp,x_int_TL,a_TL(i))
    END DO
    ! Interpolate z in y dimension
    CALL interp_1D_TL(a,ydlp,y_int_TL,a_int_TL)
    CALL interp_1D(a_TL,ylp,a_TL_int)
    z_int_TL = a_int_TL + a_TL_int
  END SUBROUTINE interp_2D_2D_TL
  
  ! 2-D with 1 dimension perturbed
  SUBROUTINE interp_2D_1D_TL( z         , & ! input
                              xlp       , & ! FWD input
                              ydlp      , & ! TL input
                              y_int_TL  , & ! TL input
                              z_int_TL    ) ! TL output
    REAL(fp), INTENT(IN)  :: z(:,:)
    REAL(fp), INTENT(IN)  :: xlp(:)
    REAL(fp), INTENT(IN)  :: ydlp(:)                         
    REAL(fp), INTENT(IN)  :: y_int_TL
    REAL(fp), INTENT(OUT) :: z_int_TL
    INTEGER :: i
    REAL(fp) :: a(NPTS)
    ! Interpolate z in x dimension for all y
    DO i = 1,NPTS
      CALL interp_1D(z(:,i),xlp,a(i))
    END DO
    ! Interpolate z in y dimension
    CALL interp_1D_TL(a,ydlp,y_int_TL,z_int_TL)
  END SUBROUTINE interp_2D_1D_TL
                     
  ! ------------
  ! 3-D routines
  ! ------------
  ! 3-D with 3 dimensions perturbed
  SUBROUTINE interp_3D_3D_TL( z                           , &  ! Input
                              wlp, xlp, ylp               , &  ! FWD Input
                              wdlp, xdlp, ydlp            , &  ! TL Input
                              w_int_TL, x_int_TL, y_int_TL, &  ! TL Input
                              z_int_TL                      )  ! TL  Output
    REAL(fp), INTENT(IN)  :: z(:,:,:)
    REAL(fp), INTENT(IN)  :: wlp(:), xlp(:), ylp(:) 
    REAL(fp), INTENT(IN)  :: wdlp(:), xdlp(:), ydlp(:)  
    REAL(fp), INTENT(IN)  :: w_int_TL, x_int_TL, y_int_TL
    REAL(fp), INTENT(OUT) :: z_int_TL
    INTEGER  :: i, j
    REAL(fp) :: a(NPTS)   , b(NPTS)
    REAL(fp) :: a_TL(NPTS), b_TL(NPTS)
    REAL(fp) :: a_int_TL, a_TL_int
    REAL(fp) :: b_int_TL, b_TL_int
    ! Interpolate a in x dimension for all y
    DO j = 1,NPTS
      ! Interpolate z in w dimension for all x and y
      DO i = 1,NPTS
        CALL interp_1D(z(:,i,j),wlp,a(i))
        CALL interp_1D_TL(z(:,i,j),wdlp,w_int_TL,a_TL(i))
      END DO
      CALL interp_1D(a,xlp,b(j))
      CALL interp_1D_TL(a,xdlp,x_int_TL,a_int_TL)
      CALL interp_1D(a_TL,xlp,a_TL_int)
      b_TL(j) = a_int_TL + a_TL_int
    END DO
    ! Interpolate b in y dimension
    CALL interp_1D_TL(b,ydlp,y_int_TL,b_int_TL)
    CALL interp_1D(b_TL,ylp,b_TL_int)
    z_int_TL = b_int_TL + b_TL_int
  END SUBROUTINE interp_3D_3D_TL
  
  ! 3-D with 2 dimensions perturbed
  SUBROUTINE interp_3D_2D_TL( z                 , &  ! Input 
                              wlp, xlp, ylp     , &  ! FWD Input
                              xdlp, ydlp        , &  ! TL Input
                              x_int_TL, y_int_TL, &  ! TL Input
                              z_int_TL            )  ! TL Output
    REAL(fp), INTENT(IN)  :: z(:,:,:)
    REAL(fp), INTENT(IN)  :: wlp(:), xlp(:), ylp(:)
    REAL(fp), INTENT(IN)  :: xdlp(:), ydlp(:)
    REAL(fp), INTENT(IN)  :: x_int_TL, y_int_TL
    REAL(fp), INTENT(OUT) :: z_int_TL
    INTEGER  :: i, j
    REAL(fp) :: a(NPTS)   , b(NPTS) 
    REAL(fp) :: b_TL(NPTS) 
    REAL(fp) :: a_int_TL
    REAL(fp) :: b_int_TL, b_TL_int
    ! Interpolate a in x dimension for all y
    DO j = 1,NPTS
      ! Interpolate z in w dimension for all x and y
      DO i = 1,NPTS
        CALL interp_1D(z(:,i,j),wlp,a(i))
      END DO
      CALL interp_1D(a,xlp,b(j))
      CALL interp_1D_TL(a,xdlp,x_int_TL,a_int_TL)
      b_TL(j) = a_int_TL
    END DO
    ! Interpolate b in y dimension
    CALL interp_1D_TL(b,ydlp,y_int_TL,b_int_TL)
    CALL interp_1D(b_TL,ylp,b_TL_int)
    z_int_TL = b_int_TL + b_TL_int
  END SUBROUTINE interp_3D_2D_TL
  
  ! 3-D with 1 dimension perturbed
  SUBROUTINE interp_3D_1D_TL( z       , &  ! Input
                              wlp, xlp, &  ! FWD Input
                              ydlp    , &  ! TL Input
                              y_int_TL, &  ! TL Input
                              z_int_TL  )  ! TL Output
    REAL(fp), INTENT(IN)  :: z(:,:,:)
    REAL(fp), INTENT(IN)  :: wlp(:), xlp(:)
    REAL(fp), INTENT(IN)  :: ydlp(:)
    REAL(fp), INTENT(IN)  :: y_int_TL
    REAL(fp), INTENT(OUT)  :: z_int_TL
    INTEGER  :: i, j
    REAL(fp) :: a(NPTS)   , b(NPTS)
    REAL(fp) :: b_int_TL
    ! Interpolate a in x dimension for all y
    DO j = 1,NPTS
      ! Interpolate z in w dimension for all x and y
      DO i = 1,NPTS
        CALL interp_1D(z(:,i,j),wlp,a(i))
      END DO 
      CALL interp_1D(a,xlp,b(j))
    END DO
    ! Interpolate b in y dimension
    CALL interp_1D_TL(b,ydlp,y_int_TL,z_int_TL)
  END SUBROUTINE interp_3D_1D_TL                        
    
  ! 1-D adjoint routine interpolation routine
  ! that accepts the derivatives of the Lagrangian
  ! interpolating polynomials as input rather
  ! than calculating them in place
  SUBROUTINE interp_1D_AD( y, xdlp , &  ! Input
                           y_int_AD, &  ! AD  Input
                           x_int_AD  )  ! AD  Output
    REAL(fp), INTENT(IN)     :: y(:), xdlp(:)
    REAL(fp), INTENT(IN OUT) :: y_int_AD
    REAL(fp), INTENT(IN OUT) :: x_int_AD
    INTEGER  :: i
    DO i = 1,NPTS
      x_int_AD = x_int_AD + y_int_AD*xdlp(i)*y(i)
    END DO
      y_int_AD = ZERO
  END SUBROUTINE interp_1D_AD

  ! 1-D routine for adjoint of FWD
  ! interpolation of TL quantities
  SUBROUTINE interp_1D_FWD_AD(lp,      &  ! Input
                              y_int_AD, &  ! Input
                              y_AD      )  ! Output
    REAL(fp), INTENT(IN)     :: lp(:)
    REAL(fp), INTENT(IN OUT) :: y_int_AD
    REAL(fp), INTENT(IN OUT) :: y_AD(:)
    INTEGER  :: i
    ! Compute the adjoint interpolate
    DO i = 1,NPTS
      y_AD(i) = y_AD(i) + lp(i)*y_int_AD
    END DO
    y_int_AD = ZERO
  END SUBROUTINE interp_1D_FWD_AD
  
  
  ! 2-D routine
  SUBROUTINE interp_2D_2D_AD( z         , &  ! Input
                              xlp, ylp  , &  ! FWD Input
                              xdlp, ydlp, &  ! TL Input
                              z_int_AD  , &  ! AD Input
                              x_int_AD  , &  ! AD Output
                              y_int_AD    )  ! AD Output
    REAL(fp), INTENT(IN)     :: z(:,:)
    REAL(fp), INTENT(IN)     :: xlp(:)   , ylp(:)
    REAL(fp), INTENT(IN)     :: xdlp(:)  , ydlp(:)
    REAL(fp), INTENT(IN OUT) :: z_int_AD
    REAL(fp), INTENT(IN OUT) :: x_int_AD, y_int_AD
    INTEGER  :: i
    REAL(fp) :: a(NPTS)
    REAL(fp) :: a_AD(NPTS)
    REAL(fp) :: a_AD_int
    ! Forward calculations
    ! Interpolate z in x dimension for all y
    DO i = 1,NPTS
      CALL interp_1D(z(:,i),xlp,a(i))
    END DO
    ! Adjoint calculations
    !
    ! Initialize local AD variables
    a_AD     = ZERO
    a_AD_int = ONE
    ! Adjoint of z interpolation in y dimension
    ! The first part provides y_int_AD (dz/dy)
    CALL interp_1D_AD(a,ydlp,z_int_AD,y_int_AD)
    CALL interp_1D_FWD_AD(ylp,a_AD_int,a_AD)
    ! Adjoint of z interpolation in x dimension for all y
    ! This provides x_int_AD (dz/dx)
    DO i = 1,NPTS
      CALL interp_1D_AD(z(:,i),xdlp,a_AD(i),x_int_AD)
    END DO
  END SUBROUTINE interp_2D_2D_AD
  
  
  ! 2-D routine with 1 dimension perturbed
  SUBROUTINE interp_2D_1D_AD( z       , &  ! Input
                              xlp     , &  ! Input
                              ydlp    , &  ! TL Input
                              z_int_AD, &  ! AD Input
                              y_int_AD  )  ! AD Output
    REAL(fp), INTENT(IN)     :: z(:,:)
    REAL(fp), INTENT(IN)     :: xlp(:)   
    REAL(fp), INTENT(IN)     :: ydlp(:)
    REAL(fp), INTENT(IN OUT) :: z_int_AD
    REAL(fp), INTENT(IN OUT) :: y_int_AD
    INTEGER  :: i
    REAL(fp) :: a(NPTS)
    REAL(fp) :: a_AD(NPTS)
    REAL(fp) :: a_AD_int
    ! Forward calculations
    ! Interpolate z in x dimension for all y
    DO i = 1,NPTS
      CALL interp_1D(z(:,i),xlp,a(i))
    END DO
    ! Adjoint calculations
    ! Adjoint of z interpolation in y dimension
    ! The first part provides y_int_AD (dz/dy)
    CALL interp_1D_AD(a,ydlp,z_int_AD,y_int_AD)
  END SUBROUTINE interp_2D_1D_AD
                               

  ! 3-D routine
  SUBROUTINE interp_3D_3D_AD( z               , &  ! Input
                              wlp, xlp, ylp   , &  ! FWD Input
                              wdlp, xdlp, ydlp, &  ! TL Input
                              z_int_AD        , &  ! AD Input
                              w_int_AD        , &  ! AD Output
                              x_int_AD        , &  ! AD Output
                              y_int_AD          )  ! AD  Output
    REAL(fp), INTENT(IN)     :: z(:,:,:)
    REAL(fp), INTENT(IN)     :: wlp(:), xlp(:), ylp(:)
    REAL(fp), INTENT(IN)     :: wdlp(:), xdlp(:), ydlp(:)
    REAL(fp), INTENT(IN OUT) :: w_int_AD, x_int_AD, y_int_AD, z_int_AD
    INTEGER  :: i, j
    REAL(fp) :: a(NPTS,NPTS)   , b(NPTS)
    REAL(fp) :: a_AD(NPTS), b_AD(NPTS)
    REAL(fp) :: a_AD_int
    REAL(fp) :: b_AD_int
    
    ! Forward calculations
    ! Interpolate a in x dimension for all y
    DO j = 1,NPTS
      ! Interpolate z in w dimension for all x and y
      DO i = 1,NPTS
        CALL interp_1D(z(:,i,j),wlp,a(i,j))
      END DO
      CALL interp_1D(a(:,j),xlp,b(j))
    END DO

    ! Adjoint calculations
    ! Initialize local AD variables
    b_AD = ZERO
    a_AD = ZERO
    ! Adjoint of b interpolation in y direction
    ! The first part provides y_int_AD (dz/dy)
    b_AD_int = z_int_AD
    CALL interp_1D_AD(b,ydlp,z_int_AD,y_int_AD)
    CALL interp_1D_FWD_AD(ylp,b_AD_int,b_AD)
    ! Adjoint of a interpolation in x dimension for all y
    ! The first part provides x_int_AD (dz/dx)
    DO j = 1,NPTS
      a_AD_int = b_AD(j)
      CALL interp_1D_AD(a(:,j),xdlp,b_AD(j),x_int_AD)
      CALL interp_1D_FWD_AD(xlp,a_AD_int,a_AD) 
      ! Adjoint of z interpolation in w dimension for all x and y
      ! This provides w_int_AD (dz/dw)
      DO i = 1,NPTS
        CALL interp_1D_AD(z(:,i,j),wdlp,a_AD(i),w_int_AD)
      END DO
    END DO
  END SUBROUTINE interp_3D_3D_AD
  
  ! 3-D routine with 2 dimensions perturbed
  SUBROUTINE interp_3D_2D_AD( z            , & ! Input
                              wlp, xlp, ylp, & ! FWD Input
                              xdlp, ydlp   , & ! TL Input
                              z_int_AD     , & ! AD Input
                              x_int_AD     , & ! AD Output
                              y_int_AD       ) ! AD Output    
    REAL(fp), INTENT(IN)     :: z(:,:,:)
    REAL(fp), INTENT(IN)     :: wlp(:), xlp(:), ylp(:)
    REAL(fp), INTENT(IN)     :: xdlp(:), ydlp(:)
    REAL(fp), INTENT(IN OUT) :: x_int_AD, y_int_AD, z_int_AD
    INTEGER  :: i, j
    REAL(fp) :: a(NPTS,NPTS)   , b(NPTS)
    REAL(fp) :: b_AD(NPTS)
    REAL(fp) :: b_AD_int
    ! Forward calculations
    ! Interpolate a in x dimension for all y
    DO j = 1,NPTS
      ! Interpolate z in w dimension for all x and y
      DO i = 1,NPTS
        CALL interp_1D(z(:,i,j),wlp,a(i,j))
      END DO
      CALL interp_1D(a(:,j),xlp,b(j))
    END DO
    ! Adjoint calculations
    ! Initialize local AD variables
    b_AD = ZERO
    ! Adjoint of b interpolation in y direction
    ! The first part provides y_int_AD (dz/dy)
    b_AD_int = z_int_AD
    CALL interp_1D_AD(b,ydlp,z_int_AD,y_int_AD)
    CALL interp_1D_FWD_AD(ylp,b_AD_int,b_AD)
    ! Adjoint of a interpolation in x dimension for all y
    ! The first part provides x_int_AD (dz/dx)
    DO j = 1,NPTS
      CALL interp_1D_AD(a(:,j),xdlp,b_AD(j),x_int_AD)
    END DO
  END SUBROUTINE interp_3D_2D_AD
  
  ! 3-D routine with 2 dimensions perturbed
  SUBROUTINE interp_3D_1D_AD( z       , & ! Input
                              wlp, xlp, & ! FWD Input  
                              ydlp    , & ! TL Input
                              z_int_AD, & ! AD Input
                              y_int_AD  ) ! AD Output 
    REAL(fp), INTENT(IN)      :: z(:,:,:)
    REAL(fp), INTENT(IN)      :: wlp(:), xlp(:)
    REAL(fp), INTENT(IN)      :: ydlp(:)
    REAL(fp), INTENT(IN OUT)  :: z_int_AD, y_int_AD
    INTEGER  :: i, j
    REAL(fp) :: a(NPTS,NPTS), b(NPTS) 
    ! Forward calculations
    ! Interpolate a in x dimension for all y
    DO j = 1,NPTS
      ! Interpolate z in w dimension for all x and y
      DO i = 1,NPTS
        CALL interp_1D(z(:,i,j),wlp,a(i,j))
      END DO
      CALL interp_1D(a(:,j),xlp,b(j))
    END DO
    ! Adjoint calculation
    CALL interp_1D_AD(b,ydlp,z_int_AD,y_int_AD)
  END SUBROUTINE interp_3D_1D_AD
       
       
  ! 3-D routine
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
  
  ! Function to compute the Lagrangian
  ! polynomials for interpolation
  FUNCTION lpoly(x, x_int) RESULT(lp)
    REAL(fp), INTENT(IN)  :: x(:)
    REAL(fp), INTENT(IN)  :: x_int
    REAL(fp) :: lp(SIZE(x))
    REAL(fp) :: xi

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
            
    ! Compute derivative of Lagrangian polynomials
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

