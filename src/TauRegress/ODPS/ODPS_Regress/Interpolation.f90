
MODULE Interpolation

  ! -----------------
  ! Module use
  ! -----------------
  USE Type_Kinds,  ONLY: Double => fp_kind  


  ! Disable all implicit typing
  IMPLICIT NONE

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE

  PUBLIC :: find_index_linear
  PUBLIC :: interp_linear
  PUBLIC :: find_index
  PUBLIC :: lpoly
  PUBLIC :: interp

  REAL(Double), PARAMETER :: ZERO = 0.0_Double, ONE = 1.0_Double

CONTAINS

  SUBROUTINE find_index_linear(x, x_int, index_int)
    REAL(Double), INTENT(IN)      :: x(:)
    REAL(Double), INTENT(IN)      :: x_int(:)
    INTEGER,      INTENT(OUT)     :: index_int(:)

    CALL find_index(x, x_int, index_int)

  END SUBROUTINE find_index_linear

  SUBROUTINE interp_linear(y, x, x_int, y_int, index_int)

    REAL(Double), INTENT(IN)       :: y(:)            
    REAL(Double), INTENT(IN)       :: x(:)            
    REAL(Double), INTENT(IN)       :: x_int(:)        
    REAL(Double), INTENT(OUT)      :: y_int(:)        
    INTEGER, OPTIONAL, INTENT(IN)  :: index_int(:)

    ! Local
    INTEGER      :: index_int_local(SIZE(x_int))
    REAL(Double) :: xlp(SIZE(x_int),2)
    
    IF(PRESENT(index_int))THEN
      CALL lpoly(x, x_int, index_int, xlp)
      CALL interp(y, xlp, index_int, y_int)
    ELSE
      CALL find_index(x, x_int, index_int_local)
      CALL lpoly(x, x_int, index_int_local, xlp)
      CALL interp(y, xlp, index_int_local, y_int)
    END IF

  END SUBROUTINE interp_linear

!------------------------------------------------------------------------
!  Lagrange interpolation:
!            (x -x2)(x -x3)...(x -xn)      (x -x1)(x -x3)...(x -xn)
!     P(x) = ------------------------*y1 + ------------------------*y2
!            (x1-x2)(x1-x3)...(x1-xn)      (x2-x1)(x2-x3)...(x2-xn)
!
!                                          (x -x1)(x -x2)...(x -xn-1)
!            + ...                       + ------------------------*yn
!                                          (xn-x1)(xn-x3)...(xn-xn-1)
!     where n is the number of data points for interpolation
!     n = 2 is a linear interpolation
!------------------------------------------------------------------------

!----------------------------------------------------------------
! The subroutine finds the indexes for interpolation routines
!
! Input arguments:
!   x     :     Rank-1 array containg the original points
!               Type: Double
!               Dimension: nx
!   x_int :     Rank-1 array containg the interpolation points
!               Type: Double
!               Dimension: nx_int 
! Optional input arguments
!   n_points  : The number of data point for interpolation. If not
!               provided a value of 2 is assumed (linear interpolation).
!               Dimension: Scalar
! Output arguments
!   index_int : Rank-1 array containing the indexes of the first
!               data point of the n_points data series
!               Dimension: nx_int
!
!       e.g. if n_points = 3 (three points interpolation),              
!       x(index_int(i)), x(index_int(i)+1) and x(index_int(i)+2)        
!       will be used as the locations of three points for the           
!       interpolation on x_int(i)                                       
!----------------------------------------------------------------  
  SUBROUTINE find_index(x, x_int, index_int, n_points)
    REAL(Double), INTENT(IN)      :: x(:)
    REAL(Double), INTENT(IN)      :: x_int(:)
    INTEGER,      INTENT(OUT)     :: index_int(:)
    INTEGER, OPTIONAL, INTENT(IN) :: n_points

    ! Local variable
    INTEGER :: j, j1, k, k1, k2, js, djs, nx, nx_int, npts, half
    INTEGER, PARAMETER :: NPTS_default = 2 
    REAL(Double)       :: d1, d2

    nx     = SIZE(x)
    nx_int = SIZE(x_int)
    
    npts = NPTS_default
    IF(PRESENT(n_points))npts = n_points

    k1 = 1
    DO k = 1, nx_int
      IF(x_int(k) < x(1))THEN
        index_int(k) = 1
      ELSE
        k1 = k
        EXIT
      ENDIF
    ENDDO

    k2 = nx_int
    DO k = nx_int, 1, -1
      IF(x_int(k) > x(nx))THEN
        index_int(k) = nx-npts+1
      ELSE
        k2 = k
        EXIT
      ENDIF
    ENDDO

    half = npts/2
    j1 = 1
    DO k=k1, k2
      DO j = j1, nx-1
        d1 = x_int(k) - x(j)
        d2 = x(j+1) - x_int(k)
        IF(d1 >= ZERO .AND. d2 >= ZERO)THEN 
          ! pickup j or j+1 depending on which near to the point x_int(k).
          ! j (or j+1) will be the center point. js in the following calculation
          ! is the leftest point.
          IF(d1 < d2)THEN
            js = j - half + MOD(npts+1, 2)
          ELSE
            js = (j+1) - half
          END IF
          IF(js < 1)js = 1
          djs = nx - (js+npts-1) 
          IF(djs < 0)js = js + djs
          index_int(k) = js  
          j1 = j
          EXIT
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE find_index

!--------------------------------------------------------------------
!  The subroutine computes Lagrange Polynomial for interploation
!  
! Input arguments
! 
!   x     :     Rank-1 array containg the original points
!               Type: Double
!               Dimension: nx
!   x_int :     Rank-1 array containg the interpolation points
!               Type: Double
!               Dimension: nx_int 
!
!  index_int :  Rank-1 array containing the indexes of the first
!               data point of the n_points data series
!               Dimension: nx_int
!
! Optional input arguments
!   n_points  : The number of data point for interpolation. If not
!               provided a value of 2 is assumed (linear interpolation).
!               Dimension: Scalar
!
! Output arguments:
!   lp :        Rank-2 array containing Polynomial data
!               Type: Double
!               Dimension: nx_int x n_points
!----------------------------------------------------------------------              

  SUBROUTINE lpoly(x, x_int, index_int, lp, n_points)
    REAL(Double),     INTENT(IN)  :: x(:)
    REAL(Double),     INTENT(IN)  :: x_int(:)
    INTEGER,          INTENT(IN)  :: index_int(:)
    REAL(Double),     INTENT(OUT) :: lp(:,:)
    INTEGER,OPTIONAL, INTENT(IN)  :: n_points

    ! Local
    INTEGER, PARAMETER :: NPTS_default = 2
    INTEGER :: nx, nx_int, i, i1, i2, j, jj, k, kk, m, npts
    REAL(Double) :: pn, pd

    nx     = SIZE(x)
    nx_int = SIZE(x_int)
    
    npts = NPTS_default
    IF(PRESENT(n_points))npts = n_points
 
    DO m = 1, nx_int
      i1 = index_int(m)                           
      i2 = i1 + npts - 1                          
      DO j = 1, npts                              
        pn = ONE                                    
        pd = ONE                                  
        jj = i1+j-1                               
        DO k = 1, npts                            
          IF(k == j)CYCLE                       
          kk = i1+k-1                             
          pn = pn*(x_int(m)-x(kk))
          pd = pd*(x(jj)-x(kk))                   
        ENDDO                                     
        lp(m, j) = pn / pd  

      END DO                                      
    END DO

  END SUBROUTINE lpoly

!------------------------------------------------------------------
! Subroutine does the interpolation given the Lagrange Polynomials.
!
! Input arguments
!   y  :   Rang-1 array containing the original data
!          Type: Double
!          Dimension: nx
!  xlp :   Lagrange Polynomials
!          Type: Double
!          Dimension: nx_int x n_points
!  index_int :  Rank-1 array containing the indexes of the first
!               data point of the n_points data series
!               Dimension: nx_int
! Output arguments
!  y_int : Rank-1 array containing the results
!          Type: Double
!          Dimension: nx_int
!-------------------------------------------------------------------

  SUBROUTINE interp(y, xlp, index_int, y_int)
    REAL(Double), INTENT(IN)      :: y(:)
    REAL(Double), INTENT(IN)      :: xlp(:,:)
    INTEGER,      INTENT(IN)      :: index_int(:)
    REAL(Double), INTENT(OUT)     :: y_int(:)

    ! Local
    INTEGER :: npts, ny_int, i, k

    npts   = SIZE(xlp, DIM=2)
    ny_int = SIZE(y_int)

    y_int = ZERO
    DO k = 1,ny_int 
    DO i = 1,npts
      y_int(k) = y_int(k) + xlp(k,i)*y(index_int(k)+i-1)
    END DO
    END DO

  END SUBROUTINE interp

END MODULE Interpolation
