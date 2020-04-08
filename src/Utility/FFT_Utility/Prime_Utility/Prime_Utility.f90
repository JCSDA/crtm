MODULE Prime_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds     , ONLY: fp=>fp_kind
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Prime_Define   , ONLY: Prime_type      , &
                             Associated_Prime, &
                             Destroy_Prime   , &
                             Allocate_Prime  , &
                             Assign_Prime    , &
                             Equal_Prime     , &
                             Info_Prime
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities  
  ! ------------
  PRIVATE
  ! Prime data structure definition inherited from Prime_Define
  PUBLIC :: Prime_type
  ! Structure procedures inherited from Prime_Define
  PUBLIC :: Associated_Prime
  PUBLIC :: Destroy_Prime
  PUBLIC :: Allocate_Prime
  PUBLIC :: Assign_Prime
  PUBLIC :: Equal_Prime
  PUBLIC :: Info_Prime
  ! Module procedure
  PUBLIC :: IsPrime
  PUBLIC :: Primes
  PUBLIC :: PrimeFactor
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),PARAMETER :: MODULE_RCS_ID = &


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Specification function for the size of the possible
  ! multiples list automatic array in IsPrime
  PURE FUNCTION mSize(n)
    INTEGER, INTENT(IN) :: n
    INTEGER :: mSize
    mSize = INT(SQRT(REAL(n,fp)))
  END FUNCTION mSize

  ! Subroutine to swap integer values
  SUBROUTINE Swap(i,j)
    INTEGER, INTENT(IN OUT) :: i,j
    INTEGER :: k
    k=i; i=j; j=k
  END SUBROUTINE Swap




!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  FUNCTION IsPrime(n)
    ! Arguments
    INTEGER, INTENT(IN) :: n
    ! Function result
    LOGICAL :: IsPrime
    ! Local variables
    INTEGER :: multiples(mSize(n))
    INTEGER :: i, nMultiples

    ! Set up
    IsPrime = .FALSE.
    
    ! Handle edge cases of 1 and 2
    IF ( n == 1 ) RETURN ! Not a prime
    IF ( n == 2 ) THEN
      IsPrime = .TRUE.
      RETURN
    END IF
    
    ! Create list of candidate multiples of n, p_i, such
    ! that p_i <= SQRT(n)
    multiples = (/ (i,i=2,mSize(n)+1) /)

    ! Is it a prime?
    nMultiples = COUNT( MOD(n,multiples) == 0 )
    IF ( nMultiples == 0 ) IsPrime = .TRUE.
    
  END FUNCTION IsPrime


  FUNCTION Primes(n) RESULT(Prime)
    ! Arguments
    INTEGER, INTENT(IN) :: n
    ! Function result
    TYPE(Prime_type) :: Prime
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Primes'
    ! Local variables
    INTEGER :: errStatus
    INTEGER :: listA(n-1,2), idx(n-1)
    INTEGER :: listB(n-1)
    INTEGER :: i, i1,i2, nR, nP, nNM
    
    ! Create list of candidate primes, p_i, such
    ! that 2 <= p_i <= n
    listA(:,1) = (/(i,i=2,n)/); i1=1  ! Each iteration will bounce
    listA(:,2) = 0            ; i2=2  ! between these lists
    
    ! Initialise the "found" primes list
    nP=1
    listB(nP) = 2
    
    ! Initialise the count of candidate numbers remaining to check
    nR=n-1

    ! Remove all multiples of listB from listA
    Sieve_Loop: DO
    
      ! Count the multiples of the current prime number,
      ! listB(nP), in listA
      nNM = COUNT( MOD(listA(1:nR,i1),listB(nP)) /= 0 )
      
      ! If none, then we've found them all
      IF (nNM == 0) EXIT Sieve_Loop
      
      ! Determine the indices of the non-multiples of listB(nP)
      idx(1:nNM) = PACK( (/(i,i=1,nR)/), &
                         MOD(listA(1:nR,i1),listB(nP)) /= 0 )
      
      ! Only keep the non-multiples of listB(nP)
      listA(1:nNM,i2) = listA(idx(1:nNM),i1)
      
      ! Update array indices
      nR=nNM   ! The new size of the array of candidate primes
      nP=nP+1  ! The new size of the array of found primes
      
      ! Save the current found prime number
      listB(nP)=listA(1,i2)
      
      ! Swap the indices referencing the current
      ! list of candidate prime numbers
      CALL Swap(i1,i2)
    END DO Sieve_Loop 
    
    ! Allocate and fill the return structure
    errStatus = Allocate_Prime(nP,Prime)
    IF (errStatus /= SUCCESS ) THEN
      CALL Display_Message(ROUTINE_NAME, &
                           'Error allocating Prime return structure', &
                           FAILURE )
      RETURN
    END IF
    Prime%number = n
    Prime%n      = listB(1:nP)

  END FUNCTION Primes


  FUNCTION PrimeFactor(n) RESULT(Factors)
    ! Arguments
    INTEGER, INTENT(IN) :: n
    ! Function result
    TYPE(Prime_type) :: Factors
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PrimeFactor'
    ! Local variables
    INTEGER :: errStatus
    INTEGER :: i, j
    INTEGER :: nF, idx(1)
    INTEGER :: nFactors
    TYPE(Prime_type) :: Prime
    TYPE(Prime_type) :: f
    
    ! Get the list of candidate primes to test
    Prime=Primes(n)

    ! Allocate the local factors structure to hold the candidates
    errStatus = Allocate_Prime(Prime%nPrimes,f)
    IF (errStatus /= SUCCESS ) THEN
      CALL Display_Message(ROUTINE_NAME, &
                           'Error allocating f structure', &
                           FAILURE )
      RETURN
    END IF
    f%number = n
    f%nExp   = 0

    ! Begin the prime factor search    
    i=1
    nFactors=0
    Factor_Search: DO
      Prime_Search: DO
        IF (MOD(f%number,Prime%n(i)) == 0 ) EXIT Prime_Search
        i=i+1 
      END DO Prime_Search
      ! O.k., we have a prime factor
      ! Do we already have it in the list?
      nF = COUNT( f%n == Prime%n(i) )
      IF (nF == 0) THEN
        ! It doesn't exist, so add it
        nFactors=nFactors+1
        f%n(nFactors)    = Prime%n(i)
        f%nExp(nFactors) = f%nExp(nFactors) + 1
      ELSE
        ! It does exist, so just increment the exponent counter
        idx = PACK( (/(j,j=1,f%nPrimes)/), &
                    f%n == Prime%n(i) )
        f%nExp(idx(1)) = f%nExp(idx(1)) + 1
      END IF
      ! Update the integer
      f%number = f%number/Prime%n(i)
      ! If the number is 1, we're done
      IF ( f%number == 1 ) EXIT Factor_Search
    END DO Factor_Search
    
    ! Allocate and fill the return structure
    errStatus = Allocate_Prime(nFactors,Factors)
    IF (errStatus /= SUCCESS ) THEN
      CALL Display_Message(ROUTINE_NAME, &
                           'Error allocating return Factors structure', &
                           FAILURE )
      RETURN
    END IF
    Factors%number = n
    Factors%n      = f%n(1:nFactors)
    Factors%nExp   = f%nExp(1:nFactors)
    
    ! Destroy the local f structure
    errStatus = Destroy_Prime(f)
    IF (errStatus /= SUCCESS ) THEN
      CALL Display_Message(ROUTINE_NAME, &
                           'Error destroying local f structure', &
                           WARNING )
    END IF
    
    ! Destroy the local Prime structure
    errStatus = Destroy_Prime(Prime)
    IF (errStatus /= SUCCESS ) THEN
      CALL Display_Message(ROUTINE_NAME, &
                           'Error destroying local Prime structure', &
                           WARNING )
    END IF
  END FUNCTION PrimeFactor
  
END MODULE Prime_Utility
