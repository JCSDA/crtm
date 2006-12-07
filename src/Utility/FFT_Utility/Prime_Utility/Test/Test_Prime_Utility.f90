PROGRAM Test_Prime_Utility
  USE Prime_Utility
  IMPLICIT NONE
  ! Parameters
  INTEGER, PARAMETER :: NTEST=500
  INTEGER, PARAMETER :: NPFTEST=9438
  ! Variables
  INTEGER :: i, j, errStatus
  INTEGER :: prod
  TYPE(Prime_type) :: Prime

  WRITE(*,'(/5x,"Primes in range 1..",i0," derived via IsPrime:")') NTEST
  j=0
  DO i = 1, NTEST
    IF (IsPrime(i)) THEN
      j=j+1
      IF(MOD(j-1,10)==0 .AND. j/=1) WRITE(*,*)
      WRITE(*,'(1x,i5)',ADVANCE='NO') i
    END IF
  END DO

  Prime=Primes(NTEST)
  WRITE(*,'(//5x,"Primes in range 1..",i0," derived via Primes:")') Prime%number
  WRITE(*,'(10(1x,i5))') Prime%n
  errStatus = Destroy_Prime(Prime)

  Prime=PrimeFactor(NPFTEST)
  WRITE(*,'(//5x,"Prime factors for ",i0,":")') Prime%number
  prod=1
  DO i=1,Prime%nPrimes
    prod=prod*(Prime%n(i)**Prime%nExp(i))
    WRITE(*,'(i0,"^",i0)',ADVANCE='NO') Prime%n(i), Prime%nExp(i)
    IF(i < Prime%nPrimes) WRITE(*,'(" x ")',ADVANCE='NO')
  END DO
  WRITE(*,'(" = ",i0)')prod
  errStatus = Destroy_Prime(Prime)
  
END PROGRAM Test_Prime_Utility
