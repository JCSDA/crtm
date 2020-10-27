!
! Test_ApodFunction
!
! Program to test the FFT Spectral Utility ApodFunction procedure
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-Nov-2006
!                       paul.vandelst@noaa.gov

PROGRAM Test_ApodFunction

  ! Environment setup
  ! ...Module use
  USE Type_Kinds          , ONLY: fp=>fp_kind
  USE File_Utility        , ONLY: Get_Lun
  USE Message_Handler     , ONLY: SUCCESS, Program_Message
  USE FFT_Spectral_Utility, ONLY: ApodFunction     , &
                                  BARTLETT         , &
                                  WELCH            , &
                                  CONNES           , &
                                  COSINE           , &
                                  HAMMING          , &
                                  HANNING          , &
                                  NORTONBEER_WEAK  , &
                                  NORTONBEER_MEDIUM, &
                                  NORTONBEER_STRONG, &
                                  BLACKMANHARRIS_3 , &
                                  BLACKMANHARRIS_4 , &
                                  BLACKMANHARRIS_4M, &
                                  FFT_Spectral_UtilityVersion
  ! ...Disable all implicit typing
  IMPLICIT NONE

  ! Parameters
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_ApodFunction'
  CHARACTER(*),  PARAMETER :: PROGRAM_VERSION_ID = &
  INTEGER,  PARAMETER :: N = 1000
  INTEGER,  PARAMETER :: NHALF = N/2
  REAL(fp), PARAMETER :: MAXX = 1.0_fp
  REAL(fp), PARAMETER :: ONE = 1.0_fp
  INTEGER,  PARAMETER :: NAPOD = 12
  INTEGER,  PARAMETER :: IAPOD(NAPOD) = (/ BARTLETT         , &
                                           WELCH            , &
                                           CONNES           , &
                                           COSINE           , &
                                           HAMMING          , &
                                           HANNING          , &
                                           NORTONBEER_WEAK  , &
                                           NORTONBEER_MEDIUM, &
                                           NORTONBEER_STRONG, &
                                           BLACKMANHARRIS_3 , &
                                           BLACKMANHARRIS_4 , &
                                           BLACKMANHARRIS_4M /)
  CHARACTER(*), PARAMETER :: CAPOD(NAPOD) = (/ 'BARTLETT         ', &
                                               'WELCH            ', &
                                               'CONNES           ', &
                                               'COSINE           ', &
                                               'HAMMING          ', &
                                               'HANNING          ', &
                                               'NORTONBEER_WEAK  ', &
                                               'NORTONBEER_MEDIUM', &
                                               'NORTONBEER_STRONG', &
                                               'BLACKMANHARRIS_3 ', &
                                               'BLACKMANHARRIS_4 ', &
                                               'BLACKMANHARRIS_4M' /)

  ! Variables
  CHARACTER(256) :: Id
  INTEGER :: i, fileId
  REAL(fp), DIMENSION(N) :: x, y
  
  
  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the ApodFunction procedure.', &
                        '$Revision: $' )

  ! Version output
  CALL FFT_Spectral_UtilityVersion(Id)
  WRITE( *,'(a)' ) TRIM(Id)

  ! Create optical delay array
  x(NHALF:N)   = MAXX * (/(REAL(i,fp),i=0,NHALF)/) / REAL(NHALF,fp)
  x(1:NHALF-1) = -ONE * x(N-1:NHALF+1:-1)

  ! Open ASCII file for viewing results
  fileId=Get_Lun()
  OPEN(fileId,FILE='Test_ApodFunction.dat',STATUS='UNKNOWN')
  WRITE(fileId,*) NAPOD+1, N
  WRITE(fileId,*) x
  
  ! Call the function repeatedly
  WRITE(*, '(/5x,"Computing apodisation function...")')
  DO i=1,NAPOD
    WRITE(*, '(7x,a)') CAPOD(i)
    y=ApodFunction(n, apodType=i)
    WRITE(fileId,*) CAPOD(i)
    WRITE(fileId,*) y
  END DO
  
  ! Default option
  WRITE(*, '(7x,a)')'DEFAULT'
  y=ApodFunction(n)
  WRITE(fileId,*) 'DEFAULT'
  WRITE(fileId,*) y

  CLOSE(fileId)

END PROGRAM Test_ApodFunction
