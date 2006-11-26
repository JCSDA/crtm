PROGRAM Test_ApodFunction
  USE Type_Kinds          , ONLY: fp=>fp_kind
  USE File_Utility        , ONLY: Get_Lun
  USE Message_Handler     , ONLY: SUCCESS
  USE FFT_Spectral_Utility, ONLY: ApodFunction , &
                                  BARTLETT_APOD, &
                                  WELCH_APOD   , &
                                  CONNES_APOD  , &
                                  COSINE_APOD  , &
                                  HAMMING_APOD , &
                                  HANNING_APOD
  IMPLICIT NONE

  INTEGER,  PARAMETER :: N = 1000
  INTEGER,  PARAMETER :: NHALF = N/2
  REAL(fp), PARAMETER :: MAXX = 1.0_fp
  REAL(fp), PARAMETER :: ONE = 1.0_fp
  INTEGER,  PARAMETER :: NAPOD=6
  INTEGER,  PARAMETER :: IAPOD(NAPOD) = (/ BARTLETT_APOD, &
                                           WELCH_APOD   , &
                                           CONNES_APOD  , &
                                           COSINE_APOD  , &
                                           HAMMING_APOD , &
                                           HANNING_APOD  /)
  CHARACTER(*), PARAMETER :: CAPOD(NAPOD) = (/ 'BARTLETT_APOD', &
                                               'WELCH_APOD   ', &
                                               'CONNES_APOD  ', &
                                               'COSINE_APOD  ', &
                                               'HAMMING_APOD ', &
                                               'HANNING_APOD ' /)

  INTEGER :: i, fileId
  REAL(fp), DIMENSION(N) :: x, y
  
  ! Create optical delay array
  x(NHALF:N)   = maxX * (/(REAL(i,fp),i=0,NHALF+1)/) / REAL(NHALF,fp)
  x(1:NHALF-1) = -ONE * x(N-1:NHALF+1:-1)

  ! Open ASCII file for viewing results
  fileId=Get_Lun()
  OPEN(fileId,FILE='Test_ApodFunction.dat',STATUS='UNKNOWN')
  WRITE(fileId,*) NAPOD+1, N
  WRITE(fileId,*) x
  
  ! Call the function repeatedly
  DO i=1,NAPOD
    WRITE(*,*)CAPOD(i)
    y=ApodFunction(n, apodType=i)
    WRITE(fileId,*) CAPOD(i)
    WRITE(fileId,*) y
  END DO
  
  ! Default option
  WRITE(*,*)'DEFAULT'
  y=ApodFunction(n)
  WRITE(fileId,*) 'DEFAULT'
  WRITE(fileId,*) y

  CLOSE(fileId)

END PROGRAM Test_ApodFunction
