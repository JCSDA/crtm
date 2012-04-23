PROGRAM Get_SenInfo

  USE Type_Kinds, only : fp_kind
  USE Message_Handler
  USE SpcCoeff_Define
  USE SpcCoeff_netCDF_IO

  IMPLICIT none

  CHARACTER(256)      :: inFilename_spcCoef                     
  TYPE(SpcCoeff_Type) :: SpcCoeff  
  INTEGER             :: error_status                             


!!  WRITE(*, *)'Enter SpcCoeff filename: '                        
  READ(*, '(A)')inFilename_spcCoef                              

  !--- read spectral coefficients                               

  error_status = SpcCoeff_netCDF_ReadFile( inFilename_spcCoef, SpcCoeff, Quiet=.TRUE. )  

  if( error_status /= SUCCESS ) then                                   
    print *, '### ERROR ###'                                    
    print *, 'Fail to read a spectral coefficient file'         
    print *, 'File name: ', InFileName_SpcCoef                  
    stop 90                                                     
  endif                                                         

  WRITE(*, '(i3, 1x, i5)')SpcCoeff%Sensor_Type, SpcCoeff%n_Channels

END PROGRAM Get_SenInfo
