!------------------------------------------------------------------------------
!M+
! NAME:
!       Cat_ODPS
!
! PURPOSE:
!       Concatenate two ODPS TauCoeff files in netCDF or binary format to a third file 
!       also in same format
!
! CATEGORY:
!       generate transmittance coefficients
!
! LANGUAGE:
!       Fortran-90
!
! FILES ACCESSED:
!       Input:  netCDF file 1 containing transmittance coefficients
!               netCDF file 2 containing transmittance coefficients
!
!       Output: netCDF or binary file containing merged transmittance coefficients
!
!------------------------------------------------------------------------------

PROGRAM Cat_ODPS

  ! ------------
  ! Module usage
  ! ------------

  USE Message_handler
!  Use TauCoeff_netCDF_IO
  USE ODPS_Define,  ONLY : TauCoeff_type => ODPS_type, &
                           Concatenate_Channel_TauCoeff => Concatenate_Channel_ODPS, &
                           Concatenate_Absorber_TauCoeff => Concatenate_Absorber_ODPS
  USE ODPS_netCDF_IO, ONLY : Read_TauCoeff_netCDF => Read_ODPS_netCDF, &
                             Write_TauCoeff_netCDF => Write_ODPS_netCDF
 
  IMPLICIT NONE

    ! ----------
    ! Parameters
    ! ----------
 
 
    character(256) :: file1, file2, file_out
  
    TYPE( TauCoeff_type )  :: TauCoeff1, TauCoeff2
       
!    character(2048) :: History
!    character(2048) :: Title, Comment, ID_Tag
!    character(64)   :: Sensor_Name, Platform_Name
    CHARACTER(80)     :: ID_Tag
    CHARACTER(256)    :: Title
    CHARACTER(3000)   :: History
    CHARACTER(3000)   :: Comment

    integer :: cat_type, Error_Status
  
    print *, 'Enter in filename #1: '
    read(*, '(A)') file1
    
    print *, 'Enter in filename #2: '
    read(*, '(A)') file2
    
    print *, 'Enter out filename: '
    read(*, '(A)') file_out
    
    print *, 'Enter type of concatenation (= 1, along absorbers; = 2 along channels) : '
    read(*, '(i3)') cat_type

    !--- read Tau coefficient data
    
    Error_Status = Read_TauCoeff_netCDF( file1, TauCoeff1,        &
                                         Title=Title,             &  
                                         History=History,         &  
                                         Comment=Comment,         &  
                                         Profile_Set_ID =ID_Tag )  
     
    
    if ( Error_Status /= SUCCESS ) then

      print *, 'Error in reading the file '//file1
      stop 90
      
    endif      


    Error_Status = Read_TauCoeff_netCDF( file2, TauCoeff2 ) 

    if ( Error_Status /= SUCCESS ) then

      print *, 'Error in reading the file '//file2
      stop 90
      
    endif      
  
    select case (cat_type)
    
      case (1) 
    
        Error_Status = Concatenate_Absorber_TauCoeff( TauCoeff1, TauCoeff2 )
      
      case (2)
          
        Error_Status = Concatenate_Channel_TauCoeff( TauCoeff1, TauCoeff2 )
      
      case default
      
        print *, "wrong concatenation type: ", cat_type
        stop 90

    end select
        
    if ( Error_Status /= SUCCESS ) then

      print *, 'Error in merging the two files: '//TRIM(file1)//' and '//TRIM(file2) 
      stop 90
      
    endif
    
    Error_Status = Write_TauCoeff_netCDF( file_out,                      &  
                                          TauCoeff1,                     &
                                          Title         = TRIM(Title)  , &
                                          History       = TRIM(History), &
                                          Comment       = TRIM(Comment), &
                                          Profile_Set_ID= TRIM(ID_Tag) )
 
    if ( Error_Status /= SUCCESS ) then

      print *, 'Error in writing ot the file: '//file_out 
      stop 90
      
    endif     

    print *, 'Normal End'
    
end program Cat_ODPS
    
