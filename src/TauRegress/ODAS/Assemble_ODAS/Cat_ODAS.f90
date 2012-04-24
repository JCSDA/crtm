!------------------------------------------------------------------------------
!M+
! NAME:
!       Cat_ODAS
!
! PURPOSE:
!       Concatenate two ODAS TauCoeff files in netCDF format to a third file 
!       also in netCDF format
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
!               netCDF LBLRTM data files
!
!       Output: netCDF file containing merged transmittance coefficients
!
!------------------------------------------------------------------------------

PROGRAM Cat_ODAS

  ! ------------
  ! Module usage
  ! ------------

  USE Message_handler
  USE TauCoeff_Define
  Use TauCoeff_netCDF_IO
   
  IMPLICIT NONE

    ! ----------
    ! Parameters
    ! ----------
 
 
    character(256) :: nc_file1, nc_file2, nc_file_out
  
    TYPE( TauCoeff_type )  :: TauCoeff1, TauCoeff2
       
    character(2048) :: History
    character(2048) :: Title, Comment, ID_Tag
    character(64)   :: Sensor_Name, Platform_Name

    integer :: cat_type, Error_Status
  
    print *, 'Enter in filename #1: '
    read(*, '(A)') nc_file1
    
    print *, 'Enter in filename #2: '
    read(*, '(A)') nc_file2
    
    print *, 'Enter out filename: '
    read(*, '(A)') nc_file_out
    
    print *, 'Enter type of concatenation (= 1, along absorbers; = 2 along channels) : '
    read(*, '(i3)') cat_type
            

    !--- read Tau coefficient data
    Error_Status = Read_TauCoeff_netCDF( nc_file1, TauCoeff1, &

                                    Title=Title,             &  
                                    History=History,           &  
                                    Sensor_Name=Sensor_Name,       &  
                                    Platform_Name=Platform_Name,     &  
                                    Comment=Comment,           &  
                                    ID_Tag=ID_Tag )  
    
    
    if ( Error_Status /= SUCCESS ) then

      print *, 'Error in reading the file '//nc_file1
      stop 90
      
    endif      


    Error_Status = Read_TauCoeff_netCDF( nc_file2, TauCoeff2 ) 

    if ( Error_Status /= SUCCESS ) then

      print *, 'Error in reading the file '//nc_file2
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

      print *, 'Error in merging the two files: '//nc_file1//' and '//nc_file2 
      stop 90
      
    endif      
    
    Error_Status = Write_TauCoeff_netCDF( nc_file_out,   &  
                                  TauCoeff1,      &  
                                  Title=Title,         & 
                                  History=History,       & 
                                  Sensor_Name=Sensor_Name,   & 
                                  Platform_Name=Platform_Name, & 
                                  Comment=Comment,       & 
                                  ID_Tag=ID_Tag )

    if ( Error_Status /= SUCCESS ) then

      print *, 'Error in writing ot the file: '//nc_file_out 
      stop 90
      
    endif      

    print *, 'Normal End'
    
     
end program Cat_ODAS
    
