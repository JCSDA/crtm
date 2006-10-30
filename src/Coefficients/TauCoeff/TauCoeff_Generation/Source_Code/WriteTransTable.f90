!
! Output GDAS transmittance coefficient table
!

module WriteTransTable

  !--- Modules

  use type_kinds, only : fp_kind
  use Message_Handler
  use ParametersGenCoef
  use TauCoeff_Define
  use TauCoeff_netCDF_IO


  !--- Implicit

  implicit none


  !--- Public & Private

  Private

  Public  Write_TransTable_netCDF

 contains


  !====================================================================
  ! Output Transmittance Table in netCDF format
  ! 
  !====================================================================

  subroutine Write_TransTable_netCDF( Npredcomb, predcomb, Npolyorder, coef,  RCS_Id)

    integer      ,intent(in) :: Npredcomb(Nchan)
    integer      ,intent(in) :: predcomb(Natmpred_maxused,Nchan)
    integer      ,intent(in) :: Npolyorder(Nchan)
    real(fp_kind),intent(in) :: coef(Ncoef_max,Nchan)
    ! -- Revision control
    character( * ), intent(in) :: RCS_Id


    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_TransTable_netCDF'

    integer       :: Ichan, Ipred, Mchan, k, i, startp
    integer       :: useflag(Natmpred_max)
    
    INTEGER :: Error_Status

    TYPE( TauCoeff_type )  :: TauCoeff
    
    character(*), parameter   :: TauCoeff_Title = "upwelling transmittance coefficients"
    character(*), parameter   :: TauCoeff_Comment = " "
    
    integer            :: mapping(17)

    !index offsets for Integrated predictors: offs(1) - wet, offs(2) - dry
    !offs(3) - ozo. 
    integer, parameter :: offs(3) = (/17, 11, 23/)     
	    
    
    !--- Initialize ( Nullify )  and allocate memory for pointers
    
    Call Initialize_TauCoeff(TauCoeff)
    
    Mchan = Ichan_last - Ichan_top + 1
    
    Error_Status = Allocate_TauCoeff( Npolyorder_max,       &
                                      Natmpred_maxused, &
				      1,            & ! Number of absorbers 
				      Mchan, 		&  
                                      TauCoeff )
				      
    if ( Error_Status /= SUCCESS ) then
      CALL display_message( ROUTINE_NAME,    &
                            'Error in allocating memory for TauCoeff', &
                            Error_Status    &
                            )    
      stop 90
      
    endif				      
    
    !--- Fill the structure TauCoeff
    
    TauCoeff%NCEP_Sensor_ID(:) = SenIdNcep
    TauCoeff%WMO_Satellite_ID(:) = SatIdWmo
    TauCoeff%WMO_Sensor_ID(:) = SenIdWmo
    TauCoeff%Sensor_Channel(:) = channel_list(Ichan_top:Ichan_last)
    TauCoeff%Absorber_ID(1) =  gasID_convert(Iabsorber)    
    TauCoeff%Alpha(1) = Alpha
    TauCoeff%Alpha_c1(1) = Abslvl_coef1
    TauCoeff%Alpha_c2(1) = Abslvl_coef2

    TauCoeff%C = ZERO

    k = 0
    do Ichan = Ichan_top, Ichan_last 

      k = k + 1
      
      !--- store both polyoder and Npred in the same integer cell
      
      TauCoeff%Order_Index(0:,1,k) = Npolyorder(Ichan)
      TauCoeff%Predictor_Index(0,1,k) = Npredcomb(Ichan)      

      mapping = (/1,2,3,4,5,6,7,8,9,10,11,&
                    offs(Iabsorber)+1,offs(Iabsorber)+2,&
		    offs(Iabsorber)+3,offs(Iabsorber)+4,&
		    offs(Iabsorber)+5,offs(Iabsorber)+6/)
		    		    
      do i = 1, Natmpred_maxused
      
        if ( predcomb(i,Ichan) == 0 )then
	
	  TauCoeff%Predictor_Index(i,1,k) = 0
	
	else  
        
	  TauCoeff%Predictor_Index(i,1,k) = mapping(predcomb(i,Ichan))

        endif
	
      enddo 

      do Ipred=0, Npredcomb(Ichan)
      
        startp = (Npolyorder(Ichan)+1)*Ipred + 1
        
        TauCoeff%C(0:Npolyorder(Ichan),Ipred,1,k) = &
            coef(startp:(startp+Npolyorder(Ichan)),Ichan)

      enddo
      
    enddo
    
    !--- write out TauCoeff

    History_all = TRIM(RCS_Id)//TRIM(History_all)

    Error_Status = Write_TauCoeff_netCDF( &
                                  OutFileName_TransCoeff, &
                                  TauCoeff, &
				  Title = TauCoeff_Title, &
				  History = History_all, &
				  Sensor_Name = SenName, &
				  Platform_Name = SatName, &
				  Comment = TauCoeff_Comment, &
				  ID_Tag = ProfileTag )

    if ( Error_Status /= SUCCESS ) then
      CALL display_message( ROUTINE_NAME,    &
                            'Error in writing out TauCoeff', &
                            Error_Status  )    
      stop 90
      
    endif				      


    !--- write atmos pred availability flags set 1 for selected

    do Ichan = Ichan_top, Ichan_last
      useflag(:) = 0
      do Ipred = 1, Npredcomb(Ichan)
        useflag(predcomb(Ipred,Ichan)) = 1
      enddo
      
      write(OutAtmPred,'(i3,4x,9(1x,5i2))') Ichan, useflag(:)
    enddo
    
    !--- print table
 
    print *
    print *, '*** Transmittance coefficients ***'

    k = 0
    do Ichan = Ichan_top, Ichan_last

      k=k+1
      
      print *
      print *, 'Channel (Seq #, Ch #) : ', Ichan, channel_list(Ichan)

      print *, 'Predictors :', Npredcomb(Ichan), ' :', predcomb(:,Ichan) 

      do Ipred = 0, Npredcomb(Ichan)
        print '(a,20e11.3)', 'Coef:', TauCoeff%C(0:Npolyorder(Ichan),Ipred,1,k)
      enddo

    enddo

    Error_Status = Destroy_TauCoeff( TauCoeff )

    if ( Error_Status /= SUCCESS ) then
      CALL display_message( ROUTINE_NAME,    &
                            'Error in deallocating TauCoeff', &
                            Error_Status )    
      stop 90
      
    endif				      

  end subroutine Write_TransTable_netCDF

end module WriteTransTable

!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Date: 2003/05/23, Name: Yong Han
!
!  --- Add subroutine Write_TransTable_netCDF to output Tau coeff. table in 
!      netCDF format
!
!
