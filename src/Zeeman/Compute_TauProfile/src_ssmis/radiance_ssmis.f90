MODULE radiance_ssmis

  USE type_kinds
  USE transmittance_microwave_lbl
  USE ssmis_passband

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: radiance
  PUBLIC :: transmittance
  PUBLIC :: transmittance_lbl

  real(fp_kind), parameter :: ZERO = 0.0_fp_kind
  
  real(fp_kind), parameter :: THOUSAND = 1000.0_fp_kind

  INTEGER, PARAMETER       :: PASSBAND_SHAPE = SN2ffpb

CONTAINS

  function radiance  (ichan, secant_view_angle, &
                       z_level, p_lay, t_lay, w_lay, &   
                       Bfield, CBTH, delta_f, RCP) Result( rad )   

    integer,       intent(in)               :: ichan                                    
    real(fp_kind), intent(in)               :: secant_view_angle                    
    real(fp_kind), intent(in)               :: z_level(0:)                              
    real(fp_kind), intent(in)               :: p_lay(:), t_lay(:), w_lay(:) 
     
    real(fp_kind), optional, intent(in)     :: Bfield                                   
    real(fp_kind), optional, intent(in)     :: CBTH                                     
    real(fp_kind), optional, intent(in)     :: delta_f   ! frequency_shift GHz                                  
    integer, optional, intent(in)           :: RCP       ! Right-circlar polarization
                                                         ! if not present, the computation is
                                                         ! for left-circlar pol.                                  

    ! result
    real(fp_kind)                           :: rad, rad0

    real(fp_kind)    :: tau_dry_lbl(SIZE(p_lay),NFREQ_CHAN)                                 
    real(fp_kind)    :: tau_wet_lbl(SIZE(p_lay),NFREQ_CHAN)   
    real(fp_kind)    :: tau_total_lbl(SIZE(p_lay))                               
    integer          :: ifreq, ilay, nlay

    ! get the frequencies for computing channel transmittace                            
  
    IF(.NOT. loaded)THEN
!      CALL Load_passband(PASSBAND_SHAPE)
!      loaded = .true.
      print *, 'Error: the SRFs are not loaded'
      stop
    ENDIF

    call transmittance_lbl (freqs(:,ichan), secant_view_angle, &   
                            z_level, p_lay, t_lay, w_lay, &        
                            tau_dry_lbl, tau_wet_lbl, &            
                            Bfield, CBTH, &
                            delta_f=delta_f, RCP=RCP)                          

    nlay = SIZE(p_lay)
    rad = ZERO

    do ifreq = 1, NFREQ_CHAN

      tau_total_lbl = tau_dry_lbl(:, ifreq) * tau_wet_lbl(:, ifreq)

      ! check transmittance
      if(tau_total_lbl(1) < ZERO)then
        tau_total_lbl(1) = ZERO
      endif
      do ilay = 2, nlay
        if(tau_total_lbl(ilay) < ZERO)then
           tau_total_lbl(ilay) = ZERO
        else
           if(tau_total_lbl(ilay) > tau_total_lbl(ilay-1))then
              tau_total_lbl(ilay) = tau_total_lbl(ilay-1)
           endif
        endif
      enddo

      rad0 = (1.0_fp_kind - tau_total_lbl(1))*t_lay(1)
                  
      do ilay = 2, nlay
          rad0 = rad0 + (tau_total_lbl(ilay-1) - tau_total_lbl(ilay))*t_lay(ilay)
      enddo

      rad = rad + rad0*weights(ifreq, ichan)

    enddo

  end function radiance


  subroutine transmittance (ichan, secant_view_angle, &
                            z_level, p_lay, t_lay, w_lay, &
                            tau_total, tau_dry, tau_wet, &
                            Bfield, CBTH, delta_f, RCP)

    integer,       intent(in)               :: ichan                                    
    real(fp_kind), intent(in)               :: secant_view_angle                    
    real(fp_kind), intent(in)               :: z_level(0:)                              
    real(fp_kind), intent(in)               :: p_lay(:), t_lay(:), w_lay(:)            
    real(fp_kind), intent(out)              :: tau_total(:), tau_dry(:), tau_wet(:)            
    real(fp_kind), optional, intent(in)     :: Bfield                                   
    real(fp_kind), optional, intent(in)     :: CBTH 
    real(fp_kind), optional, intent(in)     :: delta_f   ! frequency_shift GHz                                  
    integer, optional, intent(in)           :: RCP       ! Right-circlar polarization
                                                         ! if not present, the computation is
                                                         ! for left-circlar pol.                                  

    real(fp_kind)    :: tau_dry_lbl(SIZE(p_lay),NFREQ_CHAN)                                 
    real(fp_kind)    :: tau_wet_lbl(SIZE(p_lay),NFREQ_CHAN)                                       
    integer          :: ifreq                                                           

    ! get the frequencies for computing channel transmittace                            

    IF(.NOT. loaded)THEN
!      CALL Load_passband(PASSBAND_SHAPE)
!      loaded = .true.
      print *, 'Error: the SRFs are not loaded'
      stop
    ENDIF
                                                                                      

    call transmittance_lbl (freqs(:,ichan), secant_view_angle, &   
                            z_level, p_lay, t_lay, w_lay, &        
                            tau_dry_lbl, tau_wet_lbl, &            
                            Bfield=Bfield, CBTH=CBTH, &
                            delta_f=delta_f, RCP=RCP)              

    tau_total(:) = ZERO                                                                 
    tau_dry(:)   = ZERO                                                                 
    tau_wet(:)   = ZERO                                                                 

    do ifreq = 1, NFREQ_CHAN                                              

      tau_total = tau_total + &
            tau_dry_lbl(:, ifreq)*tau_wet_lbl(:, ifreq)*weights(ifreq, ichan)      
      tau_dry = tau_dry + tau_dry_lbl(:, ifreq)*weights(ifreq, ichan)                                           
      tau_wet = tau_wet + tau_wet_lbl(:, ifreq)*weights(ifreq, ichan)                               

    enddo                                                                     

  end subroutine transmittance                                            

  subroutine transmittance_lbl (freqs, secant_view_angle, &
                                z_level, p_lay, t_lay, w_lay, &
			        tau_dry_lbl, tau_wet_lbl, &
			        Bfield, CBTH, delta_f, RCP)

    real(fp_kind), intent(in)               :: freqs(:)                                   
    real(fp_kind), intent(in)               :: secant_view_angle                   
    real(fp_kind), intent(in)               :: z_level(0:)                             
    real(fp_kind), intent(in)               :: p_lay(:), t_lay(:), w_lay(:) ! K           
    real(fp_kind), intent(out)              :: tau_dry_lbl(:,:)    ! K x L                   
    real(fp_kind), intent(out)              :: tau_wet_lbl(:,:)    ! K x L                          
    real(fp_kind), optional, intent(in)     :: Bfield                                  
    real(fp_kind), optional, intent(in)     :: CBTH                                    
    real(fp_kind), optional, intent(in)     :: delta_f   ! frequency_shift GHz                                  
    integer, optional, intent(in)           :: RCP       ! Right-circlar polarization
                                                         ! if not present, the computation is
                                                         ! for left-circlar pol.                                  

    ! local                                                                            
    real(fp_kind), dimension(SIZE(p_lay))   :: tau_dry_tmp, tau12_re, tau12_im, &  
                                               Bfield_lay, CBTH_lay
                         
    integer          :: ifreq, nfreq                                                        
    logical          :: zeeman_chan  
    real(fp_kind)    :: frequency(SIZE(freqs))                                       

    nfreq = SIZE( freqs )
    zeeman_chan = .false.
   
    ! check if it is a zeeman channel and if yes, set the flag                         
    if(PRESENT(Bfield) .and. PRESENT(CBTH))then                                        
       zeeman_chan = .true.                                                            
       ! assuming a constant megnetic field                                               
       Bfield_lay(:) = Bfield                                                             
       CBTH_lay(:) = CBTH                                                                 
    endif                                                                              

    ! shift frequencies if delta_f is present
    IF(PRESENT(delta_f))THEN
      frequency = freqs + delta_f
    ELSE
      frequency = freqs
    ENDIF
                                                                                    
    ! compute channel transmittances                                                   

    do ifreq = 1, nfreq                                                  

      if(zeeman_chan)then ! zeeman channel

        if(PRESENT(RCP))then

          ! --- tau_dry_lbl(:,ifreq) is the RC polarization
          call tauO2N2_zeeman_lbl(frequency(ifreq), secant_view_angle, &            
                                   z_level, p_lay, t_lay, w_lay, &              
                                   tau_dry_lbl(:,ifreq), tau_dry_tmp, &
                                   tau12_re, tau12_im, &
                                   Bfield=Bfield_lay, CBTH=CBTH_lay)
        else  
          ! --- tau_dry_lbl(:,ifreq) is the LC polarization
          call tauO2N2_zeeman_lbl(frequency(ifreq), secant_view_angle, &            
                                   z_level, p_lay, t_lay, w_lay, &              
                                   tau_dry_tmp, tau_dry_lbl(:,ifreq),  &
                                   tau12_re, tau12_im, &
                                   Bfield=Bfield_lay, CBTH=CBTH_lay)  
        endif

      else  ! none zeeman channel                                               

        call tauO2N2_lbl(frequency(ifreq), secant_view_angle, &                     
                         z_level, p_lay, t_lay, w_lay, &                        
                         tau_dry_lbl(:, ifreq))                                           

!        call tauO2N2_Liebe93_lbl(frequency(ifreq), secant_view_angle, &                       
!                       z_level, p_lay, t_lay, w_lay, tau_dry_lbl(:, ifreq))               

      endif                                                                     

      call tau_H2O_lbl(frequency(ifreq), secant_view_angle, &                       
                       z_level, p_lay, t_lay, w_lay, tau_wet_lbl(:, ifreq))               


    enddo                                                                        

  end subroutine transmittance_lbl
 
	  	  
end MODULE radiance_ssmis
	    
