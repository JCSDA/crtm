MODULE ssmis_passband

  USE type_kinds
  USE Message_Handler
  USE interpolate
  USE SRF_Define
  USE SRF_netCDF_IO

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: Load_passband
   
  INTEGER, PUBLIC, PARAMETER :: RECTANGULAR = 1            
  INTEGER, PUBLIC, PARAMETER :: SN2ffpb     = 2 
  INTEGER, PUBLIC, PARAMETER :: NetCDF      = 3
           
  CHARACTER(*), PARAMETER  :: FILE_SN2ffpb = 'SN2ffpb.dat'        
  LOGICAL, PUBLIC, SAVE    :: loaded = .false.                                                           


  REAL(fp_kind), PARAMETER :: ZERO = 0.0_fp_kind, ONE = 1.0_fp_kind, TWO = 2.0_fp_kind
                                                                 
  ! ------------------------------                                
  ! DMSP-16 SSMIS, band paramters                                 
  ! ------------------------------                                

  INTEGER, PUBLIC, PARAMETER :: MAX_N_SUBBANDS = 4                
  INTEGER, PUBLIC, PARAMETER :: N_SSMIS_CHANNELS = 24             

  ! -- SSMIS                                                      
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_SSMIS_CHANNELS ) :: &  
    SSMIS_N_SIDEBANDS = (/ 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, &        
                           2, 1, 1, 1, 1, 1, 2, 2, 2, 2, &        
                           4, 4, 4, 4 /)                          

  ! zeeman channel flag  1 
  integer, public, parameter :: ZEEMAN = 1
  integer, public, parameter :: zeeman_flag(N_SSMIS_CHANNELS) = (/ &
                                   0,0,0,0,0,  0,0,0,0,0,  &
                                   0,0,0,0,0,  0,0,0,1,1,  &
                                   1,1,1,1 /)

  ! frequency interval (GHz)
  integer, public, parameter    :: NFREQ_CHAN = 512  ! number of frequencies in a channel, a multiple of 8 

  REAL( fp_kind ), SAVE, PUBLIC :: freqs(NFREQ_CHAN, N_SSMIS_CHANNELS)
  REAL( fp_kind ), SAVE, PUBLIC :: weights(NFREQ_CHAN, N_SSMIS_CHANNELS)
  
  ! band center frequencies
  REAL( fp_kind ), PUBLIC, PARAMETER, &
                   DIMENSION( MAX_N_SUBBANDS, N_SSMIS_CHANNELS ) :: SSMIS_F16_PASSBAND_F0 = &
    RESHAPE( (/ 50.3000_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch1 
                52.8000_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch2 
                53.5960_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch3 
                54.4000_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch4 
                55.5000_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch5 
                57.2900_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch6 
                59.4000_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch7 
                148.750_fp_kind, 151.2500_fp_kind,   ZERO,              ZERO,            &      ! SSMIS ch8
                176.710_fp_kind, 189.9100_fp_kind,   ZERO,              ZERO,            &      ! SSMIS ch9
                180.310_fp_kind, 186.3100_fp_kind,   ZERO,              ZERO,            &      ! SSMIS ch10
                182.310_fp_kind, 184.3100_fp_kind,   ZERO,              ZERO,            &      ! SSMIS ch11
                19.3500_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch12
                19.3500_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch13
                22.2350_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch14
                37.0000_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch15
                37.0000_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch16
                90.7550_fp_kind,   92.5550_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch17
                90.7550_fp_kind,   92.5550_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch18
                62.9980_fp_kind,   63.5685_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch19
                60.4348_fp_kind,   61.1506_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch20
                60.4328_fp_kind,   60.4368_fp_kind,  61.1486_fp_kind,   61.1526_fp_kind, &      ! SSMIS ch21
                60.4293_fp_kind,   60.4403_fp_kind,  61.1451_fp_kind,   61.1561_fp_kind, &      ! SSMIS ch22
                60.4188_fp_kind,   60.4508_fp_kind,  61.1346_fp_kind,   61.1666_fp_kind, &      ! SSMIS ch23
                60.3848_fp_kind,   60.4848_fp_kind,  61.1006_fp_kind,   61.2006_fp_kind /), &   ! SSMIS ch24

             (/ MAX_N_SUBBANDS, N_SSMIS_CHANNELS /) )


CONTAINS

   SUBROUTINE Load_passband(passband_shape, sensor_id)
     INTEGER, INTENT(IN) :: passband_shape
     CHARACTER(*), OPTIONAL, INTENT(IN) :: sensor_id

     SELECT CASE( passband_shape )
       CASE( RECTANGULAR )
          CALL Load_rectangular_passband()
       CASE( SN2ffpb )
          CALL Load_SN2ffpb_passband()
       CASE( NetCDF )
          IF(.NOT. PRESENT(sensor_id))THEN
            print *, 'Error: the sensor_id must be specified'
            stop
          END IF
          CALL Load_NetCDF_passband(sensor_id)
       CASE DEFAULT
          print *, 'error: cannot find the passband shap'
          stop
     END SELECT

     loaded = .true.

   END SUBROUTINE Load_passband

   SUBROUTINE Load_NetCDF_passband(sensor_id)
     CHARACTER(*), INTENT(IN) :: sensor_id
     
     ! Local
     INTEGER        :: Error_Status
     CHARACTER(256) :: filename
     TYPE(SRF_type) :: SRF
     REAL(fp_kind)  :: df, sumw
     INTEGER        :: i, j, l, np_band, n1, n2, offset 
  
     filename = TRIM(sensor_id)//".srf.nc"
     DO l = 1, N_SSMIS_CHANNELS
       Error_Status = Read_SRF_netCDF(filename, l, SRF)
       IF(Error_Status /= SUCCESS)THEN
         print *, 'Error loading SRF data from the file: ', TRIM(filename)
         STOP
       END IF
       np_band = NFREQ_CHAN/SRF%n_Bands
       offset = 0
       DO i = 1, SRF%n_Bands
         df = (SRF%f2_band(i) - SRF%f1_band(i))/(np_band-1)
         DO j = 1, np_band
           freqs(j+offset, l) = SRF%f1_band(i) + (j-1)*df
         END DO
         n1 = offset + 1
         n2 = offset + np_band
         error_status = polynomial_interpolate( SRF%Frequency, SRF%Response, &
                                                freqs(n1:n2, l), weights(n1:n2,l)) 
         ! weights for linear variations in the two adjacent points. 
         weights(n1:n2,l) = weights(n1:n2,l)*df
         weights(n1,l) = weights(n1,l)/TWO
         weights(n2,l) = weights(n2,l)/TWO
         
         offset = offset + np_band
       END DO

       Error_Status = Destroy_SRF(SRF)
       IF(Error_Status /= SUCCESS)THEN
         print *, 'Error destroying the SRF structure ', TRIM(filename)
         STOP
       END IF

       sumw = SUM(weights(:,l))
       weights(:,l) = weights(:,l) / sumw
       
     END DO
     
   END SUBROUTINE Load_NetCDF_passband
     
          
   SUBROUTINE Load_rectangular_passband()

     !-------------------------------------------------------------
     ! Rectangular passband shapes
     !-------------------------------------------------------------

     ! -- SSMIS I/F bandwidths in GHz at F1 < F2 < F3 < F4

     REAL( fp_kind ), PARAMETER, &
                     DIMENSION( MAX_N_SUBBANDS, N_SSMIS_CHANNELS ) :: SSMIS_F16_BW = &
      RESHAPE( (/ 0.386300_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch1
                  0.385600_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch2
                  0.371300_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch3
                  0.375600_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch4
                  0.383100_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch5
                  0.333100_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch6
                  0.239400_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch7
                  1.648000_fp_kind,  1.648000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch8
                  1.530000_fp_kind,  1.530000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch9
                  1.017000_fp_kind,  1.017000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch10
                  0.517500_fp_kind,  0.517500_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch11
                  0.356300_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch12
                  0.358800_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch13
                  0.420600_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch14
                  1.578000_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch15
                  1.542000_fp_kind,              ZERO,  ZERO,              ZERO,            &      ! SSMIS ch16
                  1.432000_fp_kind,  1.432000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch17
                  1.401000_fp_kind,  1.401000_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch18
                  0.001340_fp_kind,  0.001360_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch19
                  0.001340_fp_kind,  0.001370_fp_kind,  ZERO,              ZERO,            &      ! SSMIS ch20
                  0.001260_fp_kind,  0.001230_fp_kind,  0.001330_fp_kind,  0.001330_fp_kind, &     ! SSMIS ch21
                  0.002620_fp_kind,  0.002610_fp_kind,  0.002660_fp_kind,  0.002670_fp_kind, &     ! SSMIS ch22
                  0.007010_fp_kind,  0.007170_fp_kind,  0.007400_fp_kind,  0.007440_fp_kind, &     ! SSMIS ch23
                  0.026630_fp_kind,  0.026330_fp_kind,  0.026040_fp_kind,  0.026880_fp_kind /), &  ! SSMIS ch24

               (/ MAX_N_SUBBANDS, N_SSMIS_CHANNELS /) )

     integer  :: i, j, k, nfreqs_perBand, ichan
     real(fp_kind) :: freq_band_start(4), freq_band_end(4), freq_interval


     DO ichan = 1, N_SSMIS_CHANNELS

       ! for both left and right side bands
       nfreqs_perBand = NFREQ_CHAN / SSMIS_N_SIDEBANDS(ichan)

       if(nfreqs_perBand*SSMIS_N_SIDEBANDS(ichan) /= NFREQ_CHAN)then
         print *, 'The size of freqs is not a mutiple of 8'
         stop
       endif

       do i = 1, SSMIS_N_SIDEBANDS(ichan)
         freq_band_start(i)   = SSMIS_F16_PASSBAND_F0(i, Ichan) - &
                                SSMIS_F16_BW(i, Ichan)*0.5
         freq_band_end(i)     = SSMIS_F16_PASSBAND_F0(i, Ichan) + &
                                SSMIS_F16_BW(i, Ichan)*0.5
       enddo
    
       k = 0
       do i = 1, SSMIS_N_SIDEBANDS(ichan)
         freq_interval = (freq_band_end(i) - freq_band_start(i)) / &
                         real(nfreqs_perBand, fp_kind)
         do j = 1, nfreqs_perBand
           k = k + 1
           freqs(k, ichan) = freq_band_start(i) + &
                        (real(j-1, fp_kind)+0.5_fp_kind)*freq_interval
         enddo
       enddo 

       weights = ONE/REAL(NFREQ_CHAN, fp_kind)

     ENDDO

   END SUBROUTINE Load_rectangular_passband         

   SUBROUTINE Load_SN2ffpb_passband()

     integer  :: i, j, k, jj, n, n1, nfreqs_perBand, ichan, idummy, &
                 order_index(10), nps(4), ipb 
     real(fp_kind) :: fr(300, 4), weight0(300, 4), f0(300), ff(300), &
                      bw(300), freq_interval 
     integer, parameter :: fid = 22
     integer :: status

     OPEN(fid, file=FILE_SN2ffpb, status='old')

     DO ichan=1, N_SSMIS_CHANNELS

       ! for both left and right side bands
       nfreqs_perBand = NFREQ_CHAN / SSMIS_N_SIDEBANDS(ichan)

       ! re-order the passbands so that the frequency is in ascending order
       order_index(1:2) = (/1, 2/)
       IF(SSMIS_N_SIDEBANDS(ichan) > 2)THEN
         order_index(1:4) = (/1, 3, 2, 4/)
       ENDIF

       ! read in data
       DO ipb=1, SSMIS_N_SIDEBANDS(ichan)
         READ(fid, *)idummy, idummy, f0(ipb), bw(ipb), nps(ipb)
         READ(fid, *)fr(1:nps(ipb), ipb)
         READ(fid, *)weight0(1:nps(ipb), ipb)
       ENDDO

       n = 0
       DO ipb=1, SSMIS_N_SIDEBANDS(ichan)
         j = order_index(ipb)
         k = nps(j)

         ! convert relative frequencies to absolute ones
         ff(1:k) = SSMIS_F16_PASSBAND_F0(ipb, ichan) + &
                        0.5*bw(j)*fr(1:k, j)*0.001
         ! check if equal interval
         if(ANY(ABS( ABS(ff(2:k) - ff(1:k-1)) - ABS(ff(2) - ff(1)) ) > 0.0001))then
            print *, 'Error: unequal frequency interval found'
            stop
         endif

         ! interpolate ff on new freqency grids         
         freq_interval = (ff(k) - ff(1)) / &
                         real(nfreqs_perBand, fp_kind)
         n1 = n + 1
         do jj = 1, nfreqs_perBand
           n = n + 1
           freqs(n, ichan) = ff(1) + &
                        (real(jj-1, fp_kind)+0.5_fp_kind)*freq_interval
         enddo

         status = polynomial_interpolate( ff(1:k),weight0(1:k, j), &
                       freqs(n1:n, ichan),weights(n1:n, ichan) )
         weights(n1:n, ichan) = weights(n1:n, ichan)*ABS(freq_interval)
                       
       ENDDO
       weights(:, ichan) = weights(:, ichan) / SUM(weights(:, ichan))


     ENDDO

     CLOSE(fid)

   END SUBROUTINE Load_SN2ffpb_passband


END MODULE ssmis_passband
