!
! Apodize_RadSpc_with_IRF
!
! Program to Fourier transform radiance spectra in to the interferometric
! domain, apodize the resultant interferogram (IFG) with an interferometer
! instrument response function (IRF) and then Fourier transform the result
! back into the spectral domain.
!
! Currently set-up for IASI and CrIS processing only.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Feb-2007
!                       paul.vandelst@ssec.wisc.edu
!       Modified by:    Yong Chen, CIRA/JCSDA 23-Sep-2008
!                       Yong.Chen@noaa.gov 
!       Modified for radiance spectrum:  Yong Han, June 2010
!       

PROGRAM Apodize_TauSpc_with_IRF

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, &
                                       Program_Message, Display_Message
  USE LBLRTM_netCDF_IO         , ONLY: Create_LBLRTM_netCDF, &
                                       Inquire_LBLRTM_netCDF, &
                                       Read_LBLRTM_netCDF, &
                                       Write_LBLRTM_netCDF
  USE FFT_Spectral_Utility     , ONLY: CosFilter, &
                                       SPCtoIFG, &
                                       IFGtoSPC, &
                                       ComputeNPoints, &
                                       ComputeIndex, &
                                       ComputeNextPO2, &
                                       ComputeNSPC, &
                                       ComputeNIFG, &
                                       ComputeX
  USE IASI_Define              , ONLY: IASI_MIN_FREQUENCY, &
                                       IASI_MAX_FREQUENCY, &
                                       IASI_D_FREQUENCY, &
                                       IASI_RESAMPLE_MAXX, &
                                       N_IASI_CHANNELS, &
                                       N_IASI_BANDS, &
                                       IASI_BAND_F1, &
                                       IASI_BAND_F2, &
                                       IASI_F, &
                                       IASI_GFT, &
                                       IASI_Channels
  USE CRIS_Define              , ONLY: CRIS_MIN_FREQUENCY, & 
                                       CRIS_MAX_FREQUENCY, & 
                                       CRIS_D_FREQUENCY, &   
                                       CRIS_RESAMPLE_MAXX, & 
                                       N_CRIS_CHANNELS, &    
                                       N_CRIS_BANDS, &       
                                       CRIS_BAND_F1, &       
                                       CRIS_BAND_F2, &       
                                       CRIS_F, &             
                                       CRIS_GFT, &           
                                       CRIS_Channels         
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Apodize_TauSpc_with_IRF'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  ! Rollof filter width
  REAL(fp), PARAMETER :: DEFAULT_FILTER_WIDTH = 20.0_fp
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: ONEpointFIVE = 1.5_fp
  ! Sensor id
!  CHARACTER(*), PARAMETER :: SENSOR_ID        = 'cris_npp'
!  INTEGER,      PARAMETER :: WMO_SENSOR_ID    = 221
!  INTEGER,      PARAMETER :: WMO_SATELLITE_ID = 4
  ! The spectral bandwidth, 0-f(Nyquist), definitions
!  REAL(fp), PARAMETER :: MASTER_F1 = ZERO
!  REAL(fp), PARAMETER :: MASTER_F2 = 6912.0_fp
  ! relate to FFT in LBLRTM spectrum for df=0.001
  ! No. of truncated IFG points for IFG->SPC should be Prime Factor
  ! for CRIS band , resolution, IFG points, Prime Factors Decomposition  
  !           1       0.625      20480      2^12 * 5
  !           2       1.250      10240      2^11 * 5
  !           3       2.500       5120      2^10 * 5
!  REAL(fp), PARAMETER :: MASTER_F2 = 6400.0_fp
!  REAL(fp), PARAMETER :: F_NYQUIST = MASTER_F2

  ! TauProfile version number
  INTEGER, PARAMETER :: TAUPROFILE_VERSION = 2

  INTEGER, PARAMETER :: N_SENSOR_SETS = 2

  CHARACTER(*), PARAMETER,  DIMENSION(N_SENSOR_SETS) :: &
                SENSOR_ID=( / 'iasi_metop-a', & 
                              'cris_npp    ' /)
  INTEGER,  PARAMETER,  DIMENSION(N_SENSOR_SETS) :: Sensor_idx = (/1, 2/)
  INTEGER,  PARAMETER,  DIMENSION(N_SENSOR_SETS) :: WMO_SENSOR_ID    = (/221, 620/)
  INTEGER,  PARAMETER,  DIMENSION(N_SENSOR_SETS) :: WMO_SATELLITE_ID = (/4, 224/)
  REAL(fp), PARAMETER,  DIMENSION(N_SENSOR_SETS) :: MASTER_F1 = (/ZERO, ZERO/)
  REAL(fp), PARAMETER,  DIMENSION(N_SENSOR_SETS) :: MASTER_F2 = (/6912.0_fp, 6400.0_fp/)
  REAL(fp), PARAMETER,  DIMENSION(N_SENSOR_SETS) :: F_NYQUIST = MASTER_F2
  INTEGER,  PARAMETER,  DIMENSION(N_SENSOR_SETS) :: N_BANDS = (/3, 3/)
  
 
 
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(2000) :: LBLRTM_History
  CHARACTER(2000) :: LBLRTM_Comment
  CHARACTER(2000) :: LBLRTM_Id_Tag
  CHARACTER(2000) :: Comment
  CHARACTER(256) :: Answer
  CHARACTER(256) :: Message
  CHARACTER(256) :: LBL_Filename
  CHARACTER(80)  :: cBand, sBand
  INTEGER  :: iBand, iSensor
  INTEGER  :: Error_Status
  INTEGER  :: IO_Status
  INTEGER  :: Allocate_Status
  INTEGER  :: i, j, ilayer
  INTEGER  :: is1, is2
  INTEGER  :: if1, if2
  INTEGER  :: ib1, ib2
  INTEGER  :: it1, it2
  INTEGER  :: ic1, ic2
  INTEGER  :: n_filter
  INTEGER  :: n_ispc
  INTEGER  :: n_spc , n_ifg
  INTEGER  :: n_tspc, n_tifg
  INTEGER  :: n_cspc
  INTEGER  :: n_lbl_layers, dirn
  INTEGER  :: idx(1)
  INTEGER, PARAMETER  :: fid_out = 101
  REAL(fp) :: bf, ef, df
  REAL(fp) :: f1, f2
  REAL(fp) :: filter_width
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: ffilter, bfilter  ! For the cosine filter
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: irf     ! The instrument response function
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: f       ! I/P to SPC->IFG FFT
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: spc     ! I/P to SPC->IFG FFT
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: x       ! O/P from SPC->IFG FFT; I/P to IFG->SPC FFT
  COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: ifg     ! O/P from SPC->IFG FFT; I/P to IFG->SPC FFT
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: cf      ! O/P from IFG->SPC FFT
  COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: cspc    ! O/P from IFG->SPC FFT
  INTEGER,     DIMENSION(:), ALLOCATABLE :: Channel ! channel indexes
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: Rad     ! Radiance

  INTEGER, PARAMETER :: SL = 128
  CHARACTER( SL ) :: A_SENSOR_ID
  
  CHARACTER( 256 ) :: Out_Filename
  CHARACTER( * ), PARAMETER :: Signal_Filename = 'Convol_Completion.signal'
  

  ! ---------------------
  ! Output program header
  ! ---------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to Fourier transform transmittance spectra into '//&
                        'the interferometric domain, apodize the resultant '//&
                        'interferogram (IFG) with an interferometer instrument '//&
                        'response function (IRF), and then Fourier transform the result '//&
                        'back into the spectral domain.', &
                        '$Revision$' )


  ! --------------
  ! Get user input
  ! --------------
  ! The netCDF LBL file
  ! -------------------
  WRITE(*, FMT='(/5x,"Enter the netCDF LBL filename: ")', ADVANCE='NO')
  READ(*,'(a)') LBL_Filename
  LBL_Filename = ADJUSTL(LBL_Filename)

  ! Inquire the file
  Error_Status = Inquire_LBLRTM_netCDF( LBL_Filename                     , &
                                        n_Frequencies     =n_ispc        , &
                                        n_Layers          =n_lbl_layers  , &
                                        Direction         =dirn          , &
                                        Begin_Frequency   =bf            , &
                                        End_Frequency     =ef            , &
                                        Frequency_Interval=df            , &
                                        History           =LBLRTM_History, &
                                        Comment           =LBLRTM_Comment, &
                                        Id_Tag            =LBLRTM_Id_Tag   )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring '//TRIM(LBL_Filename), &
                          Error_Status )
    STOP
  END IF

  WRITE(*,'(/5x,"nF = ",i0,&
           &/5x,"nL = ",i0,&
           &/5x,"dirn = ",i0,&
           &/5x,"bf   = ",es13.6," cm^-1",&
           &/5x,"ef   = ",es13.6," cm^-1",&
           &/5x,"df   = ",es13.6," cm^-1")') &
           n_ispc, n_lbl_layers, dirn, bf, ef, df

  ! The output file
  ! -------------------
  WRITE(*, FMT='(/5x,"Enter the output filename: ")', ADVANCE='NO')
  READ(*,'(a)') Out_Filename
  Out_Filename = ADJUSTL(Out_Filename)


  ! Ask for the sensor index
  ! ----------------------------
  WRITE(*, FMT='(/5x,"Select the SENSOR INDEX SET")')
  DO i = 1, N_SENSOR_SETS
    WRITE(*,FMT='(10x,i2,") ",a," Sensor set")') i, TRIM(SENSOR_ID(i))
  END DO
  WRITE(*,FMT='(5x,"Enter choice: ")',ADVANCE='NO')
  READ(*,FMT='(i1)',IOSTAT=IO_Status ) iSensor
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid SENSOR SET identifier input.', &
                          FAILURE )
    STOP
  END IF
  IF ( iSensor < 1 .OR. iSensor > N_SENSOR_SETS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid DEPENDENT PROFILE SET identifier value.', &
                          FAILURE )
    STOP
  ENDIF
   
  ! Ask for the band number
  ! ----------------------------
  WRITE(*, FMT='(/5x,"Enter the band number: ")', ADVANCE='NO')
  READ(*,FMT='(i5)',IOSTAT=IO_Status) iBand
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid BAND input.', &
                          FAILURE )
    STOP
  END IF
  IF ( iBand < 1 .OR. iBand > N_BANDS(iSensor) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid BAND value.', &
                          FAILURE )
    STOP
  ENDIF
  WRITE(cBand,'("band",i0)') iBand
  WRITE(sBand,'("band",i3.3)') iBand
 

  ! ----------------------------------------
  ! Determine where the input spectrum slots
  ! into the 0-f(Nyquist) spectrum
  ! ----------------------------------------
  is1 = ComputeIndex(bf, df)
  is2 = ComputeIndex(ef, df)
  WRITE(*,'(/5x,"Index positions of input SPC             : ",i0,",",i0)') is1, is2

  ! Compute the number of spectral points 
  n_spc = ComputeNPoints(F_NYQUIST(iSensor),df)
  
  
  ! -------------------------------------------
  ! Compute all the spectral length information
  ! -------------------------------------------
  ! Compute the rolloff filter info
  n_filter     = ComputeNPoints(DEFAULT_FILTER_WIDTH, df)
  filter_width = REAL(n_filter-1,fp) * df
  WRITE(*,'(/5x,"Rollof filter width: ",es13.6," cm^-1",/)') filter_width
  
  ! Compute interferogram length for the input spectrum 
  n_ifg = ComputeNIFG(n_spc)   ! No. of FFT IFG points
  
  WRITE(*,'( 5x,"No. of SPC points                        : ",i0,&
           &/5x,"No. of IFG points for SPC->IFG           : ",i0)') n_spc, n_ifg


  ! -----------------------
  ! Allocate all the arrays
  ! -----------------------
  ALLOCATE( ffilter(n_filter), &
            bfilter(n_filter), &
            irf(n_ifg)   , &  ! The instrument response function (IRF) in IFG space.
            f(n_spc)     , &  ! Input  to   SPC->IFG FFT
            spc(n_spc)   , &  ! Input  to   SPC->IFG FFT
            x(n_ifg)     , &  ! Output from SPC->IFG FFT; Input  to   IFG->SPC FFT
            ifg(n_ifg)   , &  ! Output from SPC->IFG FFT; Input  to   IFG->SPC FFT
            STAT = Allocate_Status )
  IF (Allocate_Status /= 0) THEN
    Error_Status = FAILURE
    WRITE(Message,'("Error allocating arrays. STAT=",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF

  
  ! ---------------------------------------
  ! Compute the 0-f(Nyquist) frequency grid
  ! ---------------------------------------
  f1 = MASTER_F1(iSensor)
  f2 = f1 + REAL(n_spc-1,fp)*df
  f = (/ (REAL(i,fp),i=0,n_spc-1) /) / REAL(n_spc-1,fp) 
  f = f*(f2-f1) + f1


  ! -----------------------------------
  ! Determine the IFG truncation points
  ! -----------------------------------
  ! Compute the optical delay grid
  x = computeX(f)
  
  ! Get the truncation points
  Sensor_Select: SELECT CASE (iSensor)
   CASE ( 1)
    it1 = MINLOC(x, DIM=1, MASK=(x > -IASI_RESAMPLE_MAXX))
    it2 = MAXLOC(x, DIM=1, MASK=(x <= IASI_RESAMPLE_MAXX))
   CASE ( 2 )
    it1 = MINLOC(x, DIM=1, MASK=(x > -CRIS_RESAMPLE_MAXX(iBand)))
    it2 = MAXLOC(x, DIM=1, MASK=(x <= CRIS_RESAMPLE_MAXX(iBand)))
  END SELECT  Sensor_Select
  write(*,*) it1, it2      
  ! The number of truncated IFG and SPC points
  n_tifg = it2-it1+1

  n_tspc = ComputeNSPC(n_tifg)

  WRITE(*,'( 5x,"No. of truncated IFG points for IFG->SPC : ",i0,&
           &/5x,"No. of output SPC points                 : ",i0)') n_tifg, n_tspc
           

  ! --------------------------
  ! Allocate the result arrays
  ! --------------------------
  ALLOCATE( cf(n_tspc)    , &  ! Output from IFG->SPC FFT 
            cspc(n_tspc)  , &  ! Output from IFG->SPC FFT
            STAT = Allocate_Status )
  IF (Allocate_Status /= 0) THEN
    Error_Status = FAILURE
    WRITE(Message,'("Error allocating output spectrum arrays. STAT=",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF


  ! ---------------------------
  ! Compute the rolloff filters
  ! ---------------------------
  ! The front end
  if1 = is1; if2 = is1 + n_filter - 1
  Error_Status = CosFilter( f(if1:if2)              , & ! Input
                            ffilter                 , & ! Output
                            FilterWidth=filter_width  ) ! Optional Input
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing front-end filter', &
                          Error_Status )
    STOP
  END IF

  ! The back end
  ib1 = is2-n_filter+1; ib2 = is2
  bfilter = ffilter(n_filter:1:-1)

  
  ! -------------------
  ! Compute the FTS IRF
  ! -------------------
  SELECT CASE (iSensor)
   CASE (1)
    irf = IASI_GFT(x)
    ! --------------------------------------------
    ! Determine the actual channel numbers to keep
    ! --------------------------------------------
    ic1 = INT((IASI_BAND_F1(iBand) - f1)/IASI_D_FREQUENCY + ONEpointFIVE)
    ic2 = INT((IASI_BAND_F2(iBand) - f1)/IASI_D_FREQUENCY + ONEpointFIVE)
   CASE (2)
    irf = CRIS_GFT(iBand,x)
    ! --------------------------------------------
    ! Determine the actual channel numbers to keep
    ! --------------------------------------------
    ic1 = INT((CRIS_BAND_F1(iBand) - f1)/CRIS_D_FREQUENCY(iBand) + ONEpointFIVE)
    ic2 = INT((CRIS_BAND_F2(iBand) - f1)/CRIS_D_FREQUENCY(iBand) + ONEpointFIVE)
  END SELECT   
  
  n_cspc = ic2-ic1+1
  
  WRITE( *,'(5x,"No. of output band ",i1," points         : ",i0,/)') iBand, n_cspc


  ALLOCATE(Channel(n_cspc), &
           Rad(n_cspc), &
           STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error allocating the Channel and Rad arrays. STAT = ",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF
  
  
  SELECT CASE (iSensor)
   CASE (1)
     Channel        = IASI_Channels(iBand)
   CASE (2)
     Channel        = CRIS_Channels(iBand)
  END SELECT


  ! ------------------------------------
  ! Begin layer loop to read LBLRTM data
  ! ------------------------------------
    iLayer = 1
  
    ! Initialize arrays
    ! -----------------
    spc = ZERO
    ifg = CMPLX(ZERO,ZERO,fp)


    ! Read a layer of data, slotting it into the
    ! spc array to be bookended with rolloff filter
    ! ---------------------------------------------
    Error_Status = Read_LBLRTM_netCDF( LBL_Filename, &
                                       iLayer, &
                                       Spectrum=spc(is1:is2) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading LBL '//&
                            ' spectrum from '//TRIM(LBL_Filename), &
                            Error_Status )
      STOP
    END IF


    ! Apply rollof filter to spectrum
    ! -------------------------------
    spc(if1:if2) = spc(if1:if2) * ffilter  ! Front-end
    spc(ib1:ib2) = spc(ib1:ib2) * bfilter  ! Back-end


    ! FFT filtered input spectrum to an interferogram
    ! -----------------------------------------------
    Error_Status = SPCtoIFG(f, spc, &  ! Input
                            x, ifg  )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'SPC->IFG FFT failed', &
                            Error_Status )
      STOP
    END IF


    ! Apply apodisation function to IFG
    ! ---------------------------------
    ifg = ifg * irf


    ! FFT truncated interferogram to a spectrum
    ! -----------------------------------------
    Error_Status = IFGtoSPC(x(it1:it2), ifg(it1:it2), &  ! Input
                            cf        , cspc          )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'IFG->SPC FFT failed', &
                            Error_Status )
      STOP
    END IF


    ! Copy results to output array
    ! -------------------------------------
    rad(:) = REAL(cspc(ic1:ic2),fp)


    ! write out to a file
    ! Open the file
    OPEN( fid_out, FILE   = TRIM( Out_Filename ), &
                   STATUS = 'REPLACE', &
                   ACTION = 'WRITE', &
                   ACCESS = 'SEQUENTIAL', &
                   FORM   = 'UNFORMATTED', &
                   IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Out_Filename ), IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status)
      STOP
    END IF

    A_SENSOR_ID = SENSOR_ID(iSensor)
    WRITE(fid_out)SL
    WRITE(fid_out)n_cspc
    WRITE(fid_out)A_SENSOR_ID, WMO_SATELLITE_ID(iSensor), WMO_SENSOR_ID(iSensor)
    WRITE(fid_out)Channel
    WRITE(fid_out)Rad
    
    CLOSE(fid_out)
    
    ! create a signal file
    OPEN(fid_out, FILE=Signal_Filename, STATUS='REPLACE')
    WRITE(fid_out, *)'Completion'
    CLOSE(fid_out) 
    
  ! Deallocate arrays
  DEALLOCATE( ffilter, bfilter, &
              irf             , &
              f      , spc    , &
              x      , ifg    , &
              cf     , cspc   , &
              Channel, Rad    , &
              STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating arrays. STAT = ",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          WARNING )
  END IF
  
END PROGRAM Apodize_TauSpc_with_IRF
