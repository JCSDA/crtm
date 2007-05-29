!
! Apodize_TauSpc_with_IRF
!
! Program to Fourier transform transmittance spectra in to the interferometric
! domain, apodize the resultant interferogram (IFG) with an interferometer
! instrument response function (IRF) and then Fourier transform the result
! back into the spectral domain.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Feb-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Apodize_TauSpc_with_IRF

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds          , ONLY: fp
  USE File_Utility        , ONLY: File_Exists
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, WARNING, &
                                  Program_Message, Display_Message
  USE TauProfile_Define   , ONLY: TauProfile_type, &
                                  Allocate_TauProfile, &
                                  Destroy_TauProfile
  USE TauProfile_netCDF_IO, ONLY: Create_TauProfile_netCDF, &
                                  Write_TauProfile_netCDF
  USE LBLRTM_netCDF_IO    , ONLY: Create_LBLRTM_netCDF, &
                                  Inquire_LBLRTM_netCDF, &
                                  Read_LBLRTM_netCDF, &
                                  Write_LBLRTM_netCDF
  USE FFT_Spectral_Utility, ONLY: CosFilter, &
                                  SPCtoIFG, &
                                  IFGtoSPC, &
                                  ComputeNPoints, &
                                  ComputeNextPO2, &
                                  ComputeNIFG, &
                                  ComputeX, &
                                  DEBUG_DumpReal, &
                                  DEBUG_DumpComplex
  USE IASI_Define         , ONLY: IASI_D_FREQUENCY, &
                                  IASI_MAX_NCHANNELS, &
                                  IASI_F, &
                                  IASI_GFT, &
                                  IASI_Resample_Idx, &
                                  IASI_Resample
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Apodize_TauSpc_with_IRF'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  ! Rollof filter width
  REAL(fp), PARAMETER :: DEFAULT_FILTER_WIDTH = 20.0_fp
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(2000) :: LBLRTM_History
  CHARACTER(2000) :: LBLRTM_Comment
  CHARACTER(2000) :: LBLRTM_Id_Tag
  CHARACTER(256) :: Message
  CHARACTER(256) :: LBLRTM_Filename
  CHARACTER(20)  :: c_layer
  INTEGER  :: Error_Status
  INTEGER  :: Allocate_Status
  INTEGER  :: i, k
  INTEGER  :: if1, if2
  INTEGER  :: ib1, ib2
  INTEGER  :: n_in, n_out
  INTEGER  :: po2, n_po2
  INTEGER  :: n_filter
  INTEGER  :: n_spcin
  INTEGER  :: n_spc_po2, n_ifg_po2
  INTEGER  :: n_layers, dirn
  REAL(fp) :: bf, ef, df
  REAL(fp) :: f1, f2
  REAL(fp) :: filter_width
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: ffilter, bfilter  ! For the cosine filter
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: irf     ! The instrument response function
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: f       ! I/P to SPC->IFG FFT; O/P from IFG->SPC FFT
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: spc     ! I/P to SPC->IFG FFT
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: opd     ! O/P from SPC->IFG FFT; I/P to IFG->SPC FFT
  COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: ifg     ! O/P from SPC->IFG FFT; I/P to IFG->SPC FFT
  COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: cspc    ! O/P from IFG->SPC FFT
  REAL(fp), DIMENSION(IASI_MAX_NCHANNELS) :: f_IASI
  INTEGER :: n1, n2, n_IASI_channels
  INTEGER , DIMENSION(:), ALLOCATABLE :: f_idx
  REAL(fp), DIMENSION(:), ALLOCATABLE :: spc_IASI
  TYPE(TauProfile_type) :: TauProfile
  INTEGER :: ik, il


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

  ! ---------------
  ! Get user inputs
  ! ---------------
  ! Inquire the LBLRTM netCDF datafile
  WRITE(*, FMT='(/5x,"Enter the netCDF LBLRTM file: ")', ADVANCE='NO')
  READ(*,'(a)') LBLRTM_Filename
  LBLRTM_Filename = ADJUSTL(LBLRTM_Filename)
  
  Error_Status = Inquire_LBLRTM_netCDF( LBLRTM_Filename                  , &
                                        n_Frequencies     =n_spcin       , &
                                        n_Layers          =n_layers      , &
                                        Direction         =dirn          , &
                                        Begin_Frequency   =bf            , &
                                        End_Frequency     =ef            , &
                                        Frequency_Interval=df            , &
                                        History           =LBLRTM_History, &
                                        Comment           =LBLRTM_Comment, &
                                        Id_Tag            =LBLRTM_Id_Tag   )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring '//TRIM(LBLRTM_Filename), &
                          Error_Status )
    STOP
  END IF

  WRITE(*,'(/5x,"nF = ",i0,&
           &/5x,"nL = ",i0,&
           &/5x,"dirn = ",i0,&
           &/5x,"bf   = ",es13.6," cm^-1",&
           &/5x,"ef   = ",es13.6," cm^-1",&
           &/5x,"df   = ",es13.6," cm^-1",/)') &
           n_spcin, n_layers, dirn, bf, ef, df


  ! -------------------------------------------
  ! Compute all the spectral length information
  ! -------------------------------------------
  ! Compute the rolloff filter info
  n_filter     = ComputeNPoints(DEFAULT_FILTER_WIDTH, df)
  filter_width = REAL(n_filter-1,fp) * df
  WRITE(*,'(/5x,"Rollof filter width: ",es13.6," cm^-1",/)') filter_width
  
  ! Compute the spectral and interferogram
  ! lengths for the filtered input spectrum 
  po2       = ComputeNextPO2(n_spcin)  ! Next power-of-two for input, filtered SPC
  n_spc_po2 = 2**po2 + 1               ! No. of FFT SPC points
  n_ifg_po2 = ComputeNIFG(n_spc_po2)   ! No. of FFT IFG points
  
print *, 'No. of filtered input SPC points: ', n_spcin
print *, 'No. of SPC points for SPC<->IFG:  ', n_spc_po2, '(',po2,')'
print *, 'No. of IFG points for SPC<->IFG:  ', n_ifg_po2

  ! -----------------------
  ! Allocate all the arrays
  ! -----------------------
  ALLOCATE( ffilter(n_filter), &
            bfilter(n_filter), &
            irf(n_ifg_po2)   , &  ! The instrument response function (IRF) in IFG space.
            f(n_spc_po2)     , &  ! Input  to   SPC->IFG FFT; Output from IFG->SPC FFT 
            spc(n_spc_po2)   , &  ! Input  to   SPC->IFG FFT
            opd(n_ifg_po2)   , &  ! Output from SPC->IFG FFT; Input  to   IFG->SPC FFT
            ifg(n_ifg_po2)   , &  ! Output from SPC->IFG FFT; Input  to   IFG->SPC FFT
            cspc(n_spc_po2)  , &  ! Output from IFG->SPC FFT
            STAT = Allocate_Status )
  IF (Allocate_Status /= 0) THEN
    Error_Status = FAILURE
    WRITE(Message,'("Error allocating arrays. STAT=",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF

  ! --------------------------------
  ! Compute the input frequency grid
  ! --------------------------------
  f1 = bf
  f2 = f1 + REAL(n_spc_po2-1,fp)*df
  f = (/ (REAL(i,fp),i=0,n_spc_po2-1) /) / REAL(n_spc_po2-1,fp) 
  f = f*(f2-f1) + f1


  ! ---------------------------
  ! Compute the rolloff filters
  ! ---------------------------
  ! The front end
  if1 = 1; if2 = n_filter
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
  ib1 = n_spcin-n_filter+1; ib2 = n_spcin
  bfilter = ffilter(n_filter:1:-1)

  
  ! -------------------
  ! Compute the FTS IRF
  ! -------------------
  irf = IASI_GFT(ComputeX(f))
  
  ! -------------------------------------
  ! Compute the IASI resample frequencies
  ! -------------------------------------
  f_IASI = IASI_F()
  n1 = MINLOC( f_IASI, DIM=1, MASK=(f_IASI >= bf) )
  n2 = MAXLOC( f_IASI, DIM=1, MASK=(f_IASI <= ef) )
  n_IASI_channels = n2 - n1 + 1
  
print *, 'No. of IASI channels:  ', n_IASI_channels
print *, 'IASI frequency bounds: ', f_IASI(n1), f_IASI(n2)
  
  ! Allocate the interpolation arrays
  ALLOCATE( f_idx(n_IASI_channels), &
            spc_IASI(n_IASI_channels), &
            STAT = Allocate_Status )
  IF (Allocate_Status /= 0) THEN
    Error_Status = FAILURE
    WRITE(Message,'("Error allocating interpolation arrays. STAT=",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF

  ! Compute the IASI interpolation indices
  f_idx = IASI_Resample_Idx( f, f_IASI(n1:n2) )


  ! ---------------------------------
  ! Allocate the TauProfile structure
  ! ---------------------------------
  Error_Status = Allocate_TauProfile( n_layers, &
                                      n_IASI_channels, &
                                      1, & ! n_Angles       
                                      1, & ! n_Profiles     
                                      1, & ! n_Molecule_Sets
                                      TauProfile )  
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating TauProfile', &
                          Error_Status )
    STOP
  END IF
  
  ! Assign the dimension arrays
  TauProfile%Sensor_ID      = 'iasi_metopa'
  TauProfile%Level_Pressure = (/(REAL(ik,fp),ik=0,n_layers)/)
  TauProfile%Channel        = (/(il+n1-1,il=1,n_IASI_channels)/)
  TauProfile%Angle          = (/1.5_fp/)
  TauProfile%Profile        = (/1/)
  TauProfile%Molecule_Set   = (/10/)


  ! ------------------------------------
  ! Begin layer loop to read LBLRTM data
  ! ------------------------------------
  Layer_loop: DO k = 1, n_layers
  
    WRITE(c_layer,'(i0)') k
    WRITE(*,'(5x,"Processing layer # ",i0)') k

    ! Initialize arrays
    spc = ZERO
    ifg = CMPLX(ZERO,ZERO,fp)

    ! Read a layer of data, slotting it into the
    ! spc array to be bookended with rolloff filter
    Error_Status = Read_LBLRTM_netCDF( LBLRTM_Filename, &
                                       k, &
                                       Transmittance=spc(1:n_spcin) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading layer '//TRIM(c_layer)//&
                            ' spectrum from '//TRIM(LBLRTM_Filename), &
                            Error_Status )
      STOP
    END IF

    ! Apply rollof filter to spectrum
    spc(if1:if2) = spc(if1:if2) * ffilter  ! Front-end
    spc(ib1:ib2) = spc(ib1:ib2) * bfilter  ! Back-end

    ! FFT filtered input spectrum to an interferogram
    Error_Status = SPCtoIFG(f  , spc, &  ! Input
                            opd, ifg  )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'SPC->IFG FFT failed for layer '//TRIM(c_layer), &
                            Error_Status )
      STOP
    END IF
    
    ! Apply apodisation function to IFG
    ifg = ifg * irf
    
    ! FFT truncated interferogram to a spectrum
    Error_Status = IFGtoSPC(opd, ifg,  &  ! Input
                            f  , cspc  )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'IFG->FFT FFT failed for layer '//TRIM(c_layer), &
                            Error_Status )
      STOP
    END IF
    f = f + bf  ! Restore begin frequency
    
    ! Resample result to IASI frequencies
    Error_Status = IASI_Resample( f, REAL(cspc,fp), f_IASI(n1:n2), &
                                  spc_IASI, &
                                  f_idx = f_idx )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'IASI resample failed for layer '//TRIM(c_layer), &
                            Error_Status )
      STOP
    END IF

    ! Copy result to TauProfile structure
    TauProfile%Tau(k,:,1,1,1) = spc_IASI

  END DO Layer_loop


  ! ------------------------------
  ! Write spectrally reducted data
  ! to TauProfile format file
  ! ------------------------------
  ! Create the output file
  Error_Status = Create_TauProfile_netCDF( 'iasi_metopa.TauProfile.nc'   , &  ! Input
                                           TauProfile%Level_Pressure  , &  ! Input
                                           TauProfile%Channel         , &  ! Input
                                           TauProfile%Angle           , &  ! Input
                                           TauProfile%Profile         , &  ! Input
                                           TauProfile%Molecule_Set    , &  ! Input
                                           Sensor_ID       =TauProfile%Sensor_ID , &  ! Optional Input
                                           Sensor_Name     ='IASI'          , &  ! Optional input
                                           Platform_Name   ='MetOp-A'       , &  ! Optional input
                                           ID_Tag      = TRIM(LBLRTM_ID_Tag)          , &  ! Optional input
                                           Title       = 'IASI transmittance profiles', &  ! Optional input
                                           History     = TRIM(LBLRTM_History)//'; '//  &
                                                         PROGRAM_RCS_ID               , &  ! Optional input
                                           Comment     = 'Test file'                    )  ! Optional input
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Output file creation failed', &
                          Error_Status )
    STOP
  END IF

  ! Output the entire TauProfile structure   
  Error_Status = Write_TauProfile_netCDF( 'iasi_metopa.TauProfile.nc', &  ! Input
                                          TauProfile                   )  ! Optional input
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing TauProfile to file', &
                          Error_Status )
    STOP
  END IF


  ! --------
  ! Clean up
  ! --------
  ! Destroy the TauProfile structure
  Error_Status = Destroy_TauProfile( TauProfile )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauProfile structure', &
                          WARNING )
  END IF
  ! Deallocate arrays
  DEALLOCATE( ffilter, bfilter, &
              irf             , &
              f      , spc    , &
              opd    , ifg    , &
              cspc   , &
              f_idx  , spc_IASI, &
              STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating arrays. STAT = ",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          WARNING )
  END IF
  
END PROGRAM Apodize_TauSpc_with_IRF
