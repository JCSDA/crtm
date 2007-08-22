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
                                  Destroy_TauProfile, &
                                  Assign_TauProfile
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
  CHARACTER(2000) :: Comment
  CHARACTER(256) :: Message
  CHARACTER(256) :: LBLRTM_Filename
  CHARACTER(256) :: TauProfile_Filename
  CHARACTER(20)  :: c_layer
  INTEGER  :: Error_Status
  INTEGER  :: Allocate_Status
  INTEGER  :: i, k
  INTEGER  :: if1, if2
  INTEGER  :: ib1, ib2
  INTEGER  :: n_in, n_out
  INTEGER  :: n_filter
  INTEGER  :: n_spc, n_ifg
  INTEGER  :: n_layers, dirn
  REAL(fp) :: bf, ef, df
  REAL(fp) :: f1, f2
  REAL(fp) :: filter_width
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: ffilter, bfilter  ! For the cosine filter
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: irf     ! The instrument response function
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: f       ! I/P to SPC->IFG FFT; O/P from IFG->SPC FFT
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: spc     ! I/P to SPC->IFG FFT
  REAL(fp),    DIMENSION(:), ALLOCATABLE :: x       ! O/P from SPC->IFG FFT; I/P to IFG->SPC FFT
  COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: ifg     ! O/P from SPC->IFG FFT; I/P to IFG->SPC FFT
  COMPLEX(fp), DIMENSION(:), ALLOCATABLE :: cspc    ! O/P from IFG->SPC FFT
  INTEGER :: n1, n2, n_IASI_channels
  TYPE(TauProfile_type), TARGET  :: realTau, imagTau
  TYPE(TauProfile_type), POINTER :: TauProfile => NULL()
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
                                        n_Frequencies     =n_spc         , &
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
           n_spc, n_layers, dirn, bf, ef, df


  ! -------------------------------------------
  ! Compute all the spectral length information
  ! -------------------------------------------
  ! Compute the rolloff filter info
  n_filter     = ComputeNPoints(DEFAULT_FILTER_WIDTH, df)
  filter_width = REAL(n_filter-1,fp) * df
  WRITE(*,'(/5x,"Rollof filter width: ",es13.6," cm^-1",/)') filter_width
  
  ! Compute interferogram length for the input spectrum 
  n_ifg = ComputeNIFG(n_spc)   ! No. of FFT IFG points
  
print *, 'No. of input SPC points        : ', n_spc
print *, 'No. of IFG points for SPC<->IFG: ', n_ifg

  ! -----------------------
  ! Allocate all the arrays
  ! -----------------------
  ALLOCATE( ffilter(n_filter), &
            bfilter(n_filter), &
            irf(n_ifg)   , &  ! The instrument response function (IRF) in IFG space.
            f(n_spc)     , &  ! Input  to   SPC->IFG FFT; Output from IFG->SPC FFT 
            spc(n_spc)   , &  ! Input  to   SPC->IFG FFT
            x(n_ifg)     , &  ! Output from SPC->IFG FFT; Input  to   IFG->SPC FFT
            ifg(n_ifg)   , &  ! Output from SPC->IFG FFT; Input  to   IFG->SPC FFT
            cspc(n_spc)  , &  ! Output from IFG->SPC FFT
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
  f2 = f1 + REAL(n_spc-1,fp)*df
  f = (/ (REAL(i,fp),i=0,n_spc-1) /) / REAL(n_spc-1,fp) 
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
  ib1 = n_spc-n_filter+1; ib2 = n_spc
  bfilter = ffilter(n_filter:1:-1)

  
  ! -------------------
  ! Compute the FTS IRF
  ! -------------------
  irf = IASI_GFT(ComputeX(f))


  ! --------------------------------
  ! Set up the TauProfile structures
  ! --------------------------------
  ! Allocate the structure for the REAL result
  Error_Status = Allocate_TauProfile( n_layers, &
                                      n_spc, &
                                      1, & ! n_Angles       
                                      1, & ! n_Profiles     
                                      1, & ! n_Molecule_Sets
                                      realTau )  
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating realTau', &
                          Error_Status )
    STOP
  END IF
  
  ! Assign the dimension arrays
  realTau%Sensor_ID      = 'iasi_metopa'
  realTau%Level_Pressure = (/(REAL(ik,fp),ik=0,n_layers)/)
  realTau%Channel        = (/(il,il=1,n_spc)/)
  realTau%Angle          = (/1.5_fp/)
  realTau%Profile        = (/1/)
  realTau%Molecule_Set   = (/10/)
  
  ! Copy the REAL strcuture to the IMAGINARY one
  Error_Status = Assign_TauProfile( realTau, imagTau )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error copying realTau', &
                          Error_Status )
    STOP
  END IF


  ! ------------------------------------
  ! Begin layer loop to read LBLRTM data
  ! ------------------------------------
  Layer_loop: DO k = 1, n_layers
  
    WRITE(c_layer,'(i0)') k
    WRITE(*,'(5x,"Processing layer # ",i0)') k


    ! Initialize arrays
    ! -----------------
    spc = ZERO
    ifg = CMPLX(ZERO,ZERO,fp)


    ! Read a layer of data, slotting it into the
    ! spc array to be bookended with rolloff filter
    ! ---------------------------------------------
    Error_Status = Read_LBLRTM_netCDF( LBLRTM_Filename, &
                                       k, &
                                       Transmittance=spc(1:n_spc) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading layer '//TRIM(c_layer)//&
                            ' spectrum from '//TRIM(LBLRTM_Filename), &
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
                            'SPC->IFG FFT failed for layer '//TRIM(c_layer), &
                            Error_Status )
      STOP
    END IF
    
    
    ! Apply apodisation function to IFG
    ! ---------------------------------
    ifg = ifg * irf
    
    
    ! FFT truncated interferogram to a spectrum
    ! -----------------------------------------
    Error_Status = IFGtoSPC(x, ifg,  &  ! Input
                            f, cspc  )  ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'IFG->FFT FFT failed for layer '//TRIM(c_layer), &
                            Error_Status )
      STOP
    END IF
    
    ! Restore begin frequency
    f = f + bf 
    

    ! Copy results to TauProfile structures
    ! -------------------------------------
    realTau%Tau(k,:,1,1,1) = REAL(cspc,fp)
    imagTau%Tau(k,:,1,1,1) = AIMAG(cspc)

  END DO Layer_loop


  ! ------------------------------
  ! Write spectrally reducted data
  ! to TauProfile format file
  ! ------------------------------
  DO i = 1, 2
  
    ! Assign values for different TauProfiles
    IF ( i == 1 ) THEN
      TauProfile_Filename = TRIM(realTau%Sensor_ID)//'.REAL.TauProfile.nc'
      Comment = 'REAL part of result'
      TauProfile => realTau
    ELSE
      TauProfile_Filename = TRIM(imagTau%Sensor_ID)//'.IMAG.TauProfile.nc'
      Comment = 'IMAGINARY part of result'
      TauProfile => imagTau
    END IF
    
    ! Create the output file
    Error_Status = Create_TauProfile_netCDF( TauProfile_Filename        , &  ! Input
                                             TauProfile%Level_Pressure  , &  ! Input
                                             TauProfile%Channel         , &  ! Input
                                             TauProfile%Angle           , &  ! Input
                                             TauProfile%Profile         , &  ! Input
                                             TauProfile%Molecule_Set    , &  ! Input
                                             Sensor_ID   = TauProfile%Sensor_ID , &  ! Optional Input
                                             ID_Tag      = TRIM(LBLRTM_ID_Tag)  , &  ! Optional input
                                             Title       = 'IASI transmittance profiles', &  ! Optional input
                                             History     = TRIM(LBLRTM_History)//'; '//  &
                                                           PROGRAM_RCS_ID       , &  ! Optional input
                                             Comment     = TRIM(Comment)          )  ! Optional input
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Creation of output file '//TRIM(TauProfile_Filename)//' failed', &
                            Error_Status )
      STOP
    END IF

    ! Output the entire TauProfile structure   
    Error_Status = Write_TauProfile_netCDF( TauProfile_Filename, &  ! Input
                                            TauProfile           )  ! Optional input
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing to '//TRIM(TauProfile_Filename), &
                            Error_Status )
      STOP
    END IF

    ! Nullify the pointer
    NULLIFY(TauProfile)
    
  END DO
  

  ! --------
  ! Clean up
  ! --------
  ! Destroy the TauProfile structure
  Error_Status = Destroy_TauProfile( realTau )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying realTau structure', &
                          WARNING )
  END IF
  Error_Status = Destroy_TauProfile( imagTau )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying imagTau structure', &
                          WARNING )
  END IF
  ! Deallocate arrays
  DEALLOCATE( ffilter, bfilter, &
              irf             , &
              f      , spc    , &
              x      , ifg    , &
              cspc   , &
              STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating arrays. STAT = ",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          WARNING )
  END IF
  
END PROGRAM Apodize_TauSpc_with_IRF
