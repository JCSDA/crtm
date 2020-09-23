!
! Create_FTS_SpcCoeff
!
! Program to create the infrared spectral coefficient (SpcCoeff)
! data files for FTS sensors
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Oct-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Create_FTS_SpcCoeff


  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE File_Utility             , ONLY: Get_Lun, File_Exists
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                       Display_Message, Program_Message
  USE Fundamental_Constants    , ONLY: C_1, C_2
  USE Spectral_Units_Conversion, ONLY: Inverse_cm_to_GHz
  USE SensorInfo_Define        , ONLY: UNPOLARIZED       , &
                                       SensorInfo_type   , &
                                       Destroy_SensorInfo
  USE SensorInfo_LinkedList    , ONLY: SensorInfo_List_type   , &
                                       GetFrom_SensorInfo_List, &
                                       Destroy_SensorInfo_List
  USE SensorInfo_IO            , ONLY: Read_SensorInfo
  USE SpcCoeff_Parameters      , ONLY: C_1_SCALE_FACTOR, &
                                       C_2_SCALE_FACTOR
  USE SpcCoeff_Define          , ONLY: SpcCoeff_type      , &
                                       SpcCoeff_Associated, &
                                       SpcCoeff_Create    , &
                                       SpcCoeff_Destroy   , &
                                       SpcCoeff_SetSolar
  USE SpcCoeff_netCDF_IO       , ONLY: SpcCoeff_netCDF_WriteFile
  ! The FTS instrument definition modules
  USE IASI_Define, ONLY: IASI_F     , &
                         IASI_maxX  , &
                         IASI_BeginF, &
                         IASI_EndF  , &
                         IASI_dF    , &
                         IASI_ApodFunction
  USE CrIS_Define, ONLY: CrIS_F     , &
                         CrIS_maxX  , &
                         CrIS_BeginF, &
                         CrIS_EndF  , &
                         CrIS_dF    , &
                         CrIS_ApodFunction, &
                         CrIS_Remove_Guard_Channels
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_FTS_SpcCoeff'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  ! String lengths
  INTEGER, PARAMETER :: SL = 256
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! SensorInfo file
  CHARACTER(*), PARAMETER :: SENSORINFO_FILE = 'SensorInfo'
  ! FTS sensor definitions
  INTEGER, PARAMETER :: IASI_ID = 1
  INTEGER, PARAMETER :: CRIS_ID = 2
  INTEGER, PARAMETER :: N_SENSORS = 2
  INTEGER, PARAMETER :: N_BANDS   = 3
  CHARACTER(*), PARAMETER :: SENSOR_NAME(N_SENSORS)   = (/ 'iasi', 'cris' /)
  CHARACTER(*), PARAMETER :: PLATFORM_NAME(N_SENSORS) = (/ 'metop-a', 'npp    ' /)
  

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(SL) :: msg
  CHARACTER(SL) :: filename
  CHARACTER(SL) :: sensor_id
  INTEGER :: err_stat
  INTEGER :: n
  INTEGER :: band
  INTEGER :: version
  TYPE(SensorInfo_type)      :: sinfo
  TYPE(SensorInfo_List_type) :: sinfo_list
  TYPE(SpcCoeff_type)        :: spccoeff
  REAL(fp) :: maxX


  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to create the infrared SpcCoeff '//&
                        'files for infrared FTS sensors.', &
                        '$Revision$' )

  ! Read the SensorInfo file
  err_stat = Read_SensorInfo( SENSORINFO_FILE,sinfo_list,Quiet=1 )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading SensorInfo file '//SENSORINFO_FILE
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Get the SpcCoeff version from user
  WRITE( *,FMT='(/5x,"Default SpcCoeff file version is: ",i0,". Enter value: ")', &
           ADVANCE='NO' ) spccoeff%Version
  READ( *,* ) version
  IF ( version < spccoeff%Version ) THEN
    msg = 'Invalid version number specified. Using default.'
    CALL Display_Message( PROGRAM_NAME, msg, INFORMATION )
    version = spccoeff%Version
  END IF


  ! Begin the main sensor/band loops
  Sensor_Loop: DO n = 1, N_SENSORS
    Band_Loop: DO band = 1, N_BANDS

      ! Construct the sensor id
      WRITE( sensor_id,'(a,"B",i0,"_",a)' ) TRIM(SENSOR_NAME(n)), band, TRIM(PLATFORM_NAME(n))
      
      
      ! Get the SensorInfo data
      err_stat = GetFrom_SensorInfo_List( sinfo_list, sensor_id, sinfo )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error retrieving SensorInfo for '//TRIM(sensor_id)
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      
      
      ! Allocate the SpcCoeff structure
      CALL SpcCoeff_Create( spccoeff, sinfo%n_Channels )
      IF ( .NOT. SpcCoeff_Associated( spccoeff ) ) THEN
        msg = 'Error allocating SpcCoeff for '//TRIM(sensor_id)
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF


      ! Call sensor specific procedures
      SELECT CASE(TRIM(SENSOR_NAME(n)))
        CASE('iasi')
          ! ...Assign IASI-specific components
          spccoeff%Wavenumber = IASI_F(band)
          maxX = IASI_MaxX(band)
        CASE('cris')
          ! ...Assign CrIS-specific components
          spccoeff%Wavenumber = CrIS_F(band,include_guard_channels=.FALSE.)
          maxX = CrIS_MaxX(band)
        CASE DEFAULT
          msg = 'Unrecognised sensor name: '//TRIM(SENSOR_NAME(n))
          CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END SELECT


      ! Assign sensor-independent components
      spcCoeff%Version          = version
      spccoeff%Sensor_ID        = TRIM(sensor_id)
      spccoeff%WMO_Satellite_ID = sinfo%WMO_Satellite_ID
      spccoeff%WMO_Sensor_ID    = sinfo%WMO_Sensor_ID   
      spccoeff%Sensor_Channel   = sinfo%Sensor_Channel
      spccoeff%Sensor_Type      = sinfo%Sensor_Type
      spccoeff%Polarization     = UNPOLARIZED
      spccoeff%Frequency        = Inverse_cm_to_GHz( spccoeff%Wavenumber )
      ! ...Compute Planck coefficients
      spccoeff%Planck_C1 = C_1_SCALE_FACTOR * C_1 * ( spccoeff%Wavenumber**3 )
      spccoeff%Planck_C2 = C_2_SCALE_FACTOR * C_2 *   spccoeff%Wavenumber
      ! ...Default band correction coefficients
      spccoeff%Band_C1 = ZERO
      spccoeff%Band_C2 = ONE
      ! ...Fill the cosmic background field
      spccoeff%Cosmic_Background_Radiance = ZERO
      ! ...Set the solar fields
      CALL Compute_FTS_Solar()
      CALL SpcCoeff_SetSolar( spccoeff )


      ! Write the SpcCoeff data file
      filename = TRIM(sensor_id)//'.SpcCoeff.nc'
      err_stat = SpcCoeff_netCDF_WriteFile( &
                   filename, &
                   spccoeff, &
                   Title = 'Spectral coefficients for '//TRIM(sensor_id), &
                   History = PROGRAM_VERSION_ID, &
                   Comment = 'No band correction.')
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing netCDF SpcCoeff data file '//TRIM(filename)
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF


      ! Clean up
      CALL SpcCoeff_Destroy( spccoeff )
      err_stat = Destroy_SensorInfo( sinfo )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error destroying SensorInfo data structure for '//TRIM(sensor_id)
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF

    END DO Band_Loop      
  END DO Sensor_loop


  ! Destroy the SensorInfo linked list
  err_stat = Destroy_SensorInfo_List( sinfo_list )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error destroying SensorInfo_List.'
    CALL Display_Message( PROGRAM_NAME, msg, WARNING )
  END IF


CONTAINS


  SUBROUTINE Compute_FTS_Solar()
    USE Solar_Define
    USE Solar_IO
    USE FFT_Spectral_Utility
    
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_FTS_Solar'
    ! ...Solar irradiance input file
    CHARACTER(*), PARAMETER :: SOLAR_FILE = 'solar.nc'
    ! The spectral bandwidth, 0-f(Nyquist), definitions
    ! ...The bandwidth is determined a laser laser frequency of 8000cm-1.
    !    Assuming a sampling frequency of twice that and a frequency
    !    interval, df, of 0.001cm-1, this translates into 8000001 spectral
    !    points and 16000000 points in a double-sided interferogram. The
    !    prime factors for this number is
    !      n_ifg = 2^10 x 5^6 = 16000000
    REAL(fp), PARAMETER :: MASTER_F1 = ZERO
    REAL(fp), PARAMETER :: MASTER_F2 = 8000.0_fp
    REAL(fp), PARAMETER :: F_NYQUIST = MASTER_F2
    ! ...Rolloff filter width
    REAL(fp), PARAMETER :: DEFAULT_FILTER_WIDTH = 20.0_fp
    ! Literal constants
    REAL(fp), PARAMETER :: ZERO = 0.0_fp
    REAL(fp), PARAMETER :: ONE  = 1.0_fp
    REAL(fp), PARAMETER :: ONEpointFIVE = 1.5_fp

    ! Local variables
    INTEGER :: i
    INTEGER :: err_stat, alloc_stat
    TYPE(Solar_type) :: solar
    REAL(fp) :: bf, ef, df
    REAL(fp) :: f1, f2
    REAL(fp) :: filter_width
    INTEGER :: n_filter
    INTEGER :: is1, is2
    INTEGER :: n_spc, n_ifg
    INTEGER :: it1, it2
    INTEGER :: n_tspc, n_tifg
    INTEGER :: if1, if2
    INTEGER :: ib1, ib2
    INTEGER :: ic1, ic2
    INTEGER :: n_cspc
    REAL(fp)   , ALLOCATABLE :: ffilter(:), bfilter(:)  ! For the cosine filter
    REAL(fp)   , ALLOCATABLE :: irf(:)     ! The instrument response function
    REAL(fp)   , ALLOCATABLE :: f(:)       ! I/P to SPC->IFG FFT
    REAL(fp)   , ALLOCATABLE :: spc(:)     ! I/P to SPC->IFG FFT
    REAL(fp)   , ALLOCATABLE :: x(:)       ! O/P from SPC->IFG FFT; I/P to IFG->SPC FFT
    COMPLEX(fp), ALLOCATABLE :: ifg(:)     ! O/P from SPC->IFG FFT; I/P to IFG->SPC FFT
    REAL(fp)   , ALLOCATABLE :: cf(:)      ! O/P from IFG->SPC FFT
    COMPLEX(fp), ALLOCATABLE :: cspc(:)    ! O/P from IFG->SPC FFT
    
    
    ! Read solar data
    err_stat = Solar_ReadFile( SOLAR_FILE, solar )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error destroying SensorInfo_List.'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); STOP
    END IF
    
    ! Determine where the solar spectrum slots
    ! into the 0-f(Nyquist) spectrum
    bf = solar%f1
    ef = solar%f2
    df = ComputeMeanDelta(solar%Frequency)
    WRITE(*,'(/5x,"nF = ",i0,&
             &/5x,"bf = ",f12.6," cm^-1",&
             &/5x,"ef = ",f12.6," cm^-1",&
             &/5x,"df = ",es13.6," cm^-1")') &
             solar%n_Frequencies, bf, ef, df
    is1 = ComputeIndex(bf, df)
    is2 = ComputeIndex(ef, df)
    WRITE(*,'(/5x,"Index positions of solar SPC: ",i0,",",i0)') is1, is2

    ! Compute all the spectral length information
    ! ...Compute the rolloff filter info
    n_filter     = ComputeNPoints(DEFAULT_FILTER_WIDTH, df)
    filter_width = REAL(n_filter-1,fp) * df
    WRITE(*,'(/5x,"Rollof filter width: ",es13.6," cm^-1",/)') filter_width
    ! ...Compute the number of spectral and interferogram  points 
    n_spc = ComputeNPoints(F_NYQUIST,df)
    n_ifg = ComputeNIFG(n_spc)
    WRITE(*,'( 5x,"No. of SPC points                        : ",i0,&
             &/5x,"No. of IFG points for SPC->IFG           : ",i0)') n_spc, n_ifg


    ! Allocate all the arrays
    ALLOCATE( ffilter(n_filter), &
              bfilter(n_filter), &
              irf(n_ifg)   , &  ! The instrument response function (IRF) in IFG space.
              f(n_spc)     , &  ! Input  to   SPC->IFG FFT
              spc(n_spc)   , &  ! Input  to   SPC->IFG FFT
              x(n_ifg)     , &  ! Output from SPC->IFG FFT; Input  to   IFG->SPC FFT
              ifg(n_ifg)   , &  ! Output from SPC->IFG FFT; Input  to   IFG->SPC FFT
              STAT = alloc_stat )
    IF ( alloc_stat /= 0) THEN
      msg = 'Error allocating arrays'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); STOP
    END IF


    ! Compute the 0-f(Nyquist) frequency grid
    f1 = MASTER_F1
    f2 = f1 + REAL(n_spc-1,fp)*df
    f = (/ (REAL(i,fp),i=0,n_spc-1) /) / REAL(n_spc-1,fp)
    f = f*(f2-f1) + f1


    ! Determine the IFG truncation points
    ! ...Compute the optical delay grid
    x = computeX(f)
    ! ...Get the truncation points
    it1 = MINLOC(x, DIM=1, MASK=(x > -maxX))
    it2 = MAXLOC(x, DIM=1, MASK=(x <= maxX))
    ! ...The number of truncated IFG and SPC points
    n_tifg = it2-it1+1
    n_tspc = ComputeNSPC(n_tifg)
    WRITE(*,'( 5x,"IFG sampling width                       : ",f18.15," cm",&
             &/5x,"-X,+X truncation delays                  : ",f18.15,", ",f18.15," cm",&
             &/5x,"No. of truncated IFG points for IFG->SPC : ",i0,&
             &/5x,"No. of output SPC points                 : ",i0)') &
             ComputeMeanDelta(x), x(it1), x(it2), n_tifg, n_tspc


    ! Allocate the result arrays
    ALLOCATE( cf(n_tspc)  , &  ! Output from IFG->SPC FFT 
              cspc(n_tspc), &  ! Output from IFG->SPC FFT
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      msg = 'Error allocating result arrays'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); STOP
    END IF


    ! Compute the rolloff filters
    ! ...The front end
    if1 = is1; if2 = is1 + n_filter - 1
    err_stat = CosFilter( f(if1:if2), ffilter, FilterWidth=filter_width )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error computing cosine rolloff filter'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); STOP
    END IF
    ! ...The back end
    ib1 = is2-n_filter+1; ib2 = is2
    bfilter = ffilter(n_filter:1:-1)


    ! Compute the FTS specific quantities
    SELECT CASE(TRIM(SENSOR_NAME(n)))
      CASE('iasi') 
        irf = IASI_ApodFunction(band,x)
        ! Determine the actual channel numbers to keep
        ic1 = INT((IASI_BeginF(band) - f1)/IASI_dF(band) + ONEpointFIVE)
        ic2 = INT((IASI_EndF(  band) - f1)/IASI_dF(band) + ONEpointFIVE)
      CASE('cris')
        irf = CrIS_ApodFunction(band,x)
        ! Determine the actual channel numbers to keep (including guard channels)
        ic1 = INT((CrIS_BeginF(band,include_guard_channels=.TRUE.) - f1)/CrIS_dF(band) + ONEpointFIVE)
        ic2 = INT((CrIS_EndF(  band,include_guard_channels=.TRUE.) - f1)/CrIS_dF(band) + ONEpointFIVE)
      CASE DEFAULT
        msg = 'Unrecognised sensor name: '//TRIM(SENSOR_NAME(n))
        CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); STOP
    END SELECT
    n_cspc = ic2-ic1+1
    WRITE( *,'(5x,"No. of output ",a," band ",i0," points",9x,": ",i0,/)') TRIM(SENSOR_NAME(n)), band, n_cspc


    ! Perform the spectral reduction
    ! ...Initialize arrays
    spc = ZERO
    ifg = CMPLX(ZERO,ZERO,fp)
    ! ...Slot data into spectrum array
    spc(is1:is2) = solar%Irradiance
    ! ...Apply rollof filter to spectrum
    spc(if1:if2) = spc(if1:if2) * ffilter  ! Front-end
    spc(ib1:ib2) = spc(ib1:ib2) * bfilter  ! Back-end
    ! ...FFT filtered input spectrum to an interferogram
    err_stat = SPCtoIFG(f, spc, x, ifg  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'SPC->IFG FFT failed'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Apply apodisation function to IFG
    ifg = ifg * irf
    ! ...FFT truncated interferogram to a spectrum
    err_stat = IFGtoSPC(x(it1:it2), ifg(it1:it2), cf, cspc )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'IFG->SPC FFT failed'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Copy results to SpcCoeff structures
    SELECT CASE(TRIM(SENSOR_NAME(n)))
      CASE('iasi') 
        spccoeff%Solar_Irradiance = REAL(cspc(ic1:ic2),fp)
      CASE('cris')
        spccoeff%Solar_Irradiance = CrIS_Remove_Guard_Channels(band,REAL(cspc(ic1:ic2),fp))
    END SELECT


    ! Deallocate all the arrays
    DEALLOCATE( ffilter, bfilter, irf, f, spc, x, ifg, cf, cspc, &
                STAT = alloc_stat )
    IF ( alloc_stat /= 0) THEN
      msg = 'Error deallocating arrays'
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); STOP
    END IF

  END SUBROUTINE Compute_FTS_Solar
END PROGRAM Create_FTS_SpcCoeff
