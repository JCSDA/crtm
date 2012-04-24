!====================================================================
!
! MODULE: ReadParameters
!
!  SUBROUTINE: Read_Parameters()
!
!
! Created by Y.Tahara in Aug,02
! Modified by Y.Tahara in Oct,02 to read netCDF data file, etc.
!
!====================================================================


module ReadParameters

  !--- modules

  use type_kinds, only : fp_kind
  use message_handler
  use SensorInfo_Define
  use SensorInfo_IO
  use SpcCoeff_Define
  use SpcCoeff_netCDF_IO
  use ParametersGenCoef
  use ChanType
  use PredictandPredictor
  use SensorInfo_LinkedList


  !--- implicit

  implicit none


  !--- public & private

  private

  public  Read_Parameters


 contains


  !====================================================================
  ! Read_Parameters() reads namelist parameters and sensor information.
  !====================================================================

  subroutine Read_Parameters()

    !--- local variables

    type(SensorInfo_type) :: SensorInfo
    TYPE( SensorInfo_List_type ) :: SensorInfo_List

    integer,parameter :: InFileId_Namelist   = 1
    integer,parameter :: InFileId_AtmPredFlg = 10
    integer,parameter :: InFileId_ProfParam  = 11

    real(fp_kind)     :: A0, A1
    integer           :: Ichan, Ichan1
    integer           :: i, Isen, n_Nodes
    integer           :: Iabs
    integer           :: Iflag
    
    character(LEN=History_strlen) :: TheHistory


    !--- namelist

    namelist /SATSEN/  SatName,      &         ! name of satellite	
                       SenName,      &         ! name of sensor
                       Ichan_top,    &         ! top ch seq #
                       Ichan_last                ! last ch seq #
    namelist /GENCOEF/ Iabsorber,    &         ! absorber gas ID (1:dry,2:wet,3:o3)
                       Alpha,        &         ! coef to define a curve between abs amount and level
                       A0,           &         ! min abs amount
                       A1,           &         ! max abs amount
                                               ! (if Alpha == 0, then Alpha,A0,A1 is assigned later)
                       Nangle,       &         ! # of incidence angles used in coefficient calculation
                       Natmpred_allcombsearch, & ! # of atmos predictors whose all combinations are searched
                       Flag_netCDF_file, &       ! T) read netCDF profile data, F) CIMSS3246 binary data file
                       Flag_AtmProfile_Lev2Lay   ! T) convert atmos profiles from level to layer
                              ! F) read layer atmos profiles

    !--- read namelist

    Alpha                   = ZERO   ! default
    Flag_netCDF_file        = .TRUE. ! default
    Flag_AtmProfile_Lev2Lay = .FALSE.! default

    open( InFileId_Namelist,            &
          FILE   = InFileName_Namelist, &
          STATUS = 'OLD',               &
          ACCESS = 'SEQUENTIAL',        &
          FORM   = 'FORMATTED',         &
          ACTION = 'READ'               )

    read( InFileId_Namelist, SATSEN  )
    read( InFileId_Namelist, GENCOEF )

    close( InFileId_Namelist )

    MinAbsAmount = A0
    MaxAbsAmount = A1


    !--- check namelist parameters

    if( Ichan_top < 1 .or. Ichan_last < 1 .or. &
        Ichan_top > Ichan_last )then
      print *, '### ERROR ###'
      print *, 'Specified channel numbers are not acceptable.'
      print *, 'Top  ch seq # = ', Ichan_top
      print *, 'Last ch seq # = ', Ichan_last
      stop 90
    endif

    if( Iabsorber < 1 .or. Iabsorber > Nabsorber )then
      print *, '### ERROR ###'
      print *, 'Specified gas ID is not acceptable.'
      print *, 'It should be one of 1)dry 2)wet 3)O3, but ', Iabsorber
      stop 90
    endif

    if( Nangle < 1 .or. Nangle > Nangle_max )then
      print *, '### ERROR ###'
      print *, 'Specified # of incidence angles is not acceptable.'
      print *, '# of inc ang = ', Nangle
      stop 90
    endif


    !--- read satellite and sensor information

    SensorInfo_List = New_SensorInfo_List()
    Iflag = Read_SensorInfo( InFileName_SenInfo, SensorInfo_List )


    if( Iflag /= SUCCESS ) then
      print *, '### ERROR ###'
      print *, 'Fail to read a sensor information file'
      print *, 'File name: ', InFileName_SenInfo
      stop 90
    endif

    if( Iflag /= SUCCESS ) then
      print *, '### ERROR ###'
      print *, 'Fail to destroy SensorInfo_List'
      stop 90
    endif

    !--- read spectral coefficients

    Iflag = SpcCoeff_netCDF_ReadFile( InFileName_SpcCoef, SpcCoeff, &
                                  History = TheHistory )

    if( Iflag /= SUCCESS ) then
      print *, '### ERROR ###'
      print *, 'Fail to read a spectral coefficient file'
      print *, 'File name: ', InFileName_SpcCoef
      stop 90
    endif

    !--- cascade history information

    History_all = TRIM(TheHistory)//';  '//TRIM(History_all)

    !--- retrieve # of channels

    Nchan = SpcCoeff%n_Channels

    if( Ichan_top > Nchan )then
      print *, '### ERROR ###'
      print *, 'Specified channel numbers are not acceptable.'
      print *, '# of sensor ch          = ', Nchan
      print *, 'Specified top  ch seq # = ', Ichan_top
      print *, 'Specified last ch seq # = ', Ichan_last
      stop 90
    else if( Ichan_last > Nchan )then
      Ichan_last = Nchan
      print *, '### WORNING ###'
      print *, 'Specified last ch seq # is changed to sensor channel #.'
    endif

    !--- retrieve IDs for specified sat & sen

    n_Nodes = Count_SensorInfo_Nodes( SensorInfo_List )

    Isen = 0

Loop_notes: do i =1, n_Nodes

      Iflag = GetFrom_SensorInfo_List( SensorInfo_List, i, SensorInfo )
      if( Iflag /= SUCCESS ) then
        print *, '### ERROR ###'
        print *, 'Fail to get a note from SensorInfo_List'
        stop 90
      endif

      if( trim(SensorInfo%Satellite_Name) == trim(SatName) .and. &
          trim(SensorInfo%Sensor_Name) == trim(SenName) )then
        Isen = i
        exit Loop_notes
      endif

      Iflag = Destroy_SensorInfo( SensorInfo )
      if( Iflag /= SUCCESS ) then
        print *, '### ERROR ###'
        print *, 'Failed to destroy SensorInfo for node #', i
        stop 90
      endif
                  
    enddo Loop_notes   

    Iflag = Destroy_SensorInfo_List( SensorInfo_List)

    if( Isen == 0 )then
      print *, '### ERROR ###'
      print *, 'Specified satellite or sensor is not found in satellite infomation file'
      print *, '  Sat = ', SatName
      print *, '  Sen = ', SenName
      print *, 'They must be in the list'
      stop 90
    endif

    SatIdWmo  = SensorInfo%WMO_Satellite_ID
    SenIdWmo  = SensorInfo%WMO_Sensor_ID
!    SenType   = SensorInfo%Sensor_Type
    SenIdNcep = -1 !SensorInfo%NCEP_Sensor_ID


    !--- retrieve channel type

    allocate( channel_type(Nchan) )

    do Ichan = 1, Nchan
      channel_type(Ichan) = Get_ChanType( SpcCoeff%wavenumber(Ichan), &
                                          SpcCoeff%frequency (Ichan)  )
    enddo
            
    !--- print assigned parameters

    print *
    print *, '=== Satellite & sensor'
    print *, 'Sat           = ', SatName
    print *, 'Sen           = ', SenName
    print *, 'Sat ID (WMO)  = ', SatIdWmo
    print *, 'Sen ID (WMO)  = ', SenIdWmo
    print *, 'Sen ID (NCEP) = ', SenIdNcep
!    print *, 'Sen Type      = ', SenType
    print *, '# of sen ch   = ', Nchan
    print *, 'Top  ch seq # = ', Ichan_top
    print *, 'Last ch seq # = ', Ichan_last
    print *
    do Ichan = Ichan_top, Ichan_last
      print '(1x,a,i4,i3,6E17.8)', &
            'Ch seq #, Type, Freq (cm^-1,GHz), Planck c1,c2, Band c1,c2 =', &
            Ichan, channel_type(Ichan), &
            SpcCoeff%wavenumber(Ichan), &
            SpcCoeff%frequency (Ichan), &
            SpcCoeff%planck_c1 (Ichan), &
            SpcCoeff%planck_c2 (Ichan), &
            SpcCoeff%band_c1   (Ichan), &
            SpcCoeff%band_c2   (Ichan)   
    enddo

    print *
    print *, '=== Parameters for generating transmittance coefficients'
    print *, 'Specified gas 1)dry 2)wet 3)O3        = ', Iabsorber
    print *, '# of inc ang used                     = ', Nangle
    print *, '# of atmos pred available             = ', Natmpred_max
    print *, '# of atmos pred used in regression    = ', Natmpred_maxused
    print *, '# of atmos pred used in all comb srch = ', Natmpred_allcombsearch
    print *, 'Max order of polynomial function          = ', Npolyorder_max

    if( abs(Alpha) < TOLERANCE )then
      print *
      print *, '### ABSORBER AMOUNT-LEVEL CONVERSION COEFFICIENTS WILL BE CALCULATED LATER ###'
    else
      print *, 'Abs amount-level curve coef - Alpha   = ', Alpha
      print *, 'Min absorber amount         - A0      = ', A0
      print *, 'Max absorber amount         - A1      = ', A1
    endif
    
    Iflag = Destroy_SensorInfo( SensorInfo )

  end subroutine Read_Parameters

end module ReadParameters

!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Date: 2003/05/23, Name: Yong Han
!
!  ---  1) read netCDF SpcCoeff file instead of original binary file.
!       2) Update the interface for SensorInfo file (now a linked list is used
!          for holding sensor information for each sensor).
!
!
