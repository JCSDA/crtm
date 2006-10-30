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
  use error_handler
  use SensorInfo_Define
  use SensorInfo_IO
  use SpcCoeff_Define
  use SpcCoeff_Binary_IO
  use ParametersGenCoef
  use ChanType
  use PredictandPredictor


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

    integer,parameter :: InFileId_Namelist   = 1
    integer,parameter :: InFileId_AtmPredFlg = 10
    integer,parameter :: InFileId_ProfParam  = 11

    real(fp_kind)     :: A0, A1
    integer           :: Ichan, Ichan1
    integer           :: i, Isen
    integer           :: Iabs
    integer           :: Iflag


    !--- namelist

    namelist /SATSEN/  SatName,      &	         ! name of satellite	
                       SenName,      &	         ! name of sensor
                       Ichan_top,    &	         ! top ch seq #
                       Ichan_last	         ! last ch seq #
    namelist /GENCOEF/ Iabsorber,    &	         ! absorber gas ID (1:dry,2:wet,3:o3)
                       Alpha,        &		 ! coef to define a curve between abs amount and level
                       A0,           & 	         ! min abs amount
                       A1,     	     & 	  	 ! max abs amount
						 ! (if Alpha == 0, then Alpha,A0,A1 is assigned later)
		       Nangle,       &	         ! # of incidence angles used in coefficient calculation
                       Natmpred_allcombsearch, & ! # of atmos predictors whose all combinations are searched
		       Flag_netCDF_file, &       ! T) read netCDF profile data, F) CIMSS3246 binary data file
                       Flag_AtmProfile_Lev2Lay   ! T) convert atmos profiles from level to layer
                        		         ! F) read layer atmos profiles

    !--- read namelist

    Alpha                   = ZERO	! default
    Flag_netCDF_file        = .TRUE.	! default
    Flag_AtmProfile_Lev2Lay = .FALSE.	! default

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

    call Initialize_SensorInfo( SensorInfo )

    Iflag = Read_SensorInfo( InFileName_SenInfo, SensorInfo )

    if( Iflag /= SUCCESS ) then
      print *, '### ERROR ###'
      print *, 'Fail to read a sensor information file'
      print *, 'File name: ', InFileName_SenInfo
      stop 90
    endif


    !--- read spectral coefficients
    !- (assumption)
    !-  There is no channel indices in a spectral coefficient file.
    !-  The order of channels in the file is assumed to be the same as that used in NCEP.

    Iflag = Read_SpcCoeff_Binary( InFileName_SpcCoef, SpcCoeff )

    if( Iflag /= SUCCESS ) then
      print *, '### ERROR ###'
      print *, 'Fail to read a spectral coefficient file'
      print *, 'File name: ', InFileName_SpcCoef
      stop 90
    endif


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

    Isen = 0
    do i = 1, SensorInfo%n_Sensors
      if( trim(SensorInfo%Satellite_Name(i)) == trim(SatName) .and. &
          trim(SensorInfo%Sensor_Name   (i)) == trim(SenName) )then
        Isen = i
      endif
    enddo

    if( Isen == 0 )then
      print *, '### ERROR ###'
      print *, 'Specified satellite or sensor is not found in satellite infomation file'
      print *, '  Sat = ', SatName
      print *, '  Sen = ', SenName
      print *, 'They must be in the list'
      do i = 1, SensorInfo%n_Sensors
        print '( 1x, 3( 1x, a12 ), 3( 5x, i5 ) )', &
              SensorInfo%Sensor_Name     ( i ), &
              SensorInfo%Satellite_Name  ( i ), &
              SensorInfo%File_Prefix     ( i ), &
              SensorInfo%NCEP_Sensor_ID  ( i ), &
              SensorInfo%WMO_Sensor_ID   ( i ), &
              SensorInfo%WMO_Satellite_ID( i )
      enddo
      stop 90
    endif

    SatIdWmo  = SensorInfo%WMO_Satellite_ID( Isen )
    SenIdWmo  = SensorInfo%WMO_Sensor_ID   ( Isen )
    SenIdNcep = SensorInfo%NCEP_Sensor_ID  ( Isen )


    !--- retrieve channel type

    allocate( channel_type(Nchan) )

    do Ichan = 1, Nchan
      channel_type(Ichan) = Get_ChanType( SpcCoeff%wavenumber(Ichan), &
   			                  SpcCoeff%frequency (Ichan)  )
    enddo
    

    !--- read atmos predictor use flags

    allocate( atmpredflag_avail(Natmpred_max,Nchan) )

    open( InFileId_AtmPredFlg,            &
          FILE   = InFileName_AtmPredFlg, &
          STATUS = 'OLD',                 &
          ACCESS = 'SEQUENTIAL',          &
          FORM   = 'FORMATTED',           &
          ACTION = 'READ'                 )

    do Ichan = 1, Nchan
      read(InFileId_AtmPredFlg,*) Ichan1, atmpredflag_avail(:,Ichan1)
    enddo

    close( InFileId_AtmPredFlg )


    !--- set unuse for some predictor according to channel type
    
    do Ichan = 1, Nchan
      call Unuse_PredFlag( channel_type(Ichan),       &
                           atmpredflag_avail(:,Ichan) )
    enddo
    
    
    !--- print assigned parameters

    print *
    print *, '=== Satellite & sensor'
    print *, 'Sat           = ', SatName
    print *, 'Sen           = ', SenName
    print *, 'Sat ID (WMO)  = ', SatIdWmo
    print *, 'Sen ID (WMO)  = ', SenIdWmo
    print *, 'Sen ID (NCEP) = ', SenIdNcep
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
    print *, 'Order of polynomial function          = ', Npolyorder

    if( abs(Alpha) < TOLERANCE )then
      print *
      print *, '### ABSORBER AMOUNT-LEVEL CONVERSION COEFFICIENTS WILL BE CALCULATED LATER ###'
    else
      print *, 'Abs amount-level curve coef - Alpha   = ', Alpha
      print *, 'Min absorber amount         - A0      = ', A0
      print *, 'Max absorber amount         - A1      = ', A1
    endif

    print *
    do Ichan = Ichan_top, Ichan_last
      print '(1x,A,I3,A,6(5I2,1X))', &
                'Available predictor for ch seq #', Ichan, ' = ', &
                atmpredflag_avail(:,Ichan)
    enddo

  end subroutine Read_Parameters

end module ReadParameters
