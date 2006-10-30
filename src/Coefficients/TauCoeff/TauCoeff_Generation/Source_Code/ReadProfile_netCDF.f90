!====================================================================
!
! MODULE: ReadProfile_netCDF
!
!   SUBROUTINE: Open_AtmosProf()
!   SUBROUTINE: Read_AtmosProf()
!   SUBROUTINE: Open_TransProf()
!   SUBROUTINE: Read_TransProf()
!
!
! Created by Y.Tahara in Aug,02
! Modified by Y.Tahara in Sep,02 to read netCDF trans data.
! Modified by Y.Tahara in Oct,02 to read netCDF atmos profiles.
!
!====================================================================


module ReadProfile_netCDF

  !--- modules

  use type_kinds, only : fp_kind
  use error_handler
  use ParametersGenCoef
  use AtmProfile_Define
  use AtmProfile_netCDF_IO
  use TauProfile_netCDF_IO
  use Units_Conversion
  use ConvLevelLayer

  !--- implicit

  implicit none


  !--- public & private

  Private

  Public  Open_AtmosProf
  Public  Read_AtmosProf
  Public  Open_TransProf
  Public  Read_TransProf


  !--- parameters for reading netCDF atmos and trans data files

  integer,parameter   :: AbsID_HITRAN_H2O   = 1     ! HITRAN atmospheric indices
  integer,parameter   :: AbsID_HITRAN_O3    = 3
  integer,parameter   :: UnitID_HITRAN_ppmw = 1
  integer,parameter   :: UnitID_HITRAN_gpkg = 3

  !--- module variables in private

  integer             :: InFileId_AtmosProf
  integer             :: InFileId_TransProf

  integer             :: IabsWet_AtmosProf
  integer             :: IabsOzn_AtmosProf
  integer,allocatable :: Absorber_Units_ID(:)

  logical,save        :: Flag_OzoneTrans	! False: no ozone trans


 contains


  !====================================================================
  !
  ! SUBROUTINE: Open_AtmosProf()
  !
  !   Open_AtmosProf() opens a netCDF atmospheric data file,
  !   and read its header part to get parameters.
  !
  !====================================================================

  subroutine Open_AtmosProf()

    !--- local variables

    integer             :: n_AbsInAtmos
    character(len=256)  :: ID_Tag
    character(len=256)  :: title
    character(len=512)  :: history
    character(len=256)  :: comment
    integer,allocatable :: Absorber_ID(:)

    integer             :: Iflag
    integer             :: Iabs

    !--- initialize

    print *
    print *, '=== Atmosphere profile set'
    print *, 'File Name         : ', trim( InFileName_AtmosProf )


    !--- open an atmospheric profile file

    Iflag = Open_AtmProfile_netCDF( InFileName_AtmosProf, &
                                    InFileId_AtmosProf, &
                                    mode = 'READ' )

    if( Iflag /= SUCCESS )then
      print *, '### ERROR IN OPENING AN ATMOSPHERIC PROFILE DATA FILE ###'
      stop 90
    endif


    !--- get atmos profile parameters

    ID_Tag  = ' '
    title   = ' '
    history = ' '
    comment = ' '

    Iflag = Inquire_AtmProfile_netCDF( InFileName_AtmosProf,       &
                                       InFileId_AtmosProf,         &
                                       n_Layers    = Nlay,         &
                                       n_Absorbers = n_AbsInAtmos, &
                                       n_Profiles  = Natm,         &
                                       ID_Tag      = ID_Tag,       &
                                       title       = title,        &
                                       history     = history,      &
                                       comment     = comment       )

    if( Iflag /= SUCCESS )then
      print *, '### ERROR IN GETTING ATMOSPHERIC PROFILE INFORMATION ###'
      print *, 'Error flg : ', Iflag
      stop 90
    endif

    !--- cascade history information

    History_all = TRIM(history)//TRIM(History_all)

    ProfileTag = ID_TAG
    
    print *, '# of profiles     : ', Natm
    print *, '# of layers       : ', Nlay
    print *, '# of absorbers    : ', n_AbsInAtmos
    print *, 'ID Tag            : ', trim( ID_Tag  )
    print *, 'Title             : ', trim( title   )
    print *, 'History           : ', trim( history )
    print *, 'Comment           : ', trim( comment )


    !--- check # of profiles and layers

    if( Natm < 1 .or. Natm > Natm_max .or. &
        Nlay < 1 .or. Nlay > Nlay_max )then
      print *, '### ERROR ###'
      print *, '# of profiles or layers is not acceptable.'
      stop 90
    endif


    !--- absorber ID and unit

    allocate( Absorber_ID      (n_AbsInAtmos) )
    allocate( Absorber_Units_ID(n_AbsInAtmos) )

    Iflag = Inquire_AtmProfile_netCDF( InFileName_AtmosProf, &
                                       InFileId_AtmosProf,   &
                                       Absorber_ID       = Absorber_ID, &
                                       Absorber_Units_ID = Absorber_Units_ID )

    if( Iflag /= SUCCESS )then
      print *, '### ERROR IN GETTING ATMOSPHERIC PROFILE ABSORBER INFORMATION ###'
      print *, 'Error flg : ', Iflag
      stop 90
    endif

    print *, 'Absorber ID       : ', Absorber_ID
    print *, 'Absorber Units ID : ', Absorber_Units_ID
    print *, 'Absorber Units    : ', ( trim(ATMPROFILE_ABSORBER_UNITS_NAME(Absorber_Units_ID(Iabs))) // ' ', Iabs=1, n_AbsInAtmos )


    !--- retrieve absorber profile seq num for wet and ozone

    do Iabs = 1, n_AbsInAtmos
      if( Absorber_ID(Iabs) == AbsID_HITRAN_H2O )then
        IabsWet_AtmosProf = Iabs
        goto 1100
      endif
    enddo
    print *, '### ERROR IN FINDING WATER VAPOR IN ABSORBER ID LIST ###'
    stop 90
1100 continue

    do Iabs = 1, n_AbsInAtmos
      if( Absorber_ID(Iabs) == AbsID_HITRAN_O3 )then
        IabsOzn_AtmosProf = Iabs
        goto 1200
      endif
    enddo
    print *, '### ERROR IN FINDING OZONE IN ABSORBER ID LIST ###'
    stop 90
1200 continue

    print *, 'WV index #        : ', IabsWet_AtmosProf
    print *, 'O3 index #        : ', IabsOzn_AtmosProf


    !--- check absorber units

    if( Absorber_Units_ID(IabsWet_AtmosProf) /= UnitID_HITRAN_ppmw .and. &
        Absorber_Units_ID(IabsWet_AtmosProf) /= UnitID_HITRAN_gpkg )then
      print *, '### ERROR IN ATMOS DATA ###'
      print *, 'Unit of WV is not acceptable.'
      stop 90
    endif

    if( Absorber_Units_ID(IabsOzn_AtmosProf) /= UnitID_HITRAN_ppmw )then
      print *, '### ERROR IN ATMOS DATA ###'
      print *, 'Unit of O3 is not acceptable.'
      stop 90
    endif

  end subroutine Open_AtmosProf



  !====================================================================
  !
  ! SUBROUTINE: Read_AtmosProf()
  !
  !   Read_AtmosProf() reads atmospheric profiles.
  !
  !====================================================================

  subroutine Read_AtmosProf( p_lev, P_lay, t_lay, q_lay, o3_lay )

    !--- interface

    real(fp_kind),intent(out) :: p_lev (0:Nlay)         ! pressure on levels (hPa)
    real(fp_kind),intent(out) :: p_lay (  Nlay)         ! pressure on layers (hPa)
    real(fp_kind),intent(out) :: t_lay (  Nlay,Natm)    ! temperature on layers (K)
    real(fp_kind),intent(out) :: q_lay (  Nlay,Natm)    ! wv mixing ratio on layers (g/kg)
    real(fp_kind),intent(out) :: o3_lay(  Nlay,Natm)    ! ozone content on layers (ppmv)

    !--- local variables

    type(AtmProfile_type) :: AtmProfile
    integer               :: Iflag
    integer               :: Iatm

    real(fp_kind)         :: dmy_p_lev  (0:Nlay)
    real(fp_kind)         :: dmy_p_lay  (  Nlay)
    real(fp_kind)         :: dmy_t_lev  (0:Nlay)
    real(fp_kind)         :: dmy_abs_lev(0:Nlay,2)
    real(fp_kind)         :: dmy_abs_lay(  Nlay,2)

    REAL( fp_kind ), PARAMETER   :: MW_H2O    = 18.01528_fp_kind


    !--- read pressure level and layer

    Iflag = Inquire_AtmProfile_netCDF( InFileName_AtmosProf, &
                                       InFileId_AtmosProf,   &
                                       Level_Pressure = dmy_p_lev, &
                                       Layer_Pressure = dmy_p_lay  )

    if( Iflag /= SUCCESS )then
      print *, '### ERROR IN READING PRESSURE LEVELS AND LAYERS ###'
      print *, 'Error flg : ', Iflag
      stop 90
    endif

    p_lev(0:Nlay) = dmy_p_lev(Nlay:0:-1)

    if( .not. Flag_AtmProfile_Lev2Lay )then
      p_lay(1:Nlay) = dmy_p_lay(Nlay:1:-1)
    endif

    !--- initialize AtmProfile

    CALL Initialize_AtmProfile( AtmProfile )


    !--- profile loop

    print *
    print *, '=== Atmospheric profile list'

    do Iatm = 1, Natm


      !--- read a profile

      Iflag = Read_AtmProfile_netCDF( InFileName_AtmosProf, &
                                      InFileId_AtmosProf,   &
                                      Iatm,                 &
                                      AtmProfile )

      if( Iflag /= SUCCESS )then
        print *, '### ERROR IN READING ATMOSPHERIC PROFILE DATA FILE ###'
        print *, 'File Name : ', InFileName_AtmosProf
        print *, 'Profile # : ', Iatm
        print *, 'Error flg : ', Iflag
        stop 90
      endif

      print *, 'Profile :', Iatm
      print *, '  DateTime    :', AtmProfile%DateTime
      print *, '  LatLonHgt   :', real( AtmProfile%Location%latitude,  4 ), &
                                  real( AtmProfile%Location%longitude, 4 ), &
                                  real( AtmProfile%Location%surface_altitude, 4 )
      print *, '  Climatology :', AtmProfile%Climatology_Model
      print *, '  Description :', trim( AtmProfile%Description )


      !--- retrieve temp, wv and o3 (plus p_lay in case of convering level to layer)

      if( Flag_AtmProfile_Lev2Lay )then

        !--- retrieve level values

        dmy_t_lev  (0:Nlay)   = AtmProfile%Level_Temperature((Nlay+1):1:-1)
        dmy_abs_lev(0:Nlay,2) = AtmProfile%Level_Absorber   ((Nlay+1):1:-1,IabsOzn_AtmosProf)

        if( Absorber_Units_ID(IabsWet_AtmosProf) == UnitID_HITRAN_gpkg )then
          dmy_abs_lev(0:Nlay,1) =             AtmProfile%Level_Absorber((Nlay+1):1:-1,IabsWet_AtmosProf)
        else
          dmy_abs_lev(0:Nlay,1) = ppmv_to_mr( AtmProfile%Level_Absorber((Nlay+1):1:-1,IabsWet_AtmosProf), Molecule_ID=1 )
        endif

        !--- level to layer

        call Conv_LevelLayer( p_lev, dmy_t_lev,     dmy_abs_lev, &
                              p_lay, t_lay(:,Iatm), dmy_abs_lay, &
                              Nlay+1, 2 )


        q_lay (:,Iatm) = dmy_abs_lay(:,1)
        o3_lay(:,Iatm) = dmy_abs_lay(:,2)


      else

        !--- retrieve layer values

        t_lay (1:Nlay,Iatm) = AtmProfile%Layer_Temperature(Nlay:1:-1)
        o3_lay(1:Nlay,Iatm) = AtmProfile%Layer_Absorber   (Nlay:1:-1,IabsOzn_AtmosProf)

        if( Absorber_Units_ID(IabsWet_AtmosProf) == UnitID_HITRAN_gpkg )then
          q_lay(1:Nlay,Iatm) =             AtmProfile%Layer_Absorber(Nlay:1:-1,IabsWet_AtmosProf)
        else
          q_lay(1:Nlay,Iatm) = ppmv_to_mr( AtmProfile%Layer_Absorber(Nlay:1:-1,IabsWet_AtmosProf), Molecule_ID=1 )
       endif

      endif


      !--- reinitialize AtmProfile

      Iflag = Destroy_AtmProfile( AtmProfile )

      if( Iflag /= SUCCESS )then
        print *, '### ERROR IN REINITIALIZING AtmProfile ###'
        print *, 'Error flg : ', Iflag
        stop 90
      endif

    enddo


    !--- close an atmospheric profile file

    Iflag = Close_AtmProfile_netCDF( InFileId_AtmosProf )

    if( Iflag /= SUCCESS )then
      print *, '### ERROR IN CLOSING AN ATMOSPHERIC PROFILE FILE ###'
      print *, 'Error flg : ', Iflag
      stop 90
    endif

  end subroutine Read_AtmosProf



  !====================================================================
  !
  ! SUBROUTINE: Open_TransProf()
  !
  !   Open_TransProf() opens a netCDF transmittance data file,
  !   and read its header part to get parameters.
  !
  !====================================================================

  subroutine Open_TransProf()

    !--- local variables

    integer                   :: n_layers
    integer                   :: n_channels
    integer                   :: n_angles
    integer                   :: n_profiles
    integer                   :: n_molecule_sets
    real(fp_kind),allocatable :: angle_list(:)
    integer,      allocatable :: profile_list(:)
    integer,      allocatable :: molecule_set_list(:)
    character(len=256)        :: ID_Tag
    character(len=256)        :: title
    character(len=512)        :: history
    character(len=256)        :: sensor_name
    character(len=256)        :: platform_name
    character(len=256)        :: comment

    integer                   :: Iflag


    !--- open transmittance data file


    !--- get transmittance information (no.1)

    ID_Tag        = ' '
    title         = ' '
    history       = ' '
    sensor_name   = ' '
    platform_name = ' '
    comment       = ' '

    Iflag = Inquire_TauProfile_netCDF( InFileName_TransProf,          &
                                       n_layers         = n_layers,   &
                                       n_channels       = n_channels, &
                                       n_angles         = n_angles,   &
                                       n_profiles       = n_profiles, &
                                       n_molecule_sets  = n_molecule_sets, &
                                       ID_Tag           = ID_Tag,          &
                                       title            = title,           &
                                       history          = history,         &
                                       sensor_name      = sensor_name,     &
                                       platform_name    = platform_name    )

    if( Iflag /= SUCCESS )then
      print *, '### ERROR IN GETTING TRANSMITTANCE INFORMATION ###'
      print *, 'File Name : ', InFileName_TransProf
      print *, 'Error flg : ', Iflag
      stop 90
    endif

    !--- cascade history information

    History_all = TRIM(history)//TRIM(History_all)

    print *
    print *, '=== Transmittance profile set'
    print *, 'File Name          : ', trim( InFileName_TransProf )
    print *, '# of channels      : ', n_channels
    print *, '# of profiles      : ', n_profiles
    print *, '# of angles        : ', n_angles
    print *, '# of layers        : ', n_layers
    print *, '# of molecule sets : ', n_molecule_sets
    print *, 'ID tag             : ', trim( ID_Tag        )
    print *, 'Titile             : ', trim( title         )
    print *, 'History            : ', trim( history       )
    print *, 'Sensor name        : ', trim( sensor_name   )
    print *, 'Platform name      : ', trim( platform_name )


    Iflag = Inquire_TauProfile_netCDF( InFileName_TransProf,          &
                                       InFileId_TransProf,            &
                                       comment          = comment     )

    if( Iflag > WARNING )then
      print *, '### ERROR IN GETTING TRANSMITTANCE INFORMATION ###'
      print *, 'File Name : ', InFileName_TransProf
      print *, 'Error flg : ', Iflag
      stop 90
    endif

    print *, 'Comment            : ', trim( comment       )

    if( n_layers /= Nlay )then
      print *, '### ERROR ###'
      print *, '# of layers must be', Nlay
      stop 90
    else if( n_channels /= Nchan )then
      print *, '### ERROR ###'
      print *, '# of channels must be', n_channels
      stop 90
    else if( n_angles < Nangle )then
      print *, '### ERROR ###'
      print *, '# of angles must be equal or larger than', Nangle
      stop 90
    else if( n_profiles /= Natm )then
      print *, '### ERROR ###'
      print *, '# of atmos profiles must be', Natm
      stop 90
    endif


    !--- allocate array

    allocate( channel_list     (Nchan          ) )
    allocate( angle_list       (n_angles       ) )
    allocate( profile_list     (Natm           ) )
    allocate( molecule_set_list(n_molecule_sets) )


    !--- get transmittance information (no.2)


    Iflag = Inquire_TauProfile_netCDF( InFileName_TransProf,                 &
                                       channel_list      = channel_list,     &
                                       angle_list        = angle_list,       &
                                       profile_list      = profile_list,     &
                                       molecule_set_list = molecule_set_list )

    if( Iflag /= SUCCESS )then
      print *, '### ERROR IN GETTING TRANSMITTANCE INFORMATION ###'
      print *, 'File Name : ', InFileName_TransProf
      print *, 'Error flg : ', Iflag
      stop 90
    endif

    print *, 'Channel list       : ', channel_list
    print *, 'Angle list         : ', real( angle_list, 4 )
    print *, 'Profile list       : ', profile_list
    print *, 'Molecule set list  : ', molecule_set_list


    !--- keep angle list

    allocate( secant(Nangle) )
    secant(1:Nangle) = angle_list(1:Nangle)


    !--- check whether or not ozone transmittances exist

    if( all( molecule_set_list /= IdMolecule_Ozo ) )then
      Flag_OzoneTrans = .false.
      print *
      print *, '### WARNING ###'
      print *, 'NO OZONE TRANSMITTANCES'
      print *
    else
      Flag_OzoneTrans = .true.
    endif
    

  end subroutine Open_TransProf



  !====================================================================
  !
  ! SUBROUTINE: Read_TransProf()
  !
  !   Read_TransProf() read a netCDF transmittance data file
  !   to get dry, wet and ozone gas transmittances and 
  !   total gas transmittances. 
  !
  !   Before returning transmittances, QC is applied to check
  !    1) Total transmittances be within the range between 0 and 1
  !    2) Total transmittances be equal to multiplying 3 gas
  !       transmittances
  !    3) Total transmittances decrease as descending from TOA
  !   If an illegal transmittance might be found, the program would stop.
  !   It is allowed for each gas transmittances to be larger than 1 and
  !   increase as descending.
  !
  !====================================================================

  subroutine Read_TransProf( Ichan, &
                             alltrans_lev, &
                             drytrans_lev, wettrans_lev, ozntrans_lev )

    !--- interface

    integer      ,intent(in)  :: Ichan                              ! ch seq # to read LBL trans set
    real(fp_kind),intent(out) :: alltrans_lev(0:Nlay,Nangle,Natm)   ! total gas transmittances
    real(fp_kind),intent(out) :: drytrans_lev(0:Nlay,Nangle,Natm)   ! dry
    real(fp_kind),intent(out) :: wettrans_lev(0:Nlay,Nangle,Natm)   ! wet
    real(fp_kind),intent(out) :: ozntrans_lev(0:Nlay,Nangle,Natm)   ! ozone

    !--- local variables

    integer :: Ilev, Iang, Iatm, idx_chan
    integer :: Iflag


    !--- channel index

    idx_chan = channel_list( Ichan )


    !--- profile loop

    do Iang = 1, Nangle
    do Iatm = 1, Natm


      !--- TOA transmittances

      alltrans_lev(0 ,Iang,Iatm) = ONE
      drytrans_lev(0 ,Iang,Iatm) = ONE
      wettrans_lev(0 ,Iang,Iatm) = ONE
      ozntrans_lev(0:,Iang,Iatm) = ONE


      !--- read transmittance

      Iflag = Read_TauProfile_netCDF( InFileName_TransProf, &  ! Input
                                      idx_chan,             &  ! Input
                                      secant(Iang),         &  ! Input
                                      Iatm,                 &  ! Input
                                      IdMolecule_All,       &  ! Input
                                      alltrans_lev(1:Nlay,Iang,Iatm) )  ! Output

      if( Iflag /= SUCCESS )then
        call error_readtrans( IdMolecule_All )
      endif

      Iflag = Read_TauProfile_netCDF( InFileName_TransProf,&  ! Input
                                      idx_chan,            &  ! Input
                                      secant(Iang),        &  ! Input
                                      Iatm,                &  ! Input
                                      IdMolecule_Dry,      &  ! Input
                                      drytrans_lev(1:Nlay,Iang,Iatm) )  ! Output

      if( Iflag /= SUCCESS )then
        call error_readtrans( IdMolecule_Dry )
      endif

      Iflag = Read_TauProfile_netCDF( InFileName_TransProf,&  ! Input
                                      idx_chan,            &  ! Input
                                      secant(Iang),        &  ! Input
                                      Iatm,                &  ! Input
                                      IdMolecule_Wet,      &  ! Input
                                      wettrans_lev(1:Nlay,Iang,Iatm) )  ! Output

      if( Iflag /= SUCCESS )then
        call error_readtrans( IdMolecule_Wet )
      endif

      if( Flag_OzoneTrans )then

        Iflag = Read_TauProfile_netCDF( InFileName_TransProf,&  ! Input
                                        idx_chan,               &  ! Input
                                        secant(Iang),           &  ! Input
                                        Iatm,                   &  ! Input
                                        IdMolecule_Ozo,         &  ! Input
                                        ozntrans_lev(1:Nlay,Iang,Iatm) )  ! Output

        if( Iflag /= SUCCESS )then
          call error_readtrans( IdMolecule_Ozo )
        endif

      endif


      !--- check total tau within 0 to 1

      do Ilev = 1, Nlay

        if( alltrans_lev(Ilev,Iang,Iatm) < ZERO  .or. &
            alltrans_lev(Ilev,Iang,Iatm) > ONE   )then
          print *, '### ERROR IN LBL TRANS DATA ###'
          print *, 'Total trans beyond the range between 0 and 1'
          print *, 'Atom,Angle,Lev -->', Iatm, Iang, Ilev
          print *, 'Trans          -->', alltrans_lev(Ilev,Iang,Iatm)
          stop 90
        endif

      enddo


      !--- revise missing value and check tolerance

      do Ilev = 1, Nlay

        if( alltrans_lev(Ilev,Iang,Iatm) < TOLERANCE )then
          alltrans_lev(Ilev,Iang,Iatm) = ZERO
        endif

        if( drytrans_lev(Ilev,Iang,Iatm) < ZERO )then
          drytrans_lev(Ilev,Iang,Iatm) = RMISS
        else if( drytrans_lev(Ilev,Iang,Iatm) < TOLERANCE )then
          drytrans_lev(Ilev,Iang,Iatm) = ZERO
        endif

        if( wettrans_lev(Ilev,Iang,Iatm) < ZERO )then
          wettrans_lev(Ilev,Iang,Iatm) = RMISS
        else if( wettrans_lev(Ilev,Iang,Iatm) < TOLERANCE )then
          wettrans_lev(Ilev,Iang,Iatm) = ZERO
        endif

        if( ozntrans_lev(Ilev,Iang,Iatm) < ZERO )then
          ozntrans_lev(Ilev,Iang,Iatm) = RMISS
        else if( ozntrans_lev(Ilev,Iang,Iatm) < TOLERANCE )then
          ozntrans_lev(Ilev,Iang,Iatm) = ZERO
        endif

      enddo


      !--- check consistency between total tau and effective tau

      do Ilev = 1, Nlay

        if( abs( alltrans_lev(Ilev,Iang,Iatm) &
                 - max(drytrans_lev(Ilev,Iang,Iatm),ZERO) &
                 * max(wettrans_lev(Ilev,Iang,Iatm),ZERO) &
                 * max(ozntrans_lev(Ilev,Iang,Iatm),ZERO) ) &
                     > 3 * TOLERANCE )then
          print *, '### ERROR IN LBL TRANS DATA ###'
          print *, 'Inconsistent between 3 gas trans and total trans'
          print *, 'Atom,Angle,Lev -->', Iatm, Iang, Ilev
          print *, 'Trans - total,3gas,dry,wet,o3 -->', &
                        alltrans_lev(Ilev,Iang,Iatm), &
                        max(drytrans_lev(Ilev,Iang,Iatm),ZERO)  &
                      * max(wettrans_lev(Ilev,Iang,Iatm),ZERO)  &
                      * max(ozntrans_lev(Ilev,Iang,Iatm),ZERO), &
                        max(drytrans_lev(Ilev,Iang,Iatm),ZERO), &
                        max(wettrans_lev(Ilev,Iang,Iatm),ZERO), &
                        max(ozntrans_lev(Ilev,Iang,Iatm),ZERO)
          stop 90
        endif

      enddo


      !--- check if total tau decreasing from top to bottom


      do Ilev = 1, Nlay

        if(   alltrans_lev(Ilev-1,Iang,Iatm) &
            < alltrans_lev(Ilev  ,Iang,Iatm) )then
          print *, '### ERROR IN LBL TRANS DATA ###'
          print *, 'Total transmittances increase as descending'
          print *, 'Atom,Angle,Lev -->', Iatm, Iang, Ilev
          print *, 'Total(Lev-1,Lev) -->', alltrans_lev(Ilev-1,Iang,Iatm), &
                                           alltrans_lev(Ilev  ,Iang,Iatm)
          print *, 'Dry(Lev-1,Lev)   -->', drytrans_lev(Ilev-1,Iang,Iatm), &
                                           drytrans_lev(Ilev  ,Iang,Iatm)
          print *, 'Wet(Lev-1,Lev)   -->', wettrans_lev(Ilev-1,Iang,Iatm), &
                                           wettrans_lev(Ilev  ,Iang,Iatm)
          print *, 'Ozo(Lev-1,Lev)   -->', ozntrans_lev(Ilev-1,Iang,Iatm), &
                                           ozntrans_lev(Ilev  ,Iang,Iatm)
          stop 90
        endif

      enddo


      !--- check if each gas tau decreasing from top to bottom

      do Ilev = 1, Nlay
        if( drytrans_lev(Ilev,Iang,Iatm) < HMISS .or. &
            drytrans_lev(Ilev,Iang,Iatm) > drytrans_lev(Ilev-1,Iang,Iatm) )then
          drytrans_lev(Ilev:Nlay,Iang,Iatm) = drytrans_lev(Ilev-1,Iang,Iatm)
          exit
        endif
      enddo

      do Ilev = 1, Nlay
        if( wettrans_lev(Ilev,Iang,Iatm) < HMISS .or. &
            wettrans_lev(Ilev,Iang,Iatm) > wettrans_lev(Ilev-1,Iang,Iatm) )then
          wettrans_lev(Ilev:Nlay,Iang,Iatm) = wettrans_lev(Ilev-1,Iang,Iatm)
          exit
        endif
      enddo

      do Ilev = 1, Nlay
        if( ozntrans_lev(Ilev,Iang,Iatm) < HMISS .or. &
            ozntrans_lev(Ilev,Iang,Iatm) > ozntrans_lev(Ilev-1,Iang,Iatm) )then
          ozntrans_lev(Ilev:Nlay,Iang,Iatm) = ozntrans_lev(Ilev-1,Iang,Iatm)
          exit
        endif
      enddo

    enddo
    enddo

    !--- end of subroutine Read_TransProf


  contains
  
    !--- print error message in reading trans and stop the program
  
    subroutine error_readtrans( IdMolecule )
  
      integer,intent(in) :: IdMolecule

      print *, '### ERROR IN READING TRANSMITTANCES ###'
      print *, 'File Name : ', InFileName_TransProf
      print *, 'Ch seq #  : ', Ichan
      print *, 'Angle     : ', Iang
      print *, 'Profile   : ', Iatm
      print *, 'Molec ID  : ', IdMolecule
      print *, 'Error flg : ', Iflag
      stop 90

    end subroutine error_readtrans

  end subroutine Read_TransProf

end module ReadProfile_netCDF

!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
! $date: 2003/06/09, Yong Han
!
! Remove defination for IdMolecule_Dry,IdMolecule_Wet,IdMolecule_Ozo from
! this module to module ParametersGenCoef.f90
!
