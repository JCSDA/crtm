!====================================================================
!
! MODULE: ReadProfile_netCDF
!
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
  use Message_handler
  use ParametersGenCoef
  use TauProfile_netCDF_IO
 
  !--- implicit

  implicit none


  !--- public & private

  Private

  Public  Open_TransProf
  Public  Read_TransProf


  logical,save        :: Flag_OzoneTrans ! False: no ozone trans


 contains

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
                                       history          = history )
    !                                   sensor_name      = sensor_name,     &
    !                                   platform_name    = platform_name    )

    if( Iflag /= SUCCESS )then
      print *, '### ERROR IN GETTING TRANSMITTANCE INFORMATION ###'
      print *, 'File Name : ', InFileName_TransProf
      print *, 'Error flg : ', Iflag
      stop 90
    endif

    !--- cascade history information

    History_all = TRIM(history)//';  '//TRIM(History_all)

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
!    print *, 'Sensor name        : ', trim( sensor_name   )
!    print *, 'Platform name      : ', trim( platform_name )


    Iflag = Inquire_TauProfile_netCDF( InFileName_TransProf,          &
                                       comment          = comment     )

    if( Iflag > WARNING )then
      print *, '### ERROR IN GETTING TRANSMITTANCE INFORMATION ###'
      print *, 'File Name : ', InFileName_TransProf
      print *, 'Error flg : ', Iflag
      stop 90
    endif

    !--- cascade comment

    Comment_all = TRIM(comment)//';  '//TRIM(comment_all)
 
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

      ! for IASI sensor don't check the total tau within 0 to 1

      !--- check total tau within 0 to 1

      if(ALL(SenIdWmo /= INTERFEROMETER_LIST)) THEN
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
      endif

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
      if(ALL(SenIdWmo /= INTERFEROMETER_LIST)) THEN

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
      endif

      ! for IASI sensor don't check the total tau within 0 to 1

      !--- check if total tau decreasing from top to bottom
      if(ALL(SenIdWmo /=INTERFEROMETER_LIST) ) THEN


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
     endif

      !--- check if each gas tau decreasing from top to bottom

      do Ilev = 1, Nlay
       
        if( drytrans_lev(Ilev,Iang,Iatm) < HMISS .or. &
            drytrans_lev(Ilev,Iang,Iatm) > drytrans_lev(Ilev-1,Iang,Iatm) )then
          drytrans_lev(Ilev,Iang,Iatm) = drytrans_lev(Ilev-1,Iang,Iatm)
        endif
      enddo

      do Ilev = 1, Nlay
        if( wettrans_lev(Ilev,Iang,Iatm) < HMISS .or. &
            wettrans_lev(Ilev,Iang,Iatm) > wettrans_lev(Ilev-1,Iang,Iatm) )then
          wettrans_lev(Ilev,Iang,Iatm) = wettrans_lev(Ilev-1,Iang,Iatm)
        endif
      enddo

      do Ilev = 1, Nlay
        if( ozntrans_lev(Ilev,Iang,Iatm) < HMISS .or. &
            ozntrans_lev(Ilev,Iang,Iatm) > ozntrans_lev(Ilev-1,Iang,Iatm) )then
          ozntrans_lev(Ilev,Iang,Iatm) = ozntrans_lev(Ilev-1,Iang,Iatm)
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
