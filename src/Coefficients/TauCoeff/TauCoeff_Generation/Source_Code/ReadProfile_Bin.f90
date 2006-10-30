!====================================================================
!
! MODULE: ReadProfile_Bin
!
!   SUBROUTINE: Open_AtmosProf_Bin()
!   SUBROUTINE: Read_AtmosProf_Bin()
!   SUBROUTINE: Open_TransProf_Bin()
!   SUBROUTINE: Read_TransProf_Bin()
!
!
! Created by Y.Tahara in Aug,02
! Modified by Y.Tahara in Sep,02 to read netCDF trans data.
! Modified by Y.Tahara in Oct,02 to read netCDF atmos profiles.
! Modified by Y.Tahara in Nov,02 to read binary CIMSS3246 profiles.
!
!====================================================================


module ReadProfile_Bin

  !--- modules

  use type_kinds, only : fp_kind
  use ParametersGenCoef
  use ConvLevelLayer

  !--- implicit

  implicit none


  !--- public & private

  Private

  Public  Open_AtmosProf_Bin
  Public  Read_AtmosProf_Bin
  Public  Open_TransProf_Bin
  Public  Read_TransProf_Bin


  !--- module variables in private

  character(len=255),parameter :: InFileName_PressProf   = 'pressure.txt'
  character(len=255),parameter :: InFileName_TempGasProf = 'atmprof.bin'
  integer           ,parameter :: InFileId_PressProf     = 20
  integer           ,parameter :: InFileId_TempGasProf   = 21

  character(len=255),parameter :: InFileName_TransDry = 'transdry.bin'
  character(len=255),parameter :: InFileName_TransWet = 'transwet.bin'
  character(len=255),parameter :: InFileName_TransOzo = 'transozo.bin'
  integer           ,parameter :: InFileId_TransDry   = 25
  integer           ,parameter :: InFileId_TransWet   = 26
  integer           ,parameter :: InFileId_TransOzo   = 27

  integer           ,save      :: Iflag_ExistOzo


 contains


  !====================================================================
  !
  ! SUBROUTINE: Open_AtmosProf_Bin()
  !
  !   Open_AtmosProf_Bin() opens atmospheric data files
  !   and return profile parameters
  !
  !====================================================================

  subroutine Open_AtmosProf_Bin()

    !--- initialize

    print *
    print *, '=== Atmosphere profile set'
    print *, 'File Name (P)     : ', trim( InFileName_PressProf   )
    print *, 'File Name (T,Q,O) : ', trim( InFileName_TempGasProf )


    !--- profile parameters
    
    Natm = 32
    Nlay = 45
    
    print *, '# of profiles     : ', Natm
    print *, '# of layers       : ', Nlay


    !--- open an atmospheric profile file

    open( InFileId_PressProf,            &
          FILE   = InFileName_PressProf, &
          STATUS = 'OLD',                &
          ACCESS = 'SEQUENTIAL',         &
          FORM   = 'FORMATTED',          &
          ACTION = 'READ'                )


    !--- open an atmospheric profile file

    open( InFileId_TempGasProf,            &
          FILE   = InFileName_TempGasProf, &
          STATUS = 'OLD',                  &
          ACCESS = 'DIRECT',               &
          FORM   = 'UNFORMATTED',          &
          ACTION = 'READ',                 &
	  RECL   = (4*((Nlay+1)*3+1))      )


  end subroutine Open_AtmosProf_Bin



  !====================================================================
  !
  ! SUBROUTINE: Read_AtmosProf_Bin()
  !
  !   Read_AtmosProf_Bin() reads atmospheric profiles.
  !
  !====================================================================

  subroutine Read_AtmosProf_Bin( p_lev, p_lay, t_lay, q_lay, o3_lay )

    !--- interface

    real(fp_kind),intent(out) :: p_lev (0:Nlay)         ! pressure on levels (hPa)
    real(fp_kind),intent(out) :: p_lay (  Nlay)         ! pressure on layers (hPa)
    real(fp_kind),intent(out) :: t_lay (  Nlay,Natm)    ! temperature on layers (K)
    real(fp_kind),intent(out) :: q_lay (  Nlay,Natm)    ! wv mixing ratio on layers (g/kg)
    real(fp_kind),intent(out) :: o3_lay(  Nlay,Natm)    ! ozone content on layers (ppmv)

    !--- local variables

    real(fp_kind) :: dmy_t_lev  (0:Nlay)
    real(fp_kind) :: dmy_abs_lev(0:Nlay,2)
    real(fp_kind) :: dmy_abs_lay(  Nlay,2)

    real(4)       :: dmy4_t_lev(0:Nlay)
    real(4)       :: dmy4_q_lev(0:Nlay)
    real(4)       :: dmy4_o_lev(0:Nlay)

    integer       :: Iatm, Iflag


    !--- read pressure

    read(InFileId_PressProf,*) p_lev


    !--- read temp, wv, o3 and convert level to layer

    do Iatm = 1, Natm
    
      read(InFileId_TempGasProf,rec=Iatm) dmy4_t_lev(0:Nlay), &
      				  	  dmy4_q_lev(0:Nlay), &
					  dmy4_o_lev(0:Nlay), &
					  Iflag

      print *, 'Read profile --> No:', Iatm, '   Type:', Iflag

      dmy_t_lev(:)     = real( dmy4_t_lev(:), fp_kind )
      dmy_abs_lev(:,1) = real( dmy4_q_lev(:), fp_kind )
      dmy_abs_lev(:,2) = real( dmy4_o_lev(:), fp_kind )

      call Conv_LevelLayer( p_lev, dmy_t_lev,     dmy_abs_lev, &
                            p_lay, t_lay(:,Iatm), dmy_abs_lay, &
                            Nlay+1, 2 )

      q_lay (:,Iatm) = dmy_abs_lay(:,1)
      o3_lay(:,Iatm) = dmy_abs_lay(:,2)

    enddo


  end subroutine Read_AtmosProf_Bin



  !====================================================================
  !
  ! SUBROUTINE: Open_TransProf_Bin()
  !
  !   Open_TransProf_Bin() opens transmittance data files
  !   and return some sensor parameters.
  !
  !====================================================================

  subroutine Open_TransProf_Bin()

    !--- local variables

    real(fp_kind),parameter :: angle_list(Nangle_max) &
    					= (/ 1.00_fp_kind, &
					     1.25_fp_kind, &
					     1.50_fp_kind, &
					     1.75_fp_kind, &
					     2.00_fp_kind, &
					     2.25_fp_kind, &
					     3.00_fp_kind /)

    integer :: i


    !--- initialize

    print *
    print *, '=== Transmittance profile set'
    print *, 'File Name (Dry)    : ', trim( InFileName_TransDry )
    print *, 'File Name (Wet)    : ', trim( InFileName_TransWet )
    print *, 'File Name (Ozo)    : ', trim( InFileName_TransOzo )


    !--- open transmittance data file

    open( InFileId_TransDry,            &
          FILE   = InFileName_TransDry, &
          STATUS = 'OLD',               &
          ACCESS = 'DIRECT',            &
          FORM   = 'UNFORMATTED',       &
          ACTION = 'READ',              &
	  RECL   = (4*Nlay)             )

    open( InFileId_TransWet,            &
          FILE   = InFileName_TransWet, &
          STATUS = 'OLD',               &
          ACCESS = 'DIRECT',            &
          FORM   = 'UNFORMATTED',       &
          ACTION = 'READ',              &
	  RECL   = (4*Nlay)             )

    open( InFileId_TransOzo,            &
          FILE   = InFileName_TransOzo, &
          STATUS = 'OLD',               &
          ACCESS = 'DIRECT',            &
          FORM   = 'UNFORMATTED',       &
          ACTION = 'READ',              &
	  RECL   = (4*Nlay),            &
          IOSTAT = Iflag_ExistOzo       )

    if( Iflag_ExistOzo /= 0 )then
      print *, '### NO LBL OZONE TRANSMITTANCE DATA ###'
    endif


    !--- channel list

    allocate( channel_list(Nchan) )

    if( SatName(1:4) == 'GOES' .and. SenName(1:6) == 'IMAGER' )then

      !--- goes imager
      
      channel_list(1:Nchan) = (/ (i+1,i=1,Nchan) /)

    else
   
      !--- other satellites
      
      channel_list(1:Nchan) = (/ (i,i=1,Nchan) /)

      print *, '### CHANNEL NUMBERS ARE ASSUMED BE THE SAME AS CHANNEL SEQUENTIAL NUMBER ###'

    endif

    print *, 'Channel list       : ', channel_list


    !--- angle list

    allocate( secant(Nangle) )
    secant(1:Nangle) = angle_list(1:Nangle)

    print *, 'Angle list         : ', real( angle_list, 4 )


  end subroutine Open_TransProf_Bin



  !====================================================================
  !
  ! SUBROUTINE: Read_TransProf_Bin()
  !
  !   Read_TransProf_Bin() read transmittance data files
  !
  !====================================================================

  subroutine Read_TransProf_Bin( Ichan, &
                             alltrans_lev, &
                             drytrans_lev, wettrans_lev, ozntrans_lev )

    !--- interface

    integer      ,intent(in)  :: Ichan                              ! ch seq # to read LBL trans set
    real(fp_kind),intent(out) :: alltrans_lev(0:Nlay,Nangle,Natm)   ! total gas transmittances
    real(fp_kind),intent(out) :: drytrans_lev(0:Nlay,Nangle,Natm)   ! dry
    real(fp_kind),intent(out) :: wettrans_lev(0:Nlay,Nangle,Natm)   ! wet
    real(fp_kind),intent(out) :: ozntrans_lev(0:Nlay,Nangle,Natm)   ! ozone

    !--- local variables

    real(fp_kind) :: TOLERANCE_REAL4

    real(4)       :: dmy4_lev(Nlay)
    integer       :: Ilev, Iang, Iatm, Irec


    !--- set 4 byte real tolerance

    TOLERANCE_REAL4 = real( EPSILON( 1._4 ), fp_kind )


    !--- read dry, wet trans
    
    drytrans_lev(0,:,:) = ONE
    wettrans_lev(0,:,:) = ONE

    do Iang = 1, Nangle
    do Iatm = 1, Natm

      Irec = Ichan + (Iatm - 1) * Nchan + (Iang - 1) * Nchan * Natm

      read(InFileId_TransDry,rec=Irec) dmy4_lev(1:Nlay)
      drytrans_lev(1:Nlay,Iang,Iatm) = real( dmy4_lev(1:Nlay), fp_kind )

      read(InFileId_TransWet,rec=Irec) dmy4_lev(1:Nlay)
      wettrans_lev(1:Nlay,Iang,Iatm) = real( dmy4_lev(1:Nlay), fp_kind )


    enddo
    enddo


    !--- read ozo trans

    if( Iflag_ExistOzo == 0 )then

      ozntrans_lev(0,:,:) = ONE

      Loop_ozone: &
      do Iang = 1, Nangle
      do Iatm = 1, Natm

      Irec = Ichan + (Iatm - 1) * Nchan + (Iang - 1) * Nchan * Natm

        read(InFileId_TransOzo,rec=Irec,iostat=Iflag_ExistOzo) dmy4_lev(1:Nlay)

        if( Iflag_ExistOzo == 0 )then
          ozntrans_lev(1:Nlay,Iang,Iatm) = real( dmy4_lev(1:Nlay), fp_kind )
        else
	  exit Loop_ozone
        endif

      enddo
      enddo Loop_ozone

    endif


    if( Iflag_ExistOzo /= 0 )then
      ozntrans_lev(:,:,:) = ONE
      print *, '### NO LBL OZONE TRANSMITTANCE DATA ###'
    endif


    !--- check tolerance

    do Iatm = 1, Natm
    do Iang = 1, Nangle
    do Ilev = 0, Nlay

      if( drytrans_lev(Ilev,Iang,Iatm) < ZERO )then
        drytrans_lev(Ilev,Iang,Iatm) = RMISS
      else if( drytrans_lev(Ilev,Iang,Iatm) < TOLERANCE_REAL4 )then
        drytrans_lev(Ilev,Iang,Iatm) = ZERO
      endif

      if( wettrans_lev(Ilev,Iang,Iatm) < ZERO )then
        wettrans_lev(Ilev,Iang,Iatm) = RMISS
      else if( wettrans_lev(Ilev,Iang,Iatm) < TOLERANCE_REAL4 )then
        wettrans_lev(Ilev,Iang,Iatm) = ZERO
      endif

      if( ozntrans_lev(Ilev,Iang,Iatm) < ZERO )then
        ozntrans_lev(Ilev,Iang,Iatm) = RMISS
      else if( ozntrans_lev(Ilev,Iang,Iatm) < TOLERANCE_REAL4 )then
        ozntrans_lev(Ilev,Iang,Iatm) = ZERO
      endif

    enddo
    enddo
    enddo


    !--- check if gas tau decreasing from top to bottom

    do Iatm = 1, Natm
    do Iang = 1, Nangle

      do Ilev = 1, Nlay
        if( drytrans_lev(Ilev,Iang,Iatm) < HMISS                          .or. &
            drytrans_lev(Ilev,Iang,Iatm) > drytrans_lev(Ilev-1,Iang,Iatm) .or. &
	    ( drytrans_lev(Ilev-1,Iang,Iatm) - drytrans_lev(Ilev,Iang,Iatm) < TOLERANCE_REAL4 .and. &
	      drytrans_lev(Ilev,Iang,Iatm) > 0.01_fp_kind                                     .and. &
	      drytrans_lev(Ilev,Iang,Iatm) < 0.99_fp_kind )               )then
          drytrans_lev(Ilev:Nlay,Iang,Iatm) = drytrans_lev(Ilev-1,Iang,Iatm)
          exit
        endif
      enddo

      do Ilev = 1, Nlay
        if( wettrans_lev(Ilev,Iang,Iatm) < HMISS                          .or. &
            wettrans_lev(Ilev,Iang,Iatm) > wettrans_lev(Ilev-1,Iang,Iatm) .or. &
	    ( wettrans_lev(Ilev-1,Iang,Iatm) - wettrans_lev(Ilev,Iang,Iatm) < TOLERANCE_REAL4 .and. &
	      wettrans_lev(Ilev,Iang,Iatm) > 0.01_fp_kind                                     .and. &
	      wettrans_lev(Ilev,Iang,Iatm) < 0.99_fp_kind )               )then
          wettrans_lev(Ilev:Nlay,Iang,Iatm) = wettrans_lev(Ilev-1,Iang,Iatm)
          exit
        endif
      enddo

      do Ilev = 1, Nlay
        if( ozntrans_lev(Ilev,Iang,Iatm) < HMISS                          .or. &
            ozntrans_lev(Ilev,Iang,Iatm) > ozntrans_lev(Ilev-1,Iang,Iatm) .or. &
	    ( ozntrans_lev(Ilev-1,Iang,Iatm) - ozntrans_lev(Ilev,Iang,Iatm) < TOLERANCE_REAL4 .and. &
	      ozntrans_lev(Ilev,Iang,Iatm) > 0.01_fp_kind                                     .and. &
	      ozntrans_lev(Ilev,Iang,Iatm) < 0.99_fp_kind )               )then
          ozntrans_lev(Ilev:Nlay,Iang,Iatm) = ozntrans_lev(Ilev-1,Iang,Iatm)
          exit
        endif
      enddo

    enddo
    enddo


    !--- total trans
    
    alltrans_lev(:,:,:) =   drytrans_lev(:,:,:) &
			  * wettrans_lev(:,:,:) &
			  * ozntrans_lev(:,:,:)


    !--- check total tau within 0 to 1

    alltrans_lev(:,:,:) = max( min( alltrans_lev(:,:,:), ONE ), ZERO )   


    !--- check tolerance for total

    where( alltrans_lev < TOLERANCE_REAL4 )
      alltrans_lev = ZERO
    endwhere


  end subroutine Read_TransProf_Bin

end module ReadProfile_Bin
