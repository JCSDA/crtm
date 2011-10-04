MODULE Read_AtmProfile

  !--- modules

  use type_kinds, only : fp_kind
  use Message_handler
  use AtmProfile_Define
  use AtmProfile_netCDF_IO
  USE Units_Conversion

  IMPLICIT NONE
  !--- public & private

  Private

  Public  :: Read_AtmosProf

  TYPE, PUBLIC :: AtmProfSet_type                                                          
                                                                                           
    integer :: Nlay        ! # of profile layers                                           
    integer :: Natm        ! # of profiles                                                 
    character(len=256) :: ProfileTag                                                       
                                                                                           
    real(fp_kind), pointer, dimension(:)   :: p_lev    ! 0:Nlay, pressure level            
    real(fp_kind), pointer, dimension(:,:) :: t_lev    ! 0:Nlay x 1:Natm, temperature      
    real(fp_kind), pointer, dimension(:,:) :: wet_lev  ! 0:Nlay x 1:Natm, vapor            
    real(fp_kind), pointer, dimension(:,:) :: ozo_lev  ! 0:Nlay x 1:Natm, ozone            
                                                                                           
    real(fp_kind), pointer, dimension(:)   :: p_lay    ! Nlay, layer pressure              
    real(fp_kind), pointer, dimension(:,:) :: t_lay    ! Nlay x 1:Natm, layer temperature  
    real(fp_kind), pointer, dimension(:,:) :: wet_lay  ! Nlay x 1:Natm, vapor              
    real(fp_kind), pointer, dimension(:,:) :: ozo_lay  ! Nlay x 1:Natm, ozone              
                                                                               
    real(fp_kind), pointer, dimension(:)   :: latitude  ! Natm           
                                                                                           
  END TYPE AtmProfSet_type                                                                 

  !--- parameters for reading netCDF atmos and trans data files
  
  integer,parameter   :: AbsID_HITRAN_H2O   = 1     ! HITRAN atmospheric indices
  integer,parameter   :: AbsID_HITRAN_O3    = 3
  integer,parameter   :: UnitID_HITRAN_ppmw = 1
  integer,parameter   :: UnitID_HITRAN_gpkg = 3

  real(fp_kind),parameter :: ZERO    =  0._fp_kind
  real(fp_kind),parameter :: ONE     =  1._fp_kind
  real(fp_kind),parameter :: TWO     =  2._fp_kind
  real(fp_kind),parameter :: THREE   =  3._fp_kind
  real(fp_kind),parameter :: FOUR    =  4._fp_kind
  real(fp_kind),parameter :: FIVE    =  5._fp_kind
  real(fp_kind),parameter :: TEN     = 10._fp_kind
                                                      
  !--- module variables in private

  integer             :: IabsWet_AtmosProf
  integer             :: IabsOzn_AtmosProf
  integer,allocatable :: Absorber_Units_ID(:)


CONTAINS

  !====================================================================
  !
  ! SUBROUTINE: Open_AtmosProf()
  !
  !   Open_AtmosProf() opens a netCDF atmospheric data file,
  !   and read its header part to get parameters.
  !
  !====================================================================

  subroutine Read_atmosProf(InFileName_AtmosProf, atmProfSet)

    character(*), intent(in)  :: InFileName_AtmosProf
    type(atmProfSet_type), intent(out) :: atmProfSet
    
    !--- local variables

    integer             :: n_AbsInAtmos
    character(len=256)  :: ID_Tag
    character(len=256)  :: title
    character(len=512)  :: history
    character(len=256)  :: comment

    type(AtmProfile_type)  :: AtmProfile
    
    integer             :: err_status, Allocate_Status
    integer             :: Iabs

    integer :: n, i, j, Ilay, Iatm, Nlay, Natm
    
    !--- initialize

    print *
    print *, '=== Atmosphere profile set'
    print *, 'File Name         : ', trim( InFileName_AtmosProf )

    !--- open an atmospheric profile file

!    err_status = Open_AtmProfile_netCDF( InFileName_AtmosProf, &
!                                    InFileId_AtmosProf, &
!                                    mode = 'READ' )

!    if( err_status /= SUCCESS )then
!      print *, '### ERROR IN OPENING AN ATMOSPHERIC PROFILE DATA FILE ###'
!      stop 
!    endif


    !--- get atmos profile parameters

    ID_Tag  = ' '
    title   = ' '
    history = ' '
    comment = ' '

    err_status = Inquire_AtmProfile_netCDF( InFileName_AtmosProf,       &
                                       n_Layers    = Nlay,    &
                                       n_Absorbers = n_AbsInAtmos,      &
                                       n_Profiles  = Natm,    &
                                       ID_Tag      = ID_Tag,            &
                                       title       = title,             &
                                       history     = history,           &
                                       comment     = comment       )

    if( err_status /= SUCCESS )then
      print *, '### ERROR IN GETTING ATMOSPHERIC PROFILE INFORMATION ###'
      print *, 'Error err_status : ', err_status
      stop
    endif

    print *, '# of profiles     : ', Natm
    print *, '# of layers       : ', Nlay
    print *, '# of absorbers    : ', n_AbsInAtmos
    print *, 'ID Tag            : ', trim( ID_Tag  )
    print *, 'Title             : ', trim( title   )
    print *, 'History           : ', trim( history )
    print *, 'Comment           : ', trim( comment )

    atmProfSet%ProfileTag = ID_TAG
    
    !--- check # of profiles and layers

    if( Natm < 1 .or. Nlay < 1 .or. Nlay > 1000 )then
      print *, '### ERROR ###'
      print *, '# of profiles or layers is not acceptable.'
      stop
    endif
      
    call allocate_atmProfSet( Nlay, Natm, atmProfSet )


    !--- read profile                                                  

    err_status = Read_AtmProfile_netCDF( InFileName_AtmosProf, &         
                                         AtmProfile )                         

    if( err_status /= SUCCESS )then                                      
      print *, '### ERROR IN READING ATMOSPHERIC PROFILE DATA FILE ###'  
      stop 90                                                            
    endif                                                                

    !--- retrieve absorber profile seq num for wet and ozone

    IabsWet_AtmosProf = -1
    IabsOzn_AtmosProf = -1
    do Iabs = 1, AtmProfile%n_Absorbers
      if( AtmProfile%Absorber_ID(Iabs) == AbsID_HITRAN_H2O )then
        IabsWet_AtmosProf = Iabs
      endif
      if( AtmProfile%Absorber_ID(Iabs) == AbsID_HITRAN_O3 )then
        IabsOzn_AtmosProf = Iabs
      endif
    enddo
    IF( IabsWet_AtmosProf <= 0 .OR. IabsOzn_AtmosProf <= 0 )THEN
      print *, '### ERROR IN FINDING WATER VAPOR OR O3 IN ABSORBER ID LIST ###'
      stop
    END IF


    !--- profile loop

    print *
    print *, '=== Atmospheric profile list'


    do Iatm = 1, Natm



      print *, 'Profile :', Iatm


      !--- retrieve level values

      atmProfSet%p_lev(0:Nlay) = AtmProfile%Level_Pressure((Nlay+1):1:-1, Iatm)
      !--- to layer
        
      atmProfSet%p_lay(1:Nlay) =  ( atmProfSet%p_lev(0:Nlay-1) - atmProfSet%p_lev(1:Nlay) ) / &
                  log( atmProfSet%p_lev(0:Nlay-1) / atmProfSet%p_lev(1:Nlay) )

      atmProfSet%t_lev(0:Nlay,Iatm)  =  AtmProfile%Level_Temperature((Nlay+1):1:-1, Iatm)
      atmProfSet%ozo_lev(0:Nlay,Iatm) =  AtmProfile%Level_Absorber((Nlay+1):1:-1,IabsOzn_AtmosProf, Iatm)

      if( AtmProfile%Absorber_Units_ID(IabsWet_AtmosProf) == UnitID_HITRAN_gpkg )then
      
        atmProfSet%wet_lev(0:Nlay, Iatm) = AtmProfile%Level_Absorber((Nlay+1):1:-1,IabsWet_AtmosProf, Iatm)

      else if( AtmProfile%Absorber_Units_ID(IabsWet_AtmosProf) == UnitID_HITRAN_ppmw)then
      
        atmProfSet%wet_lev(0:Nlay, Iatm) = ppmv_to_mr( AtmProfile%Level_Absorber((Nlay+1):1:-1,IabsWet_AtmosProf, Iatm), &
	                                 Molecule_ID = AbsID_HITRAN_H2O )

      else
 
        print *, 'ERROR: H2O units is not unexpected'
        stop
       
      endif
      
      ! --- convert to layer variables
      
      atmProfSet%t_lay(:, Iatm) =  (atmProfSet%t_lev(0:Nlay-1, Iatm)  + atmProfSet%t_lev(1:Nlay, Iatm))/TWO
      atmProfSet%wet_lay(:, Iatm) =  (atmProfSet%wet_lev(0:Nlay-1, Iatm)  + atmProfSet%wet_lev(1:Nlay, Iatm))/TWO
      atmProfSet%ozo_lay(:, Iatm) =  (atmProfSet%ozo_lev(0:Nlay-1, Iatm)  + atmProfSet%ozo_lev(1:Nlay, Iatm))/TWO

    enddo
    
  end subroutine Read_atmosProf

  subroutine allocate_atmProfSet( Nlay, Natm, atmProfSet)
  
    integer, intent(in)                      :: Nlay, Natm
    type(atmProfSet_type), intent(out)    :: atmProfSet
    
    !---local
    integer :: alloca_err
    
    allocate( atmProfSet%p_lev(0:Nlay),        &
              atmProfSet%t_lev(0:Nlay, Natm) , &
              atmProfSet%wet_lev(0:Nlay, Natm),&
              atmProfSet%ozo_lev(0:Nlay, Natm), &
              atmProfSet%p_lay(Nlay),        &
              atmProfSet%t_lay(Nlay, Natm) , &
              atmProfSet%wet_lay(Nlay, Natm),&
              atmProfSet%ozo_lay(Nlay, Natm), & 
              atmProfSet%latitude(Natm), &             
              STAT = alloca_err )
                   
    if(alloca_err /= 0)then
    
      print *, 'Insufficient space to allocate atmProfSet'
      stop
      
    endif
    
    atmProfSet%Nlay = Nlay
    atmProfSet%Natm = Natm 

  end subroutine allocate_atmProfSet
  
  subroutine destroy_atmProfSet(atmProfSet)
  
    type(atmProfSet_type), intent(inout) :: atmProfSet
    
    deallocate(atmProfSet%p_lev, &
               atmProfSet%t_lev,&
               atmProfSet%wet_lev,&
               atmProfSet%ozo_lev, &
               atmProfSet%p_lay, &
               atmProfSet%t_lay,&
               atmProfSet%wet_lay,&
               atmProfSet%ozo_lay, &
               atmProfSet%latitude )
               
    atmProfSet%Nlay = -1
    atmProfSet%Natm = -1
    
  end subroutine destroy_atmProfSet

  
end MODULE Read_AtmProfile         
