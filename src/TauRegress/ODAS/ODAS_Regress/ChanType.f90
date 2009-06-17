!====================================================================
!
! MODULE: ChanType
!
!   Function: Get_ChanType()
!
! Created by Y.Tahara in Nov,02
!
!====================================================================


module ChanType

  !--- modules
  
  use type_kinds, only : fp_kind
  use ParametersGenCoef


  !--- implicit

  implicit none


  !--- private & public

  private

  public Get_ChanType


 contains
   
  
  !====================================================================
  !
  ! Function: Get_ChanType()
  !
  !   Get_ChanType detects type of a particular channel from
  !   its frequency.
  !
  !====================================================================

  function Get_ChanType( wavenum, freq ) result ( chtype )

    !--- interface

    real(fp_kind),intent(in) :: wavenum		! wave number (cm^-1)
    real(fp_kind),intent(in) :: freq		! frequency (GHz)
    integer                  :: chtype		! channel type
                                                ! 1:dry 2:dominated by H2O abs line 3:ozone
						! 4:typically called dry but dominated by H2O continuum

    !--- infrared
    
    if( wavenum > 100._fp_kind )then

      !--- default is dry

      chtype = 1
    
      !--- Ozone dominates absorption

      if( wavenum >= 1000._fp_kind .and. wavenum < 1070._fp_kind )then
        chtype = 3
      endif 
    
      !--- H2O absorber lines dominate absorption

      if( wavenum >= 1200._fp_kind .and. wavenum < 2100._fp_kind )then
        chtype = 2
      endif

      !--- H2O continuum affected

      if( wavenum >=  710._fp_kind .and. wavenum < 2100._fp_kind )then
        chtype = chtype + 10
      endif
    
      return
      
    endif
    
    
    !--- microwave

    !--- default is dry

    chtype = 1

    !--- H2O absorber lines dominate absorption

    if( abs( freq - 23._fp_kind ) < 5._fp_kind )then
      chtype = 2
    endif

    !--- H2O absorbes more than oxigen 

    if( (freq >  80._fp_kind .and. freq < 110._fp_kind) .or. &
         freq > 130._fp_kind                            )then
      chtype = 2
    endif

  end function Get_ChanType

end module ChanType
