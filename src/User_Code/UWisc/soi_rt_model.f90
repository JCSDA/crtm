!--------------------------------------------------------------------------------------------------
!
! Successive Order of Interaction radiative transfer model for
! computing microwave TOA brightness temperatures or radiances.
!
! Version 1.0 (9/10/2004)
!
!   Written by:
!               Chris O'Dell (UW AOS)
!               Tom Greenwald (SSEC/CIMSS)
!               Andrew Heindinger (NOAA/Nesdis)
!               Ralf Bennartz (UW AOS)
!
!   PURPOSE:
!           This model performs a 1D radiative transfer with scattering.  It evaluates the column amount
!       scattering below it in order to determine the appropriate radiative transfer model to use.  If
!       there is no scattering, a simple (and efficient) no-scattering subroutine is called.
!       If there is scattering, a successive order of interaction approach is used.  This is similar
!       to the standard successive order of scatter, but inside a given layer, a photon is allowed to
!       scatter many times.  Entering a layer, scattering (one time or many times) and leaving this layer
!       is called an "interaction".  
!
!       This model assumes an atmosphere that depolarizes radiation upon scattering, but does not ever
!       induce polarization.  In particular, it uses an unpolarized (intensity only) Henyey-Greenstein
!       scattering phase function.
!   
!       The model returns the upwelling, top-of-atmosphere brightness temperature in both v and h 
!       polarization.  Its typical accuracy is about 0.4 K or better for scattering cases, and better
!       than 0.1 K accuracy for no-scattering cases.
!
!       The model is currently done in floating-point (4-byte) arithmetic everywhere.  We find this completely
!       sufficient to achieve high accuracy, as no matix inversions or other numerically unstable operations
!       are performed anywhere in the code.  
!
!
!	NOTE ON BRIGHTNESS TEMPERATURE VS. RADIANCE:
!		This program basically just adds up source terms at a given frequency.  The safest thing to do is interpret
!		the "temp" and "tsurf" variables as planck radiances, B(nu,T), which you must calculate yourself and 
!		pass to this subroutine.  If the problem is sufficiently Rayleigh-Jeans (ie lower frequency microwave channels),
!		you can usually get away with using the temperatures directly, and out will pop the brightness temperature.
!		However, I find that the CMB temperature is treated insufficiently at frequencies higher than 30 GHz.
!
!
! 	NOTE ON LEVEL/LAYER ORDERING:
!		All layers herein are ordered surface to TOA.  The arrays opd, ssa, and asy each have Nlayers elements:
!		1 is the lowest layer, 2 is the second lowest, ... Nlayers is the highest.  The array "temp" has Nlayers+1 
!		elements, and specifies the level (or boundary) temperatures between the layers.  It is ordered such that
!		temp(1) is the lowest level temperature (like 2m air temp), temp(2) is for the upper boundary of the 1st layer,
!		and so on.  temp(nlayers+1) is the TOA.  If you have the layer temperatures, and not the level temperatures, 
!		the simplest thing to do is convert layer to level temperatures using the module level_temperatures.f90.
!
!
!   CALLING SEQUENCE:
!
!       call soi_rt( opd, ssa, asy, temp, zang, e_v, e_h, tbv, tbh, &
!                    tsurf = tsurf, r_v = r_v, r_h = r_h, tspace = tspace, &
!                    delta_scaling = delta_scaling, noscatter = noscatter, &
!                    verbose = verbose, nscat = nscat, nlay = nlay )
!
!
!   INPUT VARIABLES (required):
!           NAME    TYPE                DESCRIPTION
!           opd     REAL[Nlayers]       Layer optical depths (contains Nlayers values).  Ordered surface to TOA.
!
!           ssa     REAL[Nlayers]       Layer single scatter albedos.  
!
!           asy     REAL[Nlayers]       Layer asymmetry parameters.                         
!
!           temp    REAL[Nlayers+1]     Level temperatures or Planck radiances.  Note: A "level" is the boundary
!                                       between two "layers".
!
!           zang    REAL                Observation zenith angle [degrees].
!
!           e_v     REAL[N_SOI_ANGLES]  Emissivities at v-polarization for each of the required angles.  
!                                       These angles are defined by the parameter soi_angles.
!
!           e_h     REAL[N_SOI_ANGLES]  As above, but for h-polarization.
!
!
!   OPTIONAL INPUTS:
!           tsurf   REAL                Surface Temperature [Kelvin], or Planck radiance.
!
!           r_v     REAL[N_SOI_ANGLES]  Reflectivities at v-pol for each of the required angles.  Defaults to
!                                       1 - e_v.
!
!           r_h     REAL[N_SOI_ANGLES]  Reflectivities at h-pol for each of the required angles.  Defaults to
!                                       1 - e_h.
!
!           tspace  REAL                Uniform background radiance or temperature.  Defaults to cosmic microwave 
!										temperature of 2.725 K.
!
!           Delta_Scaling   LOGICAL     Setting this to .TRUE. turns on delta-scaling.  Defaults to .FALSE.
!
!           NoScatter       LOGICAL     Setting this to .TRUE. forces absorption-only RT. Defaults to .FALSE.
!
!           Verbose         LOGICAL     Setting this to .TRUE. turns on verbose output.  Defaults to .FALSE.
!
!
!   OUTPUT VARIABLES (required):
!           tbv     REAL                Brightness Temperature (or just brightness) at TOA in v-polarization.
!
!           tbh     REAL                Same as above, but in h-polarization.
!                                               background temperature of 2.725 K.
!
!   OPTIONAL OUTPUTS:   
!           nscat   INTEGER             Number of orders of interaction achieved before convergence.
!
!           nlay    INTEGER             Number of "thin" layers used (before doubling).
!
!
!   Revision history:
!
!               9/9/2004        *       Fixed Source Term error (again) in Truncated_Doubling.  Zo term remains at 0.
!
!               9/9/2004        *       Abandoned coupling of this model to an emissivity model.  Now the
!                                       user is required to provide all necessary emissivity data.  Reflectivity
!                                       is still an optional input.
!
!               9/7/2004        *       Broke out thermal source calculation and phase function calculation
!                                       into their own subroutines.
!
!               7/9/2004        *       Corrected bug in expression for A and B used in source functions
!                                       Sp and Sm. "opd" was changed to "opd(i)"
!
!               7/2/2004        *       Explicitly saves up/down Tb's for absorption-only RT model
!                                       and R/T matrices for each doubling step and layer
!
!               6/1/2004        *       This version passes through particle and gas extinction 
!                                       coefficients as separate variables; explicitly saves upwelling
!                                       and downwelling Tb's at each level and order of interaction
!
!       4/23/2004
!               *   Changes from old version:
!                   Many options removed and hard-coded into the model, such as minimum layer thickness, 
!                   number of streams, number of legendre expansion terms for phase function, convergence
!                   threshold, delta scaling, etc.  Added branching routine to determine which RT model to use.
!                   Made the two RT subroutines NESTED within the main routine.  This simplified the code, and 
!                   made it slightly faster (no passing of variables required).
!
!       3/5/2004  (Old, original version)
!               *   Uses TRUN_DOUB_SUB to calculate doubling (only called when doubling necessary).
!               *   Changed initialization in TRUN_DOUB_SUB to be correct.
!               *   Doesn't use trun_doub_sub for no doubling of a layer.
!               *   Only calculates scattering terms when necessary
!               *   Uses F90 techniques for passing data to function, and automatically
!                   allocates necessary arrays.
!               *   Calculates source term to 2nd order in temperature for optically thick layers.
!               *       Uses Tcmb = 2.725 instead of 2.7 (more accurate).
!               *   Optional Delta Scaling, seems to work, 
!                   but doesn't help in microwave (hurts a little).
!               *   Two ways to calculate # of legendre terms.  Not sure which is better.
!               *   Brute force matrix multiplication in SOS loop.  Uses matmul in trun_doub_sub.
!
!
!
!--------------------------------------------------------------------------------------------------

MODULE type_kinds

    IMPLICIT NONE

    INTEGER, PARAMETER          ::  Single = selected_real_kind(6)
    INTEGER, PARAMETER          ::  Double = selected_real_kind(15)
    INTEGER, PARAMETER          ::  fp_kind = Double

END MODULE Type_Kinds


MODULE soi_rt_model

	USE Type_Kinds, ONLY	:	fpk => fp_kind

    IMPLICIT NONE

! MODULE Parameters 

    REAL(fpk), PUBLIC, PARAMETER    	 ::  min_sopd = 0.0001_fpk  ! cut-off sopd between no-scattering and scattering.
    REAL(fpk), PUBLIC, PARAMETER         ::  cut_sopd = 0.01_fpk   ! cut-off sopd between 2 and 4 streams.
    REAL(fpk), PUBLIC, PARAMETER         ::  d2r = 0.017453293_fpk ! converts degrees to radians.
    REAL(fpk), PUBLIC, PARAMETER         ::  log2 = 0.693147181_fpk ! natural log of 2
    REAL(fpk), PUBLIC, PARAMETER         ::  Tcmb = 2.725_fpk ! microwave background temp [K].
    REAL(fpk), PRIVATE, PARAMETER        ::  ZERO = 0.0_fpk
    REAL(fpk), PRIVATE, PARAMETER        ::  ONE = 1.0_fpk
	REAL(fpk), PRIVATE, PARAMETER		 ::  TWO = 2.0_fpk
    REAL(fpk), PRIVATE, PARAMETER        ::  HALF = 0.5_fpk

 ! Parameters for SOI Model 
    INTEGER, PUBLIC, PARAMETER  ::  N_SOI_ANGLES = 4    ! Number of total angles needed by SOI (in general)
    INTEGER, PUBLIC, PARAMETER  ::  nlay_max = 100      ! Max allowed # of layers
    INTEGER, PUBLIC, PARAMETER  ::  nscat_max = 40      ! Max allowed # of scattering events
    INTEGER, PUBLIC, PARAMETER  ::  nstrm_max = 3       ! Max allowed # of streams

	
    REAL(fpk), DIMENSION(1), PARAMETER   ::  pts2_ = (/ 0.577350269_fpk /)  ! quadrature pts for 2-stream model
    REAL(fpk), DIMENSION(2), PARAMETER   ::  pts4_ = (/ 0.211324865_fpk, 0.788675135_fpk /) ! quadrature pts for 4-stream model
    REAL(fpk), DIMENSION(1), PARAMETER   ::  wgts2_ = (/ ONE /) ! quadrature weights for 2-stream model
    REAL(fpk), DIMENSION(2), PARAMETER   ::  wgts4_ = (/ HALF, HALF /) !quadrature weights for 4-stream model


    REAL(fpk), PUBLIC, PARAMETER, DIMENSION(N_SOI_ANGLES-1)  :: soi_angles = (/ 53.7356103, & ! to be used by caller, so
                                                                           77.7999960, & ! he/she knows what angles
                                                                           37.9381274 /) ! to calculate emissivity for.

    REAL(fpk), PUBLIC, PARAMETER     ::  min_tau2 = 0.1_fpk      ! initial layer scattering optical depth for 2-stream model.
    REAL(fpk), PUBLIC, PARAMETER     ::  min_tau4 = 0.01_fpk     ! initial layer scattering optical depth for 4-stream model.
    REAL(fpk), PUBLIC, PARAMETER     ::  thresh = 0.2_fpk        ! soi loop convergence threshold [K].
    REAL(fpk), PUBLIC, PARAMETER     ::  scat_thresh = 2.e-5_fpk  ! Minimum Scattering optical depth for a layer to be allowed

! Variables within soi_rt

PUBLIC  ::  soi_rt
PRIVATE ::  truncated_doubling, &
            pleg, &
            calc_plegr, &
            thermal_source, &
            hg_phase_function 
            

CONTAINS

SUBROUTINE  soi_rt(opd  , & ! input, optical depth, Nlayers elements
                  ssa   , & ! input, single scatter albedo, Nlayers elements
                  asy   , & ! input, asy parameter, Nlayers elements
                  temp  , & ! input, temperature of each level boundary, Nlayers+1 elements
                  zang  , & ! input, Observation Angle (degrees)
                  e_v   , & ! input, surface emissivity array at specified angles (see above) for V-pol
                  e_h   , & ! input, surface emissivity array at specified angles (see above) for H-pol
                  tbv   , & ! output, Brightness Temp in V-pol
                  tbh   , & ! output, Brightness Temp in H-pol
                  tsurf , & ! Surface Temperature (Kelvin) (optional input) 
                  r_v  , & ! user-defined surface reflectivity at specified angles for V-pol(optional input)
                  r_h  , & ! user-defined surface reflectivity at specified angles for V-pol(optional input)
                  tspace, & ! user-defined TOA downwelling temperature or radiance (optional input)
                  noscatter, & ! optional logical; when true forces absorption-only RT
                  delta_scaling, & ! optional logical; when true turns on delta-scaling
                  verbose, & ! turns verbose output on (optional input)
                  nscat , & ! number of orders of interaction achieved (optional output)
                  nlay    ) ! number of layers used (optional output)

    ! Input and Output Variables
    REAL(fpk), DIMENSION(:), INTENT(in)              ::  opd, ssa, asy
    REAL(fpk), DIMENSION(:), INTENT(in)              ::  temp
    REAL(fpk), INTENT(in)                            ::  zang
    REAL(fpk), INTENT(in), DIMENSION(N_SOI_ANGLES) ::    e_v, e_h
    REAL(fpk), INTENT(out)                           ::  tbv, tbh
    REAL(fpk), INTENT(in), OPTIONAL                  ::  tsurf
    REAL(fpk), DIMENSION(N_SOI_ANGLES), INTENT(in), OPTIONAL    ::  r_v, r_h
    REAL(fpk), OPTIONAL                              ::  tspace
    LOGICAL, INTENT(in), OPTIONAL               ::  noscatter
    LOGICAL, INTENT(in), OPTIONAL               ::  delta_scaling
    LOGICAL, INTENT(in), OPTIONAL               ::  verbose
    INTEGER, INTENT(out), OPTIONAL              ::  nscat, nlay

    ! Local Variables

    REAL(fpk), DIMENSION(2)                          ::  pts2, wgts2
    REAL(fpk), DIMENSION(3)                          ::  pts4, wgts4
    REAL(fpk), DIMENSION(size(opd))                  ::  sopd
    REAL(fpk)                                        ::  tot_sopd, tot_opd, mu
    INTEGER                                     ::  Nlayers, Nz, nl, noi = 0

    LOGICAL                                     ::  noscat 
    LOGICAL                                     ::  verb 
    REAL(fpk)                                        ::  tsfc, t_space


! BEGIN MAIN CODE HERE ---------------------------------------------------------------------------------

    verb = .FALSE.  
    if (present(verbose)) then
        if (verbose) verb = .TRUE.                   
    endif

    ! assign background temperature of space if not present
    if (.NOT. PRESENT(tspace)) then
        t_space = Tcmb
    else
        t_space = tspace
    endif

    ! assign surface temp to lowest atm temp if not present
    if (.NOT. PRESENT(tsurf)) then
        tsfc = temp(1)
    else
        tsfc = tsurf
    endif

    ! check if the no-scatter keyword was set.
    noscat = .FALSE.
    if (PRESENT(noscatter)) then
        if (noscatter) noscat = .TRUE.
    endif

    Nlayers = size(opd)
    mu = cos(d2r * zang)

    ! ERROR CHECKING OF ARRAY SIZES FOR "temp":
    if (size(temp) .ne. nlayers+1) then
        print *, 'Error at soi_rt: Number of elements in temp inconsistent with Nlayers. Returning.'
        return
    endif

    ! calculate total optical depth along profile:
    tot_opd = sum(opd) ! total optical depth from surface to space

    ! if "noscatter" keyword was set, then automatically run no_scat_rt,
    !   without the need to calculate scattering optical depth.

    IF (noscat) THEN
        ! User is forcing no-scattering RT.
        call no_scat_rt
        if (PRESENT(nlay)) nlay = nlayers
        if (PRESENT(nscat)) nscat = 0
    ELSE
        ! There MIGHT be scattering. Calculate scat optical depth to see what to do:

        ! ERROR CHECK ARRAY SIZES OF "ssa" and "asy"
        if (size(ssa) .ne. nlayers) then
            print *, 'Error at soi_rt: Number of elements of ssa inconsistent with Nlayers. Returning.'
            return
        endif
        if (size(asy) .ne. nlayers) then
            print *, 'Error at soi_rt: Number of elements of asy inconsistent with Nlayers. Returning.'
            return
        endif

        sopd = opd * ssa ! calculate scattering optical depth of each layer
        tot_sopd = sum(sopd) / mu ! total slant scattering optical depth
    
        if (verb) print *, 'total_sopd = ', tot_sopd

        If (tot_sopd < min_sopd) Then
            ! Call no-scattering RT subroutine (below)
            if (VERB) print *, 'Calling No_Scat_RT'
            Call no_scat_rt
            if (PRESENT(nlay)) nlay = nlayers
            if (PRESENT(nscat)) nscat = 0
        Else
            ! We will need to use the SOI scattering subroutine.
            if (tot_sopd < cut_sopd) then
                ! USE 2-stream MODEL
                pts2 = (/ pts2_(1) , mu /)
                wgts2 = (/ wgts2_(1), ZERO /)
                if (VERB) print *, 'Using 2 stream model...'
                CALL sos_2nstrm(pts2, wgts2, min_tau2, noi, nl)
            else
                ! USE 4-stream MODEL
                pts4 = (/ pts4_(1), pts4_(2), mu /)                     
                wgts4 = (/ wgts4_(1), wgts4_(2), ZERO /)
                if (VERB) print *, 'Using 4 stream model...'
                CALL sos_2nstrm(pts4, wgts4, min_tau4, noi, nl)
            endif
            if (PRESENT(nlay)) nlay = nl
            if (PRESENT(nscat)) nscat = noi
        Endif
    ENDIF

    CONTAINS

        SUBROUTINE no_scat_rt

            REAL(fpk), dimension( size( opd ) )  ::tau, Sup, Sdn
            REAL(fpk)                            ::  ev, eh, rv, rh
            INTEGER                         ::  i

            tau = exp(-opd/mu) ! local transmittance through given layer
            do i = 1, Nlayers
                call thermal_source(opd(i)/mu, ZERO, tau(i), temp(i+1), temp(i), Sup(i), Sdn(i))
            enddo

        !   Set up single emissivity and reflectivity at the observation angle
            ev = e_v(1)
            eh = e_h(1)
            if (.NOT. present(r_v)) then
                rv = ONE - ev
            else
                rv = r_v(1)
            endif
            if (.NOT. present(r_h)) then
                rh = ONE - eh
            else
                rh = r_h(1)
            endif

        ! initially, we are unpolarized.  Just use Tbv

            Tbv = T_space
            do i = Nlayers, 1, -1 
              Tbv = Tbv * tau(i) + Sdn(i)
            end do

            Tbh  = eh * tsfc + rh * Tbv ! now we get polarization.
            Tbv  = ev * tsfc + rv * Tbv

        ! loop back up through the atm, keeping track of pol.

            do i = 1, Nlayers
              Tbv  = Tbv * tau(i) + Sup(i)
              Tbh  = Tbh * tau(i) + Sup(i)
            end do

        END SUBROUTINE no_scat_rt


        SUBROUTINE sos_2nstrm(  pts,    & ! Input: Array of quadrature points
                                wgts,   & ! Input: Array of quadrature weights
                                min_tau,& ! Input: Maximum allowed layer slant scat optical depth
                                nscat,  & ! Output: Number of orders of scatter achieved
                                nl      ) ! Output: Total number of layers used in calculations
    
    !#--------------------------------------------------------------------------#
    !#                                                                          #
    !#                          -- Type declarations --                         #
    !#                                                                          #
    !#--------------------------------------------------------------------------#


    IMPLICIT NONE

    !-----------
    ! Arguments
    !-----------

    REAL(fpk), DIMENSION(:), INTENT(in)      ::  pts       ! npts
    REAL(fpk), DIMENSION(:), INTENT(in)      ::  wgts      ! npts
    REAL(fpk), INTENT(in)            ::  min_tau
    INTEGER, INTENT(out)        ::  nscat
    INTEGER, INTENT(out)        ::  nl   

    !------------------
    ! Local variables
    !------------------

    REAL(fpk), PARAMETER                             ::  initial_error = 1.0e10_fpk

    REAL(fpk), DIMENSION(Nlayers)                    ::  w, g, f, d
    REAL(fpk), DIMENSION(2*(size(pts)-1))            ::  leg
    REAL(fpk), DIMENSION(2*(size(pts)-1), size(pts)) :: plegr
    REAL(fpk), DIMENSION(size(pts), size(pts))       ::  pf, pb
    REAL(fpk)            ::  tb_d( nstrm_max, nlay_max+1, nscat_max)  ! array to hold downwelling radiation,
                                                                ! for each stream, at each level, at
                                                                ! each order of interaction.
    REAL(fpk)            ::  tb_uv( nstrm_max, nlay_max+1, nscat_max) ! as above, for upwelling v-pol radiation.
    REAL(fpk)            ::  tb_uh( nstrm_max, nlay_max+1, nscat_max) ! as above, for upwelling h-pol radiation.
    REAL(fpk), DIMENSION(size(pts), Nlayers)         ::  tau, Sp, Sm
    REAL(fpk), DIMENSION(size(pts))                  ::  source
    REAL(fpk), DIMENSION(size(pts))                  ::  A, B
    REAL(fpk), DIMENSION(size(pts), size(pts), Nlayers)  ::  R, T
    REAL(fpk), DIMENSION(nscat_max)                  ::  tb0v, tb0h
    REAL(fpk), DIMENSION(size(pts))                  ::  ev, eh, rv, rh
    INTEGER i, j, k, l, ndoub, nleg
    INTEGER isign
    REAL(fpk)    tb_added, pp, norm, asopd
    INTEGER                 ::  npts, iscat
    REAL(fpk)                ::  logmintau
    LOGICAL             :: deltascaling 
    INTEGER             ::  n1, n2

    
    npts = size(pts)
    logmintau = log(min_tau)

    !------------------------------------
    ! Delta-Scaling (if required)
    !------------------------------------
    deltascaling = .FALSE.
    IF (PRESENT(delta_scaling)) THEN
        if (delta_scaling) deltascaling = .TRUE.
    ENDIF

    IF (deltascaling) THEN
            f      = asy * asy ! factors for delta scaling
            g = (asy - f) / (ONE - f)
            d = (ONE - ssa * f ) * opd
            w = (ONE - f )  * ssa /  ( ONE - f * ssa )
            sopd = d * w 
    ELSE
            d = opd
            w = ssa
            g = asy
    ENDIF

!-------------------------------------------
! Compute Legendre expansion coefficients 
!  (may want to take this out if slow)
!-------------------------------------------

    nleg = 2*(npts-1)

! Compute Legendre polynomials
    call calc_plegr(pts, nleg, plegr)
!-------------------------------------------------
! Calculate Simple Layer Properties
!-------------------------------------------------

    do i = 1, nlayers
        tau(:,i) = exp(-d(i)/pts)
    enddo
    nl = 0

!-------------------------------------------
! Create Normalized Phase Functions 
!-------------------------------------------
      do i = 1, nlayers ! layers
        IF ( sopd(i) > scat_thresh ) THEN 
          CALL hg_phase_function(g(i), wgts, plegr, pf, pb)

          asopd = (ONE-g(i)) * sopd(i)

          if (asopd > min_tau) then 
            ndoub = nint( (log(asopd) - logmintau)/log2 + HALF) 
          else 
            ndoub = 0
          endif

          if (VERB) print *, 'DOWN: i, asopd, ndoub = ', i, asopd, ndoub

          if (ndoub > 0) then 

              call truncated_doubling(ndoub, d(i), w(i), pf, pb, &
                                 pts, wgts, temp(i), temp(i+1), R(:,:,i), T(:,:,i), & 
                                 sp(:,i), sm(:,i) )
          else 
              ! this is a faster way to initialize layers that do not require doubling (doesn't
              ! require calling truncated_doubling, so should in principle be faster)
              do k = 1, npts 
                R(:,k,i) = w(i) * pb(:,k) * wgts(k) * (1- tau(:,i))
                T(:,k,i) = w(i) * pf(:,k) * wgts(k) * (1- tau(:,i))
                call thermal_source( d(i)/pts(k), w(i), tau(k,i), temp(i+1), &
                                    temp(i), Sp(k,i), Sm(k,i) )
              enddo
          endif
          if (verb) write (*,"('Sp= ', 3f12.7)") sp(:,i)
          if (verb) write (*,"('Sm= ', 3f12.7)") sm(:,i)
          nl = nl + 2**ndoub 
        ELSE
            if (VERB) print *, 'DOWN: i, sopd, ndoub = ', i, sopd(i), ndoub
            do k = 1, npts
                call thermal_source( d(i)/pts(k), ZERO, tau(k,i), temp(i+1), &
                                    temp(i), Sp(k,i), Sm(k,i) )
            enddo
            nl = nl + 1
        END IF
      enddo

!------------------------------------------
! Assign Surface Emissivity at each angle
!------------------------------------------
        n1 = 1
        if (npts == 3) n1 = 2
        n2 = npts + n1 - 2
        ev(npts) = e_v(1)
        eh(npts) = e_h(1)
        ev(1:(npts-1)) = e_v(n1 : n2)
        eh(1:(npts-1)) = e_h(n1 : n2)
        if (.NOT. present(r_v)) then
            rv = ONE - ev
        else
            rv(npts) = r_v(1)
            rv(1:(npts-1)) = r_v(n1 : n2)
        endif
        if (.NOT. present(r_h)) then
            rh = ONE - eh
        else
            rh(npts) = r_h(1)
            rh(1:(npts-1)) = r_h(n1 : n2)
        endif



    !-------------------------------
    ! Set upper boundary conditions
    !-------------------------------

    tb_d(:, nlayers+1, 1)  =   T_space  ! Zeroth order scatter only
    iscat = 0 ! set that we're on the zeroth order of scatter

    !--------------------------------------
    !  Loop through orders of scattering
    !--------------------------------------

    tb_added = initial_error

    sos_loop: DO WHILE ( ( tb_added > thresh ) .AND. ( (iscat+1) .LE. nscat_max) )

        iscat = iscat + 1 ! Note: iscat = 1 is the zeroth order of scattering

        if (iscat > 1) tb_d( :, nlayers + 1, iscat) = ZERO        
        
        !---------------------------------
        ! Loop through Layers (downward)
        !---------------------------------

    layersdn_loop: DO i = nlayers, 1, -1

            IF ( iscat == 1 ) THEN
                  source = sm(:,i)            
        ELSE

            !--------------------------------------------------------------
            !  Scattering source functions
            !
            !  (Note: For level 0 there is no upwelling scattering source)
            !--------------------------------------------------------------

              IF ( sopd(i) > scat_thresh ) THEN
                do j = 1, npts
                  source( j ) = ZERO

                 ! scattering erases any polarization signature (so avg last_up v & h)
                  do k = 1, npts  ! integrate over angle
                    source( j ) = source( j ) + T(j, k, i) * tb_d(k,i+1,iscat-1) + &
                                    R(j, k, i) * HALF * (tb_uv(k, i,iscat-1)  + tb_uh(k, i,iscat-1))
                  enddo
                end do
              ELSE               
                source = ZERO
              ENDIF
            ENDIF

            tb_d(:, i, iscat ) = tb_d(:, i+1, iscat ) * tau(:, i ) + source

        END DO layersdn_loop

        !-----------------------------------
        ! Surface: reflection and emission 
        !    Emission only for iscat=1 !!
        !-----------------------------------
        
        tb_uv(:, 1, iscat ) = Rv * tb_d(:,  1, iscat ) 
        tb_uh(:, 1, iscat ) = Rh * tb_d(:,  1, iscat ) 

        IF ( iscat == 1 ) THEN ! add surface emission for 0th order of scattering
            tb_uv(:,  1, iscat ) = tb_uv(:, 1, iscat ) + Ev * tsfc 
            tb_uh(:,  1, iscat ) = tb_uh(:, 1, iscat ) + Eh * tsfc
        END IF       

        !---------------------------------
        ! Loop through layers (upward)
        !---------------------------------

        layersup_loop: DO i = 1, nlayers
        IF ( iscat == 1 ) THEN        
               source = sp(:,i)             
        ELSE ! iscat > 1
               IF ( sopd(i) > scat_thresh ) THEN
                 do j = 1, npts
                    source( j ) = ZERO
                    do k = 1, npts  ! integrate over angle
                        source( j ) =  source( j )  + R(j, k, i) * tb_d( k, i+1,iscat-1) + &
                        T(j, k, i) * HALF * ( tb_uv(k, i, iscat-1) + tb_uh(k, i, iscat-1) )                  
                    enddo
                 end do             

               ELSE ! ssa(i) = 0          

                 source = ZERO

               ENDIF
            ENDIF

        tb_uv( :, i+1, iscat ) = tb_uv(:, i, iscat ) * tau(:, i ) + source
        tb_uh( :, i+1, iscat ) = tb_uh(:, i, iscat ) * tau(:, i ) + source
        
        END DO layersup_loop
        
        tb0v( iscat ) = tb_uv(npts, nlayers + 1, iscat )  ! the "last" pts angle is the observation angle
        tb0h( iscat ) = tb_uh(npts, nlayers + 1, iscat )  

        tb_added = max(tb0v(iscat), tb0h(iscat))  ! (max of v and h) TB amount added by this order of scatter.

    END DO sos_loop

    nscat = iscat
    tbv = sum(tb0v(1:nscat))
    tbh = sum(tb0h(1:nscat))
    if (verb) print *, 'Tbv(:) = ' , tb0v(1:nscat)
    

    END SUBROUTINE sos_2nstrm

END SUBROUTINE soi_rt


    SUBROUTINE TRUNCATED_DOUBLING(  k,      &   ! Input, Number of Doublings to Perform
                                    tau,    &   ! Input, Extinction optical depth of the layer
                                    wo,     &   ! Input, Single Scatter Albedo of the layer
                                    Pp,     &   ! Input, Forward-Scattering Phase Function
                                    Pm,     &   ! Input, Backward-Scattering Phase Function
                                    mu,     &   ! Input, Array of quadrature points
                                    w,      &   ! Input, Array of quadrature weights
                                    Bo,     &   ! Input, Planck Radiance of lower boundary
                                    Bn,     &   ! Input, Planck Radiance of upper boundary
                                    R,      &   ! Output, Reflection matrix
                                    T,      &   ! Output, Scattering Transmission matrix 
                                    Sp,     &   ! Upwelling source function (including scattering)
                                    Sm)         ! Downwelling source funtion (including scattering)

    ! Arguments
        integer, intent(inout) :: k
        REAL(fpk), dimension(:,:),intent(in):: Pp,Pm
        REAL(fpk), dimension(:),intent(in):: mu,w
        REAL(fpk), intent(in):: tau,wo,Bo,Bn
        REAL(fpk), dimension(:,:),intent(out):: R,T
        REAL(fpk), dimension(:),intent(out):: Sp,Sm

    ! Local Variables
        REAL(fpk), dimension(size(mu),size(mu))  :: E,T_Gamma,T_Gamma_R
        REAL(fpk)                                ::  Bd, delta_tau,exp_delta_tau_mu,g
        REAL(fpk), dimension(size(mu))           :: Y,Z
        integer                             :: i,j,l,npts

        npts = size(mu)
        delta_tau = tau/(2**k)

    !--- slope of blackbody emission through layer
        Bd = (Bn - Bo) / tau

    !--  for the first layer, initialize with single scatter approx.
        do i = 1, npts
            exp_delta_tau_mu = exp(-delta_tau/mu(i))
            do j = 1, npts
                E(i,j) = ZERO
                if (i == j) E(i,j) = ONE
                R(i,j) = wo * Pm(i,j) * w(j) * (ONE-exp_delta_tau_mu)
                if (i == j) then         
                    T(i,j) = exp_delta_tau_mu + wo*Pp(i,j)*w(j)*(ONE-exp_delta_tau_mu)
                else
                    T(i,j) = wo*Pp(i,j)*w(j)*(ONE-exp_delta_tau_mu)
                endif
            enddo
            Y(i) = (ONE-wo) * (ONE - exp_delta_tau_mu) 
            Z(i) = ZERO ! + delta_tau * delta_tau / mu(i) / 12.0_fpk * Y(i) 
        enddo
        g = delta_tau / 4.0_fpk

    !-- if there is doubling, perform doubling
        do l = 1, k 
            g = g * TWO
            T_Gamma = matmul(T, E + matmul(R,R))
            T_Gamma_R = matmul(T_Gamma , R)

            R = matmul(T_Gamma_R , T) + R
            T = matmul(T_Gamma , T)
            Z = Z + g*Y + matmul((T_Gamma - T_Gamma_R) , (Z - g*Y))
            Y = matmul((T_Gamma + T_Gamma_R + E) , Y)
        end do

    !  finish thermal source computation
        Sp = HALF*(Bo + Bn)*Y + Bd*Z
        Sm = HALF*(Bo + Bn)*Y - Bd*Z

    !---------- subtract off direct transmission from total Transmission Matrix
        do i = 1,npts
            T(i,i) = T(i,i) - exp(-tau/mu(i))
        end do

    END SUBROUTINE TRUNCATED_DOUBLING

    SUBROUTINE  thermal_source( slopd,  &   ! Slant optical depth of layer (opd/mu)
                                ssa,    &   ! ssa of layer
                                tau,    &   ! slant transmittance of layer (exp(-opd/mu))
                                Ttop,   &   ! Top Temp (or BB radiance) of layer
                                Tbot,   &   ! Bot Temp (or BB radiance) of layer
                                Sp,     &   ! Upwelling Source
                                Sm )        ! Downwelling Source


        REAL(fpk), intent(in)        ::  slopd
        REAL(fpk), intent(in)        ::  ssa
        REAL(fpk), intent(in)        ::  tau
        REAL(fpk), intent(in)        ::  Ttop
        REAL(fpk), intent(in)        ::  Tbot
        REAL(fpk), intent(out)       ::  Sp, Sm

        REAL(fpk)                    ::  A, B

        A = HALF * (Tbot + Ttop) * (1-tau)
        if (slopd < 0.2_fpk) then
            B = (Ttop-Tbot)*(slopd/12.0_fpk)*(1-tau)
        else
            B = (Ttop-Tbot)*( tau + (1-tau)*(HALF - ONE/slopd) )
        endif

        sp = (A + B) * (ONE - ssa)
        sm = (A - B) * (ONE - ssa)

    END SUBROUTINE thermal_source


    SUBROUTINE hg_phase_function(   asy, &   ! assymetry parameter
                                    wgts, & ! quadrature weights
                                    plegr, & ! needed legendre polynomials
                                    pf, & ! forward-scattering phase function
                                    pb, & ! backward-scattering phase function
                                    norm ) ! normalization for each row of phase matrix
                                                                        
        REAL(fpk), intent(in)                    ::  asy
        REAL(fpk), intent(in), dimension(:)      ::  wgts
        REAL(fpk), intent(in), dimension(:,:)    ::  plegr
        REAL(fpk), intent(out), dimension(:,:)   ::  pf, pb
        REAL(fpk), intent(out), dimension(:), optional   ::  norm
        
        
    !   REAL(fpk), dimension(size(wgts))     ::  norm_
        REAL(fpk)    ::  norm_
        integer :: j, k, l, isign, npts, nleg
        REAL(fpk)    :: pp



        pf = ZERO  ! initialize matrices
        pb = ZERO
        npts = size(wgts)
        nleg = size(plegr, 1)
        do j = 1, npts    ! up-going input angles
            norm_ = ZERO
            do k = 1, npts  ! out-going angles
              isign = 1
              do l = 1, nleg ! sum up necessary # of terms
                pp = (2*l-1)*asy**(l-1) * plegr(l,j) * plegr(l,k)
                pf( j, k) = pf( j, k) + pp
                pb( j, k) = pb( j, k) + isign * pp 
                isign = -isign
              end do
              norm_ = norm_ + (pf(j,k) + pb(j,k)) * wgts(k)
            end do
            pf(j,:) = pf(j,:) /norm_
            pb(j,:) = pb(j,:) /norm_
            if ( present(norm) ) norm(j) = norm_
        enddo
    
    END SUBROUTINE hg_phase_function

    SUBROUTINE calc_plegr(  pts, & ! vector of mu-values
                            nleg, & ! # of nlegendre terms to use
                            plegr ) ! output array

        ! note: nleg = 2*(npts-1) for usual normalization
    
        REAL(fpk), intent(in), dimension(:)      ::  pts
        INTEGER, intent(in)                 ::  nleg
        REAL(fpk), intent(out), dimension(:,:)   ::  plegr
        REAL(fpk), DIMENSION(nleg)               ::  leg
    
        integer     ::  i, j, npts
        
        npts = size(pts)
            
        do i = 1, npts
            call pleg( pts(i), nleg, leg )
            do j = 1, nleg
                plegr( j, i ) = leg( j )
            enddo
        enddo

    END SUBROUTINE calc_plegr


    SUBROUTINE pleg( x,      &   ! Input
                     nleg,   &   ! Input
                     p       )   ! Output

        !------------
        !  Arguments
        !------------

        REAL(fpk)    x
        INTEGER nleg
        REAL(fpk)    p( nleg )

        INTEGER i

        P( 1 ) = ONE
        P( 2 ) = x
        DO i = 3, nleg
          p( i ) = ( ( 2 * i - 3 ) * x * p( i - 1 ) - &
                    ( i - 2 ) * p( i - 2 ) ) / ( i - 1 )
        ENDDO 

    END SUBROUTINE pleg

END MODULE soi_rt_model


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2005/01/05 21:49:32 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: soi_rt_model.f90,v $
! Revision 1.1  2005/01/05 21:49:32  paulv
! Initial checkin. Below is the original revision history.
!
!   9/9/2004 *  Fixed Source Term error (again) in Truncated_Doubling.  Zo term remains at 0.
!
!   9/9/2004 *  Abandoned coupling of this model to an emissivity model.  Now the
!               user is required to provide all necessary emissivity data.  Reflectivity
!               is still an optional input.
!
!   9/7/2004 *  Broke out thermal source calculation and phase function calculation
!               into their own subroutines.
!
!   7/9/2004 *  Corrected bug in expression for A and B used in source functions
!               Sp and Sm. "opd" was changed to "opd(i)"
!
!   7/2/2004 *  Explicitly saves up/down Tb's for absorption-only RT model
!               and R/T matrices for each doubling step and layer
!
!   6/1/2004 *  This version passes through particle and gas extinction
!               coefficients as separate variables; explicitly saves upwelling
!               and downwelling Tb's at each level and order of interaction
!
!   4/23/2004
!           *   Changes from old version:
!               Many options removed and hard-coded into the model, such as minimum layer thickness,
!               number of streams, number of legendre expansion terms for phase function, convergence
!               threshold, delta scaling, etc.  Added branching routine to determine which RT model to use.
!               Made the two RT subroutines NESTED within the main routine.  This simplified the code, and
!               made it slightly faster (no passing of variables required).
!
!   3/5/2004  (Old, original version)
!           *   Uses TRUN_DOUB_SUB to calculate doubling (only called when doubling necessary).
!           *   Changed initialization in TRUN_DOUB_SUB to be correct.
!           *   Doesn't use trun_doub_sub for no doubling of a layer.
!           *   Only calculates scattering terms when necessary
!           *   Uses F90 techniques for passing data to function, and automatically
!               allocates necessary arrays.
!           *   Calculates source term to 2nd order in temperature for optically thick layers.
!           *       Uses Tcmb = 2.725 instead of 2.7 (more accurate).
!           *   Optional Delta Scaling, seems to work,
!               but doesn't help in microwave (hurts a little).
!           *   Two ways to calculate # of legendre terms.  Not sure which is better.
!           *   Brute force matrix multiplication in SOS loop.  Uses matmul in trun_doub_sub.
!
!
!
