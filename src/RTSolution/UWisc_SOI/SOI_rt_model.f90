!--------------------------------------------------------------------------------------------------
!
! Successive Order of Interaction radiative transfer model for
! computing microwave TOA brightness temperatures or radiances.
!
! Version 1.4.1 -- modified for use with CRTM (2/10/2005)
!
!	PERMISSIONS:	
!		Anyone can freely use this code, but please give reference to the authors (below) if 
!		results from use of this code are employed in any scholarly articles, proposals, etc.
!
!   WRITTEN BY:
!               Chris O'Dell (UW AOS)			odell@aos.wisc.edu
!               Tom Greenwald (SSEC/CIMSS)		tomg@ssec.wisc.edu
!               Andrew Heindinger (NOAA/Nesdis) heidinger@ssec.wisc.edu
!               Ralf Bennartz (UW AOS)			bennartz@aos.wisc.edu
!
!   DESCRIPTION:
!		This model performs a 1D radiative transfer with scattering.  It is basically a hybrid model that 
!		combines a simplified doubling routine, to determine the reflection and transmission matrices for each
!		individual layer in the atmosphere, with a successive-order-of-scatter loop to iteratively determine the
!		total TOA intensity (or brightness temperature) from each order of "interaction".  This is not the same as
!		each order of "scattering", because the source terms from each layer include some amount of scattering already.
!		Typically, convergence of this model is much faster than for a successive-order-of-scattering algorithm.  In
!		fact, most of the time of this algorithm is spent in the doubling part of the code.  


!	BASIC TECHNIQUES IN THE MODEL
!		This algorithm first evaluates the total column amount of scattering optical depth
!       in order to determine the appropriate radiative transfer model to use.  If
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
!       than 0.1 K accuracy for no-scattering cases.  The accuracy can be made even higher, at the expense
!		of speed, by playing around with some of its optional keywords (described later).
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
!	NOTE ON TYPE OF FLOATING POINT NUMBERS USED
!		All floating point numbers are set to the type "FPK".  This can be either set to "single" or "double".
!		The default version of this code sets it to double, which you will see in the MODULE "Type_Kinds".
!
!
!	NOTE ON SPEED
!		In order for the code the be as fast as possible, it is recommended that the user set the parameters
!		nstrm_max, nscat_max, and nlay_max to their lowest possible value.  For nstrm_max, the lowest allowed value
!		is the # of quadrature angles used + 1 (this extra one is for the observation angle).  Note that the # of 
!		quadrature angles = Nstreams / 2.  For nlay_max, simply set this to the # of layers you are using.
!		For nscat_max, you may need to play around, but rarely is a value higher than 20 needed.
!
!   CALLING SEQUENCE:
!
!       call soi_rt( opd, ssa, asy, temp, zang, e_v, e_h, tbv, tbh, &         ! all other inputs after this line are optional!
!                    tsurf = tsurf, r_v = r_v, r_h = r_h, tspace = tspace, &
!                    delta_scaling = delta_scaling, noscatter = noscatter, &
!                    verbose = verbose, nscat = nscat, nlay = nlay, & 
!					 converge = converge, Nstreams = Nstreams, max_tau = max_tau, &
!					 levels_dn = levels_dn, levels_upv = levels_upv, levels_uph = levels_uph, &
!					 I_dn = I_dn, I_upv = I_upv, I_uph = I_uph )
!
!
!   INPUT VARIABLES (required):
!           NAME    TYPE                DESCRIPTION
!           opd     REAL[Nlayers]       Layer extinction optical depths (contains Nlayers values).  Ordered surface to TOA.
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
!           e_v     REAL[:] 			4+ element array of emissivities at v-polarization for each of the required angles: 
!										(zang, 54.7356, 77.8000, and 37.9381) degrees.
!                                       These angles are defined by the parameter soi_angles.  See discussion on
!										emissivity/reflectivity (below) for more details.  Additional emissivities are
!										allowed in the elements of the e_v array beyond element 4, but this is only necessary
!										when more than four streams are used (as set with the Nstreams keyword).
!
!           e_h     REAL[:] 			As above, but for h-polarization.
!
!
!   OUTPUT VARIABLES (required):
!           tbv     REAL                Brightness Temperature (radiance) at TOA in v-polarization.
!
!           tbh     REAL                Same as above, but in h-polarization.
!                                              
!
!   OPTIONAL INPUTS:
!           tsurf   	REAL			Surface Temperature [Kelvin], or Planck radiance.
!
!			r_v     	REAL[:] 		Reflectivities at v-pol for each of the required angles.  Defaults to
!                                       1 - e_v.
!
!			r_h     	REAL[:] 		Reflectivities at h-pol for each of the required angles.  Defaults to
!                                       1 - e_h.
!
!			tspace  	REAL			Uniform background radiance or temperature.  Defaults to cosmic microwave 
!										temperature of 2.725 K.
!
!           Delta_Scaling   LOGICAL     Setting this to .TRUE. turns on delta-scaling.  Defaults to .FALSE.
!
!			NoScatter	LOGICAL 		Setting this to .TRUE. forces absorption-only RT. Defaults to .FALSE.
!
!			Verbose 	LOGICAL 		Setting this to .TRUE. turns on verbose output.  Defaults to .FALSE.
!
!			Converge	REAL			This is the convergence threshold, in intensity/brightness temperature units.
!										Basically, convergence is achieved when the contribution to the TOA intensity
!										falls below this level.  Default value is thresh_default (see below).
!									
!			Nstreams	INTEGER 		Setting this to an even integer forces this number of streams to be used.
!										When this keyword is not set, soi_rt will use either 2 or 4 streams 
!										(in scattering atmospheres). Important note: this MUST not be higher than
!										nstrm_max - 1 (should be 64).
!
!			max_tau 	REAL			Setting this over-rides the default maximum layer scattering optical
!										before doubling occurs.   A smaller number means more doublings will occur.
!										Normally, the default does not need to be over-ridden.	Defaults should
!										be 0.1 for 2 stream cases, 0.01 for 4-stream cases, and 0.001 for user-defined
!										higher numbers of streams.
!
!			levels_dn	INTEGER[:]		Set this to a list of levels at which you want the downwelling radiances
!										evaluated.  MUST be set in conjunction with I_dn.	
!
!			levels_upv	INTEGER[:]		Set this to a list of levels at which you want the upwelling (V) radiances
!										evaluated.  MUST be set in conjunction with I_upv.	
!
!			levels_uph	INTEGER[:]		Set this to a list of levels at which you want the upwelling (H) radiances
!										evaluated.  MUST be set in conjunction with I_uph.	
!
!
!   OPTIONAL OUTPUTS:   
!           nscat   	INTEGER 		Number of orders of interaction achieved before convergence.
!
!           nlay    	INTEGER 		Number of "thin" layers used (after doubling).
!
!			I_dn		REAL[:] 		Will contain the downwelling radiances at the levels set by "levels_dn".
!
!			I_upv		REAL[:] 		Will contain the upwelling (V) radiances at the levels set by "levels_upv".
!
!			I_uph		REAL[:] 		Will contain the  upwelling (H) radiances at the levels set by "levels_uph".
!
!
!   EMISSIVITY/REFLECTIVITY TREATMENT
!		The user MUST pass the variables e_v and e_h (the v and h emissivities).  These variables are
!		1D arrays of emissivities.  The ordering is as follows:
!		element 1	:	Incidence angle = Observation Angle
!		element 2	:	Incidence angle = soi_angles(1) (normally 53.736 degrees) (needed for 2-stream cases)
!		element 3	:	Incidence angle = soi_angles(2) (normally 77.800 degrees) (needed for 4-stream cases)
!		element 4	:	Incidence angle = soi_angles(3) (normally 37.938 degrees) (needed for 4-stream cases)
!		elements 5+ :	Incidence angles = Quadrature angles for higher streams.  These elements are ONLY used when
!						the user selects a higher number of quadrature streams, by setting the Nstreams keyword.
!		

!   Revision history:
!
!				version 1.4.1 			Removed levels_dn,levels_upv, levels_uph functionality.  This is unnessecary
!                                       in the CRTM.  Also, re-modularized all the code to reflect original
!                                       SOI concept.
!										
!
!				version 1.4 			No longer truncated doubling.  For Nstreams <= 4, all inverses are 
!										explicitly calculated.  For Nstreams >= 6, truncated doubling is used.
!
!
!				(12/2/2004)
!				version 1.3 (beta)		-Added optional outputs for irradiances (upwelling or downwelling)
!										at user-specified levels.  Involves using the optional output keywords 
!										I_dn, I_upv, I_uph, and the optional input keywords levels_dn, levels_upv, 
!										and levels_uph (for specifying the desired levels).
!
!				version 1.2 	*		-Changed the thin layer initialization in truncated_doubling
!										 to be either the SSI and ODL initializations, depending on
!										 the value of the single-scatter albedo.
!										 This had a good impact on accuracy, and a negligible impact on speed.
!										-Changed the thin-layer initialization when there is NO doubling
!										 to be the SSI (rather than ODL) initialization.
!										-Changed the convergence criterion to be relative rather than absolute.
!										 Also, require at least TWO passes through the soi loop.
!
!				version 1.1		*		Added Keywords converge, nstreams, and max_tau.  
!										Changed default convergence threshold to 0.1.
!										Changed all notation of "min_tau" (and related) to "max_tau".
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
!       		4/23/2004
!               				*   Changes from old version:
!                   Many options removed and hard-coded into the model, such as minimum layer thickness, 
!                   number of streams, number of legendre expansion terms for phase function, convergence
!                   threshold, delta scaling, etc.  Added branching routine to determine which RT model to use.
!                   Made the two RT subroutines NESTED within the main routine.  This simplified the code, and 
!                   made it slightly faster (no passing of variables required).
!
!       		3/5/2004  (Old, original version)
!               		 *   Uses TRUN_DOUB_SUB to calculate doubling (only called when doubling necessary).
!               		 *   Changed initialization in TRUN_DOUB_SUB to be correct.
!               		 *   Doesn't use trun_doub_sub for no doubling of a layer.
!               		 *   Only calculates scattering terms when necessary
!             			 *   Uses F90 techniques for passing data to function, and automatically
!             			     allocates necessary arrays.
!            			 *   Calculates source term to 2nd order in temperature for optically thick layers.
!            			 *       Uses Tcmb = 2.725 instead of 2.7 (more accurate).
!            			 *   Optional Delta Scaling, seems to work, 
!                   		 but doesn't help in microwave (hurts a little).
!             			 *   Two ways to calculate # of legendre terms.  Not sure which is better.
!             			 *   Brute force matrix multiplication in SOS loop.  Uses matmul in trun_doub_sub.
!
!
!
!--------------------------------------------------------------------------------------------------


MODULE soi_rt_model

	USE Type_Kinds, ONLY	:	fpk => fp_kind
    USE SOI_doubling
	USE SOI_emissivity
	USE SOI_delta_scaling
	USE SOI_thermal_source
	USE SOI_hg_phase_function

    IMPLICIT NONE

  ! -- Everything private by default
  PRIVATE

! MODULE Parameters 

    REAL(fpk), PUBLIC, PARAMETER    	 ::  min_sopd = 0.0001_fpk  ! cut-off sopd between no-scattering and scattering.
    REAL(fpk), PUBLIC, PARAMETER         ::  cut_sopd = 0.01_fpk   ! cut-off sopd between 2 and 4 streams.
    REAL(fpk), PUBLIC, PARAMETER         ::  d2r = 0.017453293_fpk ! converts degrees to radians.
    REAL(fpk), PRIVATE, PARAMETER         ::  log2 = 0.693147181_fpk ! natural log of 2
    REAL(fpk), PRIVATE, PARAMETER         ::  Tcmb = 2.725_fpk ! microwave background temp [K].
    REAL(fpk), PRIVATE, PARAMETER        ::  ZERO = 0.0_fpk
    REAL(fpk), PRIVATE, PARAMETER        ::  ONE = 1.0_fpk
	REAL(fpk), PRIVATE, PARAMETER		 ::  TWO = 2.0_fpk
    REAL(fpk), PRIVATE, PARAMETER        ::  HALF = 0.5_fpk

 ! Parameters for SOI Model 
    INTEGER, PUBLIC, PARAMETER  ::  nscat_max = 20      ! Max allowed # of scattering events
	
    ! Absorption-only model
        REAL(fpk), PUBLIC, SAVE     :: Tbv_u( 0 : nlay_max )
        REAL(fpk), PUBLIC, SAVE     :: Tbv_d( 0 : nlay_max )
        REAL(fpk), PUBLIC, SAVE     :: Tbh_u( 0 : nlay_max )
    ! Truncated doubling
        INTEGER, PUBLIC, SAVE       ::  ndoubstep( nlay_max )
    ! SOI loop
        REAL(fpk), PUBLIC, SAVE     ::  tb_d( nstrm_max, nlay_max+1, nscat_max)
        REAL(fpk), PUBLIC, SAVE     ::  tb_uv( nstrm_max, nlay_max+1, nscat_max)
        REAL(fpk), PUBLIC, SAVE     ::  tb_uh( nstrm_max, nlay_max+1, nscat_max)
        INTEGER, PUBLIC, SAVE       ::  nsoi   ! Number of SOI iterations achieved


    REAL(fpk), DIMENSION(1), PARAMETER, PUBLIC   ::  pts2_ = (/ 0.577350269_fpk /)  ! quadrature pts for 2-stream model
    REAL(fpk), DIMENSION(2), PARAMETER, PUBLIC   ::  pts4_ = (/ 0.211324865_fpk, 0.788675135_fpk /) ! quadrature pts for 4-stream model
    REAL(fpk), DIMENSION(1), PARAMETER, PUBLIC   ::  wgts2_ = (/ ONE /) ! quadrature weights for 2-stream model
    REAL(fpk), DIMENSION(2), PARAMETER, PUBLIC   ::  wgts4_ = (/ HALF, HALF /) !quadrature weights for 4-stream model

    INTEGER, PUBLIC, PARAMETER  ::  N_SOI_ANGLES = 4    ! Number of total angles needed by SOI (in general)
    REAL(fpk), PUBLIC, PARAMETER, DIMENSION(N_SOI_ANGLES-1)  :: soi_angles = (/ 54.7356103, & ! to be used by caller, so
                                                                           77.7999960, & ! he/she knows what angles
                                                                           37.9381274 /) ! to calculate emissivity for.

    REAL(fpk), PUBLIC, PARAMETER     ::  max_tau2 = 0.1_fpk      ! default initial layer scattering optical depth for 2-stream model.
    REAL(fpk), PUBLIC, PARAMETER     ::  max_tau4 = 0.01_fpk     ! default initial layer scattering optical depth for 4-stream model.
	REAL(fpk), PUBLIC, PARAMETER	 ::  max_tau_user = 0.001_fpk ! same as above, but for higher # of streams.
    REAL(fpk), PUBLIC, PARAMETER     ::  thresh_default = 0.0005_fpk        ! soi loop convergence threshold (fractional).
    REAL(fpk), PUBLIC, PARAMETER     ::  scat_thresh = 2.e-5_fpk  ! Minimum Scattering optical depth for a layer to be allowed
	REAL(fpk), PUBLIC, PARAMETER	 ::  max_lay2space_opd = 12.   ! Layers with layer->space optical depths above this
															       !  are not considered.  Saves on comp time.
	REAL(fpk), PUBLIC, PARAMETER	 ::  min_lay2space_opd = 3e-5   ! Creates a "highest layer" below which to start computations.
													


PUBLIC  ::  soi_rt, soi_rt_tl, soi_rt_ad, gauleg

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
!		OPTIONAL INPUTS:
                  tsurf , & ! Surface Temperature (Kelvin) (optional input) 
                  r_v  , & ! user-defined surface reflectivity at specified angles for V-pol(optional input)
                  r_h  , & ! user-defined surface reflectivity at specified angles for V-pol(optional input)
                  tspace, & ! user-defined TOA downwelling temperature or radiance (optional input)
                  noscatter, & ! optional logical input; when true forces absorption-only RT
                  delta_scaling, & ! optional ; f-factor for delta-scaling
                  verbose, & ! turns verbose output on (optional input)
				  nstreams, &  ! number of streams to use (optional input)
				  converge, &  ! convergence threshhold [relative fraction; 1e-4 typical]
				  max_tau, & ! maximum layer thickness for doubling initialization (optional input)
!		OPTIONAL OUTPUTS:
                  nscat , & ! number of orders of interaction achieved (optional output)
                  nlay ) ! number of layers used (optional output)


    ! Input and Output Variables
    REAL(fpk), DIMENSION(:), INTENT(in) 			::  opd, ssa, asy
    REAL(fpk), DIMENSION(:), INTENT(in) 			::  temp
    REAL(fpk), INTENT(in)							::  zang
    REAL(fpk), INTENT(in), DIMENSION(:) 			::  e_v, e_h
    REAL(fpk), INTENT(out)               			::  tbv, tbh
    REAL(fpk), INTENT(in), OPTIONAL         		::  tsurf
    REAL(fpk), DIMENSION(:), INTENT(in), OPTIONAL	::  r_v, r_h
    REAL(fpk), OPTIONAL 							::  tspace
    LOGICAL, INTENT(in), OPTIONAL               	::  noscatter
    REAL(fpk), INTENT(in), OPTIONAL, DIMENSION(:)   ::  delta_scaling
    LOGICAL, INTENT(in), OPTIONAL               	::  verbose
    INTEGER, INTENT(out), OPTIONAL              	::  nscat, nlay
	INTEGER, INTENT(in), OPTIONAL					::  nstreams
	real(fpk), intent(in), optional 				::	converge
	real(fpk), intent(in), optional 				::	max_tau

    ! Local Variables

    REAL(fpk), DIMENSION(2)                          ::  pts2, wgts2
    REAL(fpk), DIMENSION(3)                          ::  pts4, wgts4
    REAL(fpk), DIMENSION(size(opd))                  ::  sopd
    REAL(fpk)                                        ::  tot_sopd, tot_opd, mu
    INTEGER                                     ::  Nlayers, nl, noi = 0, npts, i
	INTEGER 									::	lowest_layer, highest_layer
    LOGICAL                                     ::  noscat 
    LOGICAL                                     ::  verb 
    REAL(fpk)                                        ::  tsfc, t_space, maxtau
	REAL(fpk), DIMENSION(:), ALLOCATABLE 			::	pts, wgts
	LOGICAL 								::	maxtau_SET, nstreams_SET


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

    ! if "noscatter" keyword was set, then automatically run no_scat_rt,
    !   without the need to calculate scattering optical depth.
	if (ONE==ZERO) then
		write(*, "(5a10)") ' Layer # ' , '    opd   ', '   ssa   ', '   asy   ', '   temp   '
		do i = 1, Nlayers
			write(*, "(i10, 4f10.4)") i, opd(i), ssa(i), asy(i), temp(i)
		enddo
		write(*, "(i10, 4f10.4)") Nlayers+1, 0., 0., 0., temp(Nlayers+1)
	endif


	highest_layer = Nlayers
    IF (noscat) THEN
        ! User is forcing no-scattering RT.
		lowest_layer = 1
        call no_scat_rt
        if (PRESENT(nlay)) nlay = nlayers
        if (PRESENT(nscat)) nscat = 0
    ELSE
        ! There MIGHT be scattering. Calculate scat optical depth to see what to do:
	
		! calculate lowest layer to consider
		i = Nlayers+1 ! this will become the lowest needed layer
		tot_opd = ZERO
		tot_sopd = ZERO
		do while ((tot_opd < max_lay2space_opd) .and. (i > 1) )
			i = i - 1
			tot_opd = tot_opd + opd(i)
			sopd(i) =  opd(i) * ssa(i)
			tot_sopd = tot_sopd + sopd(i) 
			if ((tot_opd/mu) < min_lay2space_opd) highest_layer = i-1
		end do	
		lowest_layer = i
		if (highest_layer .eq. 0) highest_layer = 1 ! do at least one layer!
		tot_sopd = tot_sopd / mu
        if (verb) print *, 'total_sopd = ', tot_sopd     
		if (verb) print *, 'total_opd = ', tot_opd
		if (verb) print *, 'Lowest_layer = ', lowest_layer
		if (verb) print *, 'Highest_layer = ', highest_layer

        If (tot_sopd < min_sopd) Then
            ! Call no-scattering RT subroutine (below)
            if (VERB) print *, 'Calling No_Scat_RT'
            Call no_scat_rt
            if (PRESENT(nlay)) nlay = nlayers
            if (PRESENT(nscat)) nscat = 0
        Else
            ! We will need to use the SOI scattering subroutine.
		   ! first deal with optional keywords
		   nstreams_SET = .FALSE.
		   if (PRESENT(nstreams)) then
			   if (nstreams > 0) nstreams_SET = .TRUE.
		   endif
		   maxtau_SET = .FALSE.
		   if (PRESENT(max_tau)) then
			   if (max_tau > ZERO) maxtau_SET = .TRUE.
		   endif
		   if (verb) then	
			   print *, 'nstreams_set = ', nstreams_set
			   print *, 'maxtau_set = ', maxtau_set
		   endif

			if (nstreams_SET) then ! user is forcing certain number of streams
		! Gauss-Legendre quadrature
				npts = nstreams / 2
				allocate( pts(npts+1), wgts(npts+1) )

    			IF (npts ==1) THEN
					pts(1) = pts2_(1)
					wgts(1) = ONE				
				ELSE
      				call gauleg( ZERO, ONE, pts(1:npts), wgts(1:npts), npts )
    			END IF

				pts(npts+1) = mu
	    		wgts(npts+1) = ZERO

				if (maxtau_SET) then	
					maxtau = max_tau
				else
					select case(nstreams)
						case(2) 
							maxtau = max_tau2
						case(4) 
							maxtau = max_tau4
						case default				
							maxtau = max_tau_user
					end select
				endif
				CALL sos_2nstrm(pts, wgts, maxtau, noi, nl)
				deallocate(pts, wgts)
			else
            	if (tot_sopd < cut_sopd) then
                	! USE 2-stream MODEL	
                	pts2 = (/ pts2_(1) , mu /)
                	wgts2 = (/ wgts2_(1), ZERO /)
					if (maxtau_SET) then	
						maxtau = max_tau
					else
						maxtau = max_tau2
					endif
                	if (VERB) print *, 'Using 2 stream model...'
                	CALL sos_2nstrm(pts2, wgts2, maxtau, noi, nl)
            	else
                	! USE 4-stream MODEL
                	pts4 = (/ pts4_(1), pts4_(2), mu /)                     
                	wgts4 = (/ wgts4_(1), wgts4_(2), ZERO /)
                	if (VERB) print *, 'Using 4 stream model...'
					if (maxtau_SET) then	
						maxtau = max_tau
					else
						maxtau = max_tau4
					endif
                	CALL sos_2nstrm(pts4, wgts4, maxtau, noi, nl)
            	endif
			endif ! higher_streams
            if (PRESENT(nlay)) nlay = nl
            if (PRESENT(nscat)) nscat = noi
        Endif ! ( tot_sopd < min_opd)
    ENDIF ! (noscat)

    CONTAINS

        SUBROUTINE no_scat_rt

            REAL(fpk), dimension( size( opd ) )  ::tau, Sup, Sdn
            REAL(fpk), dimension(1)         ::  ev, eh, rv, rh
            INTEGER                         ::  i

	!		if (verb) print *, '[no-scat_rt] Calling thermal source...'
            tau = exp(-opd/mu) ! local transmittance through given layer
            do i = lowest_layer, highest_layer
                call thermal_source(opd(i)/mu, ZERO, tau(i), temp(i+1), temp(i), Sup(i), Sdn(i))
            enddo

        !   Set up single emissivity and reflectivity at the observation angle
!	 		if (verb) print *, '[no-scat_rt] Assigning Emissivities...'
			if (PRESENT(r_v) .AND. PRESENT(r_h)) then 
				Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh, r_v, r_h)
			else
				Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh)
			endif
	
        ! initially, we are unpolarized.  Just use Tbv

			Tbv_d = ZERO
			if (lowest_layer == 1) then
!				if (verb) print *, 'About to Loop through layers [down].'
	            Tbv_d(highest_layer) = T_space
    	        do i = highest_layer, 1, -1 
        	      Tbv_d( i-1 ) = Tbv_d( i ) * tau(i) + Sdn(i)
                end do

            	Tbh_u(0)  = eh(1) * tsfc + rh(1) * Tbv_d(0) ! now we get polarization.
            	Tbv_u(0)  = ev(1) * tsfc + rv(1) * Tbv_d(0)
			endif

        ! loop back up through the atm, keeping track of pol.
!			if (verb) print *, 'About to Loop through layers [up].'
            do i = lowest_layer, highest_layer
            	Tbv_u(i)  = Tbv_u(i-1) * tau(i) + Sup(i)
            	Tbh_u(i)  = Tbh_u(i-1) * tau(i) + Sup(i)
            end do

            tbv = Tbv_u( highest_layer )
            tbh = Tbh_u( highest_layer )

!		if (verb) print *, 'Exiting no_scat_rt.'		

        END SUBROUTINE no_scat_rt


        SUBROUTINE sos_2nstrm(  pts,    & ! Input: Array of quadrature points
                                wgts,   & ! Input: Array of quadrature weights
                                max_tau,& ! Input: Maximum allowed layer slant scat optical depth
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

    REAL(fpk), INTENT(in)            ::  max_tau
    INTEGER, INTENT(out)        ::  nscat
    INTEGER, INTENT(out)        ::  nl   

    !------------------
    ! Local variables
    !------------------

    REAL(fpk), PARAMETER                             ::  initial_error = 1.0e10_fpk
	REAL(fpk), PARAMETER							 ::  SMALL = 1.0e-15_fpk
    REAL(fpk), DIMENSION(Nlayers)                    ::  w, g, f, d
    REAL(fpk), DIMENSION(2*(size(pts)-1))            ::  leg
    REAL(fpk), DIMENSION(2*(size(pts)-1), size(pts)) :: plegr
    REAL(fpk), DIMENSION(size(pts), size(pts))       ::  pf, pb

!	REAL(fpk), dimension(:, :, :), allocatable		::	tb_d, tb_uv, tb_uh

    REAL(fpk), DIMENSION(size(pts), Nlayers)         ::  tau, Sp, Sm
    REAL(fpk), DIMENSION(size(pts))                  ::  source
    REAL(fpk) 						                 ::  A, B, RT, Tmean, dT
    REAL(fpk), DIMENSION(size(pts), size(pts), Nlayers)  ::  R, T
    REAL(fpk), DIMENSION(nscat_max)                  ::  tb0v, tb0h
	real(fpk), dimension(size(pts)) 				 :: ev, eh, rv, rh
    INTEGER i, j, k, l, ndoub, nleg
    INTEGER isign
    REAL(fpk)    tb_change, pp, norm, asopd
    INTEGER                 ::  npts, iscat
    REAL(fpk)                ::  logmaxtau
	real(fpk)				:: thresh
    INTEGER             ::  n1, n2

    
    npts = size(pts)
    logmaxtau = log(max_tau)

	if (verb) print *, 'max_tau = ', max_tau
	! Error Checking
	if (npts > nstrm_max) then
		print *, 'Number of quadrature angles needed exceeds max allowed...quitting.'
		return
	endif
	if (nlayers > nlay_max) then 
		print *, 'Number of layers exceeds max allowed...quitting.'
		return
	endif

	! Set emissivities
	if (PRESENT(r_v) .AND. PRESENT(r_h)) then 
		Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh, r_v, r_h)
	else
		Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh)
	endif

    !------------------------------------
    ! Delta-Scaling (if required)
    !------------------------------------
  
    IF (PRESENT(Delta_Scaling)) THEN
       CALL deltascaling(opd, ssa, asy, Delta_Scaling, d, w, g)
       sopd = d * w
	  if (VERB) then
		print *, 'Before Delta_Scaling'
		do i = 1, nlayers
			write(*, "(i2, 3f12.6)") i, opd(i), ssa(i), asy(i)
		enddo
		print *, 'After Delta Scaling, f=', delta_scaling
		do i = 1, nlayers
			write(*, "(i2, 3f12.6)") i, d(i), w(i), g(i)
		enddo
   	  endif
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
      do i = lowest_layer, highest_layer ! layers
        IF ( sopd(i) > scat_thresh ) THEN 
          CALL hg_phase_function(g(i), wgts, plegr, pf, pb)
          asopd = sopd(i)
          if (asopd > max_tau) then 
            ndoub = nint( (log(asopd) - logmaxtau)/log2 + HALF) 
          else 
            ndoub = 0
          endif
		   if (verb) write(*,"('sopd(',i2,')=',f8.4,'  asy=',f8.4,'  ndoub=',i2)") i,asopd,g(i),ndoub
!		   if (verb) then
!			write (*, "(a4, 3f10.5, a5, 3f10.5)") 'pf= ',pf(1,:),' pb= ', pb(1,:)
!			do j = 2,npts 
!				write (*, "(a4, 3f10.5, a5, 3f10.5)") '    ',pf(j,:),'     ', pb(j,:)
!			enddo
!		   endif
		  ndoubstep(i) = ndoub
          call truncated_doubling(i, ndoub, d(i), w(i), pf, pb, &
                             pts, wgts, temp(i), temp(i+1), R(:,:,i), T(:,:,i), & 
                             sp(:,i), sm(:,i))
          nl = nl + 2**ndoub
        ELSE
            do k = 1, npts
                call thermal_source( d(i)/pts(k), ZERO, tau(k,i), temp(i+1), &
                                    temp(i), Sp(k,i), Sm(k,i) )
            enddo
            nl = nl + 1
        END IF
      enddo


    !-------------------------------
    ! Set initial/boundary conditions
    !-------------------------------

    tb_d(:, highest_layer+1, 1)  =   T_space  ! Zeroth order scatter only
    iscat = 0 ! set that we're on the zeroth order of scatter
	tb_change = initial_error ! loop keys off this quantity to know when TB has converged
	tbv = SMALL				  ! this stores the total V-radiance at TOA.
	tbh = SMALL				  ! this stores the total H-radiance at TOA.

	thresh = thresh_default
	if (present(converge)) then
		if (converge > ZERO) thresh = converge
	endif
	if (verb) print*, 'Thresh = ', thresh

!	if (verb) then
!		! print out R and T matrices
!		do i = 1, nlayers
!			write (*, "(a3, i2, a4, 3f10.5, a5, 3f10.5)") 'Lay',i,' R= ',R(1,:,i),'  T= ', T(1,:,i)
!			do j = 2,npts 
!				write (*, "(a3, a2, a4, 3f10.5, a5, 3f10.5)") '  ','  ','    ',R(j,:,i),'     ', T(j,:,i)
!			enddo
!		enddo
!	endif

    !--------------------------------------
    !  Loop through orders of scattering
    !--------------------------------------

    sos_loop: DO WHILE ( (( tb_change > thresh ) .AND. ( (iscat+1) .LE. nscat_max)) .OR. (iscat .LT. 2) )

        iscat = iscat + 1 ! Note: iscat = 1 is the zeroth order of scatteringcrtm_rtsolution_m         510  

        if (iscat > 1) tb_d( :, highest_layer + 1, iscat) = ZERO        
        
        !---------------------------------
        ! Loop through Layers (downward)
        !---------------------------------

    layersdn_loop: DO i = highest_layer, lowest_layer, -1

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
        if (lowest_layer == 1) then 
        	tb_uv(:, 1, iscat ) = Rv * tb_d(:,  1, iscat ) 
        	tb_uh(:, 1, iscat ) = Rh * tb_d(:,  1, iscat ) 

        	IF ( iscat == 1 ) THEN ! add surface emission for 0th order of scattering
            	tb_uv(:,  1, iscat ) = tb_uv(:, 1, iscat ) + Ev * tsfc 
            	tb_uh(:,  1, iscat ) = tb_uh(:, 1, iscat ) + Eh * tsfc
        	END IF       
		endif

        !---------------------------------
        ! Loop through layers (upward)
        !---------------------------------

        layersup_loop: DO i = lowest_layer, highest_layer
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
        
		if (ONE .eq. ZERO ) then 
			write (*, "(3f10.3)") 180.0/3.151492654 * ACOS(pts)
			write(*, "('tb_down, iscat = ', i2)") iscat
			do i = highest_layer+1, lowest_layer, -1
			  write(*, "(3f10.5)") tb_d(:,i,iscat)
		    enddo
			write(*, "('tb_up_v, iscat = ', i2)") iscat
			do i = highest_layer+1, lowest_layer, -1
			  write(*, "(3f10.5)") tb_uv(:,i,iscat)
		    enddo
			write(*, "('tb_up_h, iscat = ', i2)") iscat
			do i = highest_layer+1, lowest_layer, -1
			  write(*, "(3f10.5)") tb_uh(:,i,iscat)
		    enddo

		endif

        tb0v( iscat ) = tb_uv(npts, highest_layer + 1, iscat )  ! the "last" pts angle is the observation angle
        tb0h( iscat ) = tb_uh(npts, highest_layer + 1, iscat )  

		tbv = tbv + tb0v(iscat)
		tbh = tbh + tb0h(iscat)
        tb_change = max(tb0v(iscat)/tbv, tb0h(iscat)/tbh)  ! (max of v and h) TB amount added by this order of scatter.

    END DO sos_loop

    nscat = iscat
	nsoi = iscat
    if (verb) print *, 'Tbv(:) = ' , tb0v(1:nscat)
     

!	deallocate(tb_d, tb_uv, tb_uh)

    END SUBROUTINE sos_2nstrm

END SUBROUTINE soi_rt


	SUBROUTINE  soi_rt_TL(opd	, & !  Optical Depth, Nlayers elements
					  ssa	, & ! particle single scatter albedo, Nlayers elements
					  asy 	, & ! asy parameter, Nlayers elements
					  temp  , & ! temperature of each level boundary, Nlayers+1 elements
					  zang	, & ! Observation Angle (degrees)
					  e_v	, & ! v-pol emissivities at required angles
					  e_h	, & ! h-pol emissivities at required angles
                      tbv   , & ! output v-radiance
                      tbh   , & ! output h-radiance
!
                      
					  g_opd,  & ! variation in gas extinction
					  g_ssa,  & ! variation in particle ssa
					  g_asy,  & ! variation in particle asymmetry parameter
					  g_temp, & ! variation in temp profile
					  g_e_v,  & ! variation in e_v
				 	  g_e_h,  & ! variation in e_h
					  g_tbv,  & ! variation in output tbv
					  g_tbh , & ! variation in output tbh
!
                      tsurf , & ! Surface Temperature (Kelvin) (optional input)
 	                  r_v   , & ! surface reflectivity at specified angles for V-pol(optional input)
    	              r_h   , & ! surface reflectivity at specified angles for V-pol(optional input)
                      tspace, & ! user-defined TOA downwelling temperature or radiance (optional input)
                      noscatter, & ! optional logical; when true forces absorption-only RT
                      delta_scaling, & ! optional ; f-factor for delta-scaling
		     		  nstreams, &  ! number of streams to use (optional input)
				      converge, &  ! convergence threshhold [relative fraction; 1e-4 typical]
				      max_tau, & ! maximum layer thickness for doubling initialization (optional input)
                      nscat , & ! number of orders of interaction achieved (optional output)
                      nlay ,  & ! number of layers used (optional output)
!REAL
					  g_r_v, & ! optional - set to 0. MUST be present if r_v is present
					  g_r_h, & ! optional - set to 0. MUST be present if r_h is present
					  g_tsurf, & ! optional - set to 0. if not set and tsurf is present
                      g_delta_scaling, & ! optional ; f-factor for delta-scaling (TL version)
					  verbose ) ! optional logical

	! Input and Output Variables					   
    REAL(fpk), DIMENSION(:), INTENT(in)              ::  opd, ssa, asy
    REAL(fpk), DIMENSION(:), INTENT(in)              ::  temp
    REAL(fpk), INTENT(in)                            ::  zang
	REAL(fpk), INTENT(in), DIMENSION(N_SOI_ANGLES)	::	e_v, e_h
    REAL(fpk), INTENT(in), OPTIONAL                  ::  tsurf
	REAL(fpk), INTENT(out)							::  tbv
	REAL(fpk), INTENT(out)							::  tbh
    REAL(fpk), DIMENSION(N_SOI_ANGLES), INTENT(in), OPTIONAL    ::  r_v, r_h
    REAL(fpk), OPTIONAL                              ::  tspace
    LOGICAL, INTENT(in), OPTIONAL               ::  noscatter
    REAL(fpk), INTENT(in), OPTIONAL, DIMENSION(:)    ::  delta_scaling
    INTEGER, INTENT(out), OPTIONAL              ::  nscat, nlay
    REAL(fpk), DIMENSION(:), INTENT(in)              ::  g_opd
    REAL(fpk), DIMENSION(:), INTENT(in)              ::  g_ssa
    REAL(fpk), DIMENSION(:), INTENT(in)              ::  g_asy
    REAL(fpk), DIMENSION(:), INTENT(in)              ::  g_temp
	REAL(fpk), INTENT(in), DIMENSION(N_SOI_ANGLES)	::	g_e_v, g_e_h
	REAL(fpk), INTENT(out)							::  g_tbv
	REAL(fpk), INTENT(out)							::  g_tbh
    REAL(fpk), DIMENSION(N_SOI_ANGLES), INTENT(in), OPTIONAL    ::  g_r_v, g_r_h
	REAL(fpk), INTENT(inout), OPTIONAL				::  g_tsurf
    REAL(fpk), INTENT(in), OPTIONAL, DIMENSION(:)   ::  g_delta_scaling
	logical, intent(in), optional				::	verbose
	INTEGER, INTENT(in), OPTIONAL					::  nstreams
	real(fpk), intent(in), optional 				::	converge
	real(fpk), intent(in), optional 				::	max_tau

	! Local Variables

    REAL(fpk), DIMENSION(2)                          ::  pts2, wgts2
    REAL(fpk), DIMENSION(3)                          ::  pts4, wgts4
    REAL(fpk), DIMENSION(size(opd))                  ::  sopd
    REAL(fpk)                                        ::  tot_sopd, tot_opd, mu
    INTEGER                                     ::  Nlayers, nl, noi = 0, npts, i
	INTEGER 									::	lowest_layer, highest_layer
    LOGICAL                                     ::  noscat 
    LOGICAL                                     ::  verb 
    REAL(fpk)                                        ::  tsfc, t_space, maxtau, g_tsfc
	REAL(fpk), DIMENSION(:), ALLOCATABLE 			::	pts, wgts
	LOGICAL 								::	maxtau_SET, nstreams_SET


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
		g_tsfc = g_temp(1)
        tsfc = temp(1)
    else
        tsfc = tsurf
		if (PRESENT(g_tsurf)) then
			g_tsfc = g_tsurf
		else
			g_tsfc = ZERO
		endif
    endif

    ! check if the no-scatter keyword was set.
	noscat = .FALSE.
    if (PRESENT(noscatter)) then
        if (noscatter) noscat = .TRUE.
    endif

    Nlayers = size(opd)
    mu = cos(d2r * zang)

    ! if "noscatter" keyword was set, then automatically run no_scat_rt,
    !   without the need to calculate scattering optical depth.

	highest_layer = Nlayers
    IF (noscat) THEN
        ! User is forcing no-scattering RT.
		lowest_layer = 1
        call g_no_scat_rt
        if (PRESENT(nlay)) nlay = nlayers
        if (PRESENT(nscat)) nscat = 0
    ELSE
        ! There MIGHT be scattering. Calculate scat optical depth to see what to do:
	
		! calculate lowest layer to consider
		i = Nlayers+1 ! this will become the lowest needed layer
		tot_opd = ZERO
		tot_sopd = ZERO
		do while ((tot_opd < max_lay2space_opd) .and. (i > 1) )
			i = i - 1
			tot_opd = tot_opd + opd(i)
			sopd(i) =  opd(i) * ssa(i)
			tot_sopd = tot_sopd + sopd(i) 
			if ((tot_opd/mu) < min_lay2space_opd) highest_layer = i-1
		end do	
		lowest_layer = i
		if (highest_layer .eq. 0) highest_layer = 1 ! do at least one layer!
		tot_sopd = tot_sopd / mu
        if (verb) print *, 'total_sopd = ', tot_sopd
		if (verb) print *, 'Lowest_layer = ', lowest_layer
		if (verb) print *, 'Highest_layer = ', highest_layer
		if (verb) print *, 'NLayers = ', Nlayers
		if (verb) print *, 'i = ', i

        If (tot_sopd < min_sopd) Then
            ! Call no-scattering RT subroutine (below)
			if (VERB) print *, 'TL : Calling No_Scat_RT'
            Call g_no_scat_rt
            if (PRESENT(nlay)) nlay = nlayers
            if (PRESENT(nscat)) nscat = 0
        Else
            ! We will need to use the SOI scattering subroutine.
 		   nstreams_SET = .FALSE.
		   if (PRESENT(nstreams)) then
			   if (nstreams > 0) nstreams_SET = .TRUE.
		   endif
		   maxtau_SET = .FALSE.
		   if (PRESENT(max_tau)) then
			   if (max_tau > ZERO) maxtau_SET = .TRUE.
		   endif
		   if (verb) then	
			   print *, 'nstreams_set = ', nstreams_set
			   print *, 'maxtau_set = ', maxtau_set
		   endif

			if (nstreams_SET) then ! user is forcing certain number of streams
		! Gauss-Legendre quadrature
				npts = nstreams / 2
				allocate( pts(npts+1), wgts(npts+1) )

    			IF (npts ==1) THEN
					pts(1) = pts2_(1)
					wgts(1) = ONE				
				ELSE
      				call gauleg( ZERO, ONE, pts(1:npts), wgts(1:npts), npts )
    			END IF

				pts(npts+1) = mu
	    		wgts(npts+1) = ZERO

				if (maxtau_SET) then	
					maxtau = max_tau
				else
					select case(nstreams)
						case(2) 
							maxtau = max_tau2
						case(4) 
							maxtau = max_tau4
						case default				
							maxtau = max_tau_user
					end select
				endif
				CALL g_sos_2nstrm(pts, wgts, maxtau, noi, nl)
				deallocate(pts, wgts)
			else
            	if (tot_sopd < cut_sopd) then
                	! USE 2-stream MODEL	
                	pts2 = (/ pts2_(1) , mu /)
                	wgts2 = (/ wgts2_(1), ZERO /)
					if (maxtau_SET) then	
						maxtau = max_tau
					else
						maxtau = max_tau2
					endif
                	if (VERB) print *, 'Using 2 stream model...'
                	CALL g_sos_2nstrm(pts2, wgts2, maxtau, noi, nl)
            	else
                	! USE 4-stream MODEL
                	pts4 = (/ pts4_(1), pts4_(2), mu /)                     
                	wgts4 = (/ wgts4_(1), wgts4_(2), ZERO /)
                	if (VERB) print *, 'Using 4 stream model...'
					if (maxtau_SET) then	
						maxtau = max_tau
					else
						maxtau = max_tau4
					endif
                	CALL g_sos_2nstrm(pts4, wgts4, maxtau, noi, nl)
            	endif
			endif ! higher_streams
            if (PRESENT(nlay)) nlay = nl
            if (PRESENT(nscat)) nscat = noi
        Endif ! ( tot_sopd < min_opd)
    ENDIF ! (noscat)


	CONTAINS

		SUBROUTINE g_no_scat_rt

			REAL(fpk), dimension(Nlayers) ::	dT, Tmean, tau, Sup, Sdn
			REAL(fpk), dimension(1)	      :: ev, eh, rv, rh
			REAL(fpk), dimension(1) 	  :: g_ev, g_eh, g_rv, g_rh
			INTEGER 		    :: i

            REAL(fpk), dimension(Nlayers)        ::  g_dT
            REAL(fpk), dimension(NLayers)        ::  g_Tmean
            REAL(fpk), dimension(NLayers)        ::  g_tau
            REAL(fpk), dimension(NLayers)        ::  g_Sup
            REAL(fpk), dimension(Nlayers)        ::  g_Sdn
            REAL(fpk)                            ::  g_Tsup
	
                       
			nl = nlayers
			nscat = 0

			tau = exp(-opd/mu) ! local transmittance through given layer
			g_tau = -g_opd * tau / mu

            do i = 1, Nlayers
                call thermal_source_TL(opd(i)/mu, ZERO, tau(i), temp(i+1), temp(i), &
									g_opd(i)/mu, ZERO, g_tau(i), g_temp(i+1), g_temp(i), &
									Sup(i), Sdn(i), g_Sup(i), g_Sdn(i))
            enddo
										
			! calculate surface emissivity		

			if (verb) then
				print *, 'g_Sup = ', g_Sup
				print *, 'g_Sdn = ', g_Sdn
				print *, 'g_tau = ', g_tau
			endif
			

		!	Set up single emissivity and reflectivity at the observation angle
			if (PRESENT(r_v) .AND. PRESENT(r_h)) then 
				Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh, r_v, r_h)
			else
				Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh)
			endif
			if (PRESENT(g_r_v) .AND. PRESENT(g_r_h)) then 
				Call soi_set_emissivities_TL(g_e_v, g_e_h, g_ev, g_eh, g_rv, g_rh, g_r_v, g_r_h)
			else
				Call soi_set_emissivities_TL(g_e_v, g_e_h, g_ev, g_eh, g_rv, g_rh)
			endif

			if (verb) then
				print *, 'g_ev = ',  g_ev
				print *, 'g_eh = ', g_eh
				print *, 'g_rv = ', g_rv
				print *, 'g_rh = ', g_rh
			endif

			! initially, we are unpolarized.  Just use Tbv

			g_Tbv = ZERO
			g_Tbh = ZERO
			if (lowest_layer == 1) then
				Tbv = T_space
				do i=highest_layer, 1, -1 
					g_Tbv = g_Tbv * tau(i) + g_tau(i)* Tbv + g_Sdn(i)
					Tbv = Tbv * tau(i) + Sdn(i)
				end do
				g_Tbh = g_eh(1) * tsfc + g_tsfc * eh(1) + g_rh(1) * Tbv + g_Tbv * rh(1)
				Tbh = eh(1) * tsfc + rh(1) * Tbv ! now we get polarization.
				g_Tbv = g_ev(1) * tsfc + g_tsfc * ev(1) + g_rv(1) * Tbv + g_Tbv* rv(1)
				Tbv = ev(1) * tsfc + rv(1) * Tbv
			endif

			if (verb) print *, 'Finished Surface, g_Tbv, h = ', g_Tbv, g_Tbh
		
			! loop back up through the atm, keeping track of pol.

			do i= lowest_layer, highest_layer
				g_Tbv= g_Tbv * tau(i) + g_tau(i) * Tbv + g_Sup(i)
				Tbv= Tbv * tau(i) + Sup(i)
				g_Tbh= g_Tbh * tau(i) + g_tau(i) * Tbh + g_Sup(i)
				Tbh= Tbh * tau(i) + Sup(i)
			end do

			if (verb) print *, 'Finished No_Scat_rt_TL; g_Tbv, h = ', g_Tbv, g_Tbh

		END SUBROUTINE g_no_scat_rt

  		SUBROUTINE g_sos_2nstrm( pts, 	& ! Input: Array of quadrature points
                           wgts,		& ! Input: Array of quadrature weights
			   max_tau, 	& ! Input: Maximum allowed layer slant scat optical depth
                           nscat,		& ! Output: Number of orders of scatter achieved
                           nl			) ! Output: Total number of layers used in calculations

                      
    !#--------------------------------------------------------------------------#
    !#                                                                          #
    !#                          -- Type declarations --                         #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

	IMPLICIT NONE
    !-----------
    ! Arguments
    !-----------


    REAL(fpk), DIMENSION(:), INTENT(in)  	:: 	pts       ! npts
    REAL(fpk), DIMENSION(:), INTENT(in)  	:: 	wgts      ! npts
	REAL(fpk), INTENT(in)					::  max_tau
	INTEGER, INTENT(out) 				::	nscat
	INTEGER, INTENT(out)				::	nl   

    !------------------
    ! Local variables
    !------------------

    REAL(fpk), PARAMETER                             ::  initial_error = 1.0e10_fpk
	REAL(fpk), PARAMETER							 ::  SMALL = 1.0e-15_fpk
    REAL(fpk), DIMENSION(Nlayers)				::	d, w, g, f
    REAL(fpk), DIMENSION(2*(size(pts)-1))		        :: leg
    REAL(fpk), DIMENSION(2*(size(pts)-1),size(pts)) 	:: plegr
    REAL(fpk), DIMENSION(size(pts),Nlayers+1) 		:: tb_d, tb_uv, tb_uh
    REAL(fpk), DIMENSION(size(pts),Nlayers+1) 		:: last_d, last_uv, last_uh
    REAL(fpk), DIMENSION(size(pts), size(pts))		:: pf, pb, taudirect
    REAL(fpk), DIMENSION(size(pts), Nlayers) 		:: tau, Sp, Sm
    REAL(fpk), DIMENSION(size(pts))		  		:: source
    REAL(fpk), DIMENSION(size(pts), size(pts), Nlayers) 	:: R, T
    REAL(fpk), DIMENSION(nscat_max)  			:: tb0v, tb0h
    REAL(fpk), DIMENSION(size(pts))				:: ev, eh, rv, rh
    INTEGER i, j, k, l, ndoub, nleg, n1, n2
    INTEGER isign
    REAL(fpk)    tb_change, pp, norm, asopd
    INTEGER    :: npts, iscat
    REAL(fpk)		:: logmaxtau, thresh
	

    REAL(fpk), DIMENSION(Nlayers)				:: g_d, g_w, g_f, g_g
    REAL(fpk), DIMENSION(size(pts),Nlayers+1) 		:: g_tb_d, g_tb_uv, g_tb_uh
    REAL(fpk), DIMENSION(size(pts),Nlayers+1) 		:: g_last_d, g_last_uv, g_last_uh
    REAL(fpk), DIMENSION(size(pts), size(pts))		:: g_pf, g_pb, g_taudirect
    REAL(fpk), DIMENSION(size(pts), Nlayers) 		:: g_tau, g_Sp, g_Sm
    REAL(fpk), DIMENSION(size(pts))		  		:: g_source, one_tau
    REAL(fpk), DIMENSION(size(pts), size(pts), Nlayers) 	:: g_R, g_T
    REAL(fpk), DIMENSION(nscat_max)  			:: g_tb0v, g_tb0h
    REAL(fpk), DIMENSION(size(pts))				:: g_ev, g_eh, g_rv, g_rh
    REAL(fpk)    g_pp, g_norm, g_asopd

	npts 	= size(pts)
	logmaxtau = log(max_tau)

	! Error Checking
	if (npts > nstrm_max) then
		print *, 'Number of quadrature angles needed exceeds max allowed...quitting.'
		return
	endif
	if (nlayers > nlay_max) then 
		print *, 'Number of layers exceeds max allowed...quitting.'
		return
	endif


	! Set emissivities
	if (PRESENT(r_v) .AND. PRESENT(r_h)) then 
		Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh, r_v, r_h)
	else
		Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh)
	endif
	if (PRESENT(g_r_v) .AND. PRESENT(g_r_h)) then 
		Call soi_set_emissivities_TL(g_e_v, g_e_h, g_ev, g_eh, g_rv, g_rh, g_r_v, g_r_h)
	else
		Call soi_set_emissivities_TL(g_e_v, g_e_h, g_ev, g_eh, g_rv, g_rh)
	endif

    !------------------------------------
    ! Delta-Scaling (if required)
    !------------------------------------

	if (VERB) print *, 'About to Delta-Scale (if necessary)...'

	IF ( PRESENT(Delta_Scaling) .AND. PRESENT(g_Delta_Scaling) ) THEN
	   CALL deltascaling(opd, ssa, asy, Delta_Scaling, d, w, g)
	   CALL deltascaling_TL(opd, ssa, asy, Delta_Scaling, &
                            g_opd, g_ssa, g_asy, g_Delta_Scaling, &
                            g_d, g_w, g_g)
		sopd = d * w
	ELSE
		g_w = g_ssa
		w = ssa
		g_g = g_asy
		g = asy
		g_d = g_opd
		d = opd
	ENDIF


		if (verb) then 
			print *, 'g_d = ', g_d
			print *, 'g_w = ', g_w
			print *, 'g_g =',  g_g
			print *, 'g_temp ', g_temp
			print *, 'sopd = ', sopd
		endif
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
	    g_tau(:,i) = -g_d(i) * tau(:,i) / pts    
	enddo

	if (verb) print *, 'g_tau = ', g_tau
	nl = 0
	if (VERB) print *, 'About to Calculate Phase Functions...'
!-------------------------------------------
! Create Normalized Phase Functions 
! (which do not depend upon layer splitting
!-------------------------------------------
	  do i = lowest_layer, highest_layer ! layers
		IF ( sopd(i) > scat_thresh ) THEN 
		  call hg_phase_function_TL(g(i), wgts, plegr, g_g(i), pf, pb, g_pf, g_pb)

		  asopd = sopd(i)
		  if (asopd > max_tau) then 
		 	ndoub = nint( (log(asopd) - logmaxtau)/log2 + HALF) 
		  else 
			ndoub = 0
		  endif

		  if (VERB) print *, 'DOWN: i, asopd, ndoub = ', i, asopd, ndoub

		  call truncated_doubling_TL(ndoub, d(i), w(i), pf, pb, &
                          pts, wgts, temp(i), temp(i+1), &
                          g_d(i), g_w(i), g_pf, g_pb, g_temp(i), g_temp(i+1), &
					      R(:,:,i), T(:,:,i), sp(:,i), sm(:,i), &
                          g_R(:,:,i), g_T(:,:,i), g_sp(:,i), g_sm(:,i) )
	  
		  if (ONE == ZERO) then 
  		  write(*, "('Lay:',i2,' g_d:',f10.5,'g_w:',f10.5)") i, g_d, g_w
		  write(*, "('g_R, g_T(',i2,') = ',3f10.5,'   ',3f10.5)") i, g_R(1,:,i), g_T(1,:,i)
		  do j = 2,npts
		    write(*, "(a14,3f10.5,'   ',3f10.5)") ' ',g_R(j,:,i), g_T(j,:,i)
		  enddo
		  endif

		  nl = nl + 2**ndoub 

		ELSE
			do k = 1, npts
				call thermal_source_TL(d(i)/pts(k), ZERO, tau(k,i), temp(i+1), temp(i), &
					g_d(i)/pts(k), ZERO, g_tau(k,i), g_temp(i+1), g_temp(i), &
					Sp(k,i), Sm(k,i), g_Sp(k,i), g_Sm(k,i))
			end do
		    nl = nl + 1
		END IF
	  enddo

    !-------------------------------
    ! Set upper boundary conditions
    !-------------------------------

    g_tb_d(:, highest_layer+1)  =   ZERO
    tb_d(:, highest_layer+1)  =   T_space  ! Zeroth order scatter only
    iscat = 0 ! set that we're on the zeroth order of scatter
	tb_change = initial_error ! loop keys off this quantity to know when TB has converged
	tbv = SMALL				  ! this stores the total V-radiance at TOA.
	tbh = SMALL				  ! this stores the total H-radiance at TOA.
	g_tbv = ZERO
	g_tbh = ZERO

	thresh = thresh_default
	if (present(converge)) then
		if (converge > ZERO) thresh = converge
	endif
    !--------------------------------------
    !  Loop through orders of scattering
    !--------------------------------------

    sos_loop: DO WHILE ( (( tb_change > thresh ) .AND. ( (iscat+1) .LE. nscat_max)) .OR. (iscat .LT. 2) )

		iscat = iscat + 1 ! Note: iscat = 1 is the zeroth order of scattering

		! reset tb arrays from last order of scattering

		g_last_d = g_tb_d
		last_d = tb_d
		g_last_uv = g_tb_uv
		last_uv = tb_uv
		g_last_uh = g_tb_uh
		last_uh = tb_uh
		if (iscat > 1) g_tb_d(:,highest_layer+1) = ZERO		
		if (iscat > 1) tb_d(:,highest_layer+1) = ZERO 

        !---------------------------------
        ! Loop through Layers (downward)
        !---------------------------------

    layersdn_loop: DO i = highest_layer, lowest_layer, -1

	        IF ( iscat == 1 ) THEN
     	          g_source = g_sm(:,i)            
     	          source = sm(:,i)            		
     	       ELSE

            !--------------------------------------------------------------
            !  Scattering source functions
            !
            !  (Note: For level 0 there is no upwelling scattering source)
            !--------------------------------------------------------------

              IF ( sopd(i) > scat_thresh ) THEN

                do j = 1, npts
                  g_source( j ) = ZERO
                  source( j ) = ZERO
	! scattering erases any polarization signature (so avg last_up v & h)

                  do k = 1, npts  ! integrate over angle
                    g_source( j ) = g_source( j ) + g_T(j, k, i) * last_d(k,i+1) + &
                                    T(j, k, i) * g_last_d(k,i+1) &
                                    + g_R(j, k, i) * HALF *(last_uv(k, i)  + last_uh(k, i)) &
                                    + R(j, k, i) * HALF *(g_last_uv(k, i)  + g_last_uh(k, i))
                    source( j ) = source( j ) + T(j, k, i) * last_d(k,i+1) + &
                                    R(j, k, i) * HALF *(last_uv(k, i)  + last_uh(k, i))
                  enddo
                end do

              ELSE               

				g_source = ZERO
				source = ZERO
              ENDIF
            ENDIF

            g_tb_d(:, i ) = g_tb_d(:, i+1 ) * tau(:, i ) + g_tau(:, i ) * tb_d(:, i+1 ) + g_source
            tb_d(:, i ) = tb_d(:, i+1 ) * tau(:, i ) + source            	

        END DO layersdn_loop

        !-----------------------------------
        ! Surface: reflection and emission 
        !    Emission only for iscat=1 !!
        !-----------------------------------
		if (lowest_layer == 1) then 
            g_tb_uv(:, 1 ) = g_Rv * tb_d(:,  1 ) + g_tb_d(:,  1 ) * Rv  
            tb_uv(:, 1 ) = Rv * tb_d(:,  1 ) 
    	    g_tb_uh(:, 1 ) = g_Rh * tb_d(:,  1 ) + g_tb_d(:,  1 ) * Rh  
		    tb_uh(:, 1 ) = Rh * tb_d(:,  1 ) 

        	IF ( iscat == 1 ) THEN ! add surface emission for 0th order of scattering
	           g_tb_uv(:,  1 ) = g_tb_uv(:, 1 ) + g_Ev * tsfc + Ev * g_tsfc 
    	       tb_uv(:,  1 ) = tb_uv(:, 1 ) + Ev * tsfc 
        	   g_tb_uh(:,  1 ) = g_tb_uh(:, 1 ) + g_Eh * tsfc + Eh * g_tsfc
               tb_uh(:,  1 ) = tb_uh(:, 1 ) + Eh * tsfc
			ENDIF
		endif     

        !---------------------------------
        ! Loop through layers (upward)
        !---------------------------------
		

        layersup_loop: DO i = lowest_layer, highest_layer

			IF ( iscat == 1 ) THEN        
     	                   g_source = g_sp(:,i)             
     	            	       source = sp(:,i)            
			ELSE ! iscat > 1

      	       IF ( sopd(i) > scat_thresh ) THEN
        	       do j = 1, npts
            	     g_source( j ) = ZERO
            	     source( j ) = ZERO
                	 do k = 1, npts  ! integrate over angle
                  		 g_source(j) =  g_source(j)  + g_R(j, k, i) * last_d( k, i+1) + &
                  		                R(j, k, i) * g_last_d( k, i+1) &
					+ g_T(j, k, i) * HALF * ( last_uv(k, i) + last_uh(k, i) ) &
					+ T(j, k, i) * HALF * ( g_last_uv(k, i) + g_last_uh(k, i) )
                  		 source(j) =  source(j)  + R(j, k, i) * last_d( k, i+1) + &
					T(j, k, i) * HALF * ( last_uv(k, i) + last_uh(k, i) )
                 	 enddo
              	   end do				

               ELSE ! ssa(i) = 0          

                 g_source = ZERO
                 source = ZERO

               ENDIF

	        ENDIF

		g_tb_uv( :, i+1 ) = g_tb_uv(:, i ) * tau(:, i ) + tb_uv(:, i ) * g_tau(:, i ) &
			                                             + g_source
		tb_uv( :, i+1 ) = tb_uv(:, i ) * tau(:, i ) + source
		g_tb_uh( :, i+1 ) = g_tb_uh(:, i ) * tau(:, i ) + tb_uh(:, i ) * g_tau(:, i ) &
			                    + g_source
		tb_uh( :, i+1 ) = tb_uh(:, i ) * tau(:, i ) + source

        END DO layersup_loop

    	g_tb0v( iscat ) = g_tb_uv(npts, highest_layer + 1)
    	tb0v( iscat ) = tb_uv(npts, highest_layer + 1)  ! the "last" pts angle is the observation angle
		g_tb0h( iscat ) = g_tb_uh(npts, highest_layer + 1)  
		tb0h( iscat ) = tb_uh(npts, highest_layer + 1)  
		
		tbv = tbv + tb0v(iscat)
		tbh = tbh + tb0h(iscat)
		g_tbv = g_tbv + g_tb0v(iscat)
		g_tbh = g_tbh + g_tb0h(iscat)

       tb_change = max(tb0v(iscat)/tbv, tb0h(iscat)/tbh)  ! (max of v and h) TB amount added by this order of scatter.

    END DO sos_loop

    nscat = iscat
	if (verb) print *, 'Tbv(:) = ' , tb0v(1:nscat)

    END SUBROUTINE g_sos_2nstrm

    END SUBROUTINE soi_rt_TL

    SUBROUTINE soi_rt_AD(opd,		&	! Input: Profile of Extinction Optical Depths
                         ssa,      &   ! Input: Profile of layer effective single-scatter albedos
                         asy,       &   ! Input: Profile of asymmetry factors
                         temp,      &   ! Input: Temperatures at each level
                         zang,      &   ! Input: Observation zenith angle (degrees)
						 e_v   , & ! input, emissivity array at specified angles for V-pol
				  		 e_h   , & ! input, eemissivity array at specified angles for H-pol
!
                         adopd,     &   ! Output: Adjoint of opd
                         adssa,     &   ! Output: Adjoint of ssa
                         adasy,     &   ! Output: Adjoint of asy
                         adtemp,    &   ! Output: Adjoint of temp
						 ade_v   , & 	! Output, adjoint of e_v
				  		 ade_h   , & 	! Output, adjoint of e_h
						 adtbv,     &   ! Input: Adjoint forcing for tbv
                         adtbh, 	&   ! Input: Adjoint forcing for tbh
!
                         tsurf,     &   ! Input: Surface temperature                     [ optional ]
						 r_v   , & ! optional input, emissivity array at specified angles for V-pol
				  		 r_h   , & ! optional input, eemissivity array at specified angles for H-pol
!
						 adtsurf, 	&	! Input: Adjoint of Tsurf [optional]
						 adr_v, 	&	! Input: Adjoint of r_v [optional]
						 adr_h, 	&	! Input: Adjoint of r_h [optional]
    	                 noscatter, & 	! optional logical; when true forces absorption-only RT
        	             Delta_Scaling, & ! optional; f-factor for delta-scaling
					   adDelta_Scaling, & ! optional; f-factor adjoint for delta-scaling
						 nstreams, &    ! Optional: Set this to even number of streams to use.
						 verbose ) ! optional logical; turns verbose output ON.

!------------------
! Define arguments
!------------------
	IMPLICIT NONE

    REAL(fpk), DIMENSION( : ), INTENT( in )              ::  opd
    REAL(fpk), DIMENSION( : ), INTENT( in )              ::  ssa
    REAL(fpk), DIMENSION( : ), INTENT( in )              ::  asy
    REAL(fpk), DIMENSION( : ), INTENT( in )              ::  temp
    REAL(fpk), INTENT( in )                              ::  zang
	REAL(fpk), INTENT(in), DIMENSION(N_SOI_ANGLES)		::	e_v, e_h

    REAL(fpk), INTENT( in ), OPTIONAL                    ::  tsurf
    REAL(fpk), DIMENSION(N_SOI_ANGLES), INTENT(in), OPTIONAL    ::  r_v, r_h
    LOGICAL, INTENT(in), OPTIONAL               	::  noscatter
    REAL(fpk), INTENT(in), OPTIONAL, DIMENSION(:)        ::  Delta_Scaling
    REAL(fpk), INTENT(inout), OPTIONAL, DIMENSION(:)     ::  adDelta_Scaling
	LOGICAL, INTENT(in), OPTIONAL 					::	verbose
	INTEGER, INTENT(in), OPTIONAL					::  nstreams

    REAL(fpk), DIMENSION( : ), INTENT( inout )             ::  adopd
    REAL(fpk), DIMENSION( : ), INTENT( inout )             ::  adssa
    REAL(fpk), DIMENSION( : ), INTENT( inout )             ::  adasy
    REAL(fpk), DIMENSION( : ), INTENT( inout )             ::  adtemp 
	REAL(fpk), INTENT(inout), DIMENSION(N_SOI_ANGLES)	   ::	ade_v, ade_h
    REAL(fpk), INTENT( inout ), OPTIONAL                   ::  adtsurf
    REAL(fpk), DIMENSION(N_SOI_ANGLES), INTENT(inout), OPTIONAL    ::  adr_v, adr_h

    REAL(fpk), INTENT( in )                           ::  adtbv
    REAL(fpk), INTENT( in )                           ::  adtbh


!------------------------
! Define local variables
!------------------------
 
    REAL(fpk), DIMENSION( 2 )                            ::  pts2, wgts2
    REAL(fpk), DIMENSION( 3 )                            ::  pts4, wgts4
    REAL(fpk), DIMENSION( size( opd ) )                  ::  sopd
    REAL(fpk)                                            ::  tot_sopd, tot_opd
    REAL(fpk)                                            ::  mu
    INTEGER                                         ::  Nlayers, npts
    INTEGER                                         ::  i
	REAL(fpk)											::	tsfc, adtsfc
	LOGICAL 										::	noscat 
	LOGICAL 										::	verb 
	INTEGER 									::	lowest_layer, highest_layer
	LOGICAL 								:: nstreams_SET
	REAL(fpk), DIMENSION(:), ALLOCATABLE 			::	pts, wgts

	verb = .FALSE.
	if (present(verbose)) then
		if (verbose) verb = .TRUE.
	endif

	if (verb) print *, 'ENTERED SOI_RT_AD MODEL.'

! ZERO OUT LOCAL Adjoint Variables
	adtsfc = ZERO

!------------------------
! Forward recomputations
!------------------------
    Nlayers = size( opd )
    mu = cos( d2r * zang )

	noscat = .FALSE.
    if (PRESENT(noscatter)) then
		if (noscatter) noscat = .TRUE.
	endif
   
	highest_layer = Nlayers
	if (.NOT. (noscat)) then
		i = Nlayers+1 ! this will become the lowest needed layer
		tot_opd = ZERO
		tot_sopd = ZERO
		do while ((tot_opd < max_lay2space_opd) .and. (i > 1) )
			i = i - 1
			tot_opd = tot_opd + opd(i)
			sopd(i) =  opd(i) * ssa(i)
			tot_sopd = tot_sopd + sopd(i) 
			if ((tot_opd/mu) < min_lay2space_opd) highest_layer = i-1
		end do	
		lowest_layer = i
		if (highest_layer .eq. 0) highest_layer = 1 ! do at least one layer!
		tot_sopd = tot_sopd / mu
        if (verb) print *, 'total_sopd = ', tot_sopd
		if (verb) print *, 'Lowest_layer = ', lowest_layer
		if (verb) print *, 'Highest_layer = ', highest_layer
	endif

	if (verb) print *, 'Tot_sopd = ', tot_sopd

    IF ( .NOT. ( PRESENT( tsurf ) ) ) then
		tsfc = temp( 1 )
	ELSE
		tsfc = tsurf
	ENDIF
 

!-----------------------
! Adjoint calculations
!-----------------------
    IF ( (tot_sopd < min_sopd) .OR. (noscat) ) THEN    
      call no_scat_rt_AD
    ELSE
	  nstreams_SET = .FALSE.
	  if (PRESENT(nstreams)) then
		 if (nstreams > 0) nstreams_SET = .TRUE.
	  endif

	  IF (nstreams_SET) then 
		! Gauss-Legendre quadrature
		npts = nstreams / 2
		allocate( pts(npts+1), wgts(npts+1) )

    	IF (npts ==1) THEN
			pts(1) = pts2_(1)
			wgts(1) = ONE				
		ELSE
      		call gauleg( ZERO, ONE, pts(1:npts), wgts(1:npts), npts )
    	END IF

		pts(npts+1) = mu
	    wgts(npts+1) = ZERO
		call soi_2nstrm_AD(pts, wgts)
		deallocate(pts, wgts)
	  ELSE ! nstreams NOT set by user.  Pick 2 or 4-stream model
    	  IF ( tot_sopd < cut_sopd ) THEN
    	  ! Two stream
        	pts2 = (/ pts2_(1) , mu /)
        	wgts2 = (/ wgts2_(1), ZERO /)
			if (verb) print *, 'Entering soi_2nstrm_AD, 2-stream model'
        	call soi_2nstrm_AD( pts2, wgts2)
    	  ELSE      
    	  ! Four stream
        	pts4 = (/ pts4_(1), pts4_(2), mu /)                     
        	wgts4 = (/ wgts4_(1), wgts4_(2), ZERO /)
			if (verb) print *, 'Enter soi_2nstrm_AD, 4-stream model'
        	call soi_2nstrm_AD( pts4, wgts4)
    	  END IF
	  ENDIF
    END IF    

    IF ( .NOT. ( PRESENT( tsurf ) ) ) THEN
      adtemp( 1 ) = adtemp( 1 ) + adtsfc
    ELSE
	  adtsurf = adtsfc
	ENDIF


CONTAINS

	SUBROUTINE no_scat_rt_AD
!==============================================================================================
!
!   Adjoint model for absorption/emission only RT model. Uses stored values from forward 
!   model run.
!
!   Written by Tom Greenwald SSEC/UW 6/22/2004
!
!==============================================================================================


		IMPLICIT NONE
	
!------------------------
! Define local variables
!------------------------
        REAL(fpk), DIMENSION( Nlayers )          ::  dT, Tmean, tau, A, B, tmp
        REAL(fpk), DIMENSION(1)                  ::  ev, eh, rv, rh
		REAL(fpk)								 ::  Tsup
        INTEGER 		            ::  i
        REAL(fpk), DIMENSION( Nlayers )          ::  adtau
        REAL(fpk), DIMENSION( 0 : Nlayers )      ::  adTbv_u
        REAL(fpk), DIMENSION( 0 : Nlayers )      ::  adTbv_d
        REAL(fpk), DIMENSION( 0 : Nlayers )      ::  adTbh_u
        REAL(fpk), DIMENSION(Nlayers)            ::  adSup, adSdn
        REAL(fpk), DIMENSION(1)                  ::  adev, adeh
        REAL(fpk), DIMENSION(1)                  ::  adrv, adrh
		REAL(fpk)								::	slopd, slopd_AD, no_AD
!--------------------------------
! Reset local adjoint variables
!--------------------------------
        adtau = ZERO
        adTbv_u = ZERO
        adTbv_d = ZERO
        adTbh_u = ZERO
        adSup = ZERO
		adSdn = ZERO
        adev = ZERO
        adeh = ZERO
        adrv = ZERO
        adrh = ZERO

!-------------------------
! Forward recomputations
!-------------------------
		tau = exp( -opd / mu )


		!	Set up single emissivity and reflectivity at the observation angle
		if (PRESENT(r_v) .AND. PRESENT(r_h)) then 
			Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh, r_v, r_h)
		else
			Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh)
		endif
		if (VERB) print *, 'adtsfc = ', adtsfc

!-----------------------
! Adjoint calculations
!-----------------------
        adtbh_u( highest_layer ) = adtbh_u( highest_layer ) + adtbh
        adtbv_u( highest_layer ) = adtbv_u( highest_layer ) + adtbv       

        DO i = highest_layer, lowest_layer, -1
			adtau(i) = adtau(i) + (Tbv_u(i-1)*adTbv_u(i) + &
						Tbh_u(i-1)*adTbh_u(i))
			adSup(i) = adSup(i) + adTbv_u(i) + adTbh_u(i)
			adTbv_u(i-1) = adTbv_u(i-1) + tau(i)*adTbv_u(i)
			adTbh_u(i-1) = adTbh_u(i-1) + tau(i)*adTbh_u(i)
        END DO

		if (lowest_layer == 1) then 
        	adev = adev + adTbv_u( 0 ) * tsfc
        	adtsfc = adtsfc + adTbv_u( 0 ) * ev(1) + adTbh_u(0) * eh(1)
        	adrv = adrv + adTbv_u( 0 ) * Tbv_d( 0 )
        	adTbv_d( 0 ) = adTbv_d( 0 ) + adTbv_u( 0 ) * rv(1) + adTbh_u(0)*rh(1)
        	adeh = adeh + adTbh_u( 0 ) * tsfc
        	adrh = adrh + adTbh_u( 0 ) * Tbv_d( 0 )
        	adTbv_u( 0 ) = ZERO
        	adTbh_u( 0 ) = ZERO
        	DO i = 1, Nlayers 
        	  adtau( i ) = adtau( i ) + adTbv_d( i-1 ) * Tbv_d( i  )
        	  adTbv_d( i ) = adTbv_d( i-1 ) * tau( i ) + adTbv_d( i )
        	  adSdn(i) = adSdn(i) + adTbv_d(i-1)
			  adTbv_d( i-1 ) = ZERO
        	END DO
		endif
  
		! adjoint of emissivity set-up
		if (PRESENT(adr_v) .AND. PRESENT(adr_h)) then
			CALL soi_set_emissivities_AD(ade_v, ade_h, adev, adeh, adrv, adrh, adr_v, adr_h)
		else
			CALL soi_set_emissivities_AD(ade_v, ade_h, adev, adeh, adrv, adrh)
		endif	


		do i = lowest_layer, highest_layer
			slopd = opd(i)/mu
			slopd_AD = ZERO
			no_AD = ZERO
			call thermal_source_AD(slopd, ZERO, tau(i), temp(i+1), &
								temp(i), slopd_AD, no_AD, adtau(i), adtemp(i+1), &
								adtemp(i), adSup(i), adSdn(i) )
			adopd(i) = adopd(i) + slopd_AD / mu
		enddo

        adopd = adopd - adtau * tau / mu
        adtau = ZERO

	END SUBROUTINE no_scat_rt_AD


    SUBROUTINE soi_2nstrm_AD( pts,       &   ! Input: Array of quadrature points
                              wgts        )   ! Input: Array of quadrature weights
!---------------------------------------------------------------------------------------------------
!  Adjoint model for multi-stream SOI RT model. Uses stored values from forward model run. 
!
!  Written by Chris O'Dell (UW-AOS) Dept, 2004-2005; Tom Greenwald (CIMSS), 7/2004.
!---------------------------------------------------------------------------------------------------

		IMPLICIT NONE

!----------------------
!  Define arguments
!----------------------
        REAL(fpk), DIMENSION( : ), INTENT( in )      ::  pts
        REAL(fpk), DIMENSION( : ), INTENT( in )      ::  wgts
 
!--------------------------
!  Define local variables
!--------------------------
        REAL(fpk), DIMENSION( Nlayers )                                  ::  d, w, g, f
        REAL(fpk), DIMENSION( 2 * ( size( pts ) - 1 ) )                  ::  leg
        REAL(fpk), DIMENSION( 2 * ( size( pts ) - 1 ), size(pts) )       ::  plegr
        REAL(fpk), DIMENSION( size( pts ), size( pts ) )                 ::  pf, pb, R, T
        REAL(fpk), DIMENSION( size( pts ), Nlayers )                     ::  tau
        REAL(fpk), DIMENSION( size( pts ) )                              ::  ev, eh, rv, rh
        REAL(fpk), DIMENSION( size( pts ) )                              ::  norm
        INTEGER                                                     ::  i, j, k, l, il, iscat
        INTEGER                                                     ::  isign, ndoub, n1, n2
        INTEGER                                                     ::  npts, nleg, nscat
        REAL(fpk)                                                        ::  pp
		REAL(fpk)														::	slopd, slopd_AD, no_AD
        REAL(fpk), DIMENSION( nscat_max )                                ::  adtb0h
        REAL(fpk), DIMENSION( nscat_max )                                ::  adtb0v
        REAL(fpk), DIMENSION( size( pts ), nlay_max+1, nscat_max )       ::  adtb_uv        
        REAL(fpk), DIMENSION( size( pts ), nlay_max+1, nscat_max )       ::  adtb_uh        
        REAL(fpk), DIMENSION( size( pts ), nlay_max+1, nscat_max )       ::  adtb_d
        REAL(fpk), DIMENSION( size( pts ) )                              ::  adsource
        REAL(fpk), DIMENSION( size( pts ), Nlayers )                     ::  adtau
        REAL(fpk), DIMENSION( size( pts ), Nlayers )                     ::  adsp
        REAL(fpk), DIMENSION( size( pts ), Nlayers )                     ::  adsm
        REAL(fpk), DIMENSION( size( pts ), size( pts ), Nlayers )        ::  adR
        REAL(fpk), DIMENSION( size( pts ), size( pts ), Nlayers )        ::  adT
        REAL(fpk), DIMENSION( size( pts ) )                              ::  adev, adeh, adrv, adrh
        REAL(fpk), DIMENSION( size( pts ), size( pts ) )                 ::  adpf
        REAL(fpk), DIMENSION( size( pts ), size( pts ) )                 ::  adpb
        REAL(fpk), DIMENSION( Nlayers )                                  ::  add
        REAL(fpk), DIMENSION( Nlayers )                                  ::  adw
        REAL(fpk), DIMENSION( Nlayers )                                  ::  adg
        REAL(fpk), DIMENSION( Nlayers )                                  ::  adf
        REAL(fpk)                                                        ::  adpp
        REAL(fpk), DIMENSION( size( pts ) )                              ::  adnorm

!---------------------------------
!  Reset local adjoint variables 
!---------------------------------
		adtb0h = ZERO
		adtb0v = ZERO
        adtb_uh = ZERO
        adtb_uv = ZERO
        adtb_d = ZERO
        adtau = ZERO
        adsm = ZERO
        adsp = ZERO

        adpp = ZERO
        adnorm = ZERO
        adg = ZERO
        adf = ZERO
        adw = ZERO
		add = ZERO
        adpf = ZERO
        adpb = ZERO
        adev = ZERO
        adeh = ZERO
        adrv = ZERO
        adrh = ZERO
        adsource = ZERO
        adR = ZERO
        adT = ZERO
!------------------------
! Forward recomputations
!------------------------
        npts = size( pts )
        nleg = 2 * ( npts - 1 )
        nscat = nsoi
	if (verb) then
		print *, 'in ADJOINT!'
		print *, 'nscat = ', nscat
	endif

! Set emissivities
		if (PRESENT(r_v) .AND. PRESENT(r_h)) then 
			Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh, r_v, r_h)
		else
			Call soi_set_emissivities(e_v, e_h, ev, eh, rv, rh)
		endif

!------------------------------------
! Delta-Scaling (if required)
!------------------------------------
    	IF (PRESENT(Delta_Scaling)) THEN
           CALL deltascaling(opd, ssa, asy, Delta_Scaling, d, w, g)    
           sopd = d * w
    	ELSE
            	d = opd
            	w = ssa
            	g = asy
    	ENDIF

		DO i = 1, Nlayers
			tau( :, i ) = exp( -d( i ) / pts )
		END DO
	
 		call calc_plegr(pts, nleg, plegr)


!-----------------------
! Adjoint computations
!-----------------------
        adtb0h(1:nscat) = adtbh
        adtb0v(1:nscat) = adtbv
 
		if (verb) print *, 'Entering SOI loop...'
		if (verb) print *, 'Nscat = ', nscat
		if (verb) print *, 'Ndoub = ', ndoubstep(1:nlayers)
!--------------------------------------------------------------------
! Loop through each successive order of interaction in reverse order
!--------------------------------------------------------------------
        DO iscat = nscat, 1, -1
          adtb_uh( npts, highest_layer + 1, iscat ) = adtb_uh( npts, highest_layer + 1, iscat ) + adtb0h( iscat )
          adtb0h( iscat ) = ZERO
          adtb_uv( npts, highest_layer + 1, iscat ) = adtb_uv( npts, highest_layer + 1, iscat ) + adtb0v( iscat )
          adtb0v( iscat ) = ZERO
!---------------------------------------
! Step down through upward integration
!---------------------------------------
          DO il = highest_layer, lowest_layer, -1
			ndoub = ndoubstep(il)
			R = R_store(:,:,il,ndoub)
			T = T_store(:,:,il,ndoub)
            adsource = adsource + adtb_uh( :, il + 1, iscat )
            adtau( :, il ) = adtau( :, il ) + adtb_uh( :, il + 1, iscat ) * tb_uh( :, il, iscat )
            adtb_uh( :, il, iscat ) = adtb_uh( :, il, iscat ) + adtb_uh( :, il + 1, iscat ) * &
                                      tau ( :, il )
            adtb_uh( :, il + 1, iscat ) = ZERO
            adsource = adsource + adtb_uv( :, il + 1, iscat )
            adtau( :, il ) = adtau( :, il ) + adtb_uv( :, il + 1, iscat ) * tb_uv( :, il, iscat )
            adtb_uv( :, il, iscat ) = adtb_uv( :, il, iscat ) + adtb_uv( :, il + 1, iscat ) * &
                                      tau ( :, il )
            adtb_uv( :, il + 1, iscat ) = ZERO
            IF ( iscat == 1 ) THEN              
              adsp( :, il ) = adsource
              adsource = ZERO
            ELSE
              IF ( sopd( il ) > scat_thresh ) THEN
                DO j = 1, npts
                  DO k = 1, npts
                    adR( j, k, il ) = adR( j, k, il ) + adsource( j ) * tb_d( k, il + 1, iscat - 1)     
                    adtb_d( k, il + 1, iscat - 1 ) = adtb_d( k, il + 1, iscat - 1 ) + &
                                                     adsource( j ) * R(j, k)
                    adT( j, k, il ) = adT( j, k, il ) + adsource( j ) * HALF * ( tb_uv( k, il, iscat - 1 ) + &
                                      tb_uh( k, il, iscat - 1 ) )
                    adtb_uv( k, il, iscat - 1 ) = adtb_uv( k, il, iscat - 1 ) + adsource( j ) * HALF * T(j, k)
                    adtb_uh( k, il, iscat - 1 ) = adtb_uh( k, il, iscat - 1 ) + adsource( j ) * HALF * T(j, k)
                  END DO
				  adsource(j) = ZERO
                END DO
                
              ELSE
                adsource = ZERO  
              END IF
            END IF
          END DO

		if (lowest_layer == 1) then ! only do surface if necessary
          IF ( iscat == 1 ) THEN
            adEh = adEh + adtb_uh( :, 1, iscat ) * tsfc
			adtsfc = adtsfc + sum(adtb_uh(:, 1, iscat ) * Eh)
            
            adEv = adEv + adtb_uv( :, 1, iscat ) * tsfc
			adtsfc = adtsfc + sum(adtb_uv(:, 1, iscat ) * Ev)         
          END IF

          adtb_d( :, 1, iscat ) = adtb_d( :, 1, iscat ) + adtb_uh( :, 1, iscat ) * Rh          
          adRh = adRh + adtb_uh( :, 1, iscat ) * tb_d( :, 1, iscat )          !note: found mistake here
          adtb_uh( :, 1, iscat ) = ZERO
          adtb_d( :, 1, iscat ) = adtb_d( :, 1, iscat ) + adtb_uv( :, 1, iscat ) * Rv          
          adRv = adRv + adtb_uv( :, 1, iscat ) * tb_d( :, 1, iscat )          !note: found mistake here
          adtb_uv( :, 1, iscat ) = ZERO
		endif
!---------------------------------------
! Step up through downward integration
!---------------------------------------
          DO il = lowest_layer, highest_layer
			ndoub = ndoubstep(il)
			R = R_store(:,:,il,ndoub)
			T = T_store(:,:,il,ndoub)

            adsource = adsource + adtb_d( :, il, iscat )
            adtau( :, il ) = adtau( :, il ) + adtb_d( :, il, iscat ) * tb_d( :, il + 1, iscat )
            adtb_d( :, il + 1, iscat ) = adtb_d( :, il + 1, iscat ) + adtb_d( :, il, iscat ) * &
                                         tau ( :, il )
            adtb_d( :, il, iscat ) = ZERO
            IF ( iscat == 1 ) THEN
              adsm( :, il ) = adsource
              adsource = ZERO              
            ELSE
              IF ( sopd( il ) > scat_thresh ) THEN
                DO j = 1, npts
                  DO k = 1, npts
                    adR( j, k, il ) = adR( j, k, il ) + adsource( j ) * HALF * ( tb_uv( k, il, iscat - 1 ) + &
                                      tb_uh( k, il, iscat - 1 ) )     
                    adtb_d( k, il + 1, iscat - 1 ) = adtb_d( k, il + 1, iscat - 1 ) + adsource( j ) * T(j, k)
                    adT( j, k, il ) = adT( j, k, il ) + adsource( j ) * tb_d( k, il + 1, iscat - 1 )
                    adtb_uv( k, il, iscat - 1 ) = adtb_uv( k, il, iscat - 1 ) + adsource( j ) * HALF * R(j, k)
                    adtb_uh( k, il, iscat - 1 ) = adtb_uh( k, il, iscat - 1 ) + adsource( j ) * HALF * R(j, k)
                  END DO
				  adsource(j) = ZERO
                END DO
                
              ELSE
                adsource = ZERO  
              END IF
            END IF
          END DO

		  if (iscat > 1) adtb_d( :, highest_layer + 1, iscat) = ZERO	

        END DO  ! SOI loop


		if (verb) print *, 'Finished SOI Loop.'
		adtb_d(:,highest_layer+1,1) = ZERO


!-----------------------------------------------
! Doubling and initialization of phase matrices
!----------------------------------------------- 
		DO i = lowest_layer, highest_layer
			if (verb) print *, 'Layer R, T stuff, ', i
			IF (sopd(i) > scat_thresh) THEN
				ndoub = ndoubstep(i)	
				adpf = ZERO	! these change for each layer so must be continually re-initialized
				adpb = ZERO 
	   		    CALL hg_phase_function(g(i), wgts, plegr, pf, pb, norm)
				
				if (verb) print *, 'Calling truncated_doubling_AD, i = ', i
				call truncated_doubling_AD(i, ndoub, d(i), w(i), pf, pb, &
										pts, wgts, temp(i), temp(i+1), &
										add(i), adw(i), adpf, adpb, adtemp(i), adtemp(i+1), &
										adR(:,:,i), adT(:,:,i), adSp(:,i), adSm(:,i) )

				CALL hg_phase_function_AD(g(i), wgts, plegr, pf, pb, norm, adpf, adpb, adg(i))
			ELSE
				do k = 1, npts
					slopd = d(i)/pts(k)
					slopd_AD = ZERO
					no_AD = ZERO
					call thermal_source_AD(slopd, ZERO, tau(k,i), temp(i+1), &
										temp(i), slopd_AD, no_AD, adtau(k,i), adtemp(i+1), &
										adtemp(i), adSp(k,i), adSm(k,i) )
					add(i) = add(i) + slopd_AD / pts(k)
				enddo
			ENDIF
		ENDDO

        DO i = 1, nlayers
            add( i ) = add( i ) - sum(adtau(:, i ) * tau(:, i ) / pts )
        END DO

        IF ( PRESENT(Delta_Scaling) ) THEN
           CALL deltascaling_AD(opd, ssa, asy, Delta_Scaling, &
                        adopd, adssa, adasy, adDelta_Scaling, &
                        add, adw, adg)
        ELSE
          adasy = adasy + adg
          adssa = adssa + adw
		  adopd = adopd + add
		  adg = ZERO
		  add = ZERO
          adw = ZERO
        END IF

		if (verb) print *, 'Emissivity Stuff...'
!------------------------------------------
! Adjoint of Emissivity Initializations
!------------------------------------------
		if (PRESENT(adr_v) .AND. PRESENT(adr_h)) then
			CALL soi_set_emissivities_AD(ade_v, ade_h, adev, adeh, adrv, adrh, adr_v, adr_h)
		else
			CALL soi_set_emissivities_AD(ade_v, ade_h, adev, adeh, adrv, adrh)
		endif	

        END SUBROUTINE soi_2nstrm_AD

    END SUBROUTINE soi_rt_AD


	SUBROUTINE gauleg(x1,x2,x,w,n)
    	  INTEGER n
    	  REAL(fpk) x1,x2,x(n),w(n)
    	  DOUBLE PRECISION EPS
    	  PARAMETER (EPS=3.d-14)
    	  INTEGER i,j,m
    	  DOUBLE PRECISION p1,p2,p3,pp,xl,xm,z,z1
    	  m=(n+1)/2
    	  xm=0.5d0*(x2+x1)
    	  xl=0.5d0*(x2-x1)
    	  do 12 i=1,m
        	z=cos(3.141592654d0*(i-.25d0)/(n+.5d0))
	1       continue
        	  p1=1.d0
        	  p2=0.d0
        	  do 11 j=1,n
            	p3=p2
            	p2=p1
            	p1=((2.d0*j-1.d0)*z*p2-(j-1.d0)*p3)/j
	11        continue
        	  pp=n*(z*p1-p2)/(z*z-1.d0)
        	  z1=z
        	  z=z1-p1/pp
        	if(abs(z-z1).gt.EPS)goto 1
        	x(i)=xm-xl*z
        	x(n+1-i)=xm+xl*z
        	w(i)=2.d0*xl/((1.d0-z*z)*pp*pp)
        	w(n+1-i)=w(i)
	12    continue
    	  return
	END SUBROUTINE gauleg



END MODULE soi_rt_model
