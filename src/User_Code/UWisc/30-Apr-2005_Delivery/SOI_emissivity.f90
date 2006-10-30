MODULE SOI_Emissivity

	USE Type_Kinds, ONLY	:	fpk => fp_kind

	REAL(fpk), PRIVATE, PARAMETER	::	ONE = 1.0_fpk
	
	!	EMISSIVITY/REFLECTIVITY ASSIGNMENTS IN THE SOI MODEL
	!
	!	INPUT VARIBLES
	!		e_v(1)		:	observation angle emissivity (v)
	!		e_v(2)		:	emissivity at soi_angles(1)
	!		e_v(3:4)	:	emissivity at soi_angles(2:3)
	!		e_v(5:n+3)	: 	emissivity for extra streams (only when nstreams is set)
	!	Same convention for e_h, and r_v, r_h (if they are set).
	!	If r_v (r_h) not set, they are set equal to ONE - e_v (ONE - e_h).
		
	!	OUTPUT VARIABLES
	!	"n" is the total number of streams being kept track of for
	!	a given hemisphere (upwelling or downwelling).  For the no-scattering
	!	case, n=1.  For the "2-stream" case, n = 2 (one alive stream and
	!	one 0-weight stream), and for the "4-stream" case, n=3 (two alive
	!	streams and one 0-weight stream).  

	!	For any of the variables ev, eh, rv, and rh, the indexing works
	!	like this (take ev as an example):
	!		ev(1)		:	First quadrature node
	!		ev(2)		:	Second quadrature node
	!		...
	!		ev(n)		:	Observation angle


PUBLIC	::	soi_set_emissivities, soi_set_emissivities_TL, soi_set_emissivities_AD

CONTAINS

	SUBROUTINE soi_set_emissivities(e_v, e_h, ev, eh, rv, rh, r_v, r_h)

	! This subroutine picks out the required emissivity and reflectivity
	! components from e_v, e_h, and if they are present, r_v and r_h.
	! They are returned as ev, eh, rv, and rh.
	
		implicit none

	! PASSED VARIABLES
		REAL(fpk), dimension(:), intent(in) 			::	e_v, e_h
		REAL(fpk), dimension(:), intent(in), OPTIONAL	::	r_v, r_h
		REAL(fpk), dimension(:), intent(out)			::	ev, eh, rv, rh
	
	! LOCAL VARIABLES
		integer 										::	n
			

		n = size(ev)
		! set user angle
		ev(n)	= e_v(1)
		eh(n) 	= e_h(1)
		! set main emissivities
		if (n == 2) then 
			ev(1) = e_v(2)
			eh(1) = e_h(2)
		endif

		if (n == 3) then
			ev(1:2) = e_v(3:4)
			eh(1:2) = e_h(3:4)
		endif

		if (n > 3) then
			ev(1:(n-1)) = e_v(5:(3+n))
			eh(1:(n-1)) = e_h(5:(3+n))
		endif					

		! v reflectivities
		if (PRESENT(r_v)) then
			rv(n) = r_v(1)
			if (n == 2) rv(1) = r_v(2)
			if (n == 3) rv(1:2) = r_v(3:4)
			if (n > 3)  rv(1:(n-1)) = r_v(5:(3+n))
		else
			rv = ONE - ev
		endif

		! h reflectivities
		if (PRESENT(r_h)) then
			rh(n) = r_h(1)
			if (n == 2) rh(1) = r_h(2)
			if (n == 3) rh(1:2) = r_h(3:4)
			if (n > 3)  rh(1:(n-1)) = r_h(5:(3+n))
		else
			rh = ONE - eh
		endif

	END SUBROUTINE soi_set_emissivities

	SUBROUTINE soi_set_emissivities_TL(e_v_TL, e_h_TL, ev_TL, eh_TL, rv_TL, rh_TL, r_v_TL, r_h_TL)

		implicit none

	! PASSED VARIABLES
		REAL(fpk), dimension(:), intent(in) 			::	e_v_TL, e_h_TL
		REAL(fpk), dimension(:), intent(in), OPTIONAL	::	r_v_TL, r_h_TL
		REAL(fpk), dimension(:), intent(out)			::	ev_TL, eh_TL, rv_TL, rh_TL
	
	! LOCAL VARIABLES
		integer 										::	n
			
		n = size(ev_TL)
		! set user angle
		ev_TL(n)	= e_v_TL(1)
		eh_TL(n) 	= e_h_TL(1)
		! set main emissivities
		if (n == 2) then 
			ev_TL(1) = e_v_TL(2)
			eh_TL(1) = e_h_TL(2)
		endif

		if (n == 3) then
			ev_TL(1:2) = e_v_TL(3:4)
			eh_TL(1:2) = e_h_TL(3:4)
		endif

		if (n > 3) then
			ev_TL(1:(n-1)) = e_v_TL(5:(3+n))
			eh_TL(1:(n-1)) = e_h_TL(5:(3+n))
		endif					

		! v reflectivities
		if (PRESENT(r_v_TL)) then
			rv_TL(n) = r_v_TL(1)
			if (n == 2) rv_TL(1) = r_v_TL(2)
			if (n == 3) rv_TL(1:2) = r_v_TL(3:4)
			if (n > 3)  rv_TL(1:(n-1)) = r_v_TL(5:(3+n))
		else
			rv_TL = - ev_TL
		endif

		! h reflectivities
		if (PRESENT(r_h_TL)) then
			rh_TL(n) = r_h_TL(1)
			if (n == 2) rh_TL(1) = r_h_TL(2)
			if (n == 3) rh_TL(1:2) = r_h_TL(3:4)
			if (n > 3)  rh_TL(1:(n-1)) = r_h_TL(5:(3+n))
		else
			rh_TL = - eh_TL
		endif

	END SUBROUTINE soi_set_emissivities_TL

	SUBROUTINE soi_set_emissivities_AD(e_v_AD, e_h_AD, ev_AD, eh_AD, rv_AD, rh_AD, r_v_AD, r_h_AD)

		implicit none

	! PASSED VARIABLES
		REAL(fpk), dimension(:), intent(inout)				::	ev_AD, eh_AD, rv_AD, rh_AD
		REAL(fpk), dimension(:), intent(inout) 				::	e_v_AD, e_h_AD
		REAL(fpk), dimension(:), intent(inout), OPTIONAL	::	r_v_AD, r_h_AD

	
	! LOCAL VARIABLES
		integer 											::	n

		n = size(ev_AD)

		! v reflectivities
		if (PRESENT(r_v_AD)) then
			r_v_AD(1) = rv_AD(n) + r_v_AD(1)
			if (n == 2) r_v_AD(2) = rv_AD(1) + r_v_AD(2)
			if (n == 3) r_v_AD(3:4) = rv_AD(1:2) + r_v_AD(3:4)
			if (n > 3) r_v_AD(5:(3+n)) = rv_AD(1:(n-1)) + r_v_AD(5:(3+n))
		else
			ev_AD = - rv_AD + ev_AD
		endif

		! h reflectivities
		if (PRESENT(r_h_AD)) then
			r_h_AD(1) = rh_AD(n) + r_h_AD(1)
			if (n == 2) r_h_AD(2) = rh_AD(1) + r_h_AD(2)
			if (n == 3) r_h_AD(3:4) = rh_AD(1:2) + r_h_AD(3:4)
			if (n > 3) r_h_AD(5:(3+n)) = rh_AD(1:(n-1)) + r_h_AD(5:(3+n))
		else
			eh_AD = - rh_AD + eh_AD
		endif

		! set user angle
		e_v_AD(1) = ev_AD(n) + e_v_AD(1)
		e_h_AD(1) = eh_AD(n) + e_h_AD(1)

		! set main emissivities
		if (n == 2) then 
			e_v_AD(2) = ev_AD(1) + e_v_AD(2)
			e_h_AD(2) = eh_AD(1) + e_h_AD(2)
		endif

		if (n == 3) then
			e_v_AD(3:4) = ev_AD(1:2) + e_v_AD(3:4)
			e_h_AD(3:4) = eh_AD(1:2) + e_h_AD(3:4)
		endif

		if (n > 3) then
			e_v_AD(5:(3+n)) = ev_AD(1:(n-1)) + e_v_AD(5:(3+n))
			e_h_AD(5:(3+n)) = eh_AD(1:(n-1)) + e_h_AD(5:(3+n))
		endif					


	END SUBROUTINE soi_set_emissivities_AD


END	MODULE SOI_Emissivity
