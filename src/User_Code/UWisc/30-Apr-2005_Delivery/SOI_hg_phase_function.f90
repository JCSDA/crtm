
MODULE SOI_hg_phase_function

	USE Type_Kinds, only : fpk => fp_kind

IMPLICIT NONE

	REAL(fpk), PRIVATE, PARAMETER	:: ONE = 1.0_fpk
	REAL(fpk), PRIVATE, PARAMETER	:: TWO = 2.0_fpk
	REAL(fpk), PRIVATE, PARAMETER	:: ZERO = 0.0_fpk


PUBLIC  :: hg_phase_function, hg_phase_function_TL, &
		   hg_phase_function_AD, calc_plegr, pleg

CONTAINS


	SUBROUTINE hg_phase_function(	asy, &   ! assymetry parameter
									wgts, & ! weights
									plegr, & ! legendre polynomials
									pf, & ! forward-scattering phase function
									pb, & ! backward-scattering phase function
									norm ) ! normalization for each row of phase matrix
										
									
		REAL(fpk), intent(in)					::	asy
		REAL(fpk), intent(in), dimension(:)		::	wgts
		REAL(fpk), intent(in), dimension(:,:)	::	plegr
		REAL(fpk), intent(out), dimension(:,:)	::	pf, pb
		REAL(fpk), intent(out), dimension(:), optional 	::	norm
		
		
	!	REAL(fpk), dimension(size(wgts)) 	::	norm_
		REAL(fpk) 	::	norm_
		integer :: j, k, l, isign, npts, nleg
		REAL(fpk)	:: pp



 		pf = ZERO  ! initialize matrices
		pb = ZERO
		npts = size(wgts)
		nleg = size(plegr, 1)
		do j = 1, npts    ! up-going input angles
			norm_ = ZERO
			do k = 1, npts 	! out-going angles
			  isign = 1
			  do l = 1, nleg ! sum up necessary # of terms
    			pp = (TWO*l-ONE)*asy**(l-ONE) * plegr(l,j) * plegr(l,k)
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

	SUBROUTINE hg_phase_function_TL(asy, &   ! assymetry parameter
									wgts, & ! weights
									plegr, & ! legendre polynomials
									asy_TL, & ! variation in assymetry parameter
									pf, pb, & ! phase matrices
									pf_TL, pb_TL )  ! variation in phase matrices
									
										
									
		REAL(fpk), intent(in)					::	asy
		REAL(fpk), intent(in), dimension(:)		::	wgts
		REAL(fpk), intent(in), dimension(:,:)	::	plegr
		REAL(fpk), intent(in)					::	asy_TL
		REAL(fpk), intent(out), dimension(:,:)	::	pf, pb
		REAL(fpk), intent(out), dimension(:,:)	::	pf_TL, pb_TL
		
		integer :: j, k, l, isign, npts, nleg
		REAL(fpk)	:: norm, pp, norm_TL, pp_TL

	pf = ZERO  ! initialize matrices
	pb = ZERO
	pf_TL = ZERO
	pb_TL = ZERO
	npts = size(wgts)
	nleg = size(plegr, 1)
	do j = 1, npts    ! up-going input angles
		norm_TL = ZERO
		norm = ZERO
		do k = 1, npts 	! out-going angles
		  isign = 1
		  do l = 1, nleg ! sum up necessary # of terms
    		pp = (2*l-ONE) * plegr(l,j) * plegr(l,k)
			if (l == 1) then
				pp_TL = ZERO
			else
				pp_TL = (l-ONE) * asy**(l-TWO) *asy_TL * pp
			endif
			pp = pp * asy**(l-1)
			pf_TL(j,k) = pf_TL(j,k) + pp_TL
    		pf( j, k) = pf( j, k) + pp
			pb_TL(j,k) = pb_TL(j,k) + isign * pp_TL
    		pb( j, k) = pb( j, k) + isign * pp 
    		isign = -isign
		  end do
		  norm = norm + (pf(j,k) + pb(j,k)) * wgts(k)
		  norm_TL = norm_TL + (pf_TL(j,k) + pb_TL(j,k)) * wgts(k)
		end do
		pf_TL(j,:) = pf_TL(j,:)/norm - pf(j,:)/(norm*norm) * norm_TL
		pf(j,:) = pf(j,:) /norm
		pb_TL(j,:) = pb_TL(j,:)/norm - pb(j,:)/(norm*norm) * norm_TL
		pb(j,:) = pb(j,:) /norm
	enddo


	END SUBROUTINE hg_phase_function_TL


	SUBROUTINE hg_phase_function_AD(asy, &   ! assymetry parameter
									wgts, & ! weights
									plegr, & ! legendre polynomials
									pf, pb, & ! pre-computed phase matrices
									norm, & ! pre-computed normalization vector
									pf_AD, & ! input, AD of pf
									pb_AD, & ! input, AD of pb
									asy_AD ) ! output, AD of asy

		REAL(fpk), intent(in)					::	asy
		REAL(fpk), intent(in), dimension(:)		::	wgts
		REAL(fpk), intent(in), dimension(:,:)	::	plegr
		REAL(fpk), intent(in), dimension(:,:)	::	pf, pb
		REAL(fpk), intent(in), dimension(:)		::	norm
		REAL(fpk), intent(inout), dimension(:,:)	::	pf_AD, pb_AD
		REAL(fpk), intent(inout)					::	asy_AD

		integer :: j, k, l, isign, npts, nleg
		REAL(fpk)	::  norm_AD
	
 
		npts = size(wgts)
		nleg = size(plegr, 1)

! ADJOINT CALCULATIONS
		do j = 1, npts
			norm_AD = -sum(pf(j,:) * pf_AD(j,:) + pb(j,:)*pb_AD(j,:))/norm(j) 
			pb_AD(j,:) = pb_AD(j,:) / norm(j)
			pf_AD(j,:) = pf_AD(j,:) / norm(j)
			do k = 1, npts
				pf_ad(j,k) = norm_ad * wgts(k) + pf_ad(j,k)
				pb_ad(j,k) = norm_ad * wgts(k) + pb_ad(j,k)
				isign = -1
				do l = 2, nleg
					asy_AD = asy_AD + (TWO*l-ONE)*(l-ONE)*asy**(l-TWO)*plegr(l,j)*plegr(l,k) &
							* ( pf_ad(j,k) + isign*pb_ad(j,k) )
					isign = -isign
				enddo
			enddo
		enddo	
		
	END SUBROUTINE hg_phase_function_AD


	SUBROUTINE calc_plegr(	pts, & ! vector of mu-values
							nleg, & ! # of nlegendre terms to use
							plegr ) ! output array

		! note: nleg = 2*(npts-1) for usual normalization
	
		REAL(fpk), intent(in), dimension(:)		::	pts
		INTEGER, intent(in)					::	nleg
		REAL(fpk), intent(out), dimension(:,:)	::  plegr
		REAL(fpk), DIMENSION(nleg)				::	leg
	
		integer 	::	i, j, npts
		
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
		  p( i ) = ( ( TWO * i - 3.0_fpk ) * x * p( i - 1 ) - &
					( i - TWO ) * p( i - 2 ) ) / ( i - ONE )
		ENDDO 

	END SUBROUTINE pleg

END MODULE SOI_hg_phase_function
