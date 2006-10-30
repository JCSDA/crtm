MODULE SOI_doubling

	USE Type_Kinds, only : fpk => fp_kind
	
IMPLICIT NONE

    INTEGER, PUBLIC, PARAMETER  ::  ndoub_max = 13
    INTEGER, PUBLIC, PARAMETER  ::  nstrm_max = 3
	INTEGER, PUBLIC, PARAMETER	::	nlay_max = 100
    REAL(fpk), PUBLIC, SAVE     ::  R_store( nstrm_max, nstrm_max, nlay_max, 0:ndoub_max )
    REAL(fpk), PUBLIC, SAVE     ::  T_store( nstrm_max, nstrm_max, nlay_max, 0:ndoub_max )
    REAL(fpk), PUBLIC, SAVE     ::  Y_store( nstrm_max, nlay_max, 0:ndoub_max )
    REAL(fpk), PUBLIC, SAVE     ::  Z_store( nstrm_max, nlay_max, 0:ndoub_max )
    REAL(fpk), PUBLIC, SAVE     ::  RT_store( nstrm_max, nlay_max )


	REAL, PRIVATE, PARAMETER	:: ONE = 1.0_fpk
	REAL, PRIVATE, PARAMETER	:: TWO = 2.0_fpk
	REAL, PRIVATE, PARAMETER	:: HALF = 0.5_fpk
	REAL, PRIVATE, PARAMETER	:: FOUR = 4.0_fpk
	REAL, PRIVATE, PARAMETER	:: ZERO = 0.0_fpk
	REAL, PRIVATE, PARAMETER	:: SSA_CUTOFF = 0.65_fpk

PUBLIC ::	truncated_doubling, &
			truncated_doubling_TL, &
			truncated_doubling_AD, &
			Gamma_Matrix, &
           Gamma_Matrix_TL, &
           Gamma_Matrix_AD

PRIVATE :: outer_prod

CONTAINS


    SUBROUTINE TRUNCATED_DOUBLING(  ilay,   &   ! Input, Layer number (needed for adjoint)
									k,      &   ! Input, Number of Doublings to Perform
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
                                    Sm,     &    ! Downwelling source funtion (including scattering)
									method )

    ! Arguments
		integer, intent(in) 	:: ilay
        integer, intent(inout) :: k
        REAL(fpk), dimension(:,:),intent(in):: Pp,Pm
        REAL(fpk), dimension(:),intent(in):: mu,w
        REAL(fpk), intent(in):: tau,wo,Bo,Bn
        REAL(fpk), dimension(:,:),intent(out) :: R,T
        REAL(fpk), dimension(:),intent(out):: Sp,Sm
		integer, intent(in), optional :: method

    ! Local Variables
        REAL(fpk), dimension(size(mu),size(mu))  ::  E,A,B, Gamma
        REAL(fpk)                                ::  Bd, delta_tau,g, m
        REAL(fpk), dimension(size(mu))           ::  Y,Z, RT, trans
        integer                             :: i,j,l,npts
		integer 							:: imethod

		if (.NOT. PRESENT(method)) then 
			if ( (wo > SSA_CUTOFF) ) then 
				imethod = 1
			else
				imethod = 2
			endif
		else
			imethod = method
		endif

		if (k > ndoub_max) k = ndoub_max ! do not exceed maximum number of doublings
        npts = size(mu)
        delta_tau = tau/(2**k)

    !--- slope of blackbody emission through layer
        Bd = (Bn - Bo) / tau

    !--  for the first layer, initialize with a thin layer approximation

	! This section gets the difference in up and downwelling source correct.
        do i = 1,npts
			trans(i) = exp(-delta_tau/mu(i))
			
			if (delta_tau/mu(i) < 0.2_fpk) then
			Z(i) = delta_tau * delta_tau / mu(i) / 12.0_fpk * (ONE-wo)*(ONE-trans(i))
			else 
			Z(i) = (ONE-wo)*delta_tau*(trans(i) + (ONE-trans(i))*(HALF - mu(i)/delta_tau))
			endif
        	
		    do j = 1, npts
              E(i,j) = ZERO
              if (i == j) E(i,j) = ONE
            enddo
       enddo
     
		select case(imethod)
		case(1) ! O'DELL (EIGI) initialization
        do i = 1, npts
            do j = 1, npts
                R(i,j) = wo * Pm(i,j) * w(j) * (ONE-trans(i))
				T(i,j) = E(i,j)*trans(i) + wo*Pp(i,j)*w(j)*(ONE-trans(i))
            enddo
            Y(i) = (ONE-wo) * (ONE - trans(i)) 
        enddo
		case(2)  ! Single Scatter Initialization with Tdirect determined from energy conservation
		do i = 1, npts
			RT(i) = ZERO
            do j = 1, npts
				m = mu(j) / ( mu(i) + mu(j) )
			    R(i,j) = wo * Pm(i,j) * w(j) * m *(ONE-exp(-delta_tau/(m*mu(i))))
        	 	if (i == j) then
        	    	T(i,j) = wo*Pp(i,j)*w(j) * delta_tau/mu(i) * trans(i)
        	 	else 
        	    	T(i,j) = wo*Pp(i,j)*w(j) * mu(j)/(mu(j)-mu(i))*(exp(-delta_tau/mu(j)) - trans(i))
        	 	end if
			  	RT(i) = RT(i) + R(i,j) + T(i,j)
			enddo
			Y(i) = (ONE-wo*wo)*(ONE - trans(i)) - (ONE-wo)*RT(i)
			T(i,i) = T(i,i) + ONE - Y(i) - RT(i)
			RT_store(i,ilay) = RT(i) ! store for adjoint calculation
		enddo

		end select

        g = delta_tau / 4.0_fpk
	
!	store initial matrices (needed for adjoint)
	R_store(:, :, ilay, 0) = R
	T_store(:, :, ilay, 0) = T
	Y_store(:, ilay, 0) = Y
	Z_store(:, ilay, 0) = Z

    !-- if there is doubling, perform doubling
        do l = 1, k 
            g = g * TWO
			CALL Gamma_Matrix(R, g, Gamma)

			A = matmul(T, Gamma)
            B = matmul(A , R)

            R = matmul(B , T) + R
            T = matmul(A , T)
            Z = Z + g*Y + matmul((A - B) , (Z - g*Y))
            Y = matmul((A + B + E) , Y)

			! store matrices for each iteration (needed for adjoint)
	    	R_store( :, :, ilay, l ) = R
			if (l < k) T_store(:, :, ilay, l ) = T
			Y_store(:, ilay, l) = Y
			Z_store(:, ilay, l) = Z
        end do

    !  finish thermal source computation
        Sp = HALF*(Bo + Bn)*Y + Bd*Z
        Sm = HALF*(Bo + Bn)*Y - Bd*Z

    !---------- subtract off direct transmission from total Transmission Matrix
        do i = 1,npts
            T(i,i) = T(i,i) - exp(-tau/mu(i))
        end do	
		T_store(:, :, ilay, k ) = T

    END SUBROUTINE TRUNCATED_DOUBLING


    SUBROUTINE TRUNCATED_DOUBLING_TL(  k,      &   ! Input, Number of Doublings to Perform
                                    tau,    &   ! Input, Extinction optical depth of the layer
                                    wo,     &   ! Input, Single Scatter Albedo of the layer
                                    Pp,     &   ! Input, Forward-Scattering Phase Function
                                    Pm,     &   ! Input, Backward-Scattering Phase Function
                                    mu,     &   ! Input, Array of quadrature points
                                    w,      &   ! Input, Array of quadrature weights
                                    Bo,     &   ! Input, Planck Radiance of lower boundary
                                    Bn,     &   ! Input, Planck Radiance of upper boundary
                                    tau_TL,    &   ! Input, Extinction optical depth of the layer
                                    wo_TL,     &   ! Input, Single Scatter Albedo of the layer
                                    Pp_TL,     &   ! Input, Forward-Scattering Phase Function
                                    Pm_TL,     &   ! Input, Backward-Scattering Phase Function 
                                    Bo_TL,     &   ! Input, Planck Radiance of lower boundary
                                    Bn_TL,     &   ! Input, Planck Radiance of upper boundary
                                    R,      &   ! Output, Reflection matrix
                                    T,      &   ! Output, Scattering Transmission matrix 
                                    Sp,     &   ! Upwelling source function (including scattering)
                                    Sm,     &    ! Downwelling source funtion (including scattering)
                                    R_TL,      &   ! Output, Reflection matrix
                                    T_TL,      &   ! Output, Scattering Transmission matrix 
                                    Sp_TL,     &   ! Upwelling source function (including scattering)
                                    Sm_TL,     &    ! Downwelling source funtion (including scattering)
									method )

    ! Arguments
        integer, intent(inout) :: k
        REAL(fpk), dimension(:,:),intent(in):: Pp,Pm
        REAL(fpk), dimension(:),intent(in):: mu,w
        REAL(fpk), intent(in):: tau,wo,Bo,Bn
		REAL(fpk), intent(in):: tau_TL, wo_TL, Bo_TL, Bn_TL
        REAL(fpk), dimension(:,:),intent(in):: Pp_TL,Pm_TL
        REAL(fpk), dimension(:,:),intent(out):: R, T, R_TL, T_TL
        REAL(fpk), dimension(:),intent(out):: Sp, Sm, Sp_TL,Sm_TL
		integer, intent(in), optional :: method

    ! Local Variables
        REAL(fpk), dimension(size(mu),size(mu))  :: E,A,B, Gamma, Gamma_TL, A_TL, B_TL
        REAL(fpk)                                ::  Bd, delta_tau,g, m, etemp, etemp2
		REAL(fpk)								 ::  g_TL, delta_tau_TL, Bd_TL
        REAL(fpk), dimension(size(mu))           :: Y,Z, RT, trans
		REAL(fpk), dimension(size(mu))			 :: Y_TL, Z_TL, RT_TL, trans_TL
        integer                             :: i,j,l,npts
		integer 							:: imethod

		if (.NOT. PRESENT(method)) then 
			if ( (wo > SSA_CUTOFF) ) then 
				imethod = 1
			else
				imethod = 2
			endif
		else
			imethod = method
		endif

		if (k > ndoub_max) k = ndoub_max ! do not exceed maximum number of doublings
        npts = size(mu)

        delta_tau = tau/(2**k)
		delta_tau_TL = tau_TL / (2**k)

    !--- slope of blackbody emission through layer
        Bd = (Bn - Bo) / tau
		Bd_TL = ( tau*(Bn_TL - Bo_TL) - (Bn-Bo)*tau_TL ) / (tau*tau)

    !--  for the first layer, initialize with a thin layer approximation


	! This section gets the difference in up and downwelling source correct.
        do i = 1,npts
			trans(i) = exp(-delta_tau/mu(i))
			trans_TL(i) = -delta_tau_TL * trans(i) / mu(i)

			if (delta_tau/mu(i) < 0.2_fpk) then
			Z_TL(i) = ONE/(mu(i)*12.0_fpk)*( 2*delta_tau*delta_tau_TL* (ONE-wo)*(ONE-trans(i)) &
				  - delta_tau*delta_tau* ( wo_TL * (ONE-trans(i)) + (ONE-wo)*trans_TL(i)) )
			Z(i) = delta_tau * delta_tau / mu(i) / 12.0_fpk * (ONE-wo)*(ONE-trans(i))
			else 
			Z_TL(i) = (-wo_TL*delta_tau + (ONE-wo)*delta_tau_TL) * &
    			  (trans(i) + (ONE-trans(i))*(HALF - mu(i)/delta_tau)) + &
    			  (ONE-wo)*delta_tau*(trans_TL(i)*(HALF + mu(i)/delta_tau) + &
    			  (ONE-trans(i))*mu(i)/(delta_tau**2)*delta_tau_TL )
			Z(i) = (ONE-wo)*delta_tau*(trans(i) + (ONE-trans(i))*(HALF - mu(i)/delta_tau))
			endif

		    do j = 1, npts
              E(i,j) = ZERO
              if (i == j) E(i,j) = ONE
            enddo
        enddo


		select case(imethod)
		case(1) ! O'DELL (EIGI) initialization
    	do i = 1,npts
    	  do j = 1, npts
			 R_TL(i,j) = wo_TL * Pm(i,j) * w(j) * (ONE-trans(i)) &
	            		 + Pm_TL(i,j) * wo * w(j) * (ONE-trans(i)) &
	            		 - trans_TL(i) * wo * Pm(i,j) * w(j)

			 R(i,j) = wo * Pm(i,j) * w(j) * (ONE-trans(i))  

	    	 T_TL(i,j) = E(i,j)*trans_TL(i) + &
                         wo_TL*Pp(i,j)*w(j)*(ONE-trans(i)) &
		               + wo*Pp_TL(i,j)*w(j)*(ONE-trans(i)) &
		               - trans_TL(i)*wo*Pp(i,j)*w(j)

        	 T(i,j) = E(i,j)*trans(i) + wo*Pp(i,j)*w(j)*(ONE-trans(i))     
    	  enddo
    	  Y_TL(i) = -wo_TL * (ONE - trans(i)) - trans_TL(i) * (ONE - wo)
    	  Y(i) = (ONE - wo) * (ONE - trans(i))
        enddo


		case(2)  ! Single Scatter Initialization with Tdirect determined from energy conservation
		do i = 1, npts
			RT(i) = ZERO
            RT_TL(i) = ZERO
            do j = 1, npts
				m = mu(j) / ( mu(i) + mu(j) )
				etemp = exp(-delta_tau/(m*mu(i)))
				etemp2 = w(j) * m * (ONE - etemp)
				R(i,j) = wo * Pm(i,j) * etemp2
			    R_TL(i,j) = wo_TL * Pm(i,j) * etemp2 + wo * Pm_TL(i,j) * etemp2 + wo*Pm(i,j)*w(j)*m* etemp * delta_tau_TL/(m*mu(i))

        	 	if (i == j) then
        	    	T(i,j) = wo*Pp(i,j)*w(j) * delta_tau/mu(i) * trans(i)
					T_TL(i,j) = (wo_TL*Pp(i,j)+wo*Pp_TL(i,j))*w(j) * delta_tau/mu(i) * trans(i) + &
							    wo*Pp(i,j)*w(j)/mu(i) * ( delta_tau_TL * trans(i) + delta_tau * trans_TL(i))
        	 	else 
					etemp = exp(-delta_tau/mu(j))
					T(i,j) = wo*Pp(i,j)*w(j) * mu(j)/(mu(j)-mu(i))*(etemp - trans(i))
        	    	T_TL(i,j) = w(j) * mu(j)/(mu(j)-mu(i)) * &
							( (wo_TL*Pp(i,j) + wo*Pp_TL(i,j))*(etemp - trans(i)) - &
							wo*Pp(i,j)*(delta_tau_TL/mu(j)*etemp + trans_TL(i)) )
        	 	end if
			  	RT(i) = RT(i) + R(i,j) + T(i,j)
				RT_TL(i) = RT_TL(i) + R_TL(i,j) + T_TL(i,j)
			enddo
			Y_TL(i) = -2*wo*wo_TL*(ONE - trans(i)) - (ONE-wo*wo)*trans_TL(i) + wo_TL*RT(i) - (ONE-wo)*RT_TL(i)
			Y(i) = (ONE-wo*wo)*(ONE - trans(i)) - (ONE-wo)*RT(i)
			T_TL(i,i) = T_TL(i,i) - Y_TL(i) - RT_TL(i)
			T(i,i) = T(i,i) + ONE - Y(i) - RT(i)
		enddo

		end select


        g = delta_tau / 4.0_fpk
		g_TL = delta_tau_TL / 4.0_fpk


    !-- if there is doubling, perform doubling
        do l = 1, k 
            g = g * TWO
			g_TL = g_TL * TWO
			Call Gamma_Matrix(R, g, Gamma)
			Call Gamma_Matrix_TL(R, Gamma, g, R_TL, Gamma_TL)

			A_TL = matmul(T_TL,Gamma) + matmul(T,Gamma_TL)
			A = matmul(T, Gamma)

			B_TL =matmul(A_TL, R) + matmul(A, R_TL)
            B = matmul(A, R)

			R_TL = matmul(B_TL, T) + matmul(B, T_TL) + R_TL
            R = matmul(B, T) + R

			T_TL = matmul(A_TL, T) + matmul(A, T_TL)
            T = matmul(A, T)

			Z_TL = Z_TL + g_TL*Y + g*Y_TL + matmul((A_TL - B_TL) , (Z - g*Y)) + matmul((A-B),(Z_TL-g_TL*Y-g*Y_TL))
            Z = Z + g*Y + matmul((A - B), (Z - g*Y))

			Y_TL = matmul((A_TL + B_TL) , Y) + matmul((A+B+E),Y_TL)
            Y = matmul((A + B + E) , Y)
        end do

    !  finish thermal source computation
		Sp_TL = HALF*((Bo_TL+Bn_TL)*Y + (Bo+Bn)*Y_TL)        
		Sm_TL = Sp_TL - Bd_TL*Z - Bd*Z_TL
		Sp_TL = Sp_TL + Bd_TL*Z + Bd*Z_TL
		Sp = HALF*(Bo + Bn)*Y 
        Sm = Sp - Bd*Z
		Sp = Sp + Bd*Z
    !---------- subtract off direct transmission from total Transmission Matrix
        do i = 1,npts
			etemp = exp(-tau/mu(i))
            T(i,i) = T(i,i) - etemp
			T_TL(i,i) = T_TL(i,i) + etemp*tau_TL/mu(i) 
        end do

    END SUBROUTINE TRUNCATED_DOUBLING_TL


SUBROUTINE truncated_doubling_AD(ilay,  &   ! Input: Layer index
                                 k,     &   ! Input: Number of doubling steps
                                 tau,   &   ! Input: Optical depth of layer
                                 wo,    &   ! Input: Single scatter albedo
                                 Pp,    &   ! Input: Forward scattering phase matrix
                                 Pm,    &   ! Input: Backward scattering phase matrix
                                 mu,    &   ! Input: Quadrature points
                                 w,     &   ! Input: Quadrature weights 
                                 Bo,    &   ! Input: Emission (temperature) at top of layer
                                 Bn,    &   ! Input: Emission (temperature) at bottom of layer
                                 tau_AD, &   ! Output: Adjoint of tau
                                 wo_AD,  &   ! Output: Adjoint of ssalb
                                 Pp_AD,  &   ! Output: Adjoint of Pp
                                 Pm_AD,  &   ! Output: Adjoint of Pm
                                 Bo_AD,  &   ! Output: Adjoint of Bo
                                 Bn_AD,  &   ! Output: Adjoint of Bn
                                 R_AD,   &   ! Input: Adjoint forcing for R
                                 T_AD,  &   ! Input: Adjoint forcing for Ts
                                 Sp_AD,  &   ! Input: Adjoint forcing for Sp
                                 Sm_AD,  &     ! Input: Adjoint forcing for Sm
								 method )
												
!
!------------------------------------------------------------------------------
!   Adjoint model for the truncated doubling algorithm. Uses R/T matrix values 
!   stored from forward run.
!
!   Written by Chris O'Dell
!
!  NOTE: 	It is up to the user to pre-set all in/out adjoint values to whatever they like (ie, 0)
!------------------------------------------------------------------------------

!--------------------
! Define arguments
!--------------------
        INTEGER, INTENT( in )							:: ilay
        INTEGER, INTENT( in )                 	  		:: k
        REAL(fpk), DIMENSION( :, : ), INTENT( in )      :: Pp
        REAL(fpk), DIMENSION( :, : ), INTENT( in )		:: Pm
        REAL(fpk), DIMENSION( : ), INTENT( in )       	:: mu
        REAL(fpk), DIMENSION( : ), INTENT( in )      	:: w
        REAL(fpk), INTENT( in )                   		:: tau
        REAL(fpk), INTENT( in )                     	:: wo
        REAL(fpk), INTENT( in )						  	:: Bo
        REAL(fpk), INTENT( in )                    		:: Bn
        REAL(fpk), INTENT( inout )                      :: tau_AD
        REAL(fpk), INTENT( inout )                      :: wo_AD
        REAL(fpk), DIMENSION( :, : ), INTENT( inout )   :: Pp_AD
        REAL(fpk), DIMENSION( :, : ), INTENT( inout )   :: Pm_AD
        REAL(fpk), INTENT( inout )                      :: Bo_AD
        REAL(fpk), INTENT( inout )                      :: Bn_AD
        REAL(fpk), DIMENSION( :, : ), INTENT( inout )   :: R_AD
        REAL(fpk), DIMENSION( :, : ), INTENT( inout )   :: T_AD
        REAL(fpk), DIMENSION( : ), INTENT( in )         :: Sp_AD
        REAL(fpk), DIMENSION( : ), INTENT( in )         :: Sm_AD
		INTEGER, INTENT(in), OPTIONAL					:: method

!-------------------------
! Define local variables
!-------------------------
        REAL(fpk), DIMENSION(size(mu), size(mu))   :: At, Bt, Rt, Tt, At_Bt, E, Gamma, Gammat
        REAL(fpk)                                  :: Bd, delta_tau, g
        INTEGER  :: i, j, l, npts
        REAL(fpk), DIMENSION(size(mu), size(mu))   :: B_AD, A_AD, Td_AD, YadY, ZadZ_gY, Gamma_AD
		REAL(fpk), DIMENSION(size(mu))			   :: RTvector, RTvector_AD
        REAL(fpk), DIMENSION(size(mu))             :: Y_AD, Z_AD, Y, Z, trans, trans_AD
        REAL(fpk)                                  :: Bd_AD, g_AD
		REAL(fpk)								   :: delta_tau_AD
		REAL(fpk)								   :: tmp, etemp, m
		INTEGER                                    :: imethod


!-------------------------
! Forward recomputations
!-------------------------

		if (.NOT. PRESENT(method)) then 
			if ( (wo > SSA_CUTOFF) ) then 
				imethod = 1
			else
				imethod = 2
			endif
		else
			imethod = method
		endif

        npts = size( mu )
        delta_tau = tau / ( 2 ** k )
        Bd = ( Bn - Bo ) / tau
        E = ZERO
        DO i = 1, npts
          E( i, i ) = ONE
        END DO

	!-----------------------
	! Adjoint computations
	!-----------------------
	Td_AD = -T_AD

	do i = 1, npts
		tau_AD = - exp(-tau/mu(i)) / mu(i) * Td_AD(i,i) + tau_AD
	enddo

	tmp = sum( HALF * (Sp_AD + Sm_AD) * Y_store(:,ilay,k))
	Bo_AD = tmp + Bo_AD
	Bn_AD = tmp + Bn_AD
	Bd_AD = sum((Sp_AD - Sm_AD) * Z_store(:,ilay,k) )

	Y_AD = HALF * (Bo+Bn) * (Sp_AD + Sm_AD)
	Z_AD = Bd * (Sp_AD - Sm_AD)

	g_AD = ZERO
	g = (delta_tau / FOUR) * 2**k ! set initial value of g

	!--- PERFORM DOUBLING ADJOINT

	do l = k, 1, -1

!	extract stored matrices for the index (l-1)
		Rt = transpose(R_store(:, :, ilay, l-1))
		Tt = transpose(T_store(:, :, ilay, l-1))
		Y = Y_store(:, ilay, l-1)
		Z = Z_store(:, ilay, l-1)

!	re-calculate Gamma, A and B matrices for this index (l-1)
		Call Gamma_Matrix(transpose(Rt), g, Gamma) 
		Gammat = transpose(Gamma)
		At = matmul(Gammat, Tt)
		Bt = matmul(Rt, At)
		At_Bt = At - Bt
		YadY = outer_prod(Y_AD,Y)
		ZadZ_gY = outer_prod(Z_AD, Z - g*Y)

! Begin Adjoint Doubling Rules
		g_AD	=		sum( (Y - matmul(Y, At_Bt)) * Z_AD ) + g_AD

		B_AD	=		matmul(R_AD, Tt) + YadY - ZadZ_gY

		A_AD	=		matmul(T_AD, Tt) + YadY + ZadZ_gY

		Y_AD	=		matmul((At + Bt + E), Y_AD) + g*matmul((E - At_Bt), Z_AD)

		Z_AD	=		matmul( (E + At_Bt) , Z_AD)

		T_AD	=		matmul(Bt, R_AD) + matmul(At, T_AD)

		A_AD	=		matmul(B_AD, Rt) + A_AD

		R_AD	=		matmul(At, B_AD) + R_AD

		T_AD	=		matmul(A_AD, Gammat) + T_AD

		Gamma_AD =		matmul(Tt, A_AD)  ! in this cycle, create this adjoint variable here!
		
		CALL Gamma_Matrix_AD(Rt, Gammat, g, Gamma_AD, R_AD)

		g_AD = TWO * g_AD

		g = g / TWO ! re-calculate g for next iteration

	enddo

	delta_tau_AD = g_AD / FOUR

	! This section gets the difference in up and downwelling source correct.
        Z = ZERO
		trans_AD = ZERO
        do i = 1,npts
		  trans(i) = exp(-delta_tau/mu(i))
		  if (delta_tau/mu(i) < 0.2_fpk) then
			wo_AD = wo_AD - delta_tau * delta_tau / mu(i) / 12.0_fpk*(ONE-trans(i)) * Z_AD(i)
			trans_AD(i) = - delta_tau * delta_tau / mu(i) / 12.0_fpk * (ONE-wo) * Z_AD(i) + trans_AD(i)
            delta_tau_AD = 2 * delta_tau / mu(i) / 12.0_fpk * (ONE-wo)*(ONE-trans(i)) * Z_AD(i) + delta_tau_AD
          else 
			wo_AD = wo_AD - delta_tau*(trans(i) + (ONE-trans(i))*(HALF - mu(i)/delta_tau))*Z_AD(i)
			trans_AD(i) = (ONE-wo)*delta_tau*(ONE - (HALF - mu(i)/delta_tau))*Z_AD(i) + trans_AD(i)
			delta_tau_AD = (ONE-wo)*((trans(i) + (ONE-trans(i))*(HALF - mu(i)/delta_tau)) + &
							delta_tau*(ONE-trans(i))*mu(i)/delta_tau**2 ) * Z_AD(i) + delta_tau_AD
          endif
        enddo

	! Now perform Adjoint of layer initialization
	select case(imethod)
	case(1) ! O'DELL (EIGI) initialization

	do i = 1, npts	    
		wo_AD = -(ONE-trans(i)) * Y_AD(i) + wo_AD
		trans_AD(i) = trans_AD(i) - (ONE-wo) * Y_AD(i) 

		do j = 1, npts

			wo_AD = wo_AD + Pm(i,j) * w(j) * (ONE-trans(i)) * R_AD(i,j)
			Pm_AD(i,j) = wo * w(j) * (ONE-trans(i)) * R_AD(i,j) 
			trans_AD(i) = trans_AD(i) - wo*Pm(i,j)*w(j)*R_AD(i,j)

			if (i==j) then
				trans_AD(i) = (ONE - wo*Pp(i,j)*w(j))*T_AD(i,j) + trans_AD(i)
			else
				trans_AD(i) =  - wo*Pp(i,j)*w(j)*T_AD(i,j) + trans_AD(i)
			endif
			wo_AD = wo_AD + Pp(i,j) * w(j) * (ONE-trans(i)) * T_AD(i,j)
			Pp_AD(i,j) = wo*w(j)*(ONE-trans(i)) * T_AD(i,j)
		enddo

		delta_tau_AD = -trans(i)/mu(i) * trans_AD(i) + delta_tau_AD
	enddo


	case(2)  ! Single Scatter Initialization with Tdirect determined from energy conservation

	do i = 1, npts
		RTvector(i) = RT_store(i,ilay) ! retrieve RTvector(i) from FORWARD calc
 		Y_AD(i) = -T_AD(i,i) + Y_AD(i)
		RTvector_AD(i) = -T_AD(i,i)
		wo_AD = (RTvector(i) - TWO*wo*(ONE-trans(i)))*Y_AD(i) + wo_AD
		trans_AD(i) = trans_AD(i) - (ONE - wo*wo) * Y_AD(i)
        RTvector_AD(i) = RTvector_AD(i) - (ONE-wo)*Y_AD(i)

		
        do j = 1, npts
			m = mu(j) / ( mu(i) + mu(j) ) ! needed for each iteration

			R_AD(i,j) = RTvector_AD(i) + R_AD(i,j)
			T_AD(i,j) = RTvector_AD(i) + T_AD(i,j)

			etemp = exp(-delta_tau/(m*mu(i)))
			wo_AD = Pm(i,j) * w(j) * m *(ONE-etemp) * R_AD(i,j) + wo_AD
			Pm_AD(i,j) =  wo * w(j) * m *(ONE-etemp) * R_AD(i,j)
			delta_tau_AD = wo * Pm(i,j) * w(j) * etemp / mu(i) * R_AD(i,j) + delta_tau_AD

        	if (i == j) then
				wo_AD = Pp(i,j)*w(j) * delta_tau/mu(i) * trans(i) * T_AD(i,j) + wo_AD
				Pp_AD(i,j) = wo*w(j) * delta_tau/mu(i) * trans(i) * T_AD(i,j)
				delta_tau_AD = wo*Pp(i,j)*w(j) / mu(i) * trans(i) * T_AD(i,j) + delta_tau_AD
				trans_AD(i) = wo*Pp(i,j)*w(j) * delta_tau/mu(i) * T_AD(i,j) + trans_AD(i)
        	else 
				etemp = exp(-delta_tau/mu(j))
				m = mu(j) / (mu(j)-mu(i))
				wo_AD = Pp(i,j)*w(j) * m *(etemp - trans(i))*T_AD(i,j) + wo_AD
				Pp_AD(i,j) = wo*w(j) * m *(etemp - trans(i))*T_AD(i,j)
				delta_tau_AD = - wo*Pp(i,j)*w(j) * m /mu(j) * etemp* T_AD(i,j) + delta_tau_AD
				trans_AD(i) = -wo*Pp(i,j)*w(j)*m*T_AD(i,j) + trans_AD(i)
        	end if
		
		enddo

		delta_tau_AD = -trans(i)/mu(i) * trans_AD(i) + delta_tau_AD
	enddo
	end select

	Bn_AD = Bd_AD / tau + Bn_AD
	Bo_AD = -Bd_AD / tau + Bo_AD
	tau_AD = - Bd/tau * Bd_AD + tau_AD

	tau_AD = delta_tau_AD / (2**k) + tau_AD
 	 
END SUBROUTINE truncated_doubling_AD


	Subroutine Gamma_Matrix( R, g, Gamma )
		! This simple routine calculates Gamma = (E - R^2)^-1, 
		!	It is assumed that R has last column all zeroes.
		!   For R = 2x2 or 3x3, inverse is explicitly calculated.
		!   For R > 3x3, use truncated doubling technique.

		implicit none

		real(fpk), intent(in), dimension(:,:)		::	R
		real(fpk), intent(in)						::	g
		real(fpk), intent(out), dimension(:,:)		::	Gamma

		real(fpk), dimension(size(R,1),size(R,2))   ::  store, store2

		integer ::	n, k
		real(fpk)	:: idet
		real(fpk), dimension(size(R,1), size(R,2))	::	A, E

		n = size(R, 1)
		E = ZERO
		do k = 1, n
			E(k,k) = ONE
		enddo

		select case(n)
		case(2) ! 2-stream case
			idet = ONE/(ONE-R(1,1)*R(1,1))
			Gamma(1,1) = idet
			Gamma(1,2) = ZERO
			Gamma(2,1) = R(1,1)*R(2,1) * idet
			Gamma(2,2) = ONE

		case(3)
			A(1,1) = ONE - (R(1,1)*R(1,1) + R(1,2)*R(2,1))
			A(1,2) = -R(1,2) * ( R(1,1) + R(2,2) )
			A(2,1) = -R(2,1) * ( R(1,1) + R(2,2) )
			A(2,2) = ONE - (R(1,2) * R(2,1) + R(2,2)*R(2,2))
			A(3,1) = -( R(1,1)*R(3,1) + R(2,1)*R(3,2) )
			A(3,2) = -( R(1,2)*R(3,1) + R(2,2)*R(3,2) )
			A(1:2, 3) = ZERO
			A(3,3) = ONE

			idet = ONE/(A(1,1) * A(2,2) - A(1,2) * A(2,1))
			Gamma(1:2,3) = ZERO
			Gamma(1,1) = A(2,2) * idet
			Gamma(1,2) = - A(1,2) * idet
			Gamma(2,1) = - A(2,1) * idet
			Gamma(2,2) = A(1,1) * idet
			Gamma(3,1) = idet * ( A(2,1)*A(3,2) - A(3,1)*A(2,2) )
			Gamma(3,2) = idet * ( A(1,2)*A(3,1) - A(1,1)*A(3,2) )
			Gamma(3,3) = ONE
		case default
			store = matmul(R,R)
			Gamma = E + store
			if (g > 0.05_fpk) then
				store2 = matmul(store, store)
	            Gamma = Gamma + store2
			endif
			if (g > 0.5_fpk) Gamma = Gamma + matmul(store2, store)
		end select
				
	END SUBROUTINE Gamma_Matrix

	Subroutine Gamma_Matrix_TL( R, Gamma, g, R_TL, Gamma_TL )

		implicit none

		real(fpk), intent(in), dimension(:,:)		::	R, Gamma, R_TL
		real(fpk), intent(in)						::	g
		real(fpk), intent(out), dimension(:,:)		::	Gamma_TL

		integer ::	n
		real(fpk), dimension(size(R,1), size(R,2))	::	store, store2
		real(fpk), dimension(size(R,1), size(R,2))	::  store_TL, store2_TL

		n = size(R, 1)
		if ( (n==2) .OR. (n==3) ) then 
			Gamma_TL = Matmul(Gamma, Matmul(matmul(R,R_TL)+matmul(R_TL,R),Gamma))
		else
			Gamma_TL = Matmul(Gamma, Matmul(matmul(R,R_TL)+matmul(R_TL,R),Gamma))
!			store = matmul(R,R)
!			store_TL = matmul(R,R_TL)+matmul(R_TL,R)
!			Gamma = E + store
!			Gamma_TL = E + store_TL
!			if (g > 0.05_fpk) then
!				store2 = matmul(store, store)
!				store2_TL = matmul(store,store_TL) + matmul(store_TL,store)
!	            Gamma = Gamma + store2
!				Gamma_TL = Gamma_TL + store2_TL
!			endif
!			if (g > 0.5_fpk) then 
!				Gamma = Gamma + matmul(store2, store)
!				Gamma_TL = Gamma_TL + matmul(store2,store_TL) + matmul(store2_TL,store)
!			endif	
		endif
				
	END SUBROUTINE Gamma_Matrix_TL

	Subroutine Gamma_Matrix_AD( Rt, Gammat, g, Gamma_AD, R_AD )

		implicit none

		real(fpk), intent(in), dimension(:,:)		::	Rt, Gammat, Gamma_AD
		real(fpk), intent(in)						::	g
		real(fpk), intent(inout), dimension(:,:)		::	R_AD

		integer ::	n
!		real(fpk)	:: idet
		real(fpk), dimension(size(Rt,1), size(Rt,2))	::	BB_AD

		n = size(Rt, 1)

		if ( (n==2) .OR. (n==3) ) then 
	!		B = I - R^2.  The adjoint of B is straightforward; the negative of it is given by:
			BB_AD = matmul(gammat, matmul(Gamma_AD,gammat))
			R_AD = matmul(Rt, BB_AD) + matmul(BB_AD, Rt) + R_AD
		else
			! n is greater than three. Technically I should code the correct adjoint
			!  here, but I am lazy.  Just use above approximation.
			BB_AD = matmul(gammat, matmul(Gamma_AD,gammat))
			R_AD = matmul(Rt, BB_AD) + matmul(BB_AD, Rt) + R_AD
		endif
	END SUBROUTINE Gamma_Matrix_AD


FUNCTION outer_prod(a,b) result(c)
	REAL(fpk), intent(in), dimension(:) :: a, b
	REAL(fpk) :: c(size(a),size(b))
	integer :: i, j

	do j = 1, size(b)
		do i = 1, size(a)
			c(i,j) = a(i) * b(j)
		enddo
	enddo
END FUNCTION outer_prod


END MODULE SOI_doubling

