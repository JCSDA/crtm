!
! CloudCoeff_Generate_New
!
! Program to create a new CloudCoeff file
! in both netCDF and Binary formats but with the default 
! effective radius and frequencies.
! i.e., attempt to prefectly recreate the standard CRTM format
!
!
! CREATION HISTORY:
!       Written by: Benjamin T. Johnson 10-23-2015
!                   benjamin.t.johnson@noaa.gov
!
! MODIFICATION HISTORY:
!       Modified by: Benjamin T. Johnson 11-11-2025
!                    (1) stand-alone utility to create standard format file

PROGRAM CloudCoeff_Generate_default_format

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE CloudCoeff_Define , ONLY: CloudCoeff_type, CloudCoeff_Create, CloudCoeff_Inspect
  USE CloudCoeff_IO     , ONLY: CloudCoeff_Writefile
  USE Type_Kinds
  ! Module use
  USE File_Utility        , ONLY: File_Exists
  USE CloudCoeff_Binary_IO, ONLY: CloudCoeff_Binary_InquireFile, CloudCoeff_Binary_ReadFile
  USE freq_io


  ! Disable implicit typing
  IMPLICIT NONE

  !------------------
  !  Type assignment
  !------------------
  TYPE(CloudCoeff_type) :: CloudCoeff


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CloudCoeff_Generate_New'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = ''


  ! ------------
  ! Parameters 
  ! ------------

  INTEGER, PARAMETER :: ML = 512
  INTEGER, PARAMETER :: n_MW_Frequencies = 70
  INTEGER, PARAMETER :: n_MW_Radii       = 131
  INTEGER, PARAMETER :: n_IR_Frequencies = 470
  INTEGER, PARAMETER :: n_IR_Radii       = 39  !** modified top stop at 500 microns, maybe modify more to stop at 100 if calcs bottom out past 100? 
  INTEGER, PARAMETER :: n_Temperatures   =  5
  INTEGER, PARAMETER :: n_Densities      =  4
  !  n_IR_Densities isn't a 2.4.x dimension for cloudcoeff.  the IR densities are n_Densities+1, where Densities 1:n_Densities are solid, and density 0 is liquid
  !  also, IR_Radii can be smaller or different from MW radii, but liquid and solid share the same radii set.
!  INTEGER, PARAMETER :: n_IR_Densities   =  n_Densities + 1  !** not 100% sure about this 
  INTEGER, PARAMETER :: n_Legendre_Terms = 38  !** 0:37 = 38 elements -- note, in CRTMv3+ this is 39 elements -- not sure which is correct.  
  INTEGER, PARAMETER :: n_Phase_Elements =  6  !** I think we want all phase elements in new coefficients, since we have all of the relevant matrices.


  ! ------------
  ! Arrays
  ! ------------
  
  real(double) ::  Frequency_MW(n_MW_Frequencies)
  real(double) ::  Frequency_IR(n_IR_Frequencies)
  real(double) ::  Reff_MW(n_MW_Radii)
  real(double) ::  Reff_IR(n_IR_Radii)
  real(double) ::  Temperature(n_Temperatures)
  real(double) ::  Density(n_Densities)
  !** The order of indices are reversed in the netCDF dump vs. fortran ordering. 
  real(double) ::  ke_L_MW(n_MW_Frequencies, n_MW_Radii, n_Temperatures)
  real(double) ::  w_L_MW(n_MW_Frequencies, n_MW_Radii, n_Temperatures)
  real(double) ::  g_L_MW(n_MW_Frequencies, n_MW_Radii, n_Temperatures)
!  real(double) ::  bk_L_MW(n_MW_Frequencies, n_MW_Radii, n_Temperatures) !** BTJ
  real(double) ::  pcoeff_L_MW(n_MW_Frequencies, n_MW_Radii, n_Temperatures, 0:n_Legendre_Terms, n_Phase_Elements)
  real(double) ::  ke_S_MW(n_MW_Frequencies, n_MW_Radii, n_Densities)
  real(double) ::  w_S_MW(n_MW_Frequencies, n_MW_Radii, n_Densities)
  real(double) ::  g_S_MW(n_MW_Frequencies, n_MW_Radii, n_Densities)
!  real(double) ::  bk_S_MW(n_MW_Frequencies, n_MW_Radii, n_Densities) !** BTJ
  real(double) ::  pcoeff_S_MW(n_MW_Frequencies, n_MW_Radii, n_Densities, 0:n_Legendre_Terms, n_Phase_Elements)
  real(double) ::  ke_IR(n_IR_Frequencies, n_IR_Radii, 0:n_Densities)
  real(double) ::  w_IR(n_IR_Frequencies, n_IR_Radii, 0:n_Densities)
  real(double) ::  g_IR(n_IR_Frequencies, n_IR_Radii, 0:n_Densities)
  real(double) ::  pcoeff_IR(n_IR_Frequencies, n_IR_Radii, 0:n_Densities,0:n_Legendre_Terms)

  ! ------------------
  ! Internal variables 
  ! ------------------

  LOGICAL :: Quiet, netCDF

  INTEGER :: err_stat
  INTEGER :: i, direction
  INTEGER :: ntemp, nrho, nrad, nfreq
  INTEGER :: mixflag, nshp, NUMD, MAXLEG, gnlegen
  
  REAL(DOUBLE) :: f1,f2,temp_C,ak2,grhoav, alphad, gammad, wavelength_m, gN0_m4, gL_m
  REAL(DOUBLE) :: x, Qext, Qsca, Qback, freq_GHz, reff_um
  REAL(DOUBLE) :: IWCout, gext_m, mext_m2kg, mback_m2kg, gscat_m, gssa, gbk_m, tmp, gasym, ratio, rhow, rhoi
  REAL(fp), DIMENSION(n_Legendre_Terms+1) :: coef1, coef2, coef3, coef4  
  COMPLEX(fp)  :: msphere, mcore, ck2, cfac

  CHARACTER(256) :: BIN_Filename, NC_Filename
  CHARACTER(ML)  :: msg, pid_msg

  !** variables related to phase function coefficients (pcoeff):
  !** taken from rw_cloudcoeff by P. Stegmann 
  INTEGER :: HIGH
  INTEGER :: LOW
  ! Number of stream angle definitions in the data set
  INTEGER, PARAMETER :: DEF_N_STREAM_SETS = 5
  INTEGER, PARAMETER :: DEF_N_STREAMS(DEF_N_STREAM_SETS)       = [2, 4, 6, 8, 16]
  ! ...This defines the offset in the "n_Legendre_Terms"
  ! ...dimension of the phase coefficient arrays for the
  ! ...various stream angle sets.
  INTEGER, PARAMETER :: DEF_LEGENDRE_OFFSET(DEF_N_STREAM_SETS) = [0, 0, 5, 12, 21]

#ifdef LITTLE_ENDIAN
  CHARACTER(*), PARAMETER :: ENDIAN_TYPE='little_endian'
#else
  CHARACTER(*), PARAMETER :: ENDIAN_TYPE='big_endian'
#endif
  CHARACTER(*), PARAMETER :: COEFFICIENT_PATH='coefficients/'//ENDIAN_TYPE//'/'
  CHARACTER(*), PARAMETER :: CloudCoeff_File = 'CloudCoeff.bin'

  PRINT *, 'Machine Endian: ', ENDIAN_TYPE
  !** output filenames
  BIN_Filename = ADJUSTL('CloudCoeff_New.bin')
  NC_Filename  = ADJUSTL('CloudCoeff_New.nc')

  !** default parameters for Mie code 
  mixflag = 12321  !** dielectric mixing model for ice, water, and air (see Dmiesub.f90) 
  alphad  = 0.0d0  !** 0.0 -> exponential 
  gammad  = 1.0d0  !** 1.0 -> modified gamma (see Petty Huang, 2006)
  nshp    = 1      !** Shape Parameter: 1 == Mie, see Dmiesub.f90
  mcore   = cmplx(0.0d0,0.0d0)
  NUMD    = 300
  MAXLEG  = n_Legendre_Terms+1


  
  !** modify to read from input file (frequency, GHz)
  call read_frequencies_mw("MW_freq.txt",   Frequency_MW, n_MW_Frequencies)

  !** modify to read from input file (wavenumber, cm^{-1})
  call read_wavenumbers_ir("UVIR_WN.txt",   Frequency_IR, n_IR_Frequencies)

  !** read from input file.  
  Temperature(1:n_Temperatures) = (/ 190.0, 210.0, 230.0, 250.0, 270.0 /) !** [K]  Temperatures for ice phase particles. 
  !** densities are really proxies for different microphysics schemes. 
  Density(1:n_Densities)        = (/ 1, 2, 3, 4 /)  !** [kg/m^3]

  !** a better approach here would be to read in the mass-dimension relationship, and compute density from that.  
  
  !** create the cloudcoeff structure
  CALL CloudCoeff_Create( CloudCoeff,       &
                          n_MW_Frequencies, &
                          n_MW_Radii      , &
                          n_IR_Frequencies, &
                          n_IR_Radii      , &
                          n_Temperatures  , &
                          n_Densities     , &
                          n_Legendre_Terms, &
                          n_Phase_Elements  )
  !** check here for CloudCoeff structure, is it already correctly allocated?
  !** e.g., call Inspect?
 
  
  !** Write the dimension-specific data to the new CloudCoeff structure
  CloudCoeff%n_MW_Frequencies = n_MW_Frequencies
  CloudCoeff%n_MW_Radii       = n_MW_Radii
  CloudCoeff%Frequency_MW     = Frequency_MW(:)
  CloudCoeff%Frequency_IR     = Frequency_IR(:)
  CloudCoeff%Reff_MW          = Reff_MW(:)
  CloudCoeff%Reff_IR          = Reff_IR(:)
  CloudCoeff%Temperature      = Temperature(:)
  CloudCoeff%Density          = Density(:) 

  CALL CloudCoeff_Inspect(CloudCoeff)

  !** replace solid microwave properties in CloudCoeff
  CloudCoeff%ke_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities) 
  CloudCoeff%w_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities) 
  CloudCoeff%g_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities) 

  
  
  !** from Patrick's rw_cloudcoeff.f90:
  DO i = 1, DEF_N_STREAM_SETS  !** 1:5                          =>  1,  2,  3,  4,  5
     LOW = DEF_LEGENDRE_OFFSET(i) !**  [ 0, 0, 5, 12, 21]       =>  0,  0,  5, 12, 21
     HIGH = LOW + DEF_N_STREAMS(i) !** LOW + [ 2, 4, 6,  8, 16] =>  2,  4, 11, 20, 37
     IF (n_Phase_Elements == 1) THEN                         !** min:1, max:38
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
     ELSEIF (n_Phase_Elements == 2) THEN
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
     ELSEIF (n_Phase_Elements == 3) THEN
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
     ELSEIF (n_Phase_Elements == 4) THEN
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,4) = coef4(LOW+1:HIGH+1)/4.0d0
     ELSEIF (n_Phase_Elements == 5) THEN
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,4) = coef4(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,5) = coef1(LOW+1:HIGH+1)/4.0d0
     ELSEIF (n_Phase_Elements == 6) THEN
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,4) = coef4(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,5) = coef1(LOW+1:HIGH+1)/4.0d0
        CloudCoeff%pcoeff_S_MW(1:n_MW_Frequencies,1:n_MW_Radii,1:n_Densities,LOW:HIGH,6) = coef2(LOW+1:HIGH+1)/4.0d0
     ELSE
        STOP 'number of phase elements too large (n_Phase_Elements > 6)'
     END IF
  END DO
  
  
  STOP 'bp'

  

  !** strategy from here:
  !** (1) Identify which variables require Mie Code (e.g., *_L_*)
  !** (2) Determine if pcoeffs are structure properly and use the offsets properly, also need to determine if the delta truncation method
  !**     parameters are in the right location (still not clear to me where those go, n_leg + 1 in pcoeff?) I cannot find where in CRTM this is used.
  !**     my present understanding is that this is already included in the pcoeff values themselves somehow. 


  !** here we need a decision tree about when and what Mie needs to be called for.
!!$  
!!$  !** Hydrometeor Model, populate cloudcoef structure
!!$  !** use Dmiesub:
!!$  DO nfreq = 1,n_MW_Frequencies
!!$     freq_GHz     = Frequency_MW(nfreq) !** loop over the default microwave frequencies 
!!$     wavelength_m = (299792458d0) / (freq_GHz * 1.0d9) !** [meters] 
!!$     
!!$     DO nrad = 1,n_MW_Radii  !** I'm assuming this is effective radius... 
!!$        reff_um = Reff_MW(nrad)   !**  need to convert to Lambda_m (slope) for my routines
!!$        gL_m    = 3.0d6 / (2.0d0*reff_um)  !** The factor of 2.0d0 should be here, since sL is in diameter space
!!$        gN0_m4  = 1.0d7                    !** Not sure what the N0 assumption is here, it should be constant, 
!!$        !** since it appears that N0 just scales the integrated quantities linearly
!!$        
!!$        solid_loop: DO nrho = 1,N_Densities !** loop over densities (this will change with non-spherical particles)
!!$           grhoav = Density(nrho)  !** [kg/m^3] ice/air volume fraction (0.0 = pure air, 1.0 = solid ice)
!!$           
!!$           temp_C = Temperature(1)-273.15d0  !** convert to C
!!$           f2 = grhoav/rhoi(temp_C)/1.d03 
!!$           f1 = 0.0d0
!!$           
!!$           
!!$           call cdmx(f1,f2,temp_C,freq_GHz,mixflag, ak2,msphere)
!!$           IF (AIMAG(msphere) .GT. 0.0d0) msphere = CONJG(msphere)
!!$           
!!$           !**              grhoav  = (f1*rhow(temp_C) + f2*rhoi(temp_C))*1.0d3
!!$           ratio  = (rhow(temp_C)*1.0d3/grhoav)**(1.0d0/3.0d0)
!!$           
!!$           CALL Dmie(nshp, wavelength_m, grhoav, msphere, mcore, f1,  &
!!$                NUMD, MAXLEG, gN0_m4, gL_m, alphad, gammad, IWCout,   &
!!$                gext_m, mext_m2kg, gscat_m,gssa, gbk_m, mback_m2kg,   &
!!$                gasym, gnlegen, coef1, coef2, coef3, coef4)
!!$           
!!$           !** effective size parameter
!!$           x = 2.0d0*(4.0d0*ATAN(1.0d0))*(reff_um/1.0d6) / wavelength_m 
!!$           
!!$           !**                1      2         3      4         5              6              7                                     8                             9 
!!$           PRINT '(13G12.4)', x, freq_GHz, reff_um, grhoav, temp_C+273.51, mext_m2kg, OldCC%ke_S_MW(nfreq,nrad,nrho), &
!!$                mext_m2kg/OldCC%ke_S_MW(nfreq,nrad,nrho), OldCC%ke_S_MW(nfreq,nrad,nrho)/mext_m2kg, gext_m, gscat_m, gssa, gasym
!!$
!!$           !** replace solid microwave properties in CloudCoeff
!!$           CloudCoeff%ke_S_MW(nfreq,nrad,nrho) = mext_m2kg !** [not so similar to default...]
!!$           CloudCoeff%w_S_MW( nfreq,nrad,nrho) = gssa   !** [quite similar to default]
!!$           CloudCoeff%g_S_MW( nfreq,nrad,nrho) = gasym  !** [quite similar to default]
!!$!           CloudCoeff%bk_S_MW(nfreq,nrad,nrho) = mback_m2kg  !** mass-weighted backscattering efficiency
!!$
!!$           
!!$           !** from Patrick's rw_cloudcoeff.f90:
!!$           DO i = 1, DEF_N_STREAM_SETS  !** 1:5                          =>  1,  2,  3,  4,  5
!!$              LOW = DEF_LEGENDRE_OFFSET(i) !**  [ 0, 0, 5, 12, 21]       =>  0,  0,  5, 12, 21
!!$              HIGH = LOW + DEF_N_STREAMS(i) !** LOW + [ 2, 4, 6,  8, 16] =>  2,  4, 11, 20, 37
!!$              IF (n_Phase_Elements == 1) THEN                         !** min:1, max:38 
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$              ELSEIF (n_Phase_Elements == 2) THEN 
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
!!$              ELSEIF (n_Phase_Elements == 3) THEN 
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
!!$              ELSEIF (n_Phase_Elements == 4) THEN 
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,4) = coef4(LOW+1:HIGH+1)/4.0d0
!!$              ELSEIF (n_Phase_Elements == 5) THEN 
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,4) = coef4(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,5) = coef1(LOW+1:HIGH+1)/4.0d0
!!$              ELSEIF (n_Phase_Elements == 6) THEN 
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,4) = coef4(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,5) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_S_MW(nfreq,nrad,nrho,LOW:HIGH,6) = coef2(LOW+1:HIGH+1)/4.0d0
!!$              ELSE
!!$                 STOP 'number of phase elements too large (n_Phase_Elements > 6)'
!!$              END IF
!!$           END DO
!!$        END DO solid_loop
!!$
!!$
!!$        
!!$        rain_loop: DO ntemp = 1,N_Temperatures  !** temperature loop (my dielectric table may not show a very good temperature dependence)
!!$           temp_C = Temperature(ntemp)-273.15d0  !** convert to C
!!$           f2 = 0.0d0
!!$           f1 = 1.0d0
!!$           
!!$           CALL cdmx(f1,f2,temp_C,freq_GHz,mixflag, ak2,msphere)
!!$           IF (AIMAG(msphere) .GT. 0.0d0) msphere = CONJG(msphere)
!!$           
!!$           !**              grhoav  = (f1*rhow(temp_C) + f2*rhoi(temp_C))*1.0d3
!!$           ratio  = (rhow(temp_C)*1.0d3/grhoav)**(1.0d0/3.0d0)
!!$           
!!$           CALL Dmie(nshp, wavelength_m, grhoav, msphere, mcore, f1,  &
!!$                NUMD, MAXLEG, gN0_m4, gL_m, alphad, gammad, IWCout,   &
!!$                gext_m, mext_m2kg, gscat_m,gssa, gbk_m, tmp,          &
!!$                gasym, gnlegen, coef1, coef2, coef3, coef4)
!!$           
!!$           !** effective size parameter
!!$           x = 2.0d0*(4.0d0*ATAN(1.0d0))*(reff_um/1.0d6) / wavelength_m
!!$           
!!$           CloudCoeff%ke_L_MW(nfreq,nrad,ntemp) = mext_m2kg   !** [not so similar to default...]
!!$           CloudCoeff%w_L_MW(nfreq,nrad,ntemp)  = gssa   !** [quite similar to default]
!!$           CloudCoeff%g_L_MW(nfreq,nrad,ntemp)  = gasym  !** [quite similar to default]
!!$           !** backscattering would go here, but we're developing this for legacy cloudcoeff tables.
!!$           
!!$           !**           CloudCoeff%bk_L_MW(nfreq,nrad,ntemp) = mback_m2kg  !** volume backscattering efficiency, [meters^-1]
!!$           
!!$           DO i = 1, DEF_N_STREAM_SETS  !** 1:5                          =>  1,  2,  3,  4,  5
!!$              LOW = DEF_LEGENDRE_OFFSET(i) !**  [ 0, 0, 5, 12, 21]       =>  0,  0,  5, 12, 21
!!$              HIGH = LOW + DEF_N_STREAMS(i) !** LOW + [ 2, 4, 6,  8, 16] =>  2,  4, 11, 20, 37
!!$              
!!$              IF (n_Phase_Elements == 1) THEN 
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$              ELSEIF (n_Phase_Elements == 2) THEN 
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
!!$              ELSEIF (n_Phase_Elements == 3) THEN 
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
!!$              ELSEIF (n_Phase_Elements == 4) THEN 
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,4) = coef4(LOW+1:HIGH+1)/4.0d0
!!$              ELSEIF (n_Phase_Elements == 5) THEN 
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,4) = coef4(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,5) = coef1(LOW+1:HIGH+1)/4.0d0
!!$              ELSEIF (n_Phase_Elements == 6) THEN 
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,1) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,2) = coef2(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,3) = coef3(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,4) = coef4(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,5) = coef1(LOW+1:HIGH+1)/4.0d0
!!$                 CloudCoeff%pcoeff_L_MW(nfreq,nrad,nrho,LOW:HIGH,6) = coef2(LOW+1:HIGH+1)/4.0d0
!!$              ELSE
!!$                 STOP 'number of phase elements too large (n_Phase_Elements > 6)'
!!$              END IF
!!$           END DO
!!$        END DO rain_loop
!!$
!!$     END DO
!!$  END DO
!!$
!!$  !** don't write anything yet. Inspect the updated CloudCoeff structure.  
!!$  err_stat = CloudCoeff_WriteFile( TRIM(COEFFICIENT_PATH)//BIN_Filename, CloudCoeff, netCDF = .FALSE., Quiet = .FALSE. )
!!$  IF ( err_stat /= SUCCESS ) THEN
!!$     msg = 'Error writing netCDF file '//TRIM(BIN_Filename)
!!$     CALL Display_Message( 'CloudCoeff_WriteFile', msg, err_stat )
!!$  END IF
!!$  
!!$  ! Write the netCDF file
!!$  err_stat = CloudCoeff_WriteFile( "coefficients/netcdf/"//NC_Filename, CloudCoeff, netCDF = .TRUE., Quiet = .FALSE. )
!!$  IF ( err_stat /= SUCCESS ) THEN
!!$     msg = 'Error writing netCDF file '//TRIM(NC_Filename)
!!$     CALL Display_Message( 'CloudCoeff_WriteFile', msg, err_stat )
!!$  END IF
  


END PROGRAM CloudCoeff_Generate_default_format


