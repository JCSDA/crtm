[Title: Difflog comparing CRTM REL-2.3.0 left to REL-2.2.3 right.]

[Source: REL-2.3.0: git clone -b release/REL-2.3.0 https://github.com/JCSDA/CRTM_dev.git CRTM_REL-2.3.0]
[Source: REL-2.2.3: git clone -b release/REL-2.2.3 https://github.com/JCSDA/CRTM_dev.git CRTM_REL-2.2.3]

[Author: Benjamin T. Johnson, UCAR/JCSDA (bjohns@ucar.edu)]
[Creation Date: 2020-04-08]

[Note: See CHANGELOG.md for a compact/human friendly changelog.]

[Note: diff lines without any ###c### below them are identical between versions.]
[Note: files listed at the end of this document do not exist in the older version.]

---

diff -w ./ACCoeff_Binary_IO.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ACCoeff_Binary_IO.f90
diff -w ./ACCoeff_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ACCoeff_Define.f90
diff -w ./ADA_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ADA_Module.f90
390c390
<    IF( RTV%Solar_Flag_true ) THEN
---
>    IF( RTV%Visible_Flag_true ) THEN
824c824
<      IF( RTV%Solar_Flag_true ) THEN 
---
>      IF( RTV%Visible_Flag_true ) THEN 
1254c1254
<      IF( RTV%Solar_Flag_true ) THEN 
---
>      IF( RTV%Visible_Flag_true ) THEN 
diff -w ./AOvar_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/AOvar_Define.f90
diff -w ./ASvar_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ASvar_Define.f90
diff -w ./AerosolCoeff_Binary_IO.f90 ~/CRTM/clean/REL-2.2.3/libsrc/AerosolCoeff_Binary_IO.f90
diff -w ./AerosolCoeff_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/AerosolCoeff_Define.f90
diff -w ./Azimuth_Emissivity_F6_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Azimuth_Emissivity_F6_Module.f90
diff -w ./Azimuth_Emissivity_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Azimuth_Emissivity_Module.f90
diff -w ./Binary_File_Utility.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Binary_File_Utility.f90
diff -w ./CRTM_AOD_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_AOD_Module.f90
diff -w ./CRTM_Adjoint_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Adjoint_Module.f90
37d36
<                                         CRTM_Atmosphere_Zero           , &
39d37
<                                         CRTM_Atmosphere_NonVariableCopy, &
42,43c40
<                                         CRTM_Surface_IsValid        , &
<                                         CRTM_Surface_NonVariableCopy
---
>                                         CRTM_Surface_IsValid
48,50d44
<   USE CRTM_RTSolution_Define,     ONLY: CRTM_RTSolution_type, &
<                                         CRTM_RTSolution_Destroy, &
<                                         CRTM_RTSolution_Zero
54,58c48
<                                         CRTM_Atmosphere_AddLayers_AD   , &
<                                         CRTM_Atmosphere_IsFractional   , &
<                                         CRTM_Atmosphere_Coverage       , &
<                                         CRTM_Atmosphere_ClearSkyCopy   , &
<                                         CRTM_Atmosphere_ClearSkyCopy_AD
---
>                                         CRTM_Atmosphere_AddLayers_AD
82c72,75
<   USE CRTM_AtmOptics,             ONLY: CRTM_Include_Scattering        , &
---
>   USE CRTM_AtmOptics,             ONLY: AOvar_type  , &
>                                         AOvar_Create, &
>                                         CRTM_No_Scattering           , &
>                                         CRTM_Include_Scattering      , &
85,90c78,80
<                                         CRTM_AtmOptics_Combine         , &
<                                         CRTM_AtmOptics_Combine_AD      , &
<                                         CRTM_AtmOptics_NoScatterCopy   , &
<                                         CRTM_AtmOptics_NoScatterCopy_AD
<   USE CRTM_SfcOptics_Define,      ONLY: OPERATOR(+)              , &
<                                         CRTM_SfcOptics_type      , &
---
>                                         CRTM_Combine_AtmOptics       , &
>                                         CRTM_Combine_AtmOptics_AD
>   USE CRTM_SfcOptics_Define,      ONLY: CRTM_SfcOptics_type      , &
93,94c83
<                                         CRTM_SfcOptics_Destroy   , &
<                                         CRTM_SfcOptics_Zero
---
>                                         CRTM_SfcOptics_Destroy
97c86,87
<   USE CRTM_RTSolution,            ONLY: CRTM_Compute_nStreams     , &
---
>   USE CRTM_RTSolution,            ONLY: CRTM_RTSolution_type      , &
>                                         CRTM_Compute_nStreams     , &
117d106
<   USE CRTM_CloudCover_Define,     ONLY: CRTM_CloudCover_type
120,124d108
<   ! ...AtmOptics
<   USE AOvar_Define, ONLY: AOvar_type, &
<                           AOvar_Associated, &
<                           AOvar_Destroy   , &
<                           AOvar_Create
315c299,302
<     LOGICAL :: compute_antenna_correction
---
>     LOGICAL :: Check_Input
>     LOGICAL :: User_Emissivity, User_Direct_Reflectivity, User_N_Streams
>     LOGICAL :: User_AntCorr, Compute_AntCorr
>     LOGICAL :: Apply_NLTE_Correction
317c304
<     INTEGER :: Status_FWD, Status_AD
---
>     INTEGER :: RT_Algorithm_Id
318a306
>     INTEGER :: nc, na
322c310
<     INTEGER :: ln
---
>     INTEGER :: j, ln
324d311
<     INTEGER :: cloud_coverage_flag
328,329d314
<     REAL(fp) :: transmittance_clear, transmittance_clear_AD
<     REAL(fp) :: r_cloudy
333c318
<     TYPE(CRTM_Options_type) :: Default_Options, Opt
---
>     TYPE(CRTM_Options_type) :: Default_Options
336,341d320
<     ! Clear sky structures
<     TYPE(CRTM_Atmosphere_type) :: Atm_Clear       , Atm_Clear_AD
<     TYPE(CRTM_AtmOptics_type)  :: AtmOptics_Clear , AtmOptics_Clear_AD
<     TYPE(CRTM_SfcOptics_type)  :: SfcOptics_Clear , SfcOptics_Clear_AD
<     TYPE(CRTM_RTSolution_type) :: RTSolution_Clear, RTSolution_Clear_AD
<     TYPE(RTV_type)             :: RTV_Clear
356,357d334
<     ! Cloud cover object
<     TYPE(CRTM_CloudCover_type) :: CloudCover, CloudCover_AD
412,415d388
<     ! Reinitialise the output RTSolution
<     CALL CRTM_RTSolution_Zero(RTSolution)
< 
< 
452,453c425,448
<       CALL CRTM_Atmosphere_NonVariableCopy( Atmosphere(m), Atmosphere_AD(m) )
<       CALL CRTM_Surface_NonVariableCopy( Surface(m), Surface_AD(m) )
---
>       ! ...Atmosphere
>       Atmosphere_AD(m)%Climatology = Atmosphere(m)%Climatology
>       DO j = 1, Atmosphere(m)%n_Absorbers
>         Atmosphere_AD(m)%Absorber_ID(j)    = Atmosphere(m)%Absorber_ID(j)
>         Atmosphere_AD(m)%Absorber_Units(j) = Atmosphere(m)%Absorber_Units(j)
>       END DO
>       ! Loop over and assign cloud types
>       DO nc = 1, Atmosphere(m)%n_Clouds
>         Atmosphere_AD(m)%Cloud(nc)%Type = Atmosphere(m)%Cloud(nc)%Type
>       END DO
>       ! Loop over and assign aerosol types
>       DO na = 1, Atmosphere(m)%n_Aerosols
>         Atmosphere_AD(m)%Aerosol(na)%Type = Atmosphere(m)%Aerosol(na)%Type
>       END DO
>       ! ...Surface
>       Surface_AD(m)%Land_Coverage  = Surface(m)%Land_Coverage
>       Surface_AD(m)%Water_Coverage = Surface(m)%Water_Coverage
>       Surface_AD(m)%Snow_Coverage  = Surface(m)%Snow_Coverage
>       Surface_AD(m)%Ice_Coverage   = Surface(m)%Ice_Coverage
>       Surface_AD(m)%Land_Type  = Surface(m)%Land_Type
>       Surface_AD(m)%Water_Type = Surface(m)%Water_Type
>       Surface_AD(m)%Snow_Type  = Surface(m)%Snow_Type
>       Surface_AD(m)%Ice_Type   = Surface(m)%Ice_Type
> 
457c452,459
<       Opt = Default_Options
---
>       ! ...Specify default actions
>       Check_Input           = Default_Options%Check_Input
>       User_Emissivity       = Default_Options%Use_Emissivity
>       User_AntCorr          = Default_Options%Use_Antenna_Correction
>       Apply_NLTE_Correction = Default_Options%Apply_NLTE_Correction
>       RT_Algorithm_Id       = Default_Options%RT_Algorithm_Id
>       User_N_Streams        = Default_Options%Use_N_Streams
>       ! ...Check the Options argument
459c461,483
<         Opt = Options(m)
---
>         ! Override input checker with option
>         Check_Input = Options(m)%Check_Input
>         ! Check if the supplied emissivity should be used
>         User_Emissivity = Options(m)%Use_Emissivity
>         IF ( Options(m)%Use_Emissivity ) THEN
>           ! Are the channel dimensions consistent
>           IF ( Options(m)%n_Channels < n_Channels ) THEN
>             Error_Status = FAILURE
>             WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
>                    &"than the number of requested channels (",i0, ")" )' ) &
>                    Options(m)%n_Channels, n_Channels
>             CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
>             RETURN
>           END IF
>           ! Check if the supplied direct reflectivity should be used
>           User_Direct_Reflectivity = Options(m)%Use_Direct_Reflectivity
>         END IF
>         ! Check if antenna correction should be attempted
>         User_AntCorr = Options(m)%Use_Antenna_Correction
>         ! Set NLTE correction option
>         Apply_NLTE_Correction = Options(m)%Apply_NLTE_Correction
> 
> 
462a487,503
>         ! Copy over surface optics input
>         SfcOptics%Use_New_MWSSEM = .NOT. Options(m)%Use_Old_MWSSEM
>         ! Specify the RT algorithm
>         RT_Algorithm_Id = Options(m)%RT_Algorithm_Id
>         ! Check if n_Streams should be used
>         User_N_Streams = Options(m)%Use_N_Streams
>         ! Check value for nstreams
>         IF ( User_N_Streams ) THEN
>           IF ( Options(m)%n_Streams <= 0 .OR. MOD(Options(m)%n_Streams,2) /= 0 .OR. &
>                Options(m)%n_Streams > MAX_N_STREAMS ) THEN
>               Error_Status = FAILURE
>               WRITE( Message,'( "Input Options n_Streams (", i0, ") is invalid" )' ) &
>                      Options(m)%n_Streams
>               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
>               RETURN
>           END IF
>         END IF
464,465d504
<       ! ...Assign the option specific SfcOptics input
<       SfcOptics%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
469c508
<       IF ( Opt%Check_Input ) THEN
---
>       IF ( Check_Input ) THEN
489,510d527
<           ! Are the channel dimensions consistent if emissivity is passed?
<           IF ( Options(m)%Use_Emissivity ) THEN
<             IF ( Options(m)%n_Channels < n_Channels ) THEN
<               Error_Status = FAILURE
<               WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
<                      &"than the number of requested channels (",i0, ")" )' ) &
<                      Options(m)%n_Channels, n_Channels
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
<           END IF
<           ! Check value for user-defined n_Streams
<           IF ( Options(m)%Use_N_Streams ) THEN
<             IF ( Options(m)%n_Streams <= 0 .OR. MOD(Options(m)%n_Streams,2) /= 0 .OR. &
<                  Options(m)%n_Streams > MAX_N_STREAMS ) THEN
<                 Error_Status = FAILURE
<                 WRITE( Message,'( "Input Options n_Streams (", i0, ") is invalid" )' ) &
<                        Options(m)%n_Streams
<                 CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                 RETURN
<             END IF
<           END IF
525a543,546
>       ! Average surface skin temperature for multi-surface types
>       CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )
> 
> 
546,548d566
< 
< 
<       ! Prepare the atmospheric optics structures
565,567c583,587
<       ! ...Set the Scattering Switch
<       AtmOptics%Include_Scattering    = Opt%Include_Scattering
<       AtmOptics_AD%Include_Scattering = Opt%Include_Scattering
---
>       IF (Options_Present) THEN
>         ! Set Scattering Switch
>         AtmOptics%Include_Scattering = Options(m)%Include_Scattering
>         AtmOptics_AD%Include_Scattering = Options(m)%Include_Scattering
>       END IF
591,643d610
<       ! Determine the type of cloud coverage
<       cloud_coverage_flag = CRTM_Atmosphere_Coverage( atm )
< 
< 
<       ! Setup for fractional cloud coverage
<       IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<       
<         ! Compute cloudcover
<         Error_Status = CloudCover%Compute_CloudCover(atm, Overlap = opt%Overlap_Id)
<         IF ( Error_Status /= SUCCESS ) THEN
<           WRITE( Message,'("Error computing cloud cover in profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
<         ! ...Mold the adjoint object based on the forward, and reinitialise
<         CloudCover_AD = CloudCover
<         CALL CloudCover_AD%Set_To_Zero()
< 
<         ! Allocate some of the CLEAR sky structure for fractional cloud coverage
<         ! (The AtmOptics structures are allocated during a copy)
<         ! ...Clear sky atmosphere
<         Status_FWD = CRTM_Atmosphere_ClearSkyCopy(Atm, Atm_Clear)
<         Status_AD  = CRTM_Atmosphere_ClearSkyCopy(Atm, Atm_Clear_AD)
<         IF ( Status_FWD /= SUCCESS .OR. Status_AD /= SUCCESS ) THEN
<           Error_status = FAILURE
<           WRITE( Message,'("Error copying CLEAR SKY Atmosphere structures for profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
<         CALL CRTM_Atmosphere_Zero( Atm_Clear_AD )
<         ! ...Clear sky SfcOptics
<         CALL CRTM_SfcOptics_Create( SfcOptics_Clear   , MAX_N_ANGLES, MAX_N_STOKES )
<         CALL CRTM_SfcOptics_Create( SfcOptics_Clear_AD, MAX_N_ANGLES, MAX_N_STOKES )
<         IF ( (.NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear)) .OR. &
<              (.NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear_AD))) THEN
<           Error_Status = FAILURE
<           WRITE( Message,'("Error allocating CLEAR SKY SfcOptics data structures for profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
<         CALL CRTM_SfcOptics_Zero( SfcOptics_Clear_AD )
<         ! ...Copy over surface optics input
<         SfcOptics_Clear%Use_New_MWSSEM    = .NOT. Opt%Use_Old_MWSSEM
<         SfcOptics_Clear_AD%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
<         ! ...CLEAR SKY average surface skin temperature for multi-surface types
<         CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics_Clear )
<       END IF
< 
< 
<       ! Average surface skin temperature for multi-surface types
<       CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )
< 
< 
658c625
<         compute_antenna_correction = ( Opt%Use_Antenna_Correction               .AND. &
---
>         IF ( User_AntCorr                             .AND. &
660c627,631
<                                        iFOV /= 0 )
---
>              iFOV /= 0 ) THEN
>           Compute_AntCorr = .TRUE.
>         ELSE
>           Compute_AntCorr = .FALSE.
>         END IF
693,694c664
<               SpcCoeff_IsVisibleSensor(SC(SensorIndex)) ) .AND. &
<             AtmOptics%Include_Scattering ) THEN
---
>             SpcCoeff_IsVisibleSensor( SC(SensorIndex) ) ) .and. AtmOptics%Include_Scattering ) THEN
704c674
<           RTV%RT_Algorithm_Id = Opt%RT_Algorithm_Id
---
>           RTV%RT_Algorithm_Id = RT_Algorithm_Id
709c679
<         IF ( Opt%Apply_NLTE_Correction ) THEN
---
>         IF ( Apply_NLTE_Correction ) THEN
739,747d708
<           ! ...Same for clear structures
<           RTSolution_Clear%Sensor_Id        = RTSolution(ln,m)%Sensor_Id
<           RTSolution_Clear%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
<           RTSolution_Clear%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id
<           RTSolution_Clear%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel
<           RTSolution_Clear_AD%Sensor_Id        = RTSolution(ln,m)%Sensor_Id
<           RTSolution_Clear_AD%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
<           RTSolution_Clear_AD%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id
<           RTSolution_Clear_AD%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel
752d712
<           CALL CRTM_AtmOptics_Zero( AtmOptics_AD )
754,755d713
<           CALL CRTM_RTSolution_Zero( RTSolution_Clear )
<           CALL CRTM_RTSolution_Zero( RTSolution_Clear_AD )
759c717
<           IF ( Opt%Use_N_Streams ) THEN
---
>           IF ( User_N_Streams ) THEN
772,773d729
<           ! ...Ensure clear-sky object dimensions are consistent
<           AtmOptics_Clear_AD%n_Legendre_Terms = AtmOptics_AD%n_Legendre_Terms
784a741,745
>           ! Compute the clear-sky atmospheric transmittance
>           ! for use in FASTEM-X reflection correction
>           CALL CRTM_Compute_Transmittance(AtmOptics,transmittance)
> 
> 
820,840d780
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               RTV_Clear%Visible_Flag_true = .FALSE.
<               RTV_Clear%n_Azi = 0
<             END IF
<           END IF
< 
< 
<           ! Copy the clear-sky AtmOptics
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             Status_FWD = CRTM_AtmOptics_NoScatterCopy( AtmOptics, AtmOptics_Clear )
<             Status_AD  = CRTM_AtmOptics_NoScatterCopy( AtmOptics, AtmOptics_Clear_AD )
<             IF ( Status_FWD /= SUCCESS .OR. Status_AD /= SUCCESS ) THEN
<               Error_Status = FAILURE
<               WRITE( Message,'("Error copying CLEAR SKY AtmOptics for ",a,&
<                      &", channel ",i0,", profile #",i0)' ) &
<                      TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
<             ! Initialise the adjoint
<             CALL CRTM_AtmOptics_Zero( AtmOptics_Clear_AD )
880c820
<             CALL CRTM_AtmOptics_Combine( AtmOptics, AOvar )
---
>             CALL CRTM_Combine_AtmOptics( AtmOptics, AOvar )
886,888c826,829
<           ! Compute the all-sky atmospheric transmittance
<           ! for use in FASTEM-X reflection correction
<           CALL CRTM_Compute_Transmittance(AtmOptics,transmittance)
---
>           ! Turn off FASTEM-X reflection correction for scattering conditions
>           IF ( CRTM_Include_Scattering(AtmOptics) .AND. SpcCoeff_IsMicrowaveSensor( SC(SensorIndex) ) ) THEN
>             SfcOptics%Transmittance = -ONE
>           ELSE
890,893d830
<           ! ...Clear sky for fractional cloud cover
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             CALL CRTM_Compute_Transmittance(AtmOptics_Clear,transmittance_clear)
<             SfcOptics_Clear%Transmittance = transmittance_clear
897a835
>           ! ...Indicate SfcOptics ARE to be computed
899,900c837,838
<           SfcOptics_Clear%Compute = .TRUE.
<           IF ( Opt%Use_Emissivity ) THEN
---
>           ! ...Change SfcOptics emissivity/reflectivity contents/computation status
>           IF ( User_Emissivity ) THEN
902,905c840,843
<             SfcOptics%Emissivity(1,1)       = Opt%Emissivity(ln)
<             SfcOptics%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
<             IF ( Opt%Use_Direct_Reflectivity ) THEN
<               SfcOptics%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
---
>             SfcOptics%Emissivity(1,1)       = Options(m)%Emissivity(ln)
>             SfcOptics%Reflectivity(1,1,1,1) = ONE - Options(m)%Emissivity(ln)
>             IF ( User_Direct_Reflectivity ) THEN
>               SfcOptics%Direct_Reflectivity(1,1) = Options(m)%Direct_Reflectivity(ln)
909,919d846
<             ! ...Repeat for fractional clear-sky case
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               SfcOptics_Clear%Compute = .FALSE.
<               SfcOptics_Clear%Emissivity(1,1)       = Opt%Emissivity(ln)
<               SfcOptics_Clear%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
<               IF ( Opt%Use_Direct_Reflectivity ) THEN
<                 SfcOptics_Clear%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
<               ELSE
<                 SfcOptics_Clear%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
<               END IF
<             END IF
926a854,855
>           ! ...Initialise adjoint atmospheric optics
>           CALL CRTM_AtmOptics_Zero( AtmOptics_AD )
962a892,898
>             ! Compute non-LTE correction to radiance if required
>             IF ( Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) &
>               CALL Compute_NLTE_Correction( &
>                      SC(SensorIndex)%NC       , &  ! Input
>                      ChannelIndex             , &  ! Input
>                      NLTE_Predictor           , &  ! Input
>                      RTSolution(ln,m)%Radiance  )  ! In/Output
964,973c900,901
<             ! Repeat clear sky for fractionally cloudy atmospheres
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               RTV_Clear%mth_Azi = 0 !RTV%mth_Azi
<               SfcOptics_Clear%mth_Azi = 0 !SfcOptics%mth_Azi
<               Error_Status = CRTM_Compute_RTSolution( &
<                                Atm_Clear       , &  ! Input
<                                Surface(m)      , &  ! Input
<                                AtmOptics_Clear , &  ! Input
<                                SfcOptics_Clear , &  ! Input
<                                GeometryInfo    , &  ! Input
---
>             ! Convert the radiance to brightness temperature
>             CALL CRTM_Planck_Temperature( &
976,1001c904,905
<                                RTSolution_Clear, &  ! Output
<                                RTV_Clear         )  ! Internal variable output
<               IF ( Error_Status /= SUCCESS ) THEN
<                 WRITE( Message,'( "Error computing CLEAR SKY RTSolution for ", a, &
<                        &", channel ", i0,", profile #",i0)' ) &
<                        TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<                 CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                 RETURN
<               END IF
<             END IF
< 
< 
<             ! Combine cloudy and clear radiances for fractional cloud coverage
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               r_cloudy = RTSolution(ln,m)%Radiance  ! Save the 100% cloudy radiance
<               RTSolution(ln,m)%Radiance = &
<                   ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_Clear%Radiance) + &
<                   (CloudCover%Total_Cloud_Cover * RTSolution(ln,m)%Radiance)
<               ! ...Save the cloud cover in the output structure
<               RTSolution(ln,m)%Total_Cloud_Cover = CloudCover%Total_Cloud_Cover
<             END IF
< 
< 
<             ! The radiance post-processing
<             CALL Post_Process_RTSolution(RTSolution(ln,m))
< 
---
>                    RTSolution(ln,m)%Radiance              , & ! Input
>                    RTSolution(ln,m)%Brightness_Temperature  ) ! Output
1003,1045c907,909
<             ! Perform clear-sky post and pre-processing
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               ! Radiance post-processing
<               CALL Post_Process_RTSolution(RTSolution_Clear)
<               RTSolution(ln,m)%R_Clear  = RTSolution_Clear%Radiance
<               RTSolution(ln,m)%Tb_Clear = RTSolution_Clear%Brightness_Temperature
< 
<               ! Adjoint radiance pre-processing
<               RTSolution_Clear_AD%Brightness_Temperature = RTSolution_Clear_AD%Brightness_Temperature + &
<                                                            RTSolution_AD(ln,m)%Tb_Clear
<               RTSolution_AD(ln,m)%Tb_Clear               = ZERO
<               RTSolution_Clear_AD%Radiance = RTSolution_Clear_AD%Radiance + &
<                                              RTSolution_AD(ln,m)%R_Clear
<               RTSolution_AD(ln,m)%R_Clear  = ZERO
<               CALL Pre_Process_RTSolution_AD(RTSolution_Clear, RTSolution_Clear_AD)
<             END IF
< 
< 
<             ! The adjoint radiance pre-processing
<             CALL Pre_Process_RTSolution_AD(RTSolution(ln,m), RTSolution_AD(ln,m))
< 
<           
<             ! More fractionally cloudy atmospheres processing
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
< 
<               ! The adjoint of the clear and cloudy radiance combination
<               CloudCover_AD%Total_Cloud_Cover = CloudCover_AD%Total_Cloud_Cover + &
<                                                 RTSolution_AD(ln,m)%Total_Cloud_Cover
<               RTSolution_AD(ln,m)%Total_Cloud_Cover = ZERO
<               RTSolution_Clear_AD%Radiance    = RTSolution_Clear_AD%Radiance + &
<                                                 ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_AD(ln,m)%Radiance)
<               CloudCover_AD%Total_Cloud_Cover = CloudCover_AD%Total_Cloud_Cover + &
<                                                 ((r_cloudy - RTSolution_Clear%Radiance) * RTSolution_AD(ln,m)%Radiance)
<               RTSolution_AD(ln,m)%Radiance    = CloudCover%Total_Cloud_Cover * RTSolution_AD(ln,m)%Radiance
< 
<               ! The adjoint of the clear sky radiative transfer for fractionally cloudy atmospheres
<               Error_Status = CRTM_Compute_RTSolution_AD( &
<                                Atm_Clear          , &  ! FWD Input
<                                Surface(m)         , &  ! FWD Input
<                                AtmOptics_Clear    , &  ! FWD Input
<                                SfcOptics_Clear    , &  ! FWD Input
<                                RTSolution_Clear   , &  ! FWD Input
<                                RTSolution_Clear_AD, &  ! AD  Input
---
>             ! Compute Antenna correction to brightness temperature if required
>             IF ( Compute_AntCorr ) THEN
>               CALL CRTM_Compute_AntCorr( &
1049,1060c913,918
<                                Atm_Clear_AD       , &  ! AD Output
<                                Surface_AD(m)      , &  ! AD Output
<                                AtmOptics_Clear_AD , &  ! AD Output
<                                SfcOptics_Clear_AD , &  ! AD Output
<                                RTV_Clear            )  ! Internal variable input
<               IF ( Error_Status /= SUCCESS ) THEN
<                 WRITE( Message,'( "Error computing CLEAR SKY RTSolution_AD for ", a, &
<                        &", channel ", i0,", profile #",i0)' ) &
<                        TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<                 CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                 RETURN
<               END IF
---
>                      RTSolution(ln,m)  )  ! Output
>               CALL CRTM_Compute_AntCorr_AD( &
>                      GeometryInfo       , &  ! Input
>                      SensorIndex        , &  ! Input
>                      ChannelIndex       , &  ! Input
>                      RTSolution_AD(ln,m)  )  ! Output
1062a921,936
>             ! Compute the Planck temperature adjoijnt
>             CALL CRTM_Planck_Temperature_AD( &
>                    SensorIndex                               , & ! Input
>                    ChannelIndex                              , & ! Input
>                    RTSolution(ln,m)%Radiance                 , & ! Input
>                    RTSolution_AD(ln,m)%Brightness_Temperature, & ! Input
>                    RTSolution_AD(ln,m)%Radiance                ) ! Output
>             RTSolution_AD(ln,m)%Brightness_Temperature = ZERO
> 
>             ! Compute non-LTE correction adjoint if required
>             IF ( Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) &
>               CALL Compute_NLTE_Correction_AD( &
>                      SC(SensorIndex)%NC          , &  ! Input
>                      ChannelIndex                , &  ! Input
>                      RTSolution_AD(ln,m)%Radiance, &  ! Input
>                      NLTE_Predictor_AD             )  ! Output
1093c967
<             ! Fourier expansion over azimuth angle
---
>             ! ...Fourier expansion over azimuth angle
1119,1212d992
<               ! Repeat clear sky for fractionally cloudy atmospheres
<               IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<                 RTV_Clear%mth_Azi = RTV%mth_Azi
<                 SfcOptics_Clear%mth_Azi = SfcOptics%mth_Azi
<                 Error_Status = CRTM_Compute_RTSolution( &
<                                  Atm_Clear       , &  ! Input
<                                  Surface(m)      , &  ! Input
<                                  AtmOptics_Clear , &  ! Input
<                                  SfcOptics_Clear , &  ! Input
<                                  GeometryInfo    , &  ! Input
<                                  SensorIndex     , &  ! Input
<                                  ChannelIndex    , &  ! Input
<                                  RTSolution_Clear, &  ! Output
<                                  RTV_Clear         )  ! Internal variable output
<                 IF ( Error_Status /= SUCCESS ) THEN
<                   WRITE( Message,'( "Error computing CLEAR SKY RTSolution for ", a, &
<                          &", channel ", i0,", profile #",i0)' ) &
<                          TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<                   CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                   RETURN
<                 END IF
<               END IF
<             END DO Azimuth_Fourier_Loop
< 
< 
<             ! All of the "in-between" FWD and AD processing is for fractional cloud coverage only
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
< 
<               ! FORWARD #1: Combine cloudy and clear radiances for fractional cloud coverage
<               r_cloudy = RTSolution(ln,m)%Radiance  ! Save the 100% cloudy radiance
<               RTSolution(ln,m)%Radiance = &
<                   ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_Clear%Radiance) + &
<                   (CloudCover%Total_Cloud_Cover * RTSolution(ln,m)%Radiance)
<               ! FORWARD #2: Save the cloud cover and clear radiance in the output structure
<               RTSolution(ln,m)%Total_Cloud_Cover = CloudCover%Total_Cloud_Cover
<               RTSolution(ln,m)%R_Clear           = RTSolution_Clear%Radiance
<               RTSolution(ln,m)%Tb_Clear          = ZERO      ! No Tb for visible
< 
<               ! ADJOINT #2: Of the cloud cover and clear radiance saving
<               RTSolution_Clear_AD%Tb_Clear = ZERO   ! No Tb for visible
<               RTSolution_Clear_AD%Radiance = RTSolution_Clear_AD%Radiance + &
<                                              RTSolution_AD(ln,m)%R_Clear
<               RTSolution_AD(ln,m)%R_Clear  = ZERO
<               CloudCover_AD%Total_Cloud_Cover = CloudCover_AD%Total_Cloud_Cover + &
<                                                 RTSolution_AD(ln,m)%Total_Cloud_Cover
<               RTSolution_AD(ln,m)%Total_Cloud_Cover = ZERO
< 
<               ! ADJOINT #1: Of the clear+cloudy combination
<               RTSolution_Clear_AD%Radiance    = RTSolution_Clear_AD%Radiance + &
<                                                 ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_AD(ln,m)%Radiance)
<               CloudCover_AD%Total_Cloud_Cover = CloudCover_AD%Total_Cloud_Cover + &
<                                                 ((r_cloudy - RTSolution_Clear%Radiance) * RTSolution_AD(ln,m)%Radiance)
<               RTSolution_AD(ln,m)%Radiance    = CloudCover%Total_Cloud_Cover * RTSolution_AD(ln,m)%Radiance
<             END IF
< 
< 
<             ! Adjoint Fourier expansion over azimuth angle
<             Azimuth_Fourier_Loop_AD: DO mth_Azi = 0, RTV%n_Azi
< 
< 
<               ! Set dependent component counters
<               RTV%mth_Azi = mth_Azi
<               SfcOptics%mth_Azi = mth_Azi
< 
< 
<               ! The adjoint of the clear sky radiative transfer for fractionally cloudy atmospheres
<               IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<                 RTV_Clear%mth_Azi = RTV%mth_Azi
<                 SfcOptics_Clear%mth_Azi = SfcOptics%mth_Azi
<                 Error_Status = CRTM_Compute_RTSolution_AD( &
<                                  Atm_Clear          , &  ! FWD Input
<                                  Surface(m)         , &  ! FWD Input
<                                  AtmOptics_Clear    , &  ! FWD Input
<                                  SfcOptics_Clear    , &  ! FWD Input
<                                  RTSolution_Clear   , &  ! FWD Input
<                                  RTSolution_Clear_AD, &  ! AD  Input
<                                  GeometryInfo       , &  ! Input
<                                  SensorIndex        , &  ! Input
<                                  ChannelIndex       , &  ! Input
<                                  Atm_Clear_AD       , &  ! AD Output
<                                  Surface_AD(m)      , &  ! AD Output
<                                  AtmOptics_Clear_AD , &  ! AD Output
<                                  SfcOptics_Clear_AD , &  ! AD Output
<                                  RTV_Clear            )  ! Internal variable input
<                 IF ( Error_Status /= SUCCESS ) THEN
<                   WRITE( Message,'( "Error computing CLEAR SKY RTSolution_AD for ", a, &
<                          &", channel ", i0,", profile #",i0)' ) &
<                          TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<                   CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                   RETURN
<                 END IF
<               END IF
< 
< 
1236c1016,1023
<             END DO Azimuth_Fourier_Loop_AD
---
>             END DO Azimuth_Fourier_Loop
> 
>             ! Still want to convert the final FORWARD radiance to brightness temperature
>             CALL CRTM_Planck_Temperature( &
>                    SensorIndex                            , & ! Input
>                    ChannelIndex                           , & ! Input
>                    RTSolution(ln,m)%Radiance              , & ! Input
>                    RTSolution(ln,m)%Brightness_Temperature  ) ! Output
1244,1256d1030
<           ! Compute the adjoint of the all-sky atmospheric transmittance
<           ! for use in FASTEM-X reflection correction
<           transmittance_AD = SfcOptics_AD%transmittance
<           SfcOptics_AD%transmittance = ZERO
<           CALL CRTM_Compute_Transmittance_AD(AtmOptics,transmittance_AD,AtmOptics_AD)
<           ! ...Clear sky for fractional cloud cover
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             transmittance_clear_AD = SfcOptics_Clear_AD%transmittance
<             SfcOptics_Clear_AD%transmittance = ZERO
<             CALL CRTM_Compute_Transmittance_AD(AtmOptics_Clear,transmittance_clear_AD,AtmOptics_Clear_AD)
<           END IF
< 
< 
1258,1260d1031
<           AtmOptics_AD%Scattering_Optical_Depth = AtmOptics_AD%Scattering_Optical_Depth + &
<                                                   RTSolution_AD(ln,m)%SOD
<           RTSolution_AD(ln,m)%SOD               = ZERO
1262c1033
<             CALL CRTM_AtmOptics_Combine_AD( AtmOptics, AtmOptics_AD, AOvar )
---
>             CALL CRTM_Combine_AtmOptics_AD( AtmOptics, AtmOptics_AD, AOvar )
1304,1316d1074
<           ! Adjoint of clear-sky AtmOptics copy
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             Error_Status = CRTM_AtmOptics_NoScatterCopy_AD( AtmOptics, AtmOptics_Clear_AD, AtmOptics_AD )
<             IF ( Error_Status /= SUCCESS ) THEN
<               WRITE( Message,'("Error computing CLEAR SKY AtmOptics_AD for ",a,&
<                      &", channel ",i0,", profile #",i0)' ) &
<                      TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
<           END IF
< 
< 
1335a1094,1101
>           ! Compute the adjoint of the total atmospheric transmittance
>           IF ( CRTM_No_Scattering(AtmOptics) .AND. SpcCoeff_IsMicrowaveSensor(SC(SensorIndex)) ) THEN
>             transmittance_AD = SfcOptics_AD%transmittance
>             SfcOptics_AD%transmittance = ZERO
>             CALL CRTM_Compute_Transmittance_AD(AtmOptics,transmittance_AD,AtmOptics_AD)
>           END IF
> 
> 
1347c1113
<         IF ( Opt%Apply_NLTE_Correction ) THEN
---
>         IF ( Apply_NLTE_Correction ) THEN
1364,1365d1129
<       END DO Sensor_Loop
< 
1367,1368c1131,1136
<       ! Adjoint of average surface skin temperature for multi-surface types
<       CALL CRTM_Compute_SurfaceT_AD( Surface(m), SfcOptics_AD, Surface_AD(m) )
---
>         ! Deallocate local sensor dependent data structures
>         ! ...RTV structure
>         IF ( RTV_Associated(RTV) ) CALL RTV_Destroy(RTV)
>         ! ...Predictor structures
>         CALL CRTM_Predictor_Destroy( Predictor )
>         CALL CRTM_Predictor_Destroy( Predictor_AD )
1369a1138
>       END DO Sensor_Loop
1371,1372d1139
<       ! Adjoint of cloud cover setup
<       IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
1374,1387c1141,1145
<         ! Post process the CLEAR sky structures for fractional cloud coverage
<         ! ...Clear sky SfcOptics
<         CALL CRTM_Compute_SurfaceT_AD( Surface(m), SfcOptics_Clear_AD, Surface_AD(m) )
<         ! ...Clear sky atmosphere
<         Error_Status = CRTM_Atmosphere_ClearSkyCopy_AD(Atm, Atm_Clear_AD, Atm_AD)
<         IF ( Error_Status /= SUCCESS ) THEN
<           Error_status = FAILURE
<           WRITE( Message,'("Error copying CLEAR SKY adjoint Atmosphere structure for profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
< 
<         ! Adjoint of the cloud coverage
<         Error_Status = CloudCover_AD%Compute_CloudCover_AD(CloudCover, atm, atm_AD)
---
>       ! Postprocess some input data
>       ! ...Adjoint of average surface skin temperature for multi-surface types
>       CALL CRTM_Compute_SurfaceT_AD( Surface(m), SfcOptics_AD, Surface_AD(m) )
>       ! ...Adjoint of the atmosphere layer addition
>       Error_Status = CRTM_Atmosphere_AddLayers_AD( Atmosphere(m), Atm_AD, Atmosphere_AD(m) )
1390c1148
<           WRITE( Message,'("Error computing ADJOINT cloud cover for profile #",i0)' ) m
---
>         WRITE( Message,'("Error adding AD extra layers to profile #",i0)' ) m
1394d1151
<       END IF
1397,1404c1154,1157
<       ! Adjoint of the atmosphere layer addition
<       Error_Status = CRTM_Atmosphere_AddLayers_AD( Atmosphere(m), Atm_AD, Atmosphere_AD(m) )
<       IF ( Error_Status /= SUCCESS ) THEN
<         Error_Status = FAILURE
<         WRITE( Message,'("Error adding ADJOINT extra layers to profile #",i0)' ) m
<         CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<         RETURN
<       END IF
---
>       ! Deallocate local sensor independent data structures
>       ! ...Atmospheric optics
>       CALL CRTM_AtmOptics_Destroy( AtmOptics )
>       CALL CRTM_AtmOptics_Destroy( AtmOptics_AD )
1409,1415c1162
<     ! Clean up
<     CALL CRTM_Predictor_Destroy( Predictor )
<     CALL CRTM_Predictor_Destroy( Predictor_AD )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics_AD )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear_AD )
---
>     ! Destroy any remaining structures
1418,1420d1164
<     CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear )
<     CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear_AD )
<     CALL CRTM_Atmosphere_Destroy( Atm )
1422,1509c1166
<     CALL CRTM_Atmosphere_Destroy( Atm_Clear )
<     CALL CRTM_Atmosphere_Destroy( Atm_Clear_AD )
<     ! ...Internal variables
<     CALL AOvar_Destroy( AOvar )
<     CALL CSvar_Destroy( CSvar )
<     CALL ASvar_Destroy( ASvar )
<     CALL RTV_Destroy( RTV )
< 
< 
< CONTAINS
< 
< 
<     ! ----------------------------------------------------------------
<     ! Local subroutine to post-process the FORWARD radiance, as it is
<     ! the same for all-sky and fractional clear-sky cases.
<     !
<     !   1. Apply non-LTE correction to radiance
<     !   2. Convert radiance to brightness temperature
<     !   3. Apply antenna correction to brightness temperature
<     ! ----------------------------------------------------------------
< 
<     SUBROUTINE Post_Process_RTSolution(rts)
<       TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: rts
< 
<       ! Compute non-LTE correction to radiance if required
<       IF ( Opt%Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
<         CALL Compute_NLTE_Correction( &
<                SC(SensorIndex)%NC, &  ! Input
<                ChannelIndex      , &  ! Input
<                NLTE_Predictor    , &  ! Input
<                rts%Radiance        )  ! In/Output
<       END IF
<       ! Convert the radiance to brightness temperature
<       CALL CRTM_Planck_Temperature( &
<              SensorIndex               , & ! Input
<              ChannelIndex              , & ! Input
<              rts%Radiance              , & ! Input
<              rts%Brightness_Temperature  ) ! Output
<       ! Compute Antenna correction to brightness temperature if required
<       IF ( compute_antenna_correction ) THEN
<         CALL CRTM_Compute_AntCorr( &
<                GeometryInfo, &  ! Input
<                SensorIndex , &  ! Input
<                ChannelIndex, &  ! Input
<                rts           )  ! Output
<       END IF
< 
<     END SUBROUTINE Post_Process_RTSolution
< 
< 
<     ! ----------------------------------------------------------------
<     ! Local subroutine to pre-process the ADJOINT radiance, as it is
<     ! the same for all-sky and fractional clear-sky cases.
<     !
<     !   1. Apply adjoint antenna correction to brightness temperatures
<     !   2. Convert adjoint radiances to brightness temperatures
<     !   3. Apply adjoint non-LTE correction to radiances
<     ! ----------------------------------------------------------------
< 
<     SUBROUTINE Pre_Process_RTSolution_AD(rts, rts_AD)
<       TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: rts, rts_AD
< 
<       ! Compute adjoint antenna correction to brightness temperature if required
<       IF ( compute_antenna_correction ) THEN
<         CALL CRTM_Compute_AntCorr_AD( &
<                GeometryInfo, &  ! Input
<                SensorIndex , &  ! Input
<                ChannelIndex, &  ! Input
<                rts_AD        )  ! Output
<       END IF
<       ! Compute the Planck temperature adjoint
<       CALL CRTM_Planck_Temperature_AD( &
<              SensorIndex                  , & ! Input
<              ChannelIndex                 , & ! Input
<              rts%Radiance                 , & ! Input
<              rts_AD%Brightness_Temperature, & ! Input
<              rts_AD%Radiance                ) ! Output
<       rts_AD%Brightness_Temperature = ZERO
<       ! Compute non-LTE correction adjoint if required
<       IF ( Opt%Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
<         CALL Compute_NLTE_Correction_AD( &
<                SC(SensorIndex)%NC, &  ! Input
<                ChannelIndex      , &  ! Input
<                rts_AD%Radiance   , &  ! Input
<                NLTE_Predictor_AD   )  ! Output
<       END IF
< 
<     END SUBROUTINE Pre_Process_RTSolution_AD
---
>     CALL CRTM_Atmosphere_Destroy( Atm )
diff -w ./CRTM_AerosolCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_AerosolCoeff.f90
diff -w ./CRTM_AerosolScatter.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_AerosolScatter.f90
diff -w ./CRTM_Aerosol_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Aerosol_Define.f90
57d56
<   PUBLIC :: OPERATOR(/=)
86,89d84
<   INTERFACE OPERATOR(/=)
<     MODULE PROCEDURE CRTM_Aerosol_NotEqual
<   END INTERFACE OPERATOR(/=)
< 
203,204c198
<    !ALLOCATE( list(0:N_VALID_AEROSOL_CATEGORIES), STAT=alloc_stat, ERRMSG=alloc_msg )
<     ALLOCATE( list(0:N_VALID_AEROSOL_CATEGORIES), STAT=alloc_stat )
---
>     ALLOCATE( list(0:N_VALID_AEROSOL_CATEGORIES), STAT=alloc_stat, ERRMSG=alloc_msg )
708,709c702,704
<     ! Check the object association status
<     IF ( CRTM_Aerosol_Associated(x) .NEQV. CRTM_Aerosol_Associated(y) ) RETURN
---
>     ! Check the structure association status
>     IF ( (.NOT. CRTM_Aerosol_Associated(x)) .OR. &
>          (.NOT. CRTM_Aerosol_Associated(y)) ) RETURN
711,712c706
<     ! Check contents
<     ! ...Dimensions and scalars
---
>     ! Check scalars
715,716c709,710
<     ! ...Arrays
<     IF ( CRTM_Aerosol_Associated(x) .AND. CRTM_Aerosol_Associated(y) ) THEN
---
> 
>     ! Check arrays
719d712
<     END IF
721c714
<     ! If we get here, the objects are comparable
---
>     ! If we get here, the structures are comparable
1304,1305c1297,1299
<     ! Check the object association status
<     IF ( CRTM_Aerosol_Associated(x) .NEQV. CRTM_Aerosol_Associated(y) ) RETURN
---
>     ! Check the structure association status
>     IF ( (.NOT. CRTM_Aerosol_Associated(x)) .OR. &
>          (.NOT. CRTM_Aerosol_Associated(y))      ) RETURN
1311d1304
<     IF ( CRTM_Aerosol_Associated(x) .AND. CRTM_Aerosol_Associated(y) ) THEN
1313,1317c1306,1307
<       IF ( .NOT. (ALL(x%Effective_Radius(1:n) .EqualTo. y%Effective_Radius(1:n) ) .AND. &
<                   ALL(x%Concentration(1:n)    .EqualTo. y%Concentration(1:n)    )) ) RETURN
<     END IF
< 
<     ! If we get here, then...
---
>     IF ( ALL(x%Effective_Radius(1:n) .EqualTo. y%Effective_Radius(1:n) ) .AND. &
>          ALL(x%Concentration(1:n)    .EqualTo. y%Concentration(1:n)    )       ) &
1323,1364d1312
< !------------------------------------------------------------------------------
< !
< ! NAME:
< !   CRTM_Aerosol_NotEqual
< !
< ! PURPOSE:
< !   Elemental function to test the inequality of two CRTM Aerosol objects.
< !   Used in OPERATOR(/=) interface block.
< !
< !   This function is syntactic sugar.
< !
< ! CALLING SEQUENCE:
< !   not_equal = CRTM_Aerosol_NotEqual( x, y )
< !
< !     or
< !
< !   IF ( x /= y ) THEN
< !     ...
< !   END IF
< !
< ! OBJECTS:
< !   x, y:          Two CRTM Aerosol objects to be compared.
< !                  UNITS:      N/A
< !                  TYPE:       CRTM_Aerosol_type
< !                  DIMENSION:  Scalar or any rank
< !                  ATTRIBUTES: INTENT(IN)
< !
< ! FUNCTION RESULT:
< !   not_equal:     Logical value indicating whether the inputs are not equal.
< !                  UNITS:      N/A
< !                  TYPE:       LOGICAL
< !                  DIMENSION:  Same as inputs.
< !
< !------------------------------------------------------------------------------
< 
<   ELEMENTAL FUNCTION CRTM_Aerosol_NotEqual( x, y ) RESULT( not_equal )
<     TYPE(CRTM_Aerosol_type), INTENT(IN) :: x, y
<     LOGICAL :: not_equal
<     not_equal = .NOT. (x == y)
<   END FUNCTION CRTM_Aerosol_NotEqual
< 
< 
diff -w ./CRTM_AncillaryInput_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_AncillaryInput_Define.f90
diff -w ./CRTM_AntennaCorrection.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_AntennaCorrection.f90
diff -w ./CRTM_AtmAbsorption.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_AtmAbsorption.f90
diff -w ./CRTM_AtmOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_AtmOptics.f90
25,28c25
<   USE CRTM_AtmOptics_Define, ONLY: CRTM_AtmOptics_type      , &
<                                    CRTM_AtmOptics_Associated, &
<                                    CRTM_AtmOptics_Create    , &
<                                    CRTM_AtmOptics_Zero
---
>   USE CRTM_AtmOptics_Define, ONLY: CRTM_AtmOptics_type
47d43
< 
50d45
< 
54,61c49,51
< 
<   PUBLIC :: CRTM_AtmOptics_Combine
<   PUBLIC :: CRTM_AtmOptics_Combine_TL
<   PUBLIC :: CRTM_AtmOptics_Combine_AD
< 
<   PUBLIC :: CRTM_AtmOptics_NoScatterCopy
<   PUBLIC :: CRTM_AtmOptics_NoScatterCopy_TL
<   PUBLIC :: CRTM_AtmOptics_NoScatterCopy_AD
---
>   PUBLIC :: CRTM_Combine_AtmOptics
>   PUBLIC :: CRTM_Combine_AtmOptics_TL
>   PUBLIC :: CRTM_Combine_AtmOptics_AD
68,69d57
<   ! Message string length
<   INTEGER, PARAMETER :: ML = 256
336c324
< !       CRTM_AtmOptics_Combine
---
> !       CRTM_Combine_AtmOptics
343c331
< !       CALL CRTM_AtmOptics_Combine( AtmOptics, &
---
> !       CALL CRTM_Combine_AtmOptics( AtmOptics, &
365c353
<   SUBROUTINE CRTM_AtmOptics_Combine( &
---
>   SUBROUTINE CRTM_Combine_AtmOptics( &
372c360
<     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_Combine'
---
>     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics'
428c416
<   END SUBROUTINE CRTM_AtmOptics_Combine
---
>   END SUBROUTINE CRTM_Combine_AtmOptics
436c424
< !       CRTM_AtmOptics_Combine_TL
---
> !       CRTM_Combine_AtmOptics_TL
443c431
< !       CALL CRTM_AtmOptics_Combine_TL( AtmOptics   , &
---
> !       CALL CRTM_Combine_AtmOptics_TL( AtmOptics   , &
472c460
<   SUBROUTINE CRTM_AtmOptics_Combine_TL( &
---
>   SUBROUTINE CRTM_Combine_AtmOptics_TL( &
481c469
<     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_Combine_TL'
---
>     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics_TL'
576c564
<   END SUBROUTINE CRTM_AtmOptics_Combine_TL
---
>   END SUBROUTINE CRTM_Combine_AtmOptics_TL
584c572
< !       CRTM_AtmOptics_Combine_AD
---
> !       CRTM_Combine_AtmOptics_AD
591c579
< !       CALL CRTM_AtmOptics_Combine_AD( AtmOptics,    &
---
> !       CALL CRTM_Combine_AtmOptics_AD( AtmOptics,    &
624c612
<   SUBROUTINE CRTM_AtmOptics_Combine_AD( &
---
>   SUBROUTINE CRTM_Combine_AtmOptics_AD( &
633c621
<     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_Combine_AD'
---
>     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics_AD'
738,1040c726
<   END SUBROUTINE CRTM_AtmOptics_Combine_AD
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !       CRTM_AtmOptics_NoScatterCopy
< !
< ! PURPOSE:
< !       Function to copy an instance of a CRTM AtmOptics object
< !       but without the scattering information included.
< !
< ! CALLING SEQUENCE:
< !       Error_Status = CRTM_AtmOptics_NoScatterCopy( AtmOptics, AtmOptics_Clear )
< !
< ! INPUTS:
< !       AtmOptics:       AtmOptics object to copy
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_AtmOptics_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(IN)
< !
< ! OUTPUTS:
< !       AtmOptics_Clear: Copy of the input AtmOptics object but without the
< !                        scattering information.
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_AtmOptics_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(OUT)
< !
< ! FUNCTION RESULT:
< !       Error_Status:    The return value is an integer defining the error status.
< !                        The error codes are defined in the Message_Handler module.
< !                        If == SUCCESS the operation was successful
< !                           == FAILURE an error occurred
< !                        UNITS:      N/A
< !                        TYPE:       INTEGER
< !                        DIMENSION:  Scalar
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   FUNCTION CRTM_AtmOptics_NoScatterCopy( ao, ao_clear ) RESULT( err_stat )
<     ! Arguments
<     TYPE(CRTM_AtmOptics_type), INTENT(IN)  :: ao
<     TYPE(CRTM_AtmOptics_type), INTENT(OUT) :: ao_clear
<     ! Function result
<     INTEGER :: err_stat
<     ! Local parameters
<     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_NoScatterCopy'
<     ! Local variables
<     CHARACTER(ML) :: err_msg
< 
< 
<     ! Set up
<     err_stat = SUCCESS
<     ! ...Check input
<     IF ( .NOT. CRTM_AtmOptics_Associated(ao) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input AtmOptics structure not allocated'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
< 
< 
<     ! Create the output structure
<     CALL CRTM_AtmOptics_Create( ao_clear   , &
<                                 ao%n_Layers, &
<                                 0          , &  ! No Legendre terms
<                                 0            )  ! No phase element terms
<     IF ( .NOT. CRTM_AtmOptics_Associated(ao_clear) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Error allocating output Clear-Sky AtmOptics structure'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
< 
< 
<     ! Set/Copy over the clear-sky data
<     ao_clear%Include_Scattering = .FALSE.
<     ao_clear%Optical_Depth      = ao%Optical_Depth(1:ao%n_Layers)
< 
<   END FUNCTION CRTM_AtmOptics_NoScatterCopy
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !       CRTM_AtmOptics_NoScatterCopy_TL
< !
< ! PURPOSE:
< !       Function to copy an instance of a tangent-linear CRTM AtmOptics object
< !       but without the scattering information included.
< !!
< ! CALLING SEQUENCE:
< !       Error_Status = CRTM_AtmOptics_NoScatterCopy_TL( ao, ao_TL, ao_clear_TL )
< !
< ! INPUTS:
< !       ao:              Forward AtmOptics object for consistency checking
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_AtmOptics_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(IN)
< !
< !       ao_TL:           Tangent-linear AtmOptics object to copy. This object
< !                        must be the tangent-linear equivalent of the input
< !                        forward AtmOptics object.
< !                        This
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_AtmOptics_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(IN)
< !
< ! OUTPUTS:
< !       ao_clear_TL:     Copy of the input AtmOptics tangent-linear object but
< !                        without scattering information.
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_AtmOptics_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(OUT)
< !
< ! FUNCTION RESULT:
< !       Error_Status:    The return value is an integer defining the error status.
< !                        The error codes are defined in the Message_Handler module.
< !                        If == SUCCESS the operation was successful
< !                           == FAILURE an error occurred
< !                        UNITS:      N/A
< !                        TYPE:       INTEGER
< !                        DIMENSION:  Scalar
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   FUNCTION CRTM_AtmOptics_NoScatterCopy_TL( &
<     ao         , &  ! FWD input
<     ao_TL      , &  ! TL  input
<     ao_clear_TL) &  ! TL  output
<   RESULT( err_stat )
<     ! Arguments
<     TYPE(CRTM_AtmOptics_type), INTENT(IN)  :: ao
<     TYPE(CRTM_AtmOptics_type), INTENT(IN)  :: ao_TL
<     TYPE(CRTM_AtmOptics_type), INTENT(OUT) :: ao_clear_TL
<     ! Function result
<     INTEGER :: err_stat
<     ! Local parameters
<     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_NoScatterCopy_TL'
<     ! Local variables
<     CHARACTER(ML) :: err_msg
< 
< 
<     ! Set up
<     err_stat = SUCCESS
<     ! ...Check input allocation
<     IF ( .NOT. CRTM_AtmOptics_Associated(ao   ) .OR. &
<          .NOT. CRTM_AtmOptics_Associated(ao_TL) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input AtmOptics structures not allocated'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
<     ! ...Dimension consistency
<     IF ( (ao%n_Layers         /= ao_TL%n_Layers        ) .OR. &
<          (ao%n_Legendre_Terms /= ao_TL%n_Legendre_Terms) .OR. &
<          (ao%n_Phase_Elements /= ao_TL%n_Phase_Elements) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input AtmOptics structures have incongruent dimensions'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
< 
< 
<     ! Create the output structure
<     CALL CRTM_AtmOptics_Create( ao_clear_TL   , &
<                                 ao_TL%n_Layers, &
<                                 0             , &  ! No Legendre terms
<                                 0               )  ! No phase element terms
<     IF ( .NOT. CRTM_AtmOptics_Associated(ao_clear_TL) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Error allocating output Clear-Sky AtmOptics structure'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
< 
< 
<     ! Set/Copy over the clear-sky data
<     ao_clear_TL%Include_Scattering = .FALSE.
<     ao_clear_TL%Optical_Depth      = ao_TL%Optical_Depth(1:ao_TL%n_Layers)
< 
<   END FUNCTION CRTM_AtmOptics_NoScatterCopy_TL
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !       CRTM_AtmOptics_NoScatterCopy_AD
< !
< ! PURPOSE:
< !       Function to perform the adjoint copy of an instance of the CRTM
< !       AtmOptics object without the scattering information included.
< !
< ! CALLING SEQUENCE:
< !       Error_Status = CRTM_AtmOptics_NoScatterCopy_AD( ao, ao_clear_AD, ao_AD )
< !
< ! INPUTS:
< !       ao:              AtmOptics object for consistency checking
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_AtmOptics_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(IN)
< !
< !       ao_clear_AD:     Adjoint Clear-Sky AtmOptics structure to copy
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_AtmOptics_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(IN)
< !
< ! OUTPUTS:
< !       ao_AD:           Adjoint copy of the input AtmOptics. This object
< !                        must be the adjoint equivalent of the input
< !                        forward AtmOptics object.
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_AtmOptics_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(OUT)
< !
< ! FUNCTION RESULT:
< !       Error_Status:    The return value is an integer defining the error status.
< !                        The error codes are defined in the Message_Handler module.
< !                        If == SUCCESS the operation was successful
< !                           == FAILURE an error occurred
< !                        UNITS:      N/A
< !                        TYPE:       INTEGER
< !                        DIMENSION:  Scalar
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   FUNCTION CRTM_AtmOptics_NoScatterCopy_AD( &
<     ao         , &  ! FWD input
<     ao_clear_AD, &  ! AD  input
<     ao_AD      ) &  ! AD  output
<   RESULT( err_stat )
<     ! Arguments
<     TYPE(CRTM_AtmOptics_type), INTENT(IN)     :: ao
<     TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: ao_clear_AD
<     TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: ao_AD
<     ! Function result
<     INTEGER :: err_stat
<     ! Local parameters
<     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_NoScatterCopy_AD'
<     ! Local variables
<     CHARACTER(ML) :: err_msg
<     INTEGER :: k
< 
< 
<     ! Set up
<     err_stat = SUCCESS
<     ! ...Check input allocation
<     IF ( .NOT. CRTM_AtmOptics_Associated(ao         ) .OR. &
<          .NOT. CRTM_AtmOptics_Associated(ao_clear_AD) .OR. &
<          .NOT. CRTM_AtmOptics_Associated(ao_AD      ) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input AtmOptics structures not allocated'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
<     ! ...Dimensional consistency
<     IF ( (ao%n_Layers         /= ao_AD%n_Layers        ) .OR. &
<          (ao%n_Legendre_Terms /= ao_AD%n_Legendre_Terms) .OR. &
<          (ao%n_Phase_Elements /= ao_AD%n_Phase_Elements) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input AtmOptics and AtmOptics_AD structures have incongruent dimensions'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
<     IF ( ao_clear_AD%n_Layers /= ao_AD%n_Layers ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input AtmOptics_Clear_AD structure has incongruent dimensions'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
<     ! ...Non-layer dependent data consistency
<     IF ( (ao%Include_Scattering .NEQV. ao_AD%Include_Scattering) .OR. &
<          ao_clear_AD%Include_Scattering ) THEN
<       err_stat = FAILURE
<       err_msg = 'AtmOptics structures have incongruent Scattering flags'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
< 
< 
<     ! Adjoint copy of data
<     k = ao%n_Layers
<     ao_AD%Optical_Depth(1:k) = ao_AD%Optical_Depth(1:k) + ao_clear_AD%Optical_Depth(1:k)      
< 
< 
<     ! Zero the clear result, as it has no more impact
<     CALL CRTM_AtmOptics_Zero( ao_clear_AD )
< 
<   END FUNCTION CRTM_AtmOptics_NoScatterCopy_AD
---
>   END SUBROUTINE CRTM_Combine_AtmOptics_AD
diff -w ./CRTM_AtmOptics_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_AtmOptics_Define.f90
40d39
<   PUBLIC :: OPERATOR(+)
64,67d62
<   INTERFACE OPERATOR(+)
<     MODULE PROCEDURE CRTM_AtmOptics_Add
<   END INTERFACE OPERATOR(+)
< 
89a85
>   INTEGER,  PARAMETER :: SL =  80 ! String length
157,159c153,157
< !                   status of the object components.
< !                     .TRUE.  - if the array components are allocated.
< !                     .FALSE. - if the array components are not allocated.
---
> !                   status of the NLTE members.
> !                    .TRUE.  - if ANY of the AtmOptics allocatable members
> !                              are in use.
> !                    .FALSE. - if ALL of the AtmOptics allocatable members
> !                              are not in use.
198a197,202
>     self%n_Layers           = 0
>     self%n_Legendre_Terms   = 0
>     self%n_Phase_Elements   = 0
>     self%Max_Layers         = 0
>     self%Max_Legendre_Terms = 0
>     self%Max_Phase_Elements = 0
233a238
> !                           Must be > 0
239a245
> !                           Must be > 0
268c274,276
<     IF ( n_Layers < 1 ) THEN
---
>     IF ( n_Layers         < 1 .OR. &
>          n_Legendre_Terms < 1 .OR. &
>          n_Phase_Elements < 1 ) THEN
1404d1411
<     INTEGER :: k, ip, ic
1410c1417,1418
<     IF ( CRTM_AtmOptics_Associated(x) .NEQV. CRTM_AtmOptics_Associated(y) ) RETURN
---
>     IF ( (.NOT. CRTM_AtmOptics_Associated(x)) .OR. &
>          (.NOT. CRTM_AtmOptics_Associated(y))      ) RETURN
1419,1421c1427,1428
<     ! ...Scalars
<     IF ( .NOT. (x%Scattering_Optical_Depth .EqualTo. y%Scattering_Optical_Depth) ) RETURN
<     ! ...Arrays
---
>     ! ...Scalar data
>     IF ( x%Scattering_Optical_Depth .EqualTo. y%Scattering_Optical_Depth ) &
1424,1439c1431,1437
<     IF ( CRTM_AtmOptics_Associated(x) .AND. CRTM_AtmOptics_Associated(y) ) THEN
<       k  = x%n_Layers
<       ip = x%n_Phase_Elements
<       ic = x%n_Legendre_Terms
<       k = x%n_Layers
<       IF ( .NOT. (ALL(x%Optical_Depth(1:k)         .EqualTo. y%Optical_Depth(1:k)        ) .AND. &
<                   ALL(x%Single_Scatter_Albedo(1:k) .EqualTo. y%Single_Scatter_Albedo(1:k)) .AND. &
<                   ALL(x%Asymmetry_Factor(1:k)      .EqualTo. y%Asymmetry_Factor(1:k)     ) .AND. &
<                   ALL(x%Delta_Truncation(1:k)      .EqualTo. y%Delta_Truncation(1:k)     ) .AND. &
<                   ALL(x%Phase_Coefficient(0:ic, 1:ip, 1:k) .EqualTo. &
<                       y%Phase_Coefficient(0:ic, 1:ip, 1:k))) ) RETURN
<     END IF
< 
< 
<     ! If we get here, then...
<     is_equal = .TRUE.
---
>     is_equal = is_equal .AND. &
>                ALL(x%Optical_Depth(1:x%n_Layers)         .EqualTo. y%Optical_Depth(1:y%n_Layers)        ) .AND. &
>                ALL(x%Single_Scatter_Albedo(1:x%n_Layers) .EqualTo. y%Single_Scatter_Albedo(1:y%n_Layers)) .AND. &
>                ALL(x%Asymmetry_Factor(1:x%n_Layers)      .EqualTo. y%Asymmetry_Factor(1:y%n_Layers)     ) .AND. &
>                ALL(x%Delta_Truncation(1:x%n_Layers)      .EqualTo. y%Delta_Truncation(1:y%n_Layers)     ) .AND. &
>                ALL(x%Phase_Coefficient(0:x%n_Legendre_Terms, 1:x%n_Phase_Elements, 1:x%n_Layers) .EqualTo. &
>                    y%Phase_Coefficient(0:y%n_Legendre_Terms, 1:y%n_Phase_Elements, 1:y%n_Layers) )
1447,1511d1444
< !       CRTM_AtmOptics_Add
< !
< ! PURPOSE:
< !       Pure function to add two CRTM AtmOptics objects.
< !       Used in OPERATOR(+) interface block.
< !
< ! CALLING SEQUENCE:
< !       aosum = CRTM_AtmOptics_Add( ao1, ao2 )
< !
< !         or
< !
< !       aosum = ao1 + ao2
< !
< !
< ! INPUTS:
< !       ao1, ao2: The AtmOptics objects to add.
< !                 UNITS:      N/A
< !                 TYPE:       CRTM_AtmOptics_type
< !                 DIMENSION:  Scalar
< !                 ATTRIBUTES: INTENT(IN OUT)
< !
< ! RESULT:
< !       aosum:    AtmOptics object containing the added components.
< !                 UNITS:      N/A
< !                 TYPE:       CRTM_AtmOptics_type
< !                 DIMENSION:  Scalar
< !
< !--------------------------------------------------------------------------------
< 
<   ELEMENTAL FUNCTION CRTM_AtmOptics_Add( ao1, ao2 ) RESULT( aosum )
<     TYPE(CRTM_AtmOptics_type), INTENT(IN) :: ao1, ao2
<     TYPE(CRTM_AtmOptics_type) :: aosum
<     INTEGER :: ic, ip, k
< 
<     ! Check input
<     ! ...If input structures not allocated, do nothing
<     IF ( (.NOT. CRTM_AtmOptics_Associated(ao1)) .OR. &
<          (.NOT. CRTM_AtmOptics_Associated(ao2))      ) RETURN
<     ! ...If input structure for different sizes, do nothing
<     IF ( ao1%n_Layers         /= ao2%n_Layers         .OR. &
<          ao1%n_Legendre_Terms /= ao2%n_Legendre_Terms .OR. &
<          ao1%n_Phase_Elements /= ao2%n_Phase_Elements ) RETURN
< 
<     ! Copy the first structure
<     aosum = ao1
< 
<     ! And add its components to the second one
<     ! ...The scalar values
<     aosum%Scattering_Optical_Depth = aosum%Scattering_Optical_Depth + ao2%Scattering_Optical_Depth
<     ! ...The arrays
<     k  = aosum%n_Layers
<     ip = aosum%n_Phase_Elements
<     ic = aosum%n_Legendre_Terms
<     aosum%Optical_Depth(1:k)               = aosum%Optical_Depth(1:k)               + ao2%Optical_Depth(1:k)
<     aosum%Single_Scatter_Albedo(1:k)       = aosum%Single_Scatter_Albedo(1:k)       + ao2%Single_Scatter_Albedo(1:k)
<     aosum%Asymmetry_Factor(1:k)            = aosum%Asymmetry_Factor(1:k)            + ao2%Asymmetry_Factor(1:k)
<     aosum%Delta_Truncation(1:k)            = aosum%Delta_Truncation(1:k)            + ao2%Delta_Truncation(1:k)
<     aosum%Phase_Coefficient(0:ic,1:ip,1:k) = aosum%Phase_Coefficient(0:ic,1:ip,1:k) + ao2%Phase_Coefficient(0:ic,1:ip,1:k)
< 
<   END FUNCTION CRTM_AtmOptics_Add
< 
< 
< !--------------------------------------------------------------------------------
< !
< ! NAME:
1553a1487,1489
>     ! ...If input structure for different scattering setup, do nothing
>     IF ( (ao1%Include_Scattering .NEQV. ao2%Include_Scattering ) .AND. &
>          (ao1%lOffset             /=    ao2%lOffset            ) ) RETURN
diff -w ./CRTM_Atmosphere.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Atmosphere.f90
22,23c22
<                                     MINIMUM_ABSORBER_AMOUNT, &
<                                     WATER_CONTENT_THRESHOLD
---
>                                     MINIMUM_ABSORBER_AMOUNT
49,53d47
<   PUBLIC :: CRTM_Atmosphere_Coverage
<   PUBLIC :: CRTM_Atmosphere_IsClear
<   PUBLIC :: CRTM_Atmosphere_IsFractional
<   PUBLIC :: CRTM_Atmosphere_IsOvercast
<   
57,60d50
<   
<   PUBLIC :: CRTM_Atmosphere_ClearSkyCopy
<   PUBLIC :: CRTM_Atmosphere_ClearSkyCopy_TL
<   PUBLIC :: CRTM_Atmosphere_ClearSkyCopy_AD
73c63,64
<   CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
---
>   ! RCS Id for the module
>   CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
77,85d67
<   ! The cloud coverage type
<   INTEGER, PARAMETER :: CLEAR      = -1
<   INTEGER, PARAMETER :: FRACTIONAL =  0
<   INTEGER, PARAMETER :: OVERCAST   =  1
< !  CHARACTER(*), PARAMETER, DIMENSION( -1:1 ) :: &
< !    CLOUD_COVERAGE_NAME = [ 'Clear sky          ', &
< !                            'Fractional coverage', &
< !                            'Overcast           '  ]
<   
98,197d79
<   FUNCTION CRTM_Atmosphere_IsClear(coverage_flag) RESULT(is_clear)
<     INTEGER, INTENT(IN) :: coverage_flag
<     LOGICAL :: is_clear
<     is_clear = coverage_flag == CLEAR
<   END FUNCTION CRTM_Atmosphere_IsClear
<   
<   FUNCTION CRTM_Atmosphere_IsFractional(coverage_flag) RESULT(is_fractional)
<     INTEGER, INTENT(IN) :: coverage_flag
<     LOGICAL :: is_fractional
<     is_fractional = coverage_flag == FRACTIONAL
<   END FUNCTION CRTM_Atmosphere_IsFractional
< 
<   FUNCTION CRTM_Atmosphere_IsOvercast(coverage_flag) RESULT(is_overcast)
<     INTEGER, INTENT(IN) :: coverage_flag
<     LOGICAL :: is_overcast
<     is_overcast = coverage_flag == OVERCAST
<   END FUNCTION CRTM_Atmosphere_IsOvercast
<   
<   
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !   CRTM_Atmosphere_Coverage
< !
< ! PURPOSE:
< !   Function to determine the cloud coverage type for an input
< !   atmosphere.
< !
< ! CALLING SEQUENCE:
< !   coverage_flag = CRTM_Atmosphere_Coverage( atm )  ! Input
< !
< ! INPUTS:
< !   atm:            Atmosphere structure for which the coverage type is
< !                   to be determined.
< !                   UNITS:      N/A
< !                   TYPE:       CRTM_Atmosphere_type
< !                   DIMENSION:  Scalar
< !                   ATTRIBUTES: INTENT(IN)
< !
< ! FUNCTION RESULT:
< !   coverage_flag:  An integer defining the coverage type. Valid
< !                   parameterised values are:
< !                     CLEAR     
< !                     FRACTIONAL
< !                     OVERCAST  
< !                   UNITS:      N/A
< !                   TYPE:       INTEGER
< !                   DIMENSION:  Scalar
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
<    FUNCTION CRTM_Atmosphere_Coverage(atm) RESULT(coverage_flag)
<     ! Arguments
<     TYPE(CRTM_Atmosphere_type), INTENT(IN) :: atm
<     ! Function result
<     INTEGER :: coverage_flag
<     ! Local parameters
<     REAL(fp), PARAMETER :: MIN_COVERAGE_THRESHOLD = 1.0e-06_fp
<     REAL(fp), PARAMETER :: MAX_COVERAGE_THRESHOLD = ONE - MIN_COVERAGE_THRESHOLD
<     ! Local variables
<     LOGICAL :: cloudy_layer_mask(atm%n_Layers)
<     INTEGER :: idx(atm%n_Layers)
<     INTEGER :: n, nc, k
<     
<     ! Default clear
<     coverage_flag = CLEAR
<     IF ( atm%n_Clouds == 0 ) RETURN
<  
<     ! Check each cloud separately
<     Cloud_Loop: DO n = 1, atm%n_Clouds
<     
<       ! Determine if there are ANY cloudy layers
<       cloudy_layer_mask = atm%Cloud(n)%Water_Content > WATER_CONTENT_THRESHOLD
<       nc = COUNT(cloudy_layer_mask)
<       IF ( nc == 0 ) CYCLE Cloud_Loop
< 
<       ! Get the indices of those cloudy layers
<       idx(1:nc) = PACK([(k, k=1,atm%Cloud(n)%n_Layers)], cloudy_layer_mask)
< 
<       ! Check for ANY fractional coverage
<       ! ??? How to do this without the loop ???
<       DO k = 1, nc
< !       IF ( (atm%Cloud_Fraction(idx(k)) > MIN_COVERAGE_THRESHOLD) .AND. &
< !            (atm%Cloud_Fraction(idx(k)) < MAX_COVERAGE_THRESHOLD) ) THEN
<         IF ( (atm%Cloud_Fraction(idx(k)) > MIN_COVERAGE_THRESHOLD) ) THEN       
<           coverage_flag = FRACTIONAL
<           RETURN
<         END IF
<       END DO
< 
< !     ! Check for ALL totally clear or totally cloudy
< !     IF ( ALL(atm%Cloud_Fraction(idx(1:nc)) < MIN_COVERAGE_THRESHOLD) .OR. &
< !          ALL(atm%Cloud_Fraction(idx(1:nc)) > MAX_COVERAGE_THRESHOLD) ) coverage_flag = OVERCAST                  
< 
<     END DO Cloud_Loop
<     
<   END FUNCTION CRTM_Atmosphere_Coverage
< 
< 
361d242
<     Atm_Out%Cloud_Fraction(1:n) = ZERO
629,631d509
<     ! ...Cloud fraction data
<     Atm_In_AD%Cloud_Fraction(1:no) = Atm_In_AD%Cloud_Fraction(1:no) + &
<                                      Atm_Out_AD%Cloud_Fraction(n+1:nt)    
639,1011d516
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !       CRTM_Atmosphere_ClearSkyCopy
< !
< ! PURPOSE:
< !       Function to copy an instance of the CRTM Atmosphere object
< !       but without the clouds included.
< !
< ! CALLING SEQUENCE:
< !       Error_Status = CRTM_Atmosphere_ClearSkyCopy( Atm, Atm_Clear )
< !
< ! INPUTS:
< !       Atm:             Atmosphere structure to copy
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Atmosphere_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(IN)
< !
< ! OUTPUTS:
< !       Atm_Clear:       Copy of the input atmosphere but withut cloud information.
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Atmosphere_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(OUT)
< !
< ! FUNCTION RESULT:
< !       Error_Status:    The return value is an integer defining the error status.
< !                        The error codes are defined in the Message_Handler module.
< !                        If == SUCCESS the operation was successful
< !                           == FAILURE an error occurred
< !                        UNITS:      N/A
< !                        TYPE:       INTEGER
< !                        DIMENSION:  Scalar
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   FUNCTION CRTM_Atmosphere_ClearSkyCopy( atm, atm_clear ) RESULT( err_stat )
<     ! Arguments
<     TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: atm
<     TYPE(CRTM_Atmosphere_type), INTENT(OUT) :: atm_clear
<     ! Function result
<     INTEGER :: err_stat
<     ! Local parameters
<     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_ClearSkyCopy'
<     ! Local variables
<     CHARACTER(ML) :: err_msg
<     INTEGER :: i, k
< 
< 
<     ! Set up
<     err_stat = SUCCESS
<     ! ...Check input
<     IF ( .NOT. CRTM_Atmosphere_Associated(atm) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input Atmosphere structure not allocated'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
< 
< 
<     ! Create the output structure
<     CALL CRTM_Atmosphere_Create( atm_clear      , &
<                                  atm%n_Layers   , &
<                                  atm%n_Absorbers, &
<                                  0              , &  ! NO CLOUDS !
<                                  atm%n_Aerosols   )
<     IF ( .NOT. CRTM_Atmosphere_Associated(atm_clear) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Error allocating output Clear-Sky Atmosphere structure'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
< 
< 
<     ! Copy over data
<     ! ...Extra dimensions
<     atm_clear%n_Added_Layers = atm%n_Added_Layers
<     ! ...Layer independent data
<     atm_clear%Climatology    = atm%Climatology
<     atm_clear%Absorber_ID    = atm%Absorber_ID
<     atm_clear%Absorber_Units = atm%Absorber_Units
<     ! ...Layer dependent data
<     k = atm%n_Layers
<     atm_clear%Level_Pressure = atm%Level_Pressure(0:k)
<     atm_clear%Pressure       = atm%Pressure(1:k)
<     atm_clear%Temperature    = atm%Temperature(1:k)
<     atm_clear%Absorber       = atm%Absorber(1:k,:)
<     atm_clear%Cloud_Fraction = atm%Cloud_Fraction(1:k)
<     ! ...Aerosol components
<     IF ( atm%n_Aerosols > 0 ) THEN
<       DO i = 1, atm%n_Aerosols
<         atm_clear%Aerosol(i) = atm%Aerosol(i)
<       END DO
<     END IF
< 
<   END FUNCTION CRTM_Atmosphere_ClearSkyCopy
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !       CRTM_Atmosphere_ClearSkyCopy_TL
< !
< ! PURPOSE:
< !       Function to copy an instance of a tangent-linear CRTM Atmosphere object
< !       but without the clouds included.
< !!
< ! CALLING SEQUENCE:
< !       Error_Status = CRTM_Atmosphere_ClearSkyCopy_TL( Atm, Atm_TL, Atm_Clear_TL )
< !
< ! INPUTS:
< !       Atm:             Atmosphere object for consistency checking
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Atmosphere_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(IN)
< !
< !       Atm_TL:          Tangent-linear Atmosphere object to copy. This object
< !                        must be the tangent-linear equivalent of the input
< !                        forward Atm object.
< !                        This
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Atmosphere_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(IN)
< !
< ! OUTPUTS:
< !       Atm_Clear_TL:    Copy of the input atmosphere but withut cloud information.
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Atmosphere_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(OUT)
< !
< ! FUNCTION RESULT:
< !       Error_Status:    The return value is an integer defining the error status.
< !                        The error codes are defined in the Message_Handler module.
< !                        If == SUCCESS the operation was successful
< !                           == FAILURE an error occurred
< !                        UNITS:      N/A
< !                        TYPE:       INTEGER
< !                        DIMENSION:  Scalar
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   FUNCTION CRTM_Atmosphere_ClearSkyCopy_TL( &
<     atm         , &  ! FWD input
<     atm_TL      , &  ! TL  input
<     atm_clear_TL) &  ! TL  output
<   RESULT( err_stat )
<     ! Arguments
<     TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: atm
<     TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: atm_TL
<     TYPE(CRTM_Atmosphere_type), INTENT(OUT) :: atm_clear_TL
<     ! Function result
<     INTEGER :: err_stat
<     ! Local parameters
<     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_ClearSkyCopy_TL'
<     ! Local variables
<     CHARACTER(ML) :: err_msg
<     INTEGER :: i, k
< 
< 
<     ! Set up
<     err_stat = SUCCESS
<     ! ...Check input allocation
<     IF ( .NOT. CRTM_Atmosphere_Associated(atm   ) .OR. &
<          .NOT. CRTM_Atmosphere_Associated(atm_TL) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input Atmosphere structures not allocated'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
<     ! ...Dimension consistency
<     IF ( (atm%n_Layers       /= atm_TL%n_Layers      ) .OR. &
<          (atm%n_Absorbers    /= atm_TL%n_Absorbers   ) .OR. &
<          (atm%n_Clouds       /= atm_TL%n_Clouds      ) .OR. &
<          (atm%n_Aerosols     /= atm_TL%n_Aerosols    ) .OR. &
<          (atm%n_Added_Layers /= atm_TL%n_Added_Layers) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input Atmosphere structures have incongruent dimensions'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
<     ! ...Non-layer dependent data consistency
<     IF (    (atm%Climatology    /= atm_TL%Climatology   ) .OR. &
<          ANY(atm%Absorber_ID    /= atm_TL%Absorber_ID   ) .OR. &
<          ANY(atm%Absorber_Units /= atm_TL%Absorber_Units) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input Atmosphere structures have incongruent layer independent data'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
< 
< 
<     ! Create the output structure
<     CALL CRTM_Atmosphere_Create( atm_clear_TL      , &
<                                  atm_TL%n_Layers   , &
<                                  atm_TL%n_Absorbers, &
<                                  0                 , &  ! NO CLOUDS !
<                                  atm_TL%n_Aerosols   )
<     IF ( .NOT. CRTM_Atmosphere_Associated(atm_clear_TL) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Error allocating output Clear-Sky Atmosphere structure'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
< 
< 
<     ! Copy over data
<     ! ...Extra dimensions
<     atm_clear_TL%n_Added_Layers = atm_TL%n_Added_Layers
<     ! ...Layer independent data
<     atm_clear_TL%Climatology    = atm_TL%Climatology
<     atm_clear_TL%Absorber_ID    = atm_TL%Absorber_ID
<     atm_clear_TL%Absorber_Units = atm_TL%Absorber_Units
<     ! ...Layer dependent data
<     k = atm%n_Layers
<     atm_clear_TL%Level_Pressure = atm_TL%Level_Pressure(0:k)
<     atm_clear_TL%Pressure       = atm_TL%Pressure(1:k)
<     atm_clear_TL%Temperature    = atm_TL%Temperature(1:k)
<     atm_clear_TL%Absorber       = atm_TL%Absorber(1:k,:)
<     atm_clear_TL%Cloud_Fraction = atm_TL%Cloud_Fraction(1:k)
<     ! ...Aerosol components
<     IF ( atm_TL%n_Aerosols > 0 ) THEN
<       DO i = 1, atm_TL%n_Aerosols
<         atm_clear_TL%Aerosol(i) = atm_TL%Aerosol(i)
<       END DO
<     END IF
< 
<   END FUNCTION CRTM_Atmosphere_ClearSkyCopy_TL
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !       CRTM_Atmosphere_ClearSkyCopy_AD
< !
< ! PURPOSE:
< !       Function to perform the adjoint copy of an instance of the CRTM
< !       Atmosphere object without the clouds included.
< !
< ! CALLING SEQUENCE:
< !       Error_Status = CRTM_Atmosphere_ClearSkyCopy_AD( Atm, Atm_Clear_AD, Atm_AD )
< !
< ! INPUTS:
< !       Atm:             Atmosphere object for consistency checking
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Atmosphere_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(IN)
< !
< !       Atm_Clear_AD:    Adjoint Clear-Sky Atmosphere structure to copy
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Atmosphere_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(IN)
< !
< ! OUTPUTS:
< !       Atm_AD:          Adjoint copy of the input atmosphere. This object
< !                        must be the adjoint equivalent of the input
< !                        forward Atm object.
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Atmosphere_type
< !                        DIMENSION:  Scalar
< !                        ATTRIBUTES: INTENT(OUT)
< !
< ! FUNCTION RESULT:
< !       Error_Status:    The return value is an integer defining the error status.
< !                        The error codes are defined in the Message_Handler module.
< !                        If == SUCCESS the operation was successful
< !                           == FAILURE an error occurred
< !                        UNITS:      N/A
< !                        TYPE:       INTEGER
< !                        DIMENSION:  Scalar
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   FUNCTION CRTM_Atmosphere_ClearSkyCopy_AD( &
<     atm         , &  ! FWD input
<     atm_clear_AD, &  ! AD  input
<     atm_AD      ) &  ! AD  output
<   RESULT( err_stat )
<     ! Arguments
<     TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: atm
<     TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: atm_clear_AD
<     TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: atm_AD
<     ! Function result
<     INTEGER :: err_stat
<     ! Local parameters
<     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_ClearSkyCopy_AD'
<     ! Local variables
<     CHARACTER(ML) :: err_msg
<     INTEGER :: i, k
< 
< 
<     ! Set up
<     err_stat = SUCCESS
<     ! ...Check input allocation
<     IF ( .NOT. CRTM_Atmosphere_Associated(atm         ) .OR. &
<          .NOT. CRTM_Atmosphere_Associated(atm_clear_AD) .OR. &
<          .NOT. CRTM_Atmosphere_Associated(atm_AD      ) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input Atmosphere structures not allocated'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
<     ! ...Dimensional consistency
<     IF ( (atm%n_Layers       /= atm_AD%n_Layers      ) .OR. &
<          (atm%n_Absorbers    /= atm_AD%n_Absorbers   ) .OR. &
<          (atm%n_Clouds       /= atm_AD%n_Clouds      ) .OR. &
<          (atm%n_Aerosols     /= atm_AD%n_Aerosols    ) .OR. &
<          (atm%n_Added_Layers /= atm_AD%n_Added_Layers) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input Atm and Atm_AD structures have incongruent dimensions'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
<     IF ( (atm_clear_AD%n_Layers       /= atm_AD%n_Layers      ) .OR. &
<          (atm_clear_AD%n_Absorbers    /= atm_AD%n_Absorbers   ) .OR. &
<          (atm_clear_AD%n_Aerosols     /= atm_AD%n_Aerosols    ) .OR. &
<          (atm_clear_AD%n_Clouds       /= 0                    ) .OR. &  ! NO CLOUDS !
<          (atm_clear_AD%n_Added_Layers /= atm_AD%n_Added_Layers) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Input Atm_Clear_AD structures has incongruent dimensions'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
<     ! ...Non-layer dependent data consistency
<     IF (    (atm%Climatology    /= atm_AD%Climatology   ) .OR. &
<          ANY(atm%Absorber_ID    /= atm_AD%Absorber_ID   ) .OR. &
<          ANY(atm%Absorber_Units /= atm_AD%Absorber_Units) .OR. &
<             (atm%Climatology    /= atm_clear_AD%Climatology   ) .OR. &
<          ANY(atm%Absorber_ID    /= atm_clear_AD%Absorber_ID   ) .OR. &
<          ANY(atm%Absorber_Units /= atm_clear_AD%Absorber_Units) ) THEN
<       err_stat = FAILURE
<       err_msg = 'Atmosphere structures have incongruent layer independent data'
<       CALL Display_Message( ROUTINE_NAME, err_msg, err_stat )
<       RETURN
<     END IF
< 
< 
<     ! Adjoint copy of data
<     ! ...Aerosol components
<     IF ( atm%n_Aerosols > 0 ) THEN
<       DO i = 1, atm%n_Aerosols
<         atm_AD%Aerosol(i) = atm_AD%Aerosol(i) + atm_clear_AD%Aerosol(i)
<       END DO
<     END IF
<     ! ...Layer dependent data
<     k = atm%n_Layers
<     atm_AD%Level_Pressure(0:k) = atm_AD%Level_Pressure(0:k) + atm_clear_AD%Level_Pressure(0:k)
<     atm_AD%Pressure(1:k)       = atm_AD%Pressure(1:k)       + atm_clear_AD%Pressure(1:k)      
<     atm_AD%Temperature(1:k)    = atm_AD%Temperature(1:k)    + atm_clear_AD%Temperature(1:k)   
<     atm_AD%Absorber(1:k,:)     = atm_AD%Absorber(1:k,:)     + atm_clear_AD%Absorber(1:k,:)      
<     atm_AD%Cloud_Fraction(1:k) = atm_AD%Cloud_Fraction(1:k) + atm_clear_AD%Cloud_Fraction(1:k)
< 
< 
<     ! Zero the clear result, as it has no more impact
<     CALL CRTM_Atmosphere_Zero( atm_clear_AD )
< 
<   END FUNCTION CRTM_Atmosphere_ClearSkyCopy_AD
< 
< 
< 
< 
diff -w ./CRTM_Atmosphere_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Atmosphere_Define.f90
100d99
<   PUBLIC :: OPERATOR(/=)
196d194
<   PUBLIC :: CRTM_Atmosphere_NonVariableCopy
209c207
<   PUBLIC :: CRTM_Atmosphere_Add
---
> 
218,221d215
<   INTERFACE OPERATOR(/=)
<     MODULE PROCEDURE CRTM_Atmosphere_NotEqual
<   END INTERFACE OPERATOR(/=)
< 
353d346
< 
390d382
<     REAL(fp), ALLOCATABLE :: Cloud_Fraction(:)  ! K
569d560
<               Atm%Cloud_Fraction( n_Layers ), &
614d604
<     Atm%Cloud_Fraction = ZERO
698d687
<     atm_out%Cloud_Fraction(na+1:nt) = atm%Cloud_Fraction(1:no)
719,777d707
< !       CRTM_Atmosphere_NonVariableCopy
< !
< ! PURPOSE:
< !       Elemental utility subroutine to copy the "non-variable" data (climatology
< !       flag, absorber id/units, cloud type, aerosol type) from one instance of
< !       a CRTM Atmosphere object to another (usually a TL or AD one).
< !
< !       NOTE: No error checking is performed in this procedure. It is assumed the
< !             two arguments are congruent in terms of absorber, cloud, and
< !             aerosol count.
< !
< ! CALLING SEQUENCE:
< !       CALL CRTM_Atmosphere_NonVariableCopy( atm, modified_atm )
< !
< ! OBJECTS:
< !       atm:             Atmosphere object from which to copy.
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Atmosphere_type
< !                        DIMENSION:  Scalar or any rank
< !                        ATTRIBUTES: INTENT(IN)
< !
< ! IN/OUTPUTS:
< !       modified_atm:    Existing Atmosphere object to be modified.
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Atmosphere_type
< !                        DIMENSION:  Conformable with atm input
< !                        ATTRIBUTES: INTENT(IN OUT)
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   ELEMENTAL SUBROUTINE CRTM_Atmosphere_NonVariableCopy( atm, modified_atm )
<     ! Arguments
<     TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: atm
<     TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: modified_atm
<     ! Local variables
<     INTEGER :: j, n
< 
<     modified_atm%Climatology = atm%Climatology
<     DO j = 1, atm%n_Absorbers
<       modified_atm%Absorber_ID(j)    = atm%Absorber_ID(j)
<       modified_atm%Absorber_Units(j) = atm%Absorber_Units(j)
<     END DO
<     ! Loop over and assign cloud types
<     DO n = 1, atm%n_Clouds
<       modified_atm%Cloud(n)%Type = atm%Cloud(n)%Type
<     END DO
<     ! Loop over and assign aerosol types
<     DO n = 1, atm%n_Aerosols
<       modified_atm%Aerosol(n)%Type = atm%Aerosol(n)%Type
<     END DO
<     
<   END SUBROUTINE CRTM_Atmosphere_NonVariableCopy
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
809c739,742
<     ! Zero out the data
---
>     ! Reset the added layer count
>     Atmosphere%n_Added_Layers = 0
> 
>     ! Only zero out the data arrays
814d746
<     Atmosphere%Cloud_Fraction = ZERO
941,946c873
<       msg = 'Negative layer absorber found'
<       CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
<       IsValid = .FALSE.
<     ENDIF
<     IF ( ANY(Atm%Cloud_Fraction < ZERO) .OR. ANY(Atm%Cloud_Fraction > ONE) ) THEN
<       msg = 'Invalid layer cloud fraction found'
---
>       msg = 'Negative level absorber found'
1047,1048d973
<     WRITE(fid, '(3x,"Layer cloud fraction:")')    
<     WRITE(fid, '(5(1x,es13.6,:))') Atm%Cloud_Fraction(1:k)
1157d1081
<     ! Arguments
1173,1174c1097,1099
<     ! Check the object association status
<     IF ( CRTM_Atmosphere_Associated(x) .NEQV. CRTM_Atmosphere_Associated(y) ) RETURN
---
>     ! Check the structure association status
>     IF ( (.NOT. CRTM_Atmosphere_Associated(x)) .OR. &
>          (.NOT. CRTM_Atmosphere_Associated(y))      ) RETURN
1176,1177c1101
<     ! Check contents
<     ! ...Dimensions
---
>     ! Check scalars
1181,1186c1105,1108
<          (x%n_Aerosols  /= y%n_Aerosols ) ) RETURN
<     ! ...Scalars
<     IF ( (x%Climatology /= y%Climatology) ) RETURN
<     ! ...Arrays
<     IF ( CRTM_Atmosphere_Associated(x) .AND. CRTM_Atmosphere_Associated(y) ) THEN
<       ! ...Integer arrays
---
>          (x%n_Aerosols  /= y%n_Aerosols ) .OR. &
>          (x%Climatology /= y%Climatology) ) RETURN
> 
>     ! Check integer arrays
1190c1112,1113
<       ! ...Floating point arrays
---
> 
>     ! Check floating point arrays
1194,1196c1117,1119
<            (.NOT. ALL(Compares_Within_Tolerance(x%Absorber      ,y%Absorber      ,n))) .OR. &
<            (.NOT. ALL(Compares_Within_Tolerance(x%Cloud_Fraction,y%Cloud_Fraction,n)))) RETURN
<       ! ...Clouds
---
>          (.NOT. ALL(Compares_Within_Tolerance(x%Absorber      ,y%Absorber      ,n))) ) RETURN
> 
>     ! Check clouds
1200c1123,1124
<       ! ...Aerosols
---
> 
>     ! Check aerosols
1204d1127
<     END IF
1206c1129
<     ! If we get here, the objects are comparable
---
>     ! If we get here, the structures are comparable
1659,1660c1582
<    !ALLOCATE(Atmosphere(n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
<     ALLOCATE(Atmosphere(n_input_profiles), STAT=alloc_stat)
---
>     ALLOCATE(Atmosphere(n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
1707,1708c1629
<        !DEALLOCATE(Atmosphere, STAT=alloc_stat, ERRMSG=alloc_msg)
<         DEALLOCATE(Atmosphere, STAT=alloc_stat)
---
>         DEALLOCATE(Atmosphere, STAT=alloc_stat, ERRMSG=alloc_msg)
1776,1777c1697
<              STAT=alloc_stat)
<             !STAT=alloc_stat, ERRMSG=alloc_msg)
---
>              STAT=alloc_stat, ERRMSG=alloc_msg)
1827,1828c1747
<        !DEALLOCATE(Atmosphere, STAT=alloc_stat, ERRMSG=alloc_msg)
<         DEALLOCATE(Atmosphere, STAT=alloc_stat)
---
>         DEALLOCATE(Atmosphere, STAT=alloc_stat, ERRMSG=alloc_msg)
2163,2164c2082,2084
<     ! Check the object association status
<     IF ( CRTM_Atmosphere_Associated(x) .NEQV. CRTM_Atmosphere_Associated(y) ) RETURN
---
>     ! Check the structure association status
>     IF ( (.NOT. CRTM_Atmosphere_Associated(x)) .OR. &
>          (.NOT. CRTM_Atmosphere_Associated(y))      ) RETURN
2167c2087
<     ! ...Dimensions
---
>     ! ...Scalars
2171,2173c2091,2092
<          (x%n_Aerosols  /= y%n_Aerosols ) ) RETURN
<     ! ...Scalars
<     IF ( (x%Climatology /= y%Climatology) ) RETURN
---
>          (x%n_Aerosols  /= y%n_Aerosols ) .OR. &
>          (x%Climatology /= y%Climatology) ) RETURN
2175d2093
<     IF ( CRTM_Atmosphere_Associated(x) .AND. CRTM_Atmosphere_Associated(y) ) THEN
2178c2096
<       IF ( .NOT. (ALL(x%Absorber_ID(1:j)       ==     y%Absorber_ID(1:j)   ) .AND. &
---
>     IF ( ALL(x%Absorber_ID(1:j)    == y%Absorber_ID(1:j)   ) .AND. &
2180c2098
<                   ALL(x%Level_Pressure(0:k) .EqualTo. y%Level_Pressure(0:k)) .AND. &
---
>          ALL(x%Level_Pressure(0:) .EqualTo. y%Level_Pressure(0:)) .AND. &
2183,2184c2101
<                   ALL(x%Absorber(1:k,1:j)   .EqualTo. y%Absorber(1:k,1:j)  ) .AND. &
<                   ALL(x%Cloud_Fraction(1:k) .EqualTo. y%Cloud_Fraction(1:k))) ) RETURN
---
>          ALL(x%Absorber(1:k,1:j)  .EqualTo. y%Absorber(1:k,1:j) ) ) is_equal = .TRUE.
2187c2104,2105
<         IF ( .NOT. ALL(x%Cloud == y%Cloud) ) RETURN
---
>       IF ( ALL(CRTM_Cloud_Associated(x%Cloud)) .AND. ALL(CRTM_Cloud_Associated(y%Cloud)) ) &
>         is_equal = is_equal .AND. ALL(x%Cloud == y%Cloud)
2191,2192c2109,2110
<         IF ( .NOT. ALL(x%Aerosol == y%Aerosol) ) RETURN
<       END IF
---
>       IF ( ALL(CRTM_Aerosol_Associated(x%Aerosol)) .AND. ALL(CRTM_Aerosol_Associated(y%Aerosol)) ) &
>         is_equal = is_equal .AND. ALL(x%Aerosol == y%Aerosol)
2195,2198d2112
< 
<     ! If we get here, then...
<     is_equal = .TRUE.
< 
2202,2243d2115
< !------------------------------------------------------------------------------
< !
< ! NAME:
< !   CRTM_Atmosphere_NotEqual
< !
< ! PURPOSE:
< !   Elemental function to test the inequality of two CRTM Atmosphere objects.
< !   Used in OPERATOR(/=) interface block.
< !
< !   This function is syntactic sugar.
< !
< ! CALLING SEQUENCE:
< !   not_equal = CRTM_Atmosphere_NotEqual( x, y )
< !
< !     or
< !
< !   IF ( x /= y ) THEN
< !     ...
< !   END IF
< !
< ! OBJECTS:
< !   x, y:          Two CRTM Atmosphere objects to be compared.
< !                  UNITS:      N/A
< !                  TYPE:       CRTM_Atmosphere_type
< !                  DIMENSION:  Scalar or any rank
< !                  ATTRIBUTES: INTENT(IN)
< !
< ! FUNCTION RESULT:
< !   not_equal:     Logical value indicating whether the inputs are not equal.
< !                  UNITS:      N/A
< !                  TYPE:       LOGICAL
< !                  DIMENSION:  Same as inputs.
< !
< !------------------------------------------------------------------------------
< 
<   ELEMENTAL FUNCTION CRTM_Atmosphere_NotEqual( x, y ) RESULT( not_equal )
<     TYPE(CRTM_Atmosphere_type), INTENT(IN) :: x, y
<     LOGICAL :: not_equal
<     not_equal = .NOT. (x == y)
<   END FUNCTION CRTM_Atmosphere_NotEqual
< 
< 
2293c2165
<     ! ...Dimensions the same, check absorber info
---
>     ! ...Dimenions the same, check absorber info
2307d2178
<     atmsum%Cloud_Fraction(1:k) = atmsum%Cloud_Fraction(1:k) + atm2%Cloud_Fraction(1:k)
2374c2245
<     ! ...Dimensions the same, check absorber info
---
>     ! ...Dimenions the same, check absorber info
2388d2258
<     atmdiff%Cloud_Fraction(1:k) = atmdiff%Cloud_Fraction(1:k) - atm2%Cloud_Fraction(1:k)
2482,2483c2352
<       atm%Absorber, &
<       atm%Cloud_Fraction
---
>       atm%Absorber
2598,2599c2467
<       atm%Absorber(1:atm%n_Layers,:), &
<       atm%Cloud_Fraction(1:atm%n_Layers)
---
>       atm%Absorber(1:atm%n_Layers,:)
diff -w ./CRTM_ChannelInfo_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_ChannelInfo_Define.f90
diff -w ./CRTM_CloudCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_CloudCoeff.f90
diff -w ./CRTM_CloudCover_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_CloudCover_Define.f90
diff -w ./CRTM_CloudScatter.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_CloudScatter.f90
diff -w ./CRTM_Cloud_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Cloud_Define.f90
56d55
<   PUBLIC :: OPERATOR(/=)
85,88d83
<   INTERFACE OPERATOR(/=)
<     MODULE PROCEDURE CRTM_Cloud_NotEqual
<   END INTERFACE OPERATOR(/=)
< 
197,198c192
<    !ALLOCATE( list(0:N_VALID_CLOUD_CATEGORIES), STAT=alloc_stat, ERRMSG=alloc_msg )
<     ALLOCATE( list(0:N_VALID_CLOUD_CATEGORIES), STAT=alloc_stat )
---
>     ALLOCATE( list(0:N_VALID_CLOUD_CATEGORIES), STAT=alloc_stat, ERRMSG=alloc_msg )
710,711c704,706
<     ! Check the object association status
<     IF ( CRTM_Cloud_Associated(x) .NEQV. CRTM_Cloud_Associated(y) ) RETURN
---
>     ! Check the structure association status
>     IF ( (.NOT. CRTM_Cloud_Associated(x)) .OR. &
>          (.NOT. CRTM_Cloud_Associated(y)) ) RETURN
713,714c708
<     ! Check contents
<     ! ...Scalars
---
>     ! Check scalars
717,718c711,712
<     ! ...Arrays
<     IF ( CRTM_Cloud_Associated(x) .AND. CRTM_Cloud_Associated(y) ) THEN
---
> 
>     ! Check arrays
722d715
<     END IF
724c717
<     ! If we get here, the objects are comparable
---
>     ! If we get here, the structures are comparable
1027,1028c1020
<    !ALLOCATE(Cloud(nc), STAT=alloc_stat, ERRMSG=alloc_msg)
<     ALLOCATE(Cloud(nc), STAT=alloc_stat)
---
>     ALLOCATE(Cloud(nc), STAT=alloc_stat, ERRMSG=alloc_msg)
1074,1075c1066
<        !DEALLOCATE(Cloud, STAT=alloc_stat, ERRMSG=alloc_msg)
<         DEALLOCATE(Cloud, STAT=alloc_stat)
---
>         DEALLOCATE(Cloud, STAT=alloc_stat, ERRMSG=alloc_msg)
1317,1318c1308,1310
<     ! Check the object association status
<     IF ( CRTM_Cloud_Associated(x) .NEQV. CRTM_Cloud_Associated(y) ) RETURN
---
>     ! Check the structure association status
>     IF ( (.NOT. CRTM_Cloud_Associated(x)) .OR. &
>          (.NOT. CRTM_Cloud_Associated(y))      ) RETURN
1324d1315
<     IF ( CRTM_Cloud_Associated(x) .AND. CRTM_Cloud_Associated(y) ) THEN
1326c1317
<       IF ( .NOT. (ALL(x%Effective_Radius(1:n)   .EqualTo. y%Effective_Radius(1:n)  ) .AND. &
---
>     IF ( ALL(x%Effective_Radius(1:n)   .EqualTo. y%Effective_Radius(1:n)  ) .AND. &
1328,1331c1319
<                   ALL(x%Water_Content(1:n)      .EqualTo. y%Water_Content(1:n)     )) ) RETURN
<     END IF
<     
<     ! If we get here, then...
---
>          ALL(x%Water_Content(1:n)      .EqualTo. y%Water_Content(1:n)     )       ) &
1337,1378d1324
< !------------------------------------------------------------------------------
< !
< ! NAME:
< !   CRTM_Cloud_NotEqual
< !
< ! PURPOSE:
< !   Elemental function to test the inequality of two CRTM Cloud objects.
< !   Used in OPERATOR(/=) interface block.
< !
< !   This function is syntactic sugar.
< !
< ! CALLING SEQUENCE:
< !   not_equal = CRTM_Cloud_NotEqual( x, y )
< !
< !     or
< !
< !   IF ( x /= y ) THEN
< !     ...
< !   END IF
< !
< ! OBJECTS:
< !   x, y:          Two CRTM Cloud objects to be compared.
< !                  UNITS:      N/A
< !                  TYPE:       CRTM_Cloud_type
< !                  DIMENSION:  Scalar or any rank
< !                  ATTRIBUTES: INTENT(IN)
< !
< ! FUNCTION RESULT:
< !   not_equal:     Logical value indicating whether the inputs are not equal.
< !                  UNITS:      N/A
< !                  TYPE:       LOGICAL
< !                  DIMENSION:  Same as inputs.
< !
< !------------------------------------------------------------------------------
< 
<   ELEMENTAL FUNCTION CRTM_Cloud_NotEqual( x, y ) RESULT( not_equal )
<     TYPE(CRTM_Cloud_type), INTENT(IN) :: x, y
<     LOGICAL :: not_equal
<     not_equal = .NOT. (x == y)
<   END FUNCTION CRTM_Cloud_NotEqual
< 
< 
diff -w ./CRTM_Fastem1.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Fastem1.f90
diff -w ./CRTM_FastemX.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_FastemX.f90
146d145
<     INTEGER  :: n_Angles     = ZERO 
154d152
<     REAL(fp) :: cos_z_mod = ONE  
306d303
<     n_Angles     , &  ! Input  
319d315
<     INTEGER,                 INTENT(IN)  :: n_Angles       
337d332
<     iVar%n_Angles     = n_Angles   
340,343d334
<     iVar%cos_z_mod = COS(Zenith_Angle*DEGREES_TO_RADIANS)
<     if (n_Angles > 1 .and. Zenith_Angle > 60.0_fp) &     
<        iVar%cos_z_mod = COS(60.0_fp*DEGREES_TO_RADIANS)   
< 
436c427
<                iVar%cos_z_mod , &   
---
>                iVar%cos_z     , &
diff -w ./CRTM_Forward_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Forward_Module.f90
44,46d43
<   USE CRTM_RTSolution_Define,     ONLY: CRTM_RTSolution_type   , &
<                                         CRTM_RTSolution_Destroy, &
<                                         CRTM_RTSolution_Zero
49,52c46
<   USE CRTM_Atmosphere,            ONLY: CRTM_Atmosphere_AddLayers   , &
<                                         CRTM_Atmosphere_IsFractional, &
<                                         CRTM_Atmosphere_Coverage, &
<                                         CRTM_Atmosphere_ClearSkyCopy
---
>   USE CRTM_Atmosphere,            ONLY: CRTM_Atmosphere_AddLayers
72c66,68
<   USE CRTM_AtmOptics,             ONLY: CRTM_Include_Scattering   , &
---
>   USE CRTM_AtmOptics,             ONLY: AOvar_type  , &
>                                         AOvar_Create, &
>                                         CRTM_Include_Scattering, &
74,75c70
<                                         CRTM_AtmOptics_Combine    , &
<                                         CRTM_AtmOptics_NoScatterCopy
---
>                                         CRTM_Combine_AtmOptics
81c76,77
<   USE CRTM_RTSolution,            ONLY: CRTM_Compute_nStreams   , &
---
>   USE CRTM_RTSolution,            ONLY: CRTM_RTSolution_type    , &
>                                         CRTM_Compute_nStreams   , &
95d90
<   USE CRTM_CloudCover_Define,     ONLY: CRTM_CloudCover_type
98,102d92
<   ! ...AtmOptics
<   USE AOvar_Define, ONLY: AOvar_type, &
<                           AOvar_Associated, &
<                           AOvar_Destroy   , &
<                           AOvar_Create
249c239,242
<     LOGICAL :: compute_antenna_correction
---
>     LOGICAL :: Check_Input
>     LOGICAL :: User_Emissivity, User_Direct_Reflectivity, User_N_Streams
>     LOGICAL :: User_AntCorr, Compute_AntCorr
>     LOGICAL :: Apply_NLTE_Correction
250a244
>     INTEGER :: RT_Algorithm_Id
257d250
<     INTEGER :: cloud_coverage_flag
260c253,254
<     REAL(fp) :: transmittance, transmittance_clear
---
>     REAL(fp) :: Aircraft_Pressure
>     REAL(fp) :: transmittance
263,264c257,258
<     ! Local options structure for default and use values
<     TYPE(CRTM_Options_type) :: Default_Options, Opt
---
>     ! Local options structure for default values
>     TYPE(CRTM_Options_type) :: Default_Options
267,272d260
<     ! Clear sky structures
<     TYPE(CRTM_Atmosphere_type) :: Atm_Clear
<     TYPE(CRTM_AtmOptics_type)  :: AtmOptics_Clear
<     TYPE(CRTM_SfcOptics_type)  :: SfcOptics_Clear
<     TYPE(CRTM_RTSolution_type) :: RTSolution_Clear
<     TYPE(RTV_type)             :: RTV_Clear
287,288d274
<     ! Cloud cover object
<     TYPE(CRTM_CloudCover_type) :: CloudCover
339,342d324
<     ! Reinitialise the output RTSolution
<     CALL CRTM_RTSolution_Zero(RTSolution)
< 
< 
377c359,367
<       Opt = Default_Options
---
>       ! ...Specify default actions
>       Check_Input           = Default_Options%Check_Input
>       User_Emissivity       = Default_Options%Use_Emissivity
>       User_AntCorr          = Default_Options%Use_Antenna_Correction
>       Apply_NLTE_Correction = Default_Options%Apply_NLTE_Correction
>       RT_Algorithm_Id       = Default_Options%RT_Algorithm_Id
>       User_N_Streams        = Default_Options%Use_N_Streams
>       Aircraft_Pressure     = Default_Options%Aircraft_Pressure
>       ! ...Check the Options argument
379,380c369,393
<         Opt = Options(m)
<         ! Copy over ancillary input (just add AncillaryInput structure to options?)
---
>         ! Override input checker with option
>         Check_Input = Options(m)%Check_Input
>         ! Check if the supplied emissivity should be used
>         User_Emissivity = Options(m)%Use_Emissivity
>         IF ( Options(m)%Use_Emissivity ) THEN
>           ! Are the channel dimensions consistent
>           IF ( Options(m)%n_Channels < n_Channels ) THEN
>             Error_Status = FAILURE
>             WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
>                    &"than the number of requested channels (",i0, ")" )' ) &
>                    Options(m)%n_Channels, n_Channels
>             CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
>             RETURN
>           END IF
>           ! Check if the supplied direct reflectivity should be used
>           User_Direct_Reflectivity = Options(m)%Use_Direct_Reflectivity
>         END IF
>         ! Check if antenna correction should be attempted
>         User_AntCorr = Options(m)%Use_Antenna_Correction
>         ! Set NLTE correction option
>         Apply_NLTE_Correction = Options(m)%Apply_NLTE_Correction
>         ! Set aircraft pressure altitude
>         Aircraft_Pressure = Options(m)%Aircraft_Pressure
> 
>         ! Copy over ancillary input
382a396,412
>         ! Copy over surface optics input
>         SfcOptics%Use_New_MWSSEM = .NOT. Options(m)%Use_Old_MWSSEM
>         ! Specify the RT algorithm
>         RT_Algorithm_Id = Options(m)%RT_Algorithm_Id
>         ! Check if n_Streams should be used
>         User_N_Streams = Options(m)%Use_N_Streams
>         ! Check value for nstreams
>         IF ( User_N_Streams ) THEN
>           IF ( Options(m)%n_Streams <= 0 .OR. MOD(Options(m)%n_Streams,2) /= 0 .OR. &
>                Options(m)%n_Streams > MAX_N_STREAMS ) THEN
>               Error_Status = FAILURE
>               WRITE( Message,'( "Input Options n_Streams (", i0, ") is invalid" )' ) &
>                      Options(m)%n_Streams
>               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
>               RETURN
>           END IF
>         END IF
384,385d413
<       ! ...Assign the option specific SfcOptics input
<       SfcOptics%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
389c417
<       IF ( Opt%Check_Input ) THEN
---
>       IF ( Check_Input ) THEN
409,430d436
<           ! Are the channel dimensions consistent if emissivity is passed?
<           IF ( Options(m)%Use_Emissivity ) THEN
<             IF ( Options(m)%n_Channels < n_Channels ) THEN
<               Error_Status = FAILURE
<               WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
<                      &"than the number of requested channels (",i0, ")" )' ) &
<                      Options(m)%n_Channels, n_Channels
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
<           END IF
<           ! Check value for user-defined n_Streams
<           IF ( Options(m)%Use_N_Streams ) THEN
<             IF ( Options(m)%n_Streams <= 0 .OR. MOD(Options(m)%n_Streams,2) /= 0 .OR. &
<                  Options(m)%n_Streams > MAX_N_STREAMS ) THEN
<                 Error_Status = FAILURE
<                 WRITE( Message,'( "Input Options n_Streams (", i0, ") is invalid" )' ) &
<                        Options(m)%n_Streams
<                 CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                 RETURN
<             END IF
<           END IF
445a452,455
>       ! Average surface skin temperature for multi-surface types
>       CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )
> 
> 
463,466c473
< 
< 
<       ! Prepare the atmospheric optics structures
<       ! ...Allocate the AtmOptics structure based on Atm extension
---
>       ! ...Allocate the atmospheric optics structures based on Atm extension
470a478,481
>       IF (Options_Present) THEN
>         ! Set Scattering Switch
>         AtmOptics%Include_Scattering = Options(m)%Include_Scattering
>       END IF
477,478d487
<       ! ...Set the scattering switch
<       AtmOptics%Include_Scattering = Opt%Include_Scattering
482a492,509
>       ! Process aircraft pressure altitude
>       IF ( Aircraft_Pressure > ZERO ) THEN
>         RTV%aircraft%rt = .TRUE.
>         RTV%aircraft%idx = CRTM_Get_PressureLevelIdx(Atm, Aircraft_Pressure)
>         ! ...Issue warning if profile level is TOO different from flight level
>         IF ( ABS(Atm%Level_Pressure(RTV%aircraft%idx)-Aircraft_Pressure) > AIRCRAFT_PRESSURE_THRESHOLD ) THEN
>           WRITE( Message,'("Difference between aircraft pressure level (",es13.6,&
>                           &"hPa) and closest input profile level (",es13.6,&
>                           &"hPa) is larger than recommended (",f4.1,"hPa) for profile #",i0)') &
>                           Aircraft_Pressure, Atm%Level_Pressure(RTV%aircraft%idx), &
>                           AIRCRAFT_PRESSURE_THRESHOLD, m
>           CALL Display_Message( ROUTINE_NAME, Message, WARNING )
>         END IF
>       ELSE
>         RTV%aircraft%rt = .FALSE.
>       END IF
> 
> 
502,563d528
<       ! Determine the type of cloud coverage
<       cloud_coverage_flag = CRTM_Atmosphere_Coverage( atm )
< 
< 
<       ! Setup for fractional cloud coverage
<       IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<       
<         ! Compute cloudcover
<         Error_Status = CloudCover%Compute_CloudCover(atm, Overlap = opt%Overlap_Id)
<         IF ( Error_Status /= SUCCESS ) THEN
<           WRITE( Message,'("Error computing cloud cover in profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
< 
<         ! Allocate all the CLEAR sky structures for fractional cloud coverage
<         ! ...A clear sky atmosphere
<         Error_Status = CRTM_Atmosphere_ClearSkyCopy(Atm, Atm_Clear)
<         IF ( Error_Status /= SUCCESS ) THEN
<           WRITE( Message,'("Error copying CLEAR SKY atmopshere in profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
<         ! ...Clear sky SfcOptics
<         CALL CRTM_SfcOptics_Create( SfcOptics_Clear, MAX_N_ANGLES, MAX_N_STOKES )
<         IF ( .NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear) ) THEN
<           Error_Status = FAILURE
<           WRITE( Message,'("Error allocating CLEAR SKY SfcOptics data structure for profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
<         ! ...Copy over surface optics input
<         SfcOptics_Clear%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
<         ! ...CLEAR SKY average surface skin temperature for multi-surface types
<         CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics_Clear )
<       END IF
< 
< 
<       ! Average surface skin temperature for multi-surface types
<       CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )
< 
< 
<       ! Process aircraft pressure altitude
<       IF ( Opt%Aircraft_Pressure > ZERO ) THEN
<         RTV%aircraft%rt = .TRUE.
<         RTV%aircraft%idx = CRTM_Get_PressureLevelIdx(Atm, Opt%Aircraft_Pressure)
<         ! ...Issue warning if profile level is TOO different from flight level
<         IF ( ABS(Atm%Level_Pressure(RTV%aircraft%idx)-Opt%Aircraft_Pressure) > AIRCRAFT_PRESSURE_THRESHOLD ) THEN
<           WRITE( Message,'("Difference between aircraft pressure level (",es13.6,&
<                           &"hPa) and closest input profile level (",es13.6,&
<                           &"hPa) is larger than recommended (",f4.1,"hPa) for profile #",i0)') &
<                           Opt%Aircraft_Pressure, Atm%Level_Pressure(RTV%aircraft%idx), &
<                           AIRCRAFT_PRESSURE_THRESHOLD, m
<           CALL Display_Message( ROUTINE_NAME, Message, WARNING )
<         END IF
<       ELSE
<         RTV%aircraft%rt = .FALSE.
<       END IF
< 
< 
< 
< 
578c543
<         compute_antenna_correction = ( Opt%Use_Antenna_Correction               .AND. &
---
>         IF ( User_AntCorr                             .AND. &
580c545,549
<                                        iFOV /= 0 )
---
>              iFOV /= 0 ) THEN
>           Compute_AntCorr = .TRUE.
>         ELSE
>           Compute_AntCorr = .FALSE.
>         END IF
608,609c577
<               SpcCoeff_IsVisibleSensor(SC(SensorIndex)) ) .AND. &
<             AtmOptics%Include_Scattering ) THEN
---
>             SpcCoeff_IsVisibleSensor( SC(SensorIndex) ) ) .and. AtmOptics%Include_Scattering ) THEN
619c587
<           RTV%RT_Algorithm_Id = Opt%RT_Algorithm_Id
---
>           RTV%RT_Algorithm_Id = RT_Algorithm_Id
624c592
<         IF ( Opt%Apply_NLTE_Correction ) THEN
---
>         IF ( Apply_NLTE_Correction ) THEN
654,656d621
<           CALL CRTM_AtmOptics_Zero( AtmOptics_Clear )
<           CALL CRTM_RTSolution_Zero( RTSolution_Clear )
< 
659,660c624,625
<           IF ( Opt%Use_N_Streams ) THEN
<             n_Full_Streams = Opt%n_Streams
---
>           IF ( User_N_Streams ) THEN
>             n_Full_Streams = Options(m)%n_Streams
681a647,651
>           ! Compute the clear-sky atmospheric transmittance
>           ! for use in FASTEM-X reflection correction
>           CALL CRTM_Compute_Transmittance(AtmOptics,transmittance)
> 
> 
687d656
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) RTV_Clear%Solar_Flag_true = .TRUE.
717,733d685
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               RTV_Clear%Visible_Flag_true = .FALSE.
<               RTV_Clear%n_Azi = 0
<             END IF
<           END IF
< 
< 
<           ! Copy the clear-sky AtmOptics
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             Error_Status = CRTM_AtmOptics_NoScatterCopy( AtmOptics, AtmOptics_Clear )
<             IF ( Error_Status /= SUCCESS ) THEN
<               WRITE( Message,'("Error copying CLEAR SKY AtmOptics for ",a,&
<                      &", channel ",i0,", profile #",i0)' ) &
<                      TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
773c725
<             CALL CRTM_AtmOptics_Combine( AtmOptics, AOvar )
---
>             CALL CRTM_Combine_AtmOptics( AtmOptics, AOvar )
775c727
<           ! ...Save vertically integrated scattering optical depth for output
---
>           ! ...Save vertically integrated scattering optical depth fro output
779,781c731,734
<           ! Compute the all-sky atmospheric transmittance
<           ! for use in FASTEM-X reflection correction
<           CALL CRTM_Compute_Transmittance(AtmOptics,transmittance)
---
>           ! Turn off FASTEM-X reflection correction for scattering conditions
>           IF ( CRTM_Include_Scattering(AtmOptics) .AND. SpcCoeff_IsMicrowaveSensor( SC(SensorIndex) ) ) THEN
>             SfcOptics%Transmittance = -ONE
>           ELSE
783,786d735
<           ! ...Clear sky for fractional cloud cover
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             CALL CRTM_Compute_Transmittance(AtmOptics_Clear,transmittance_clear)
<             SfcOptics_Clear%Transmittance = transmittance_clear
790c739,741
<           ! Fill the SfcOptics structures for the optional emissivity input case.
---
> 
>           ! Fill the SfcOptics structure for the optional emissivity input case.
>           ! ...Indicate SfcOptics ARE to be computed
792,794c743,744
<           SfcOptics_Clear%Compute = .TRUE.
<           IF ( Opt%Use_Emissivity ) THEN
<             ! ...Cloudy/all-sky case
---
>           ! ...Change SfcOptics emissivity/reflectivity contents/computation status
>           IF ( User_Emissivity ) THEN
796,799c746,749
<             SfcOptics%Emissivity(1,1)       = Opt%Emissivity(ln)
<             SfcOptics%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
<             IF ( Opt%Use_Direct_Reflectivity ) THEN
<               SfcOptics%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
---
>             SfcOptics%Emissivity(1,1)       = Options(m)%Emissivity(ln)
>             SfcOptics%Reflectivity(1,1,1,1) = ONE - Options(m)%Emissivity(ln)
>             IF ( User_Direct_Reflectivity ) THEN
>               SfcOptics%Direct_Reflectivity(1,1) = Options(m)%Direct_Reflectivity(ln)
803,813d752
<             ! ...Repeat for fractional clear-sky case
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               SfcOptics_Clear%Compute = .FALSE.
<               SfcOptics_Clear%Emissivity(1,1)       = Opt%Emissivity(ln)
<               SfcOptics_Clear%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
<               IF ( Opt%Use_Direct_Reflectivity ) THEN
<                 SfcOptics_Clear%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
<               ELSE
<                 SfcOptics_Clear%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
<               END IF
<             END IF
846,870d784
< 
< 
<             ! Repeat clear sky for fractionally cloudy atmospheres
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               RTV_Clear%mth_Azi = mth_Azi
<               SfcOptics_Clear%mth_Azi = mth_Azi
<               Error_Status = CRTM_Compute_RTSolution( &
<                                Atm_Clear       , &  ! Input
<                                Surface(m)      , &  ! Input
<                                AtmOptics_Clear , &  ! Input
<                                SfcOptics_Clear , &  ! Input
<                                GeometryInfo    , &  ! Input
<                                SensorIndex     , &  ! Input
<                                ChannelIndex    , &  ! Input
<                                RTSolution_Clear, &  ! Output
<                                RTV_Clear         )  ! Internal variable output
<               IF ( Error_Status /= SUCCESS ) THEN
<                 WRITE( Message,'( "Error computing CLEAR SKY RTSolution for ", a, &
<                        &", channel ", i0,", profile #",i0)' ) &
<                        TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<                 CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                 RETURN
<               END IF
<             END IF
< 
873,932d786
< 
<           ! Combine cloudy and clear radiances for fractional cloud coverage
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             RTSolution(ln,m)%Radiance = &
<                 ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_Clear%Radiance) + &
<                 (CloudCover%Total_Cloud_Cover * RTSolution(ln,m)%Radiance)
<             ! ...Save the cloud cover in the output structure
<             RTSolution(ln,m)%Total_Cloud_Cover = CloudCover%Total_Cloud_Cover
<           END IF
< 
< 
<           ! The radiance post-processing
<           CALL Post_Process_RTSolution(RTSolution(ln,m))
< 
< 
<           ! Perform clear-sky post-processing
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             CALL Post_Process_RTSolution(RTSolution_Clear)
<             ! ...Save the results in the output structure
<             RTSolution(ln,m)%R_Clear  = RTSolution_Clear%Radiance
<             RTSolution(ln,m)%Tb_Clear = RTSolution_Clear%Brightness_Temperature
<           END IF
< 
<         END DO Channel_Loop
< 
<       END DO Sensor_Loop
< 
<     END DO Profile_Loop
< 
< 
<     ! Clean up
<     CALL CRTM_Predictor_Destroy( Predictor )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear )
<     CALL CRTM_SfcOptics_Destroy( SfcOptics )
<     CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear )
<     CALL CRTM_Atmosphere_Destroy( Atm )
<     CALL CRTM_Atmosphere_Destroy( Atm_Clear )
<     ! ...Internal variables
<     CALL AOvar_Destroy( AOvar )
<     CALL CSvar_Destroy( CSvar )
<     CALL ASvar_Destroy( ASvar )
<     CALL RTV_Destroy( RTV ) 
< 
< 
<   CONTAINS
< 
< 
<     ! ----------------------------------------------------------------
<     ! Local subroutine to post-process the radiance, as it is the same
<     ! for all-sky and fractional clear-sky cases.
<     !
<     !   1. Apply non-LTE correction to radiance
<     !   2. Convert radiance to brightness temperature
<     !   3. Apply antenna correction to brightness temperature
<     ! ----------------------------------------------------------------
< 
<     SUBROUTINE Post_Process_RTSolution(rts)
<       TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: rts
< 
934c788
<       IF ( Opt%Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
---
>           IF ( Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
939c793
<                rts%Radiance        )  ! In/Output
---
>                    RTSolution(ln,m)%Radiance  )  ! In/Output
940a795
> 
945,946c800,802
<              rts%Radiance              , & ! Input
<              rts%Brightness_Temperature  ) ! Output
---
>                  RTSolution(ln,m)%Radiance              , & ! Input
>                  RTSolution(ln,m)%Brightness_Temperature  ) ! Output
> 
948c804
<       IF ( compute_antenna_correction ) THEN
---
>           IF ( Compute_AntCorr ) THEN
953c809
<                rts           )  ! Output
---
>                    RTSolution(ln,m)  )  ! Output
954a811,825
>         END DO Channel_Loop
> 
> 
>         ! Deallocate local sensor dependent data structures
>         ! ...RTV structure
>         IF ( RTV_Associated(RTV) ) CALL RTV_Destroy(RTV)
>         ! ...Predictor structure
>         CALL CRTM_Predictor_Destroy( Predictor )
> 
>       END DO Sensor_Loop
> 
> 
>       ! Deallocate local sensor independent data structures
>       ! ...Atmospheric optics
>       CALL CRTM_AtmOptics_Destroy( AtmOptics )
956c827,832
<     END SUBROUTINE Post_Process_RTSolution
---
>     END DO Profile_Loop
> 
> 
>     ! Destroy any remaining structures
>     CALL CRTM_SfcOptics_Destroy( SfcOptics )
>     CALL CRTM_Atmosphere_Destroy( Atm )
diff -w ./CRTM_GeometryInfo.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_GeometryInfo.f90
diff -w ./CRTM_GeometryInfo_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_GeometryInfo_Define.f90
864,865c864
<    !ALLOCATE(GeometryInfo(n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
<     ALLOCATE(GeometryInfo(n_input_profiles), STAT=alloc_stat)
---
>     ALLOCATE(GeometryInfo(n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
910,911c909
<        !DEALLOCATE(GeometryInfo, STAT=alloc_stat, ERRMSG=alloc_msg)
<         DEALLOCATE(GeometryInfo, STAT=alloc_stat)
---
>         DEALLOCATE(GeometryInfo, STAT=alloc_stat, ERRMSG=alloc_msg)
diff -w ./CRTM_Geometry_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Geometry_Define.f90
1168,1169c1168
<    !ALLOCATE(Geometry(n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
<     ALLOCATE(Geometry(n_input_profiles), STAT=alloc_stat)
---
>     ALLOCATE(Geometry(n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
1216,1217c1215
<        !DEALLOCATE(Geometry, STAT=alloc_stat, ERRMSG=alloc_msg)
<         DEALLOCATE(Geometry, STAT=alloc_stat)
---
>         DEALLOCATE(Geometry, STAT=alloc_stat, ERRMSG=alloc_msg)
diff -w ./CRTM_IRSSEM.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_IRSSEM.f90
diff -w ./CRTM_IR_Ice_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_IR_Ice_SfcOptics.f90
diff -w ./CRTM_IR_Land_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_IR_Land_SfcOptics.f90
diff -w ./CRTM_IR_Snow_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_IR_Snow_SfcOptics.f90
diff -w ./CRTM_IR_Water_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_IR_Water_SfcOptics.f90
diff -w ./CRTM_IRiceCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_IRiceCoeff.f90
diff -w ./CRTM_IRlandCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_IRlandCoeff.f90
diff -w ./CRTM_IRsnowCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_IRsnowCoeff.f90
diff -w ./CRTM_IRwaterCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_IRwaterCoeff.f90
diff -w ./CRTM_Interpolation.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Interpolation.f90
diff -w ./CRTM_K_Matrix_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_K_Matrix_Module.f90
34,36c34
<   USE CRTM_Atmosphere_Define,     ONLY: OPERATOR(+)                    , &
<                                         CRTM_Atmosphere_type           , &
<                                         CRTM_Atmosphere_Associated     , &
---
>   USE CRTM_Atmosphere_Define,     ONLY: CRTM_Atmosphere_type, &
39d36
<                                         CRTM_Atmosphere_Zero           , &
41d37
<                                         CRTM_Atmosphere_NonVariableCopy, &
44,45c40
<                                         CRTM_Surface_IsValid        , &
<                                         CRTM_Surface_NonVariableCopy
---
>                                         CRTM_Surface_IsValid
50,52d44
<   USE CRTM_RTSolution_Define,     ONLY: CRTM_RTSolution_type   , &
<                                         CRTM_RTSolution_Destroy, &
<                                         CRTM_RTSolution_Zero
56,60c48
<                                         CRTM_Atmosphere_AddLayers_AD   , &
<                                         CRTM_Atmosphere_IsFractional   , &
<                                         CRTM_Atmosphere_Coverage       , &
<                                         CRTM_Atmosphere_ClearSkyCopy   , &
<                                         CRTM_Atmosphere_ClearSkyCopy_AD
---
>                                         CRTM_Atmosphere_AddLayers_AD
90,95c78,80
<                                         CRTM_AtmOptics_Combine       , &
<                                         CRTM_AtmOptics_Combine_AD    , &
<                                         CRTM_AtmOptics_NoScatterCopy , &
<                                         CRTM_AtmOptics_NoScatterCopy_AD
<   USE CRTM_SfcOptics_Define,      ONLY: OPERATOR(+)              , &
<                                         CRTM_SfcOptics_type      , &
---
>                                         CRTM_Combine_AtmOptics       , &
>                                         CRTM_Combine_AtmOptics_AD
>   USE CRTM_SfcOptics_Define,      ONLY: CRTM_SfcOptics_type      , &
98,99c83
<                                         CRTM_SfcOptics_Destroy   , &
<                                         CRTM_SfcOptics_Zero, crtm_sfcoptics_inspect
---
>                                         CRTM_SfcOptics_Destroy
102c86,87
<   USE CRTM_RTSolution,            ONLY: CRTM_Compute_nStreams     , &
---
>   USE CRTM_RTSolution,            ONLY: CRTM_RTSolution_type      , &
>                                         CRTM_Compute_nStreams     , &
122d106
<   USE CRTM_CloudCover_Define,     ONLY: CRTM_CloudCover_type
125,129d108
<   ! ...AtmOptics
<   USE AOvar_Define, ONLY: AOvar_type, &
<                           AOvar_Associated, &
<                           AOvar_Destroy   , &
<                           AOvar_Create
321c300,303
<     LOGICAL :: compute_antenna_correction
---
>     LOGICAL :: Check_Input
>     LOGICAL :: User_Emissivity, User_Direct_Reflectivity, User_N_Streams
>     LOGICAL :: User_AntCorr, Compute_AntCorr
>     LOGICAL :: Apply_NLTE_Correction
323c305
<     INTEGER :: Status_FWD, Status_K
---
>     INTEGER :: RT_Algorithm_Id
324a307
>     INTEGER :: nc, na
328c311
<     INTEGER :: ln
---
>     INTEGER :: j, ln
330d312
<     INTEGER :: cloud_coverage_flag
334,335d315
<     REAL(fp) :: transmittance_clear, transmittance_clear_K
<     REAL(fp) :: r_cloudy
339c319
<     TYPE(CRTM_Options_type) :: Default_Options, Opt
---
>     TYPE(CRTM_Options_type) :: Default_Options
342,347d321
<     ! Clear sky structures
<     TYPE(CRTM_Atmosphere_type) :: Atm_Clear       , Atm_Clear_K
<     TYPE(CRTM_AtmOptics_type)  :: AtmOptics_Clear , AtmOptics_Clear_K
<     TYPE(CRTM_SfcOptics_type)  :: SfcOptics_Clear , SfcOptics_Clear_K
<     TYPE(CRTM_RTSolution_type) :: RTSolution_Clear, RTSolution_Clear_K
<     TYPE(RTV_type)             :: RTV_Clear
362,363d335
<     ! Cloud cover object
<     TYPE(CRTM_CloudCover_type) :: CloudCover, CloudCover_K
423,426d394
<     ! Reinitialise the output RTSolution
<     CALL CRTM_RTSolution_Zero(RTSolution)
< 
< 
464,465c432,455
<         CALL CRTM_Atmosphere_NonVariableCopy( Atmosphere(m), Atmosphere_K(l,m) )
<         CALL CRTM_Surface_NonVariableCopy( Surface(m), Surface_K(l,m) )
---
>         ! ...Atmosphere
>         Atmosphere_K(l,m)%Climatology = Atmosphere(m)%Climatology
>         ! Loop over absorbers
>         DO j = 1, Atmosphere(m)%n_Absorbers
>           Atmosphere_K(l,m)%Absorber_ID(j)    = Atmosphere(m)%Absorber_ID(j)
>           Atmosphere_K(l,m)%Absorber_Units(j) = Atmosphere(m)%Absorber_Units(j)
>         END DO
>         ! Loop over and assign cloud types
>         DO nc = 1, Atmosphere(m)%n_Clouds
>           Atmosphere_K(l,m)%Cloud(nc)%Type = Atmosphere(m)%Cloud(nc)%Type
>         END DO
>         ! Loop over and assign aerosol types
>         DO na = 1, Atmosphere(m)%n_Aerosols
>           Atmosphere_K(l,m)%Aerosol(na)%Type = Atmosphere(m)%Aerosol(na)%Type
>         END DO
>         ! ...Surface
>         Surface_K(l,m)%Land_Coverage  = Surface(m)%Land_Coverage
>         Surface_K(l,m)%Water_Coverage = Surface(m)%Water_Coverage
>         Surface_K(l,m)%Snow_Coverage  = Surface(m)%Snow_Coverage
>         Surface_K(l,m)%Ice_Coverage   = Surface(m)%Ice_Coverage
>         Surface_K(l,m)%Land_Type  = Surface(m)%Land_Type
>         Surface_K(l,m)%Water_Type = Surface(m)%Water_Type
>         Surface_K(l,m)%Snow_Type  = Surface(m)%Snow_Type
>         Surface_K(l,m)%Ice_Type   = Surface(m)%Ice_Type
470c460,467
<       Opt = Default_Options
---
>       ! ...Specify default actions
>       Check_Input           = Default_Options%Check_Input
>       User_Emissivity       = Default_Options%Use_Emissivity
>       User_AntCorr          = Default_Options%Use_Antenna_Correction
>       Apply_NLTE_Correction = Default_Options%Apply_NLTE_Correction
>       RT_Algorithm_Id       = Default_Options%RT_Algorithm_Id
>       User_N_Streams        = Default_Options%Use_N_Streams
>       ! ...Check the Options argument
472c469,490
<         Opt = Options(m)
---
>         ! Override input checker with option
>         Check_Input = Options(m)%Check_Input
>         ! Check if the supplied emissivity should be used
>         User_Emissivity = Options(m)%Use_Emissivity
>         IF ( Options(m)%Use_Emissivity ) THEN
>           ! Are the channel dimensions consistent
>           IF ( Options(m)%n_Channels < n_Channels ) THEN
>             Error_Status = FAILURE
>             WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
>                    &"than the number of requested channels (",i0, ")" )' ) &
>                    Options(m)%n_Channels, n_Channels
>             CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
>             RETURN
>           END IF
>           ! Check if the supplied direct reflectivity should be used
>           User_Direct_Reflectivity = Options(m)%Use_Direct_Reflectivity
>         END IF
>         ! Check if antenna correction should be attempted
>         User_AntCorr = Options(m)%Use_Antenna_Correction
>         ! Set NLTE correction option
>         Apply_NLTE_Correction = Options(m)%Apply_NLTE_Correction
> 
475a494,510
>         ! Copy over surface optics input
>         SfcOptics%Use_New_MWSSEM = .NOT. Options(m)%Use_Old_MWSSEM
>         ! Specify the RT algorithm
>         RT_Algorithm_Id = Options(m)%RT_Algorithm_ID
>         ! Check if n_Streams should be used
>         User_N_Streams = Options(m)%Use_N_Streams
>         ! Check value for nstreams
>         IF ( User_N_Streams ) THEN
>           IF ( Options(m)%n_Streams <= 0 .OR. MOD(Options(m)%n_Streams,2) /= 0 .OR. &
>                Options(m)%n_Streams > MAX_N_STREAMS ) THEN
>               Error_Status = FAILURE
>               WRITE( Message,'( "Input Options n_Streams (", i0, ") is invalid" )' ) &
>                      Options(m)%n_Streams
>               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
>               RETURN
>           END IF
>         END IF
477,478d511
<       ! ...Assign the option specific SfcOptics input
<       SfcOptics%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
482c515
<       IF ( Opt%Check_Input ) THEN
---
>       IF ( Check_Input ) THEN
502,523d534
<           ! Are the channel dimensions consistent if emissivity is passed?
<           IF ( Options(m)%Use_Emissivity ) THEN
<             IF ( Options(m)%n_Channels < n_Channels ) THEN
<               Error_Status = FAILURE
<               WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
<                      &"than the number of requested channels (",i0, ")" )' ) &
<                      Options(m)%n_Channels, n_Channels
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
<           END IF
<           ! Check value for user-defined n_Streams
<           IF ( Options(m)%Use_N_Streams ) THEN
<             IF ( Options(m)%n_Streams <= 0 .OR. MOD(Options(m)%n_Streams,2) /= 0 .OR. &
<                  Options(m)%n_Streams > MAX_N_STREAMS ) THEN
<                 Error_Status = FAILURE
<                 WRITE( Message,'( "Input Options n_Streams (", i0, ") is invalid" )' ) &
<                        Options(m)%n_Streams
<                 CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                 RETURN
<             END IF
<           END IF
538a550,553
>       ! Average surface skin temperature for multi-surface types
>       CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )
> 
> 
557,559d571
< 
< 
<       ! Prepare the atmospheric optics structures
576,578c588,592
<       ! ...Set the Scattering Switch
<       AtmOptics%Include_Scattering   = Opt%Include_Scattering
<       AtmOptics_K%Include_Scattering = Opt%Include_Scattering
---
>       IF (Options_Present) THEN
>         ! Set Scattering Switch
>         AtmOptics%Include_Scattering   = Options(m)%Include_Scattering
>         AtmOptics_K%Include_Scattering = Options(m)%Include_Scattering
>       END IF
602,651d615
<       ! Determine the type of cloud coverage
<       cloud_coverage_flag = CRTM_Atmosphere_Coverage( atm )
< 
< 
<       ! Setup for fractional cloud coverage
<       IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<       
<         ! Compute cloudcover
<         Error_Status = CloudCover%Compute_CloudCover(atm, Overlap = opt%Overlap_Id)
<         IF ( Error_Status /= SUCCESS ) THEN
<           WRITE( Message,'("Error computing cloud cover in profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
<         ! ...Mold the K-matrix object based on the forward, and reinitialise
<         CloudCover_K = CloudCover
<         CALL CloudCover_K%Set_To_Zero()
< 
<         ! Allocate some of the CLEAR sky structure for fractional cloud coverage
<         ! (The AtmOptics structures are allocated during a copy)
<         ! ...Clear sky atmosphere
<         Error_Status = CRTM_Atmosphere_ClearSkyCopy(Atm, Atm_Clear)
<         IF ( Error_Status /= SUCCESS  ) THEN
<           Error_status = FAILURE
<           WRITE( Message,'("Error copying CLEAR SKY Atmosphere structures for profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
<         ! ...Clear sky SfcOptics
<         CALL CRTM_SfcOptics_Create( SfcOptics_Clear  , MAX_N_ANGLES, MAX_N_STOKES )
<         CALL CRTM_SfcOptics_Create( SfcOptics_Clear_K, MAX_N_ANGLES, MAX_N_STOKES )
<         IF ( (.NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear)) .OR. &
<              (.NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear_K))) THEN
<           Error_Status = FAILURE
<           WRITE( Message,'("Error allocating CLEAR SKY SfcOptics data structures for profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
<         ! ...Copy over surface optics input
<         SfcOptics_Clear%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
<         ! ...CLEAR SKY average surface skin temperature for multi-surface types
<         CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics_Clear )
<       END IF
< 
< 
<       ! Average surface skin temperature for multi-surface types
<       CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )
< 
< 
< 
666c630
<         compute_antenna_correction = ( Opt%Use_Antenna_Correction               .AND. &
---
>         IF ( User_AntCorr                             .AND. &
668c632,636
<                                        iFOV /= 0 )
---
>              iFOV /= 0 ) THEN
>           Compute_AntCorr = .TRUE.
>         ELSE
>           Compute_AntCorr = .FALSE.
>         END IF
703,704c671
<               SpcCoeff_IsVisibleSensor(SC(SensorIndex)) ) .AND. &
<             AtmOptics%Include_Scattering ) THEN
---
>             SpcCoeff_IsVisibleSensor( SC(SensorIndex) ) ) .and. AtmOptics%Include_Scattering ) THEN
714c681
<           RTV%RT_Algorithm_Id = Opt%RT_Algorithm_Id
---
>           RTV%RT_Algorithm_Id = RT_Algorithm_Id
718,719c685,686
<         ! Compute NLTE correction predictors
<         IF ( Opt%Apply_NLTE_Correction ) THEN
---
>         ! Compute NLTE predictors
>         IF ( Apply_NLTE_Correction ) THEN
750,758d716
<           ! ...Same for clear structures
<           RTSolution_Clear%Sensor_Id        = RTSolution(ln,m)%Sensor_Id
<           RTSolution_Clear%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
<           RTSolution_Clear%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id
<           RTSolution_Clear%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel
<           RTSolution_Clear_K%Sensor_Id        = RTSolution(ln,m)%Sensor_Id
<           RTSolution_Clear_K%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
<           RTSolution_Clear_K%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id
<           RTSolution_Clear_K%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel
763,765d720
<           CALL CRTM_AtmOptics_Zero( AtmOptics_K )
<           CALL CRTM_AtmOptics_Zero( AtmOptics_Clear )
<           CALL CRTM_AtmOptics_Zero( AtmOptics_Clear_K )
767,768d721
<           CALL CRTM_RTSolution_Zero( RTSolution_Clear )
<           CALL CRTM_RTSolution_Zero( RTSolution_Clear_K )
773,787d725
<           ! ...Same for K-matrix CLEAR sky structure for fractional cloud coverage
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             Error_Status = CRTM_Atmosphere_ClearSkyCopy(Atm_K, Atm_Clear_K)
<             IF ( Error_Status /= SUCCESS  ) THEN
<               Error_status = FAILURE
<               WRITE( Message,'("Error copying CLEAR SKY Atmosphere_K structure for ",a,&
<                      &", channel ",i0,", profile #",i0)') &
<                      TRIM(ChannelInfo(n)%Sensor_ID), &
<                      ChannelInfo(n)%Sensor_Channel(l), &
<                      m
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
<             CALL CRTM_Atmosphere_Zero( Atm_Clear_K )
<           END IF
791c729
<           IF ( Opt%Use_N_Streams ) THEN
---
>           IF ( User_N_Streams ) THEN
804,805d741
<           ! ...Ensure clear-sky object dimensions are consistent
<           AtmOptics_Clear_K%n_Legendre_Terms = AtmOptics_K%n_Legendre_Terms
816a753,757
>           ! Compute the clear-sky atmospheric transmittance
>           ! for use in FASTEM-X reflection correction
>           CALL CRTM_Compute_Transmittance(AtmOptics,transmittance)
> 
> 
852,872d792
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               RTV_Clear%Visible_Flag_true = .FALSE.
<               RTV_Clear%n_Azi = 0
<             END IF
<           END IF
< 
< 
<           ! Copy the clear-sky AtmOptics
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             Status_FWD = CRTM_AtmOptics_NoScatterCopy( AtmOptics, AtmOptics_Clear )
<             Status_K   = CRTM_AtmOptics_NoScatterCopy( AtmOptics, AtmOptics_Clear_K )
<             IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
<               Error_Status = FAILURE
<               WRITE( Message,'("Error copying CLEAR SKY AtmOptics for ",a,&
<                      &", channel ",i0,", profile #",i0)' ) &
<                      TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
<             ! Initialise the adjoint
<             CALL CRTM_AtmOptics_Zero( AtmOptics_Clear_K )
912c832
<             CALL CRTM_AtmOptics_Combine( AtmOptics, AOvar )
---
>             CALL CRTM_Combine_AtmOptics( AtmOptics, AOvar )
918,920c838,841
<           ! Compute the all-sky atmospheric transmittance
<           ! for use in FASTEM-X reflection correction
<           CALL CRTM_Compute_Transmittance(AtmOptics,transmittance)
---
>           ! Turn off FASTEM-X reflection correction for scattering conditions
>           IF ( CRTM_Include_Scattering(AtmOptics) .AND. SpcCoeff_IsMicrowaveSensor( SC(SensorIndex) ) ) THEN
>             SfcOptics%Transmittance = -ONE
>           ELSE
922,925d842
<           ! ...Clear sky for fractional cloud cover
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             CALL CRTM_Compute_Transmittance(AtmOptics_Clear,transmittance_clear)
<             SfcOptics_Clear%Transmittance = transmittance_clear
929a847
>           ! ...Indicate SfcOptics ARE to be computed
931,932c849,850
<           SfcOptics_Clear%Compute = .TRUE.
<           IF ( Opt%Use_Emissivity ) THEN
---
>           ! Change SfcOptics emissivity/reflectivity contents/computation status
>           IF ( User_Emissivity ) THEN
934,937c852,855
<             SfcOptics%Emissivity(1,1)       = Opt%Emissivity(ln)
<             SfcOptics%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
<             IF ( Opt%Use_Direct_Reflectivity ) THEN
<               SfcOptics%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
---
>             SfcOptics%Emissivity(1,1)       = Options(m)%Emissivity(ln)
>             SfcOptics%Reflectivity(1,1,1,1) = ONE - Options(m)%Emissivity(ln)
>             IF ( User_Direct_Reflectivity ) THEN
>               SfcOptics%Direct_Reflectivity(1,1) = Options(m)%Direct_Reflectivity(ln)
941,951c859
<             ! ...Repeat for fractional clear-sky case
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               SfcOptics_Clear%Compute = .FALSE.
<               SfcOptics_Clear%Emissivity(1,1)       = Opt%Emissivity(ln)
<               SfcOptics_Clear%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
<               IF ( Opt%Use_Direct_Reflectivity ) THEN
<                 SfcOptics_Clear%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
<               ELSE
<                 SfcOptics_Clear%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
<               END IF
<             END IF
---
> 
958a867,868
>           ! ...Initialise K-matrix atmospheric optics
>           CALL CRTM_AtmOptics_Zero( AtmOptics_K )
994a905,911
>             ! Compute non-LTE correction to radiance if required
>             IF ( Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) &
>               CALL Compute_NLTE_Correction( &
>                      SC(SensorIndex)%NC       , &  ! Input
>                      ChannelIndex             , &  ! Input
>                      NLTE_Predictor           , &  ! Input
>                      RTSolution(ln,m)%Radiance  )  ! In/Output
996,997c913,918
<             ! Perform clear sky calcs for fractionally cloudy atmospheres
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
---
>             ! Convert the radiance to brightness temperature
>             CALL CRTM_Planck_Temperature( &
>                    SensorIndex                            , & ! Input
>                    ChannelIndex                           , & ! Input
>                    RTSolution(ln,m)%Radiance              , & ! Input
>                    RTSolution(ln,m)%Brightness_Temperature  ) ! Output
999,1006c920,922
<               ! Repeat radiative transfer for clear-sky
<               RTV_Clear%mth_Azi = RTV%mth_Azi
<               SfcOptics_Clear%mth_Azi = SfcOptics%mth_Azi
<               Error_Status = CRTM_Compute_RTSolution( &
<                                Atm_Clear       , &  ! Input
<                                Surface(m)      , &  ! Input
<                                AtmOptics_Clear , &  ! Input
<                                SfcOptics_Clear , &  ! Input
---
>             ! Compute Antenna correction to brightness temperature if required
>             IF ( Compute_AntCorr ) THEN
>               CALL CRTM_Compute_AntCorr( &
1010,1076c926,927
<                                RTSolution_Clear, &  ! Output
<                                RTV_Clear         )  ! Internal variable output
<               IF ( Error_Status /= SUCCESS ) THEN
<                 WRITE( Message,'( "Error computing CLEAR SKY RTSolution for ", a, &
<                        &", channel ", i0,", profile #",i0)' ) &
<                        TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<                 CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                 RETURN
<               END IF
< 
<               ! Combine cloudy and clear radiances for fractional cloud coverage
<               r_cloudy = RTSolution(ln,m)%Radiance  ! Save the 100% cloudy radiance
<               RTSolution(ln,m)%Radiance = &
<                   ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_Clear%Radiance) + &
<                   (CloudCover%Total_Cloud_Cover * RTSolution(ln,m)%Radiance)
<               ! ...Save the cloud cover in the output structure
<               RTSolution(ln,m)%Total_Cloud_Cover = CloudCover%Total_Cloud_Cover
<             END IF
< 
< 
<             ! The radiance post-processing
<             CALL Post_Process_RTSolution(RTSolution(ln,m))
<             
<             
<             ! Perform clear-sky post and pre-processing
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               ! Radiance post-processing
<               CALL Post_Process_RTSolution(RTSolution_Clear)
<               RTSolution(ln,m)%R_Clear  = RTSolution_Clear%Radiance
<               RTSolution(ln,m)%Tb_Clear = RTSolution_Clear%Brightness_Temperature
< 
<               ! Adjoint radiance pre-processing
<               RTSolution_Clear_K%Brightness_Temperature = RTSolution_Clear_K%Brightness_Temperature + &
<                                                           RTSolution_K(ln,m)%Tb_Clear
<               RTSolution_K(ln,m)%Tb_Clear               = ZERO
<               RTSolution_Clear_K%Radiance = RTSolution_Clear_K%Radiance + &
<                                             RTSolution_K(ln,m)%R_Clear
<               RTSolution_K(ln,m)%R_Clear  = ZERO
<               CALL Pre_Process_RTSolution_K(RTSolution_Clear, RTSolution_Clear_K)
<             END IF
< 
< 
<             ! The adjoint radiance pre-processing
<             CALL Pre_Process_RTSolution_K(RTSolution(ln,m), RTSolution_K(ln,m))
< 
< 
<             ! More fractionally cloudy atmospheres processing
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
< 
<               ! The adjoint of the clear and cloudy radiance combination
<               CloudCover_K%Total_Cloud_Cover = CloudCover_K%Total_Cloud_Cover + &
<                                                RTSolution_K(ln,m)%Total_Cloud_Cover
<               RTSolution_K(ln,m)%Total_Cloud_Cover = ZERO
<               RTSolution_Clear_K%Radiance    = RTSolution_Clear_K%Radiance + &
<                                                ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_K(ln,m)%Radiance)
<               CloudCover_K%Total_Cloud_Cover = CloudCover_K%Total_Cloud_Cover + &
<                                                ((r_cloudy - RTSolution_Clear%Radiance) * RTSolution_K(ln,m)%Radiance)
<               RTSolution_K(ln,m)%Radiance    = CloudCover%Total_Cloud_Cover * RTSolution_K(ln,m)%Radiance
< 
<               ! The adjoint of the clear sky radiative transfer for fractionally cloudy atmospheres
<               Error_Status = CRTM_Compute_RTSolution_AD( &
<                                Atm_Clear         , &  ! FWD Input
<                                Surface(m)        , &  ! FWD Input
<                                AtmOptics_Clear   , &  ! FWD Input
<                                SfcOptics_Clear   , &  ! FWD Input
<                                RTSolution_Clear  , &  ! FWD Input
<                                RTSolution_Clear_K, &  ! K   Input
---
>                      RTSolution(ln,m)  )  ! Output
>               CALL CRTM_Compute_AntCorr_AD( &
1080,1091c931
<                                Atm_Clear_K       , &  ! K  Output
<                                Surface_K(ln,m)   , &  ! K  Output
<                                AtmOptics_Clear_K , &  ! K  Output
<                                SfcOptics_Clear_K , &  ! K  Output
<                                RTV_Clear            )  ! Internal variable input
<               IF ( Error_Status /= SUCCESS ) THEN
<                 WRITE( Message,'( "Error computing CLEAR SKY RTSolution_K for ", a, &
<                        &", channel ", i0,", profile #",i0)' ) &
<                        TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<                 CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                 RETURN
<               END IF
---
>                      RTSolution_K(ln,m)  )  ! Output
1093a934,949
>             ! Compute the Planck temperature adjoijnt
>             CALL CRTM_Planck_Temperature_AD( &
>                    SensorIndex                              , & ! Input
>                    ChannelIndex                             , & ! Input
>                    RTSolution(ln,m)%Radiance                , & ! Input
>                    RTSolution_K(ln,m)%Brightness_Temperature, & ! Input
>                    RTSolution_K(ln,m)%Radiance                ) ! Output
>             RTSolution_K(ln,m)%Brightness_Temperature = ZERO
> 
>             ! Compute non-LTE correction adjoint if required
>             IF ( Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) &
>               CALL Compute_NLTE_Correction_AD( &
>                      SC(SensorIndex)%NC         , &  ! Input
>                      ChannelIndex               , &  ! Input
>                      RTSolution_K(ln,m)%Radiance, &  ! Input
>                      NLTE_Predictor_K             )  ! Output
1150,1243d1005
< 
<               ! Repeat clear sky for fractionally cloudy atmospheres
<               IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<                 RTV_Clear%mth_Azi = RTV%mth_Azi
<                 SfcOptics_Clear%mth_Azi = SfcOptics%mth_Azi
<                 Error_Status = CRTM_Compute_RTSolution( &
<                                  Atm_Clear       , &  ! Input
<                                  Surface(m)      , &  ! Input
<                                  AtmOptics_Clear , &  ! Input
<                                  SfcOptics_Clear , &  ! Input
<                                  GeometryInfo    , &  ! Input
<                                  SensorIndex     , &  ! Input
<                                  ChannelIndex    , &  ! Input
<                                  RTSolution_Clear, &  ! Output
<                                  RTV_Clear         )  ! Internal variable output
<                 IF ( Error_Status /= SUCCESS ) THEN
<                   WRITE( Message,'( "Error computing CLEAR SKY RTSolution for ", a, &
<                          &", channel ", i0,", profile #",i0)' ) &
<                          TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<                   CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                   RETURN
<                 END IF
<               END IF
<             END DO Azimuth_Fourier_Loop
< 
< 
<             ! All of the "in-between" FWD and AD processing is for fractional cloud coverage only
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
< 
<               ! FORWARD #1: Combine cloudy and clear radiances for fractional cloud coverage
<               r_cloudy = RTSolution(ln,m)%Radiance  ! Save the 100% cloudy radiance
<               RTSolution(ln,m)%Radiance = &
<                   ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_Clear%Radiance) + &
<                   (CloudCover%Total_Cloud_Cover * RTSolution(ln,m)%Radiance)
<               ! FORWARD #2: Save the cloud cover and clear radiance in the output structure
<               RTSolution(ln,m)%Total_Cloud_Cover = CloudCover%Total_Cloud_Cover
<               RTSolution(ln,m)%R_Clear           = RTSolution_Clear%Radiance
<               RTSolution(ln,m)%Tb_Clear          = ZERO      ! No Tb for visible
< 
<               ! ADJOINT #2: Of the cloud cover and clear radiance saving
<               RTSolution_Clear_K%Tb_Clear = ZERO   ! No Tb for visible
<               RTSolution_Clear_K%Radiance = RTSolution_Clear_K%Radiance + &
<                                             RTSolution_K(ln,m)%R_Clear
<               RTSolution_K(ln,m)%R_Clear  = ZERO
<               CloudCover_K%Total_Cloud_Cover = CloudCover_K%Total_Cloud_Cover + &
<                                                RTSolution_K(ln,m)%Total_Cloud_Cover
<               RTSolution_K(ln,m)%Total_Cloud_Cover = ZERO
< 
<               ! ADJOINT #1: Of the clear+cloudy combination
<               RTSolution_Clear_K%Radiance    = RTSolution_Clear_K%Radiance + &
<                                                ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_K(ln,m)%Radiance)
<               CloudCover_K%Total_Cloud_Cover = CloudCover_K%Total_Cloud_Cover + &
<                                                ((r_cloudy - RTSolution_Clear%Radiance) * RTSolution_K(ln,m)%Radiance)
<               RTSolution_K(ln,m)%Radiance    = CloudCover%Total_Cloud_Cover * RTSolution_K(ln,m)%Radiance
<             END IF
< 
< 
<             ! Adjoint Fourier expansion over azimuth angle
<             Azimuth_Fourier_Loop_K: DO mth_Azi = 0, RTV%n_Azi
< 
<               ! Set dependent component counters
<               RTV%mth_Azi = mth_Azi
<               SfcOptics%mth_Azi = mth_Azi
< 
< 
<               ! The adjoint of the clear sky radiative transfer for fractionally cloudy atmospheres
<               IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<                 RTV_Clear%mth_Azi = RTV%mth_Azi
<                 SfcOptics_Clear%mth_Azi = SfcOptics%mth_Azi
<                 Error_Status = CRTM_Compute_RTSolution_AD( &
<                                  Atm_Clear         , &  ! FWD Input
<                                  Surface(m)        , &  ! FWD Input
<                                  AtmOptics_Clear   , &  ! FWD Input
<                                  SfcOptics_Clear   , &  ! FWD Input
<                                  RTSolution_Clear  , &  ! FWD Input
<                                  RTSolution_Clear_K, &  ! AD  Input
<                                  GeometryInfo      , &  ! Input
<                                  SensorIndex       , &  ! Input
<                                  ChannelIndex      , &  ! Input
<                                  Atm_Clear_K       , &  ! AD Output
<                                  Surface_K(ln,m)   , &  ! AD Output
<                                  AtmOptics_Clear_K , &  ! AD Output
<                                  SfcOptics_Clear_K , &  ! AD Output
<                                  RTV_Clear           )  ! Internal variable input
<                 IF ( Error_Status /= SUCCESS ) THEN
<                   WRITE( Message,'( "Error computing CLEAR SKY RTSolution_AD for ", a, &
<                          &", channel ", i0,", profile #",i0)' ) &
<                          TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<                   CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                   RETURN
<                 END IF
<               END IF
< 
< 
1267c1029,1036
<             END DO Azimuth_Fourier_Loop_K
---
>             END DO Azimuth_Fourier_Loop
> 
>             ! Still want to convert the final FORWARD radiance to brightness temperature
>             CALL CRTM_Planck_Temperature( &
>                    SensorIndex                            , & ! Input
>                    ChannelIndex                           , & ! Input
>                    RTSolution(ln,m)%Radiance              , & ! Input
>                    RTSolution(ln,m)%Brightness_Temperature  ) ! Output
1275,1287d1043
<           ! Compute the adjoint of the all-sky atmospheric transmittance
<           ! for use in FASTEM-X reflection correction
<           transmittance_K = SfcOptics_K%transmittance
<           SfcOptics_K%transmittance = ZERO
<           CALL CRTM_Compute_Transmittance_AD(AtmOptics,transmittance_K,AtmOptics_K)
<           ! ...Clear sky for fractional cloud cover
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             transmittance_clear_K = SfcOptics_Clear_K%transmittance
<             SfcOptics_Clear_K%transmittance = ZERO
<             CALL CRTM_Compute_Transmittance_AD(AtmOptics_Clear,transmittance_clear_K,AtmOptics_Clear_K)
<           END IF
< 
< 
1289,1291d1044
<           AtmOptics_K%Scattering_Optical_Depth = AtmOptics_K%Scattering_Optical_Depth + &
<                                                  RTSolution_K(ln,m)%SOD
<           RTSolution_K(ln,m)%SOD               = ZERO
1293c1046
<             CALL CRTM_AtmOptics_Combine_AD( AtmOptics, AtmOptics_K, AOvar )
---
>             CALL CRTM_Combine_AtmOptics_AD( AtmOptics, AtmOptics_K, AOvar )
1335,1347d1087
<           ! Adjoint of clear-sky AtmOptics copy
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             Error_Status = CRTM_AtmOptics_NoScatterCopy_AD( AtmOptics, AtmOptics_Clear_K, AtmOptics_K )
<             IF ( Error_Status /= SUCCESS ) THEN
<               WRITE( Message,'("Error computing CLEAR SKY AtmOptics_K for ",a,&
<                      &", channel ",i0,", profile #",i0)' ) &
<                      TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
<           END IF
< 
< 
1366a1107,1114
>           ! Compute the adjoint of the total atmospheric transmittance
>           IF ( CRTM_No_Scattering(AtmOptics) .AND. SpcCoeff_IsMicrowaveSensor(SC(SensorIndex)) ) THEN
>             transmittance_K = SfcOptics_K%transmittance
>             SfcOptics_K%transmittance = ZERO
>             CALL CRTM_Compute_Transmittance_AD(AtmOptics,transmittance_K,AtmOptics_K)
>           END IF
> 
> 
1377c1125
<           IF ( Opt%Apply_NLTE_Correction ) THEN
---
>           IF ( Apply_NLTE_Correction ) THEN
1395c1143,1144
<           ! K-matrix of average surface skin temperature for multi-surface types
---
>           ! Postprocess some input data
>           ! ...K-matrix of average surface skin temperature for multi-surface types
1397,1434c1146
< 
< 
<           ! Adjoint of cloud cover setup
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
< 
<             ! Post process the CLEAR sky structures for fractional cloud coverage
<             ! ...Clear sky SfcOptics
<             CALL CRTM_Compute_SurfaceT_AD( Surface(m), SfcOptics_Clear_K, Surface_K(ln,m) )
<             CALL CRTM_SfcOptics_Zero(SfcOptics_Clear_K)
<             ! ...Clear sky atmosphere
<             Error_Status = CRTM_Atmosphere_ClearSkyCopy_AD(Atm, Atm_Clear_K, Atm_K)
<             IF ( Error_Status /= SUCCESS ) THEN
<               Error_status = FAILURE
<               WRITE( Message,'("Error computing CLEAR SKY Atm_K object for ",a,&
<                      &", channel ",i0,", profile #",i0)' ) &
<                      TRIM(ChannelInfo(n)%Sensor_ID), &
<                      ChannelInfo(n)%Sensor_Channel(l), &
<                      m
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
< 
<             ! K-matrix of the cloud coverage
<             Error_Status = CloudCover_K%Compute_CloudCover_AD(CloudCover, atm, atm_K)
<             IF ( Error_Status /= SUCCESS ) THEN
<               Error_Status = FAILURE
<               WRITE( Message,'("Error computing K-MATRIX cloud cover for ",a,&
<                      &", channel ",i0,", profile #",i0)' ) &
<                      TRIM(ChannelInfo(n)%Sensor_ID), &
<                      ChannelInfo(n)%Sensor_Channel(l), &
<                      m
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
<           END IF
<           
< 
<           ! K-matrix of the atmosphere layer addition
---
>           ! ...K-matrix of the atmosphere layer addition
1438,1442c1150
<             WRITE( Message,'("Error computing K-MATRIX atmosphere extra layers for ",a,&
<                    &", channel ",i0,", profile #",i0)' ) &
<                    TRIM(ChannelInfo(n)%Sensor_ID), &
<                    ChannelInfo(n)%Sensor_Channel(l), &
<                    m
---
>             WRITE( Message,'("Error adding AD extra layers to profile #",i0)' ) m
1449,1452d1156
<       END DO Sensor_Loop
< 
<     END DO Profile_Loop
< 
1454c1158,1161
<     ! Clean up
---
>         ! Deallocate local sensor dependent data structures
>         ! ...RTV structure
>         IF ( RTV_Associated(RTV) ) CALL RTV_Destroy(RTV)
>         ! ...Predictor structures
1457,1513d1163
<     CALL CRTM_AtmOptics_Destroy( AtmOptics )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics_K )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear_K )
<     CALL CRTM_SfcOptics_Destroy( SfcOptics )
<     CALL CRTM_SfcOptics_Destroy( SfcOptics_K )
<     CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear )
<     CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear_K )
<     CALL CRTM_Atmosphere_Destroy( Atm )
<     CALL CRTM_Atmosphere_Destroy( Atm_K )
<     CALL CRTM_Atmosphere_Destroy( Atm_Clear )
<     CALL CRTM_Atmosphere_Destroy( Atm_Clear_K )
<     ! ...Internal variables
<     CALL AOvar_Destroy( AOvar )
<     CALL CSvar_Destroy( CSvar )
<     CALL ASvar_Destroy( ASvar )
<     CALL RTV_Destroy( RTV )
< 
< 
< 
< CONTAINS
< 
< 
<     ! ----------------------------------------------------------------
<     ! Local subroutine to post-process the FORWARD radiance, as it is
<     ! the same for all-sky and fractional clear-sky cases.
<     !
<     !   1. Apply non-LTE correction to radiance
<     !   2. Convert radiance to brightness temperature
<     !   3. Apply antenna correction to brightness temperature
<     ! ----------------------------------------------------------------
< 
<     SUBROUTINE Post_Process_RTSolution(rts)
<       TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: rts
< 
<       ! Compute non-LTE correction to radiance if required
<       IF ( Opt%Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
<         CALL Compute_NLTE_Correction( &
<                SC(SensorIndex)%NC, &  ! Input
<                ChannelIndex      , &  ! Input
<                NLTE_Predictor    , &  ! Input
<                rts%Radiance        )  ! In/Output
<       END IF
<       ! Convert the radiance to brightness temperature
<       CALL CRTM_Planck_Temperature( &
<              SensorIndex               , & ! Input
<              ChannelIndex              , & ! Input
<              rts%Radiance              , & ! Input
<              rts%Brightness_Temperature  ) ! Output
<       ! Compute Antenna correction to brightness temperature if required
<       IF ( compute_antenna_correction ) THEN
<         CALL CRTM_Compute_AntCorr( &
<                GeometryInfo, &  ! Input
<                SensorIndex , &  ! Input
<                ChannelIndex, &  ! Input
<                rts           )  ! Output
<       END IF
1515c1165
<     END SUBROUTINE Post_Process_RTSolution
---
>       END DO Sensor_Loop
1518,1525c1168,1171
<     ! ----------------------------------------------------------------
<     ! Local subroutine to pre-process the K-MATRIX radiance, as it is
<     ! the same for all-sky and fractional clear-sky cases.
<     !
<     !   1. Apply adjoint antenna correction to brightness temperatures
<     !   2. Convert adjoint radiances to brightness temperatures
<     !   3. Apply adjoint non-LTE correction to radiances
<     ! ----------------------------------------------------------------
---
>       ! Deallocate local sensor independent data structures
>       ! ...Atmospheric optics
>       CALL CRTM_AtmOptics_Destroy( AtmOptics )
>       CALL CRTM_AtmOptics_Destroy( AtmOptics_K )
1527,1528c1173
<     SUBROUTINE Pre_Process_RTSolution_K(rts, rts_K)
<       TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: rts, rts_K
---
>     END DO Profile_Loop
1530,1553d1174
<       ! Compute adjoint antenna correction to brightness temperature if required
<       IF ( compute_antenna_correction ) THEN
<         CALL CRTM_Compute_AntCorr_AD( &
<                GeometryInfo, &  ! Input
<                SensorIndex , &  ! Input
<                ChannelIndex, &  ! Input
<                rts_K         )  ! Output
<       END IF
<       ! Compute the Planck temperature adjoint
<       CALL CRTM_Planck_Temperature_AD( &
<              SensorIndex                 , & ! Input
<              ChannelIndex                , & ! Input
<              rts%Radiance                , & ! Input
<              rts_K%Brightness_Temperature, & ! Input
<              rts_K%Radiance                ) ! Output
<       rts_K%Brightness_Temperature = ZERO
<       ! Compute non-LTE correction adjoint if required
<       IF ( Opt%Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
<         CALL Compute_NLTE_Correction_AD( &
<                SC(SensorIndex)%NC, &  ! Input
<                ChannelIndex      , &  ! Input
<                rts_K%Radiance    , &  ! Input
<                NLTE_Predictor_K    )  ! Output
<       END IF
1555c1176,1180
<     END SUBROUTINE Pre_Process_RTSolution_K
---
>     ! Destroy any remaining structures
>     CALL CRTM_SfcOptics_Destroy( SfcOptics )
>     CALL CRTM_SfcOptics_Destroy( SfcOptics_K )
>     CALL CRTM_Atmosphere_Destroy( Atm_K )
>     CALL CRTM_Atmosphere_Destroy( Atm )
diff -w ./CRTM_LifeCycle.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_LifeCycle.f90
diff -w ./CRTM_LowFrequency_MWSSEM.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_LowFrequency_MWSSEM.f90
diff -w ./CRTM_MW_Ice_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_MW_Ice_SfcOptics.f90
41,42c41
<                                           WMO_SSMIS, &
<                                           WMO_ATMS
---
>                                           WMO_SSMIS
49,50d47
<   USE NESDIS_ATMS_SeaICE_Module,    ONLY: NESDIS_ATMS_SeaICE
< 
190d186
<     INTEGER,  PARAMETER :: ATMS_INDEX(5)    = (/1, 2, 3, 16,17/)          ! With mixed polarisations
203,214d198
<       ! ATMSemissivity model
<       CASE( WMO_ATMS )    
<          DO i = 1, SfcOptics%n_Angles
<           CALL NESDIS_ATMS_SeaICE(  Sensor_Zenith_Angle,                     &  ! Input, Degree           
<                                    SfcOptics%Angle(i),                      &  ! Input, Degree           
<                                    SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz                  
<                                    Surface%Ice_Temperature,                 &  ! Input, K
<                                    Surface%SensorData%Tb(ATMS_INDEX),       &  ! Input, ATMS           
<                                    SfcOptics%Emissivity(i,2),               &  ! Output, H component      
<                                    SfcOptics%Emissivity(i,1)   )               ! Output, V component 
<        END DO                                                                                           
< 
diff -w ./CRTM_MW_Land_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_MW_Land_SfcOptics.f90
diff -w ./CRTM_MW_Snow_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_MW_Snow_SfcOptics.f90
diff -w ./CRTM_MW_Water_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_MW_Water_SfcOptics.f90
78c78
<     TYPE(FastemX_type), DIMENSION(MAX_N_ANGLES)   :: FastemX_Var
---
>     TYPE(FastemX_type) :: FastemX_Var
80c80
<     TYPE(LF_MWSSEM_type), DIMENSION(MAX_N_ANGLES) :: LF_MWSSEM_Var
---
>     TYPE(LF_MWSSEM_type) :: LF_MWSSEM_Var
195c195
<     TYPE(iVar_type),              INTENT(OUT)    :: iVar
---
>     TYPE(iVar_type),              INTENT(IN OUT) :: iVar
227d226
<                SfcOptics%n_Angles                     , &  ! Input
232c231
<                iVar%FastemX_Var(i)                    , &  ! Internal variable output
---
>                iVar%FastemX_Var                       , &  ! Internal variable output
255c254
<                  iVar%LF_MWSSEM_Var(i)      )  ! Internal variable output
---
>                  iVar%LF_MWSSEM_Var         )  ! Internal variable output
434c433
<                iVar%FastemX_Var(i)                         , &  ! Internal variable input
---
>                iVar%FastemX_Var                            , &  ! Internal variable input
440,441d438
<           !we probably need further low-level check and modifications
<           !SfcOptics_TL%Reflectivity(i,j,i,j) = -Reflectivity_TL(j)
457c454
<                  iVar%LF_MWSSEM_Var(i)         )  ! Internal variable input
---
>                  iVar%LF_MWSSEM_Var            )  ! Internal variable input
637c634
<                iVar%FastemX_Var(i)                          , &  ! Internal variable input
---
>                iVar%FastemX_Var                             , &  ! Internal variable input
659c656
<                  iVar%LF_MWSSEM_Var(i)         )  ! Internal variable input
---
>                  iVar%LF_MWSSEM_Var            )  ! Internal variable input
diff -w ./CRTM_MWwaterCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_MWwaterCoeff.f90
diff -w ./CRTM_Model_Profiles.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Model_Profiles.f90
diff -w ./CRTM_MoleculeScatter.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_MoleculeScatter.f90
diff -w ./CRTM_NLTECorrection.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_NLTECorrection.f90
diff -w ./CRTM_Options_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Options_Define.f90
28,29c28
<   USE CRTM_Parameters      , ONLY: RT_ADA, RT_SOI, &
<                                    MAX_N_STREAMS
---
>   USE CRTM_Parameters      , ONLY: RT_ADA
46,53d44
<   USE CRTM_CloudCover_Define, ONLY: DEFAULT_OVERLAP_ID, &
<                                     CloudCover_Maximum_Overlap, &
<                                     CloudCover_Random_Overlap , &
<                                     CloudCover_MaxRan_Overlap , &
<                                     CloudCover_Average_Overlap, &
<                                     CloudCover_Overcast_Overlap, &
<                                     CloudCover_Overlap_IsValid, &
<                                     CloudCover_Overlap_Name
65,67d55
<   ! ...Inherited types
<   PUBLIC :: SSU_Input_type
<   PUBLIC :: Zeeman_Input_type
77,78d64
<   PUBLIC :: CRTM_Options_SetValue
<   PUBLIC :: CRTM_Options_SetEmissivity
85d70
< 
89a75
> 
93,97d78
<   INTERFACE CRTM_Options_SetEmissivity
<     MODULE PROCEDURE SetEmissivity_scalar
<     MODULE PROCEDURE SetEmissivity_rank1
<   END INTERFACE CRTM_Options_SetEmissivity
< 
154,156d134
<     ! Cloud cover overlap id is set to averaging type by default
<     INTEGER(Long) :: Overlap_Id = DEFAULT_OVERLAP_ID
< 
174d151
< 
194,565d170
< !   CRTM_Options_SetValue
< !
< ! PURPOSE:
< !   Elemental subroutine to set the values of the non-dimensional,
< !   non-contained-object CRTM_Options object components.
< !
< ! CALLING SEQUENCE:
< !   CALL CRTM_Options_SetValue( &
< !          Options                                          , &
< !          Check_Input             = Check_Input            , &
< !          Use_Old_MWSSEM          = Use_Old_MWSSEM         , &
< !          Use_Antenna_Correction  = Use_Antenna_Correction , &
< !          Apply_NLTE_Correction   = Apply_NLTE_Correction  , &
< !          Set_ADA_RT              = Set_ADA_RT             , &
< !          Set_SOI_RT              = Set_SOI_RT             , &
< !          Include_Scattering      = Include_Scattering     , &
< !          Set_Maximum_Overlap     = Set_Maximum_Overlap    , &
< !          Set_Random_Overlap      = Set_Random_Overlap     , &
< !          Set_MaxRan_Overlap      = Set_MaxRan_Overlap     , &
< !          Set_Average_Overlap     = Set_Average_Overlap    , &
< !          Set_Overcast_Overlap    = Set_Overcast_Overlap   , &
< !          Use_Emissivity          = Use_Emissivity         , &
< !          Use_Direct_Reflectivity = Use_Direct_Reflectivity, &
< !          n_Streams               = n_Streams              , &
< !          Aircraft_Pressure       = Aircraft_Pressure        )
< !
< ! OBJECTS:
< !   Options:                  Options object for which the indicated component
< !                             values are to be set.
< !                             UNITS:      N/A
< !                             TYPE:       CRTM_Options_type
< !                             DIMENSION:  Scalar or any rank
< !                             ATTRIBUTES: INTENT(IN OUT)
< !
< ! OPTIONAL INPUTS:
< !   Check_Input:              Set this logical argument to control checking of 
< !                             the CRTM input data.
< !                             If == .TRUE. , the CRTM input data is checked [DEFAULT]
< !                                == .FALSE., no input data checking is done.
< !                             UNITS:      N/A
< !                             TYPE:       LOGICAL
< !                             DIMENSION:  Conformable with Options object
< !                             ATTRIBUTES: INTENT(IN), OPTIONAL
< !     
< !   Use_Old_MWSSEM:           Set this logical argument to invoke the previous version
< !                             of the microwave sea surface emissivity model.
< !                             If == .TRUE. , the old model is used.
< !                                == .FALSE., the current model is used [DEFAULT]
< !                             UNITS:      N/A
< !                             TYPE:       LOGICAL
< !                             DIMENSION:  Conformable with Options object
< !                             ATTRIBUTES: INTENT(IN), OPTIONAL
< !              
< !   Use_Antenna_Correction:   Set this logical argument to apply an antenna correction
< !                             to the computed brightness temperatures for certain
< !                             microwave instruments (AMSU-A/B, MHS)
< !                             If == .TRUE. , antenna correction is applied
< !                                == .FALSE., no correction is applied [DEFAULT]
< !                             UNITS:      N/A
< !                             TYPE:       LOGICAL
< !                             DIMENSION:  Conformable with Options object
< !                             ATTRIBUTES: INTENT(IN), OPTIONAL
< !               
< !   Apply_NLTE_Correction:    Set this logical argument to apply an non-LTE correction
< !                             to shortwave infrared radiances.
< !                             If == .TRUE. , non-LTE correction is applied [DEFAULT]
< !                                == .FALSE., no correction is applied
< !                             UNITS:      N/A
< !                             TYPE:       LOGICAL
< !                             DIMENSION:  Conformable with Options object
< !                             ATTRIBUTES: INTENT(IN), OPTIONAL
< !                 
< !   Set_ADA_RT:
< !   Set_SOI_RT:               Set this logical argument to use the specified algorithm
< !                             for scattering radiative transfer.
< !                             If == .TRUE. , the corresponding RT algorithm is used.
< !                             Note: - By default, the ADA algorithm is used.
< !                                   - If MORE THAN ONE argument is specified, the
< !                                     the default ADA algorithm is used.
< !                             UNITS:      N/A
< !                             TYPE:       LOGICAL
< !                             DIMENSION:  Conformable with Options object
< !                             ATTRIBUTES: INTENT(IN), OPTIONAL
< !                                
< !   Include_Scattering:       Set this logical argument to control the inclusion of
< !                             cloud and aerosol scattering in the radiative transfer.
< !                             If == .TRUE. , scattering calculations are performed [DEFAULT]
< !                                == .FALSE., only cloud/aerosol absorption is considered.
< !                             UNITS:      N/A
< !                             TYPE:       LOGICAL
< !                             DIMENSION:  Conformable with Options object
< !                             ATTRIBUTES: INTENT(IN), OPTIONAL
< !                          
< !   Set_Maximum_Overlap:      
< !   Set_Random_Overlap:       
< !   Set_MaxRan_Overlap:       
< !   Set_Average_Overlap:      Use these logical arguments to set the cloud overlap 
< !                             methodology for fractionally cloudy input profiles.
< !                             If == .TRUE. , the corresponding overlap method is used.
< !                             Note: - By default, the average overlap method is used.
< !                                   - If MORE THAN ONE overlap argument is specified,
< !                                     the default overlap method is used.
< !                             UNITS:      N/A
< !                             TYPE:       LOGICAL
< !                             DIMENSION:  Conformable with Options object
< !                             ATTRIBUTES: INTENT(IN), OPTIONAL
< !
< !   Use_Emissivity:           Set this logical argument to control the use of the emissivity
< !                             spectrum included in the object.
< !                             If == .TRUE. , use the included emissivity spectrum
< !                                == .FALSE., let the CRTM compute the emissivity spectrum
< !                             Note: - This argument is ignored if the object does not
< !                                     contain any emissivity data
< !                                   - See the CRTM_Options_SetEmissivity() procedure for
< !                                     loading emissivity data into an Options object.
< !                             UNITS:      N/A
< !                             TYPE:       LOGICAL
< !                             DIMENSION:  Conformable with Options object
< !                             ATTRIBUTES: INTENT(IN), OPTIONAL
< !                          
< !   Use_Direct_Reflectivity:  Set this logical argument to control the use of the direct
< !                             reflectivity spectrum included in the object.
< !                             If == .TRUE. , use the included direct reflectivity spectrum
< !                                == .FALSE., let the CRTM compute the direct reflectivity spectrum
< !                             Note: - This argument is ignored if the object does not
< !                                     contain any direct reflectivity data
< !                                   - See the CRTM_Options_SetEmissivity() procedure for
< !                                     loading direct relfectivity data into an Options object.
< !                             UNITS:      N/A
< !                             TYPE:       LOGICAL
< !                             DIMENSION:  Conformable with Options object
< !                             ATTRIBUTES: INTENT(IN), OPTIONAL
< !                          
< !   n_Streams:                Set this integer argument to the number of streams (up + down)
< !                             to use in the radiative transfer solver for scattering
< !                             atmospheres.
< !                             By default, a channel-specific value is selected based
< !                             on the Mie parameter.
< !                             UNITS:      N/A
< !                             TYPE:       INTEGER
< !                             DIMENSION:  Conformable with Options object
< !                             ATTRIBUTES: INTENT(IN), OPTIONAL
< !                                          
< !   Aircraft_Pressure:        Set this real argument to aircraft pressure level to use
< !                             for an aircraft instrument simulation.
< !                             Note: This option has not been rigorously tested.
< !                             UNITS:      hPa
< !                             TYPE:       REAL(fp)
< !                             DIMENSION:  Conformable with Options object
< !                             ATTRIBUTES: INTENT(IN), OPTIONAL
< !                                              
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   ELEMENTAL SUBROUTINE CRTM_Options_SetValue( &
<     self                   , &
<     Check_Input            , &
<     Use_Old_MWSSEM         , &
<     Use_Antenna_Correction , &
<     Apply_NLTE_Correction  , &
<     Set_ADA_RT             , &
<     Set_SOI_RT             , &
<     Include_Scattering     , &
<     Set_Maximum_Overlap    , &
<     Set_Random_Overlap     , &
<     Set_MaxRan_Overlap     , &
<     Set_Average_Overlap    , &
<     Set_Overcast_Overlap   , &
<     Use_Emissivity         , &
<     Use_Direct_Reflectivity, &
<     n_Streams              , &
<     Aircraft_Pressure        )
<     ! Arguments
<     TYPE(CRTM_Options_type), INTENT(IN OUT) :: self
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Check_Input
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Use_Old_MWSSEM
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Use_Antenna_Correction
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Apply_NLTE_Correction
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Set_ADA_RT
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Set_SOI_RT
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Include_Scattering
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Set_Maximum_Overlap
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Set_Random_Overlap
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Set_MaxRan_Overlap
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Set_Average_Overlap
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Set_Overcast_Overlap
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Use_Emissivity         
<     LOGICAL ,      OPTIONAL, INTENT(IN)     :: Use_Direct_Reflectivity
<     INTEGER ,      OPTIONAL, INTENT(IN)     :: n_Streams
<     REAL(fp),      OPTIONAL, INTENT(IN)     :: Aircraft_Pressure
< 
<     ! Set the "direct copy" components
<     IF ( PRESENT(Check_Input           ) ) self%Check_Input            = Check_Input
<     IF ( PRESENT(Use_Old_MWSSEM        ) ) self%Use_Old_MWSSEM         = Use_Old_MWSSEM
<     IF ( PRESENT(Use_Antenna_Correction) ) self%Use_Antenna_Correction = Use_Antenna_Correction
<     IF ( PRESENT(Apply_NLTE_Correction ) ) self%Apply_NLTE_Correction  = Apply_NLTE_Correction
<     IF ( PRESENT(Include_Scattering    ) ) self%Include_Scattering     = Include_Scattering
<     IF ( PRESENT(Aircraft_Pressure     ) ) self%Aircraft_Pressure      = Aircraft_Pressure
< 
<     ! Set the "minimal processing" components
<     IF ( PRESENT(n_Streams) ) THEN
<       self%Use_n_Streams = .TRUE.
<       self%n_Streams     = n_Streams
<     END IF
< 
<     ! Only one RT algorithm allowed!
<     IF ( COUNT([PRESENT(Set_ADA_RT), PRESENT(Set_SOI_RT)]) > 1 ) THEN
<       self%RT_Algorithm_Id = RT_ADA
<     ELSE
<       IF ( PRESENT(Set_ADA_RT) ) self%RT_Algorithm_Id = RT_ADA
<       IF ( PRESENT(Set_SOI_RT) ) self%RT_Algorithm_Id = RT_SOI
<     END IF
< 
<     ! Only one overlap option allowed!
<     IF ( COUNT([PRESENT(Set_Maximum_Overlap), PRESENT(Set_Random_Overlap ), &
<                 PRESENT(Set_MaxRan_Overlap ), PRESENT(Set_Average_Overlap), &
<                 PRESENT(Set_Overcast_Overlap) ]) > 1 ) THEN
<       self%Overlap_Id = DEFAULT_OVERLAP_ID
<     ELSE
<       IF ( PRESENT(Set_Maximum_Overlap) ) self%Overlap_Id = CloudCover_Maximum_Overlap()
<       IF ( PRESENT(Set_Random_Overlap ) ) self%Overlap_Id = CloudCover_Random_Overlap() 
<       IF ( PRESENT(Set_MaxRan_Overlap ) ) self%Overlap_Id = CloudCover_MaxRan_Overlap() 
<       IF ( PRESENT(Set_Average_Overlap) ) self%Overlap_Id = CloudCover_Average_Overlap()
<       IF ( PRESENT(Set_Overcast_Overlap)) self%Overlap_Id = CloudCover_Overcast_Overlap()
<     END IF
< 
<     ! The emissivity and reflectivity spectra
<     IF ( PRESENT(Use_Emissivity) ) &
<       self%Use_Emissivity = Use_Emissivity .AND. self%Is_Allocated
<       
<     IF ( PRESENT(Use_Direct_Reflectivity) ) &
<       self%Use_Direct_Reflectivity = Use_Direct_Reflectivity .AND. self%Is_Allocated
< 
<   END SUBROUTINE CRTM_Options_SetValue
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !   CRTM_Options_SetEmissivity
< !
< ! PURPOSE:
< !   Subroutine to set the values of the emissivity and direct reflectivity 
< !   spectra in a CRTM_Options object.
< !
< !   This procedure also sets the usage flags for the emissivity and direct
< !   reflectivity after successful assignment. See also the CRTM_Options_SetValue()
< !   procedure.
< !
< ! CALLING SEQUENCE:
< !   CALL CRTM_Options_SetEmissivity( &
< !          Options                                  , &
< !          Emissivity                               , &
< !          Direct_Reflectivity = Direct_Reflectivity  )
< !
< ! OBJECTS:
< !   Options:              Options object for which the emissivity and
< !                         direct reflectivity are to be set.
< !                         values are to be set.
< !                         UNITS:      N/A
< !                         TYPE:       CRTM_Options_type
< !                         DIMENSION:  Scalar
< !                         ATTRIBUTES: INTENT(IN OUT)
< !
< ! INPUTS:
< !   Emissivity:           Emissivity scalar value or spectrum array.
< !                         If SCALAR: - The Options object MUST already be allocated.
< !                                    - The scalar value is applied to every element
< !                                      of the object emissivity array.
< !                            RANK-1: - The object emissivity array is (re)allocated
< !                                      as necessary.
< !                         UNITS:      N/A
< !                         TYPE:       REAL(fp)
< !                         DIMENSION:  Scalar or Rank-1
< !                         ATTRIBUTES: INTENT(IN)
< !                        
< ! OPTIONAL INPUTS:
< !   Direct_Reflectivity:  Direct reflectivity scalar value or spectrum array.
< !                         If SCALAR: - The Options object MUST already be allocated.
< !                                    - The scalar value is applied to every element
< !                                      of the object direct reflectivity array.
< !                            RANK-1: - The array size must be the same as the 
< !                                      input emissivity array. If not, the
< !                                      object direct reflectivity array is
< !                                      (re)allocated and set to zero.
< !                         UNITS:      N/A
< !                         TYPE:       REAL(fp)
< !                         DIMENSION:  Same as Emissivity argument
< !                         ATTRIBUTES: INTENT(IN), OPTIONAL
< !                                              
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   SUBROUTINE SetEmissivity_scalar( &
<     self      , &
<     Emissivity, &
<     Direct_Reflectivity)
<     ! Arguments
<     TYPE(CRTM_Options_type), INTENT(IN OUT) :: self
<     REAL(fp),                INTENT(IN)     :: Emissivity
<     REAL(fp),      OPTIONAL, INTENT(IN)     :: Direct_Reflectivity
<     ! Local parameters
<     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Options_SetEmissivity(Scalar)'
<     ! Local variables
<     CHARACTER(ML) :: msg
<     
<     ! Setup
<     self%Use_Emissivity          = .FALSE.  ! Turn it off
<     self%Use_Direct_Reflectivity = .FALSE.  ! Turn it off
<     IF ( .NOT. CRTM_Options_Associated(self) ) THEN
<       msg = 'Options object not allocated. Disabling emissivity/direct reflectivity'
<       CALL Display_Message( ROUTINE_NAME, msg, FAILURE )
<       RETURN
<     END IF
<     
<     ! Assign the emissivity
<     self%Emissivity     = Emissivity
<     self%Use_Emissivity = .TRUE.
<     
<     ! Assign the direct reflectivity if supplied
<     IF ( PRESENT(Direct_Reflectivity) ) THEN
<       self%Direct_Reflectivity     = Direct_Reflectivity
<       self%Use_Direct_Reflectivity = .TRUE.
<     END IF
< 
<   END SUBROUTINE SetEmissivity_scalar
< 
< 
<   SUBROUTINE SetEmissivity_rank1( &
<     self      , &
<     Emissivity, &
<     Direct_Reflectivity)
<     ! Arguments
<     TYPE(CRTM_Options_type), INTENT(IN OUT) :: self
<     REAL(fp),                INTENT(IN)     :: Emissivity(:)
<     REAL(fp),      OPTIONAL, INTENT(IN)     :: Direct_Reflectivity(:)
<     ! Local parameters
<     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Options_SetEmissivity(Rank-1)'
<     ! Local variables
<     CHARACTER(ML) :: msg
<     INTEGER :: i
<     
<     ! Setup
<     self%Use_Direct_Reflectivity = .FALSE.  ! Turn it off
<     
<     ! Assign the emissivity
<     self%Emissivity     = Emissivity        ! Auto (re)allocation
<     self%Use_Emissivity = .TRUE.
<     self%n_Channels     = SIZE(Emissivity)
<     
<     ! Assign the direct reflectivity if supplied
<     IF ( PRESENT(Direct_Reflectivity) ) THEN
<       IF ( SIZE(Direct_Reflectivity) == self%n_Channels ) THEN
<         self%Direct_Reflectivity     = Direct_Reflectivity            ! Auto (re)allocation
<         self%Use_Direct_Reflectivity = .TRUE.
<       ELSE
<         msg = 'Size of Direct_Reflectivity argument different from Emissivity. Disabling'
<         CALL Display_Message( ROUTINE_NAME, msg, WARNING )
<         self%Direct_Reflectivity     = [(ZERO,i=1,self%n_Channels)]   ! Auto (re)allocation
<         self%Use_Direct_Reflectivity = .FALSE.
<       END IF
<     END IF
< 
<     ! Set the allocation flag
<     self%Is_Allocated = ALLOCATED(self%Emissivity) .AND. ALLOCATED(self%Direct_Reflectivity)
<                 
<   END SUBROUTINE SetEmissivity_rank1
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
740,748d344
<     ! Check n_Streams
<     IF ( self%Use_n_Streams ) THEN
<       IF ( self%n_Streams < 1 .OR. self%n_Streams > MAX_N_STREAMS ) THEN
<         msg = 'Invalid n_Streams'
<         CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
<         IsValid = .FALSE.
<       END IF
<     END IF
<         
751c347,352
<       IF ( CRTM_Options_Associated(self) ) THEN
---
>       IsValid = CRTM_Options_Associated(self)
>       IF ( .NOT. IsValid ) THEN
>         msg = 'Options structure not allocated'
>         CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
>         RETURN
>       ENDIF
755c356
<             CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
---
>           CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
762c363
<             CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
---
>           CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
766,770d366
<       ELSE
<         msg = 'Options structure not allocated for emissivity usage'
<         CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
<         IsValid = .FALSE.
<       ENDIF
779,781d374
<     ! Check cloud overlap option validity
<     IsValid = CloudCover_Overlap_IsValid( self%Overlap_Id ) .AND. IsValid
< 
818c411
<     WRITE(*,'(3x,"Use n_Streams flag          :",1x,l1)') self%Use_n_Streams
---
>     WRITE(*,'(3x,"Use n_Streams flag          :",1x,l1)') self%Use_N_Streams
820d412
<     WRITE(*,'(3x,"Cloud cover overlap method  :",1x,a )') TRIM(CloudCover_Overlap_Name(self%Overlap_Id))
830d421
<       WRITE(*,'(5x,"Use direct reflectivity flag :",1x,l1)') self%Use_Direct_Reflectivity
1367,1368c958
<                (x%Include_Scattering       .EQV.   y%Include_Scattering    ) .AND. &
<                (x%Overlap_Id                ==     y%Overlap_Id            )
---
>                (x%Include_Scattering       .EQV.   y%Include_Scattering    )
1503,1508d1092
<     ! ...Cloud cover overlap methodology identifier
<     READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) opt%Overlap_Id
<     IF ( io_stat /= 0 ) THEN
<       msg = 'Error reading Overlap_Id optional value - '//TRIM(io_msg)
<       CALL Read_Record_Cleanup(); RETURN
<     END IF
1677,1682d1260
<     ! ...Cloud cover overlap methodology identifier
<     WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) opt%Overlap_Id
<     IF ( io_stat /= 0 ) THEN
<       msg = 'Error writing Overlap_Id optional value - '//TRIM(io_msg)
<       CALL Write_Record_Cleanup(); RETURN
<     END IF
diff -w ./CRTM_Parameters.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Parameters.f90
diff -w ./CRTM_Planck_Functions.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Planck_Functions.f90
diff -w ./CRTM_Predictor.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Predictor.f90
diff -w ./CRTM_Predictor_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Predictor_Define.f90
diff -w ./CRTM_RTSolution.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_RTSolution.f90
diff -w ./CRTM_RTSolution_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_RTSolution_Define.f90
103d102
<   REAL(fp), PARAMETER :: ONE  = 1.0_fp
125c124
<     CHARACTER(STRLEN) :: RT_Algorithm_Name = ''
---
>     CHARACTER(STRLEN*5) :: RT_Algorithm_Name = ''
133d131
<     REAL(fp) :: SSA_Max                 = ZERO  ! Max Single Scattering Albedo in the profile                      
141,143d138
<     REAL(fp) :: Total_Cloud_Cover       = ZERO  ! Only used for fractional clear/cloudy calculation
<     REAL(fp) :: R_clear                 = ZERO  ! Only used for fractional clear/cloudy calculation
<     REAL(fp) :: Tb_clear                = ZERO  ! Only used for fractional clear/cloudy calculation
147,148c142
<     REAL(fp), ALLOCATABLE :: Single_Scatter_Albedo(:)  ! K  
<     ! Radiative transfer results for a single channel
---
>     ! Radiative transfer results for a single channel/node
279d272
<               RTSolution%Single_Scatter_Albedo(n_Layers), & 
290d282
<     RTSolution%Single_Scatter_Albedo = ZERO 
331d322
<     RTSolution%SSA_Max                 = ZERO 
339,341d329
<     RTSolution%Total_Cloud_Cover       = ZERO
<     RTSolution%R_clear                 = ZERO
<     RTSolution%Tb_clear                = ZERO
350d337
<       RTSolution%Single_Scatter_Albedo = ZERO  
416,418d402
<     WRITE(fid,'(3x,"Total cloud cover             : ",es13.6)') RTSolution%Total_Cloud_Cover
<     WRITE(fid,'(3x,"Radiance (clear)              : ",es13.6)') RTSolution%R_clear
<     WRITE(fid,'(3x,"Brightness Temperature (clear): ",es13.6)') RTSolution%Tb_clear
423,424d406
<     WRITE(fid,'(3x,"Upwelling Overcast Radiance :")')
<     WRITE(fid,'(5(1x,es13.6,:))') RTSolution%Upwelling_Overcast_Radiance
560,562d541
<          .NOT. Compares_Within_Tolerance(x%Total_Cloud_Cover      , y%Total_Cloud_Cover      , n) .OR. &
<          .NOT. Compares_Within_Tolerance(x%R_clear                , y%R_clear                , n) .OR. &
<          .NOT. Compares_Within_Tolerance(x%Tb_clear               , y%Tb_clear               , n) .OR. &
644,645c623
<               STAT = alloc_stat )
<              !STAT = alloc_stat, ERRMSG = alloc_msg )
---
>               STAT = alloc_stat, ERRMSG = alloc_msg )
675,676c653,654
<     rts_stats(:,1)%RT_Algorithm_Name = 'Average'
<     rts_stats(:,2)%RT_Algorithm_Name = 'Standard deviation'
---
>     rts_stats(:,1)%RT_Algorithm_Name = 'Object average'
>     rts_stats(:,2)%RT_Algorithm_Name = 'Object standard deviation'
827c805
< !                 ATTRIBUTES: INTENT(OUT), ALLOCATABLE
---
> !                 ATTRIBUTES: INTENT(OUT)
871a850
>     Old_Version, &  ! Optional input (Allow reading of previous version files)
876c855
<     TYPE(CRTM_RTSolution_type), ALLOCATABLE, INTENT(OUT) :: RTSolution(:,:)  ! L x M
---
>     TYPE(CRTM_RTSolution_type), INTENT(OUT) :: RTSolution(:,:)
879a859
>     LOGICAL,          OPTIONAL, INTENT(IN)  :: Old_Version
888d867
<     CHARACTER(ML) :: alloc_msg
890d868
<     INTEGER :: alloc_stat
893,894c871,872
<     INTEGER :: l, n_input_channels
<     INTEGER :: m, n_input_profiles
---
>     INTEGER :: l, n_file_channels, n_input_channels
>     INTEGER :: m, n_file_profiles, n_input_profiles
915c893
<     READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_input_channels, n_input_profiles
---
>     READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_file_channels, n_file_profiles
920,925c898,915
<     ! ...Allocate the return structure array
<    !ALLOCATE(RTSolution(n_input_channels, n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
<     ALLOCATE(RTSolution(n_input_channels, n_input_profiles), STAT=alloc_stat)
<     IF ( alloc_stat /= 0 ) THEN
<       msg = 'Error allocating RTSolution array - '//TRIM(alloc_msg)
<       CALL Read_Cleanup(); RETURN
---
>     ! ...Check if n_Channels in file is > size of output array
>     n_input_channels = SIZE(RTSolution,DIM=1)
>     IF ( n_file_channels > n_input_channels ) THEN
>       WRITE( msg,'("Number of channels, ",i0," > size of the output RTSolution", &
>                   &" array dimension, ",i0,". Only the first ",i0, &
>                   &" channels will be read.")' ) &
>                   n_file_channels, n_input_channels, n_input_channels
>       CALL Display_Message( ROUTINE_NAME, msg, WARNING )
>     END IF
>     n_input_channels = MIN(n_input_channels, n_file_channels)
>     ! ...Check if n_Profiles in file is > size of output array
>     n_input_profiles = SIZE(RTSolution,DIM=2)
>     IF ( n_file_profiles > n_input_profiles ) THEN
>       WRITE( msg,'( "Number of profiles, ",i0," > size of the output RTSolution", &
>                    &" array dimension, ",i0,". Only the first ",i0, &
>                    &" profiles will be read.")' ) &
>                    n_file_profiles, n_input_profiles, n_input_profiles
>       CALL Display_Message( ROUTINE_NAME, msg, WARNING )
926a917
>     n_input_profiles = MIN(n_input_profiles, n_file_profiles)
932c923
<         err_stat = Read_Record( fid, RTSolution(l,m) )
---
>         err_stat = Read_Record( fid, RTSolution(l,m), Old_Version=Old_Version )
970,976c961
<       IF ( ALLOCATED(RTSolution) ) THEN
<        !DEALLOCATE(RTSolution, STAT=alloc_stat, ERRMSG=alloc_msg)
<         DEALLOCATE(RTSolution, STAT=alloc_stat)
<         IF ( alloc_stat /= 0 ) &
<           msg = TRIM(msg)//'; Error deallocating RTSolution array during error cleanup - '//&
<                 TRIM(alloc_msg)
<       END IF
---
>       CALL CRTM_RTSolution_Destroy( RTSolution )
1201,1203d1185
<          (x%Total_Cloud_Cover       .EqualTo. y%Total_Cloud_Cover      ) .AND. &
<          (x%R_clear                 .EqualTo. y%R_clear                ) .AND. &
<          (x%Tb_clear                .EqualTo. y%Tb_clear               ) .AND. &
1270c1252
<     rtssum%RT_Algorithm_Name = 'Addition'
---
>     rtssum%RT_Algorithm_Name = 'Object add'
1279,1281d1260
<     rtssum%Total_Cloud_Cover       = rtssum%Total_Cloud_Cover       + rts2%Total_Cloud_Cover
<     rtssum%R_clear                 = rtssum%R_clear                 + rts2%R_clear
<     rtssum%Tb_clear                = rtssum%Tb_clear                + rts2%Tb_clear
1351c1330
<     rtsdiff%RT_Algorithm_Name = 'Subtraction'
---
>     rtsdiff%RT_Algorithm_Name = 'Object subtract'
1360,1362d1338
<     rtsdiff%Total_Cloud_Cover       = rtsdiff%Total_Cloud_Cover       - rts2%Total_Cloud_Cover
<     rtsdiff%R_clear                 = rtsdiff%R_clear                 - rts2%R_clear
<     rtsdiff%Tb_clear                = rtsdiff%Tb_clear                - rts2%Tb_clear
1429c1405
<     rts_power%RT_Algorithm_Name = 'Exponent'
---
>     rts_power%RT_Algorithm_Name = 'Object exponent'
1438,1440d1413
<     rts_power%Total_Cloud_Cover       = (rts_power%Total_Cloud_Cover      )**power
<     rts_power%R_clear                 = (rts_power%R_clear                )**power
<     rts_power%Tb_clear                = (rts_power%Tb_clear               )**power
1503c1476
<     rts_normal%RT_Algorithm_Name = 'Normalise'
---
>     rts_normal%RT_Algorithm_Name = 'Object normalise'
1512,1514d1484
<     rts_normal%Total_Cloud_Cover       = rts_normal%Total_Cloud_Cover      /factor
<     rts_normal%R_clear                 = rts_normal%R_clear                /factor
<     rts_normal%Tb_clear                = rts_normal%Tb_clear               /factor
1569c1539
<     rts_sqrt%RT_Algorithm_Name = 'Square root'
---
>     rts_sqrt%RT_Algorithm_Name = 'Object SQRT()'
1578,1580d1547
<     rts_sqrt%Total_Cloud_Cover       = SQRT(rts_sqrt%Total_Cloud_Cover      )
<     rts_sqrt%R_clear                 = SQRT(rts_sqrt%R_clear                )
<     rts_sqrt%Tb_clear                = SQRT(rts_sqrt%Tb_clear               )
1604c1571,1572
<     rts) &  ! Output
---
>     rts, &  ! Output
>     old_version) &  ! Optional input
1608a1577
>     LOGICAL,          OPTIONAL, INTENT(IN)  :: old_version
1617a1587
>     LOGICAL :: current_version
1621c1591,1593
< 
---
>     ! ...PRocess optional arguments
>     current_version = .TRUE.
>     IF ( PRESENT(old_version) ) current_version = .NOT. old_version
1670,1673c1642
<       rts%Surface_Planck_Radiance, &
<       rts%Total_Cloud_Cover      , &
<       rts%R_clear                , &
<       rts%Tb_clear
---
>       rts%Surface_Planck_Radiance
1678a1648
>       IF ( current_version ) THEN
1682a1653,1657
>       ELSE
>         READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
>           rts%Upwelling_Radiance , &
>           rts%Layer_Optical_Depth
>       END IF
1761c1736
<     ! Write the RT algorithm name
---
>     ! Write the sensor info
1778,1781c1753
<       rts%Surface_Planck_Radiance, &
<       rts%Total_Cloud_Cover      , &
<       rts%R_clear                , &
<       rts%Tb_clear
---
>       rts%Surface_Planck_Radiance
diff -w ./CRTM_SEcategory.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_SEcategory.f90
diff -w ./CRTM_SensorData_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_SensorData_Define.f90
diff -w ./CRTM_SensorInfo.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_SensorInfo.f90
diff -w ./CRTM_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_SfcOptics.f90
2086,2091d2085
<         Reflectivity_AD(1:nZ,1,1:nZ,1:nL) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1:nL)
<         SfcOptics_AD%Reflectivity = ZERO        
<         Emissivity_AD(1:nZ,1:nL) = SfcOptics_AD%Emissivity(1:nZ,1:nL)
<         SfcOptics_AD%Emissivity = ZERO
<         Direct_Reflectivity_AD(1:nZ,1) = SfcOptics_AD%Direct_Reflectivity(1:nZ,1)
<         SfcOptics_AD%Direct_Reflectivity(1:nZ,1) = ZERO
2107,2109c2101
<           SfcOptics_AD%Direct_Reflectivity(1:nZ,1:nL) = &
<             SfcOptics_AD%Direct_Reflectivity(1:nZ,1:nL) + &
<             (Direct_Reflectivity_AD(1:nZ,1:nL)*Surface%Ice_Coverage) 
---
> 
2136,2138c2128
<           SfcOptics_AD%Direct_Reflectivity(1:nZ,1:nL) = &
<             SfcOptics_AD%Direct_Reflectivity(1:nZ,1:nL) + &
<             (Direct_Reflectivity_AD(1:nZ,1:nL)*Surface%Snow_Coverage) 
---
> 
2165,2167c2155
<           SfcOptics_AD%Direct_Reflectivity(1:nZ,1:nL) = &
<             SfcOptics_AD%Direct_Reflectivity(1:nZ,1:nL) + &
<             (Direct_Reflectivity_AD(1:nZ,1:nL)*Surface%Water_Coverage) 
---
> 
2202,2204c2190
<           SfcOptics_AD%Direct_Reflectivity(1:nZ,1:nL) = &
<             SfcOptics_AD%Direct_Reflectivity(1:nZ,1:nL) + &
<             (Direct_Reflectivity_AD(1:nZ,1:nL)*Surface%Land_Coverage) 
---
> 
diff -w ./CRTM_SfcOptics_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_SfcOptics_Define.f90
40,41d39
<   PUBLIC :: OPERATOR(+)
<   PUBLIC :: OPERATOR(-)
46d43
<   PUBLIC :: CRTM_SfcOptics_Zero
59,66d55
<   INTERFACE OPERATOR(+)
<     MODULE PROCEDURE CRTM_SfcOptics_Add
<   END INTERFACE OPERATOR(+)  
<   
<   INTERFACE OPERATOR(-)
<     MODULE PROCEDURE CRTM_SfcOptics_Subtract
<   END INTERFACE OPERATOR(-)  
< 
197,198c186,187
<   ELEMENTAL FUNCTION CRTM_SfcOptics_Associated( self ) RESULT( Status )
<     TYPE(CRTM_SfcOptics_type), INTENT(IN) :: self
---
>   ELEMENTAL FUNCTION CRTM_SfcOptics_Associated( SfcOptics ) RESULT( Status )
>     TYPE(CRTM_SfcOptics_type), INTENT(IN) :: SfcOptics
200c189
<     Status = self%Is_Allocated
---
>     Status = SfcOptics%Is_Allocated
226,228c215,217
<   ELEMENTAL SUBROUTINE CRTM_SfcOptics_Destroy( self )
<     TYPE(CRTM_SfcOptics_type), INTENT(OUT) :: self
<     self%Is_Allocated = .FALSE.
---
>   ELEMENTAL SUBROUTINE CRTM_SfcOptics_Destroy( SfcOptics )
>     TYPE(CRTM_SfcOptics_type), INTENT(OUT) :: SfcOptics
>     SfcOptics%Is_Allocated = .FALSE.
242c231
< !       CALL CRTM_SfcOptics_Create( SfcOptics, n_Angles, n_Stokes )
---
> !       CALL CRTM_SfcOptics_Create( SfcOptics, n_Layers )
252,260c241
< !       n_Angles:     Number of angles for which there is SfcOptics data.
< !                     Must be > 0.
< !                     UNITS:      N/A
< !                     TYPE:       INTEGER
< !                     DIMENSION:  Conformable with SfcOptics object
< !                     ATTRIBUTES: INTENT(IN)
< !
< !       n_Stokes:     Number of Stokes components for which there is SfcOptics
< !                     data.
---
> !       n_Layers:     Number of layers for which there is SfcOptics data.
264c245
< !                     DIMENSION:  Conformable with SfcOptics object
---
> !                     DIMENSION:  Same as SfcOptics object
271c252
<     self, &
---
>     SfcOptics, &
275c256
<     TYPE(CRTM_SfcOptics_type), INTENT(OUT) :: self
---
>     TYPE(CRTM_SfcOptics_type), INTENT(OUT) :: SfcOptics
285,289c266,270
<     ALLOCATE( self%Angle( n_Angles ), &
<               self%Weight( n_Angles ), &
<               self%Emissivity( n_Angles, n_Stokes ), &
<               self%Reflectivity( n_Angles, n_Stokes, n_Angles, n_Stokes), &
<               self%Direct_Reflectivity( n_Angles, n_Stokes ), &
---
>     ALLOCATE( SfcOptics%Angle( n_Angles ), &
>               SfcOptics%Weight( n_Angles ), &
>               SfcOptics%Emissivity( n_Angles, n_Stokes ), &
>               SfcOptics%Reflectivity( n_Angles, n_Stokes, n_Angles, n_Stokes), &
>               SfcOptics%Direct_Reflectivity( n_Angles, n_Stokes ), &
295,296c276,277
<     self%n_Angles = n_Angles
<     self%n_Stokes = n_Stokes
---
>     SfcOptics%n_Angles = n_Angles
>     SfcOptics%n_Stokes = n_Stokes
298,302c279,283
<     self%Angle               = ZERO
<     self%Weight              = ZERO
<     self%Emissivity          = ZERO
<     self%Reflectivity        = ZERO
<     self%Direct_Reflectivity = ZERO
---
>     SfcOptics%Angle               = ZERO
>     SfcOptics%Weight              = ZERO
>     SfcOptics%Emissivity          = ZERO
>     SfcOptics%Reflectivity        = ZERO
>     SfcOptics%Direct_Reflectivity = ZERO
305c286
<     self%Is_Allocated = .TRUE.
---
>     SfcOptics%Is_Allocated = .TRUE.
314,349d294
< !       CRTM_SfcOptics_Zero
< !
< ! PURPOSE:
< !       Elemental subroutine to initialise the components of an SfcOptics
< !       object to a value of zero.
< !
< ! CALLING SEQUENCE:
< !       CALL CRTM_SfcOptics_Zero( SfcOptics )
< !
< ! OBJECTS:
< !       SfcOptics:   SfcOptics object which is to have its components
< !                    set to a zero value.
< !                    UNITS:      N/A
< !                    TYPE:       CRTM_SfcOptics_type
< !                    DIMENSION:  Scalar or any rank
< !                    ATTRIBUTES: INTENT(IN OUT)
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   ELEMENTAL SUBROUTINE CRTM_SfcOptics_Zero( self )
<     TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: self
<     self%Azimuth_Angle       = 999.9_fp
<     self%Transmittance       = ZERO
<     self%Surface_Temperature = ZERO
<     IF ( .NOT. CRTM_SfcOptics_Associated( self ) ) RETURN
<     self%Emissivity          = ZERO
<     self%Reflectivity        = ZERO
<     self%Direct_Reflectivity = ZERO
<   END SUBROUTINE CRTM_SfcOptics_Zero
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
368,369c313,314
<   SUBROUTINE CRTM_SfcOptics_Inspect( self )
<     TYPE(CRTM_SfcOptics_type), INTENT(IN) :: self
---
>   SUBROUTINE CRTM_SfcOptics_Inspect( SfcOptics )
>     TYPE(CRTM_SfcOptics_type), INTENT(IN) :: SfcOptics
373,374c318,319
<     WRITE(*, '(3x,"n_Angles   :",1x,i0)') self%n_Angles
<     WRITE(*, '(3x,"n_Stokes   :",1x,i0)') self%n_Stokes
---
>     WRITE(*, '(3x,"n_Angles   :",1x,i0)') SfcOptics%n_Angles
>     WRITE(*, '(3x,"n_Stokes   :",1x,i0)') SfcOptics%n_Stokes
376,383c321,328
<     WRITE(*, '(3x,"Compute flag              :",1x,l1)') self%Compute
<     WRITE(*, '(3x,"Use_New_MWSSEM flag       :",1x,l1)') self%Use_New_MWSSEM
<     WRITE(*, '(3x,"  MWSSEM- azimuth angle   :",1x,es13.6)') self%Azimuth_Angle
<     WRITE(*, '(3x,"  MWSSEM- transmittance   :",1x,es13.6)') self%Transmittance
<     WRITE(*, '(3x,"Satellite view angle index:",1x,i0)') self%Index_Sat_Ang
<     WRITE(*, '(3x,"Azimuth Fourier component :",1x,i0)') self%mth_Azi
<     WRITE(*, '(3x,"Weighted mean Tsfc        :",1x,es13.6)') self%Surface_Temperature
<     IF ( .NOT. CRTM_SfcOptics_Associated(self) ) RETURN
---
>     WRITE(*, '(3x,"Compute flag              :",1x,l1)') SfcOptics%Compute
>     WRITE(*, '(3x,"Use_New_MWSSEM flag       :",1x,l1)') SfcOptics%Use_New_MWSSEM
>     WRITE(*, '(3x,"  MWSSEM- azimuth angle   :",1x,es13.6)') SfcOptics%Azimuth_Angle
>     WRITE(*, '(3x,"  MWSSEM- transmittance   :",1x,es13.6)') SfcOptics%Transmittance
>     WRITE(*, '(3x,"Satellite view angle index:",1x,i0)') SfcOptics%Index_Sat_Ang
>     WRITE(*, '(3x,"Azimuth Fourier component :",1x,i0)') SfcOptics%mth_Azi
>     WRITE(*, '(3x,"Weighted mean Tsfc        :",1x,es13.6)') SfcOptics%Surface_Temperature
>     IF ( .NOT. CRTM_SfcOptics_Associated(SfcOptics) ) RETURN
385c330
<     WRITE(*, '(5(1x,es13.6,:))') self%Angle
---
>     WRITE(*, '(5(1x,es13.6,:))') SfcOptics%Angle
387c332
<     WRITE(*, '(5(1x,es13.6,:))') self%Weight
---
>     WRITE(*, '(5(1x,es13.6,:))') SfcOptics%Weight
389c334
<     WRITE(*, '(5(1x,es13.6,:))') self%Emissivity
---
>     WRITE(*, '(5(1x,es13.6,:))') SfcOptics%Emissivity
391c336
<     WRITE(*, '(5(1x,es13.6,:))') self%Reflectivity
---
>     WRITE(*, '(5(1x,es13.6,:))') SfcOptics%Reflectivity
393c338
<     WRITE(*, '(5(1x,es13.6,:))') self%Direct_Reflectivity
---
>     WRITE(*, '(5(1x,es13.6,:))') SfcOptics%Direct_Reflectivity
561c506,507
<     IF ( CRTM_SfcOptics_Associated(x) .NEQV. CRTM_SfcOptics_Associated(y) ) RETURN
---
>     IF ( (.NOT. CRTM_SfcOptics_Associated(x)) .OR. &
>          (.NOT. CRTM_SfcOptics_Associated(y))      ) RETURN
567,568c513,514
<     ! ...Scalars
<     IF ( .NOT. ((x%Compute               .EQV.   y%Compute            ) .AND. &
---
>     ! ...Everything else
>     IF ( (x%Compute                  .EQV.   y%Compute            ) .AND. &
574,577c520,521
<                 (x%Surface_Temperature .EqualTo. y%Surface_Temperature)) ) RETURN
<     ! ...Arrays
<     IF ( CRTM_SfcOptics_Associated(x) .AND. CRTM_SfcOptics_Associated(y) ) THEN
<       IF ( .NOT. (ALL(x%Angle               .EqualTo. y%Angle              ) .AND. &
---
>          (x%Surface_Temperature    .EqualTo. y%Surface_Temperature) .AND. &
>          ALL(x%Angle               .EqualTo. y%Angle              ) .AND. &
581,585c525
<                   ALL(x%Direct_Reflectivity .EqualTo. y%Direct_Reflectivity)) ) RETURN
<     END IF
< 
< 
<     ! If we get here, then...
---
>          ALL(x%Direct_Reflectivity .EqualTo. y%Direct_Reflectivity)       ) &
590,708d529
<   
< !------------------------------------------------------------------------------
< !
< ! NAME:
< !       CRTM_SfcOptics_Add
< !
< ! PURPOSE:
< !       Pure function to add two CRTM_SfcOptics objects.
< !       Used in OPERATOR(+) interface block.
< !
< ! CALLING SEQUENCE:
< !       sosum = CRTM_SfcOptics_Add( so1, so2 )
< !
< !         or
< !
< !       sosum = so1 + so2
< !
< ! INPUTS:
< !       so1, so2:      Two CRTM SfcOptics objects to be added.
< !                      UNITS:      N/A
< !                      TYPE:       CRTM_SfcOptics_type
< !                      DIMENSION:  Scalar
< !                      ATTRIBUTES: INTENT(IN OUT)
< !
< ! RESULT:
< !       sosum:         SfcOptics object containing the added components.
< !                      UNITS:      N/A
< !                      TYPE:       CRTM_SfcOptics_type
< !                      DIMENSION:  Scalar
< !
< !------------------------------------------------------------------------------
< 
<   ELEMENTAL FUNCTION CRTM_SfcOptics_Add( so1, so2 ) RESULT( sosum )
<     TYPE(CRTM_SfcOptics_type), INTENT(IN) :: so1, so2
<     TYPE(CRTM_SfcOptics_type) :: sosum
< 
<     ! Check the structure association status
<     IF ( (.NOT. CRTM_SfcOptics_Associated(so1)) .OR. &
<          (.NOT. CRTM_SfcOptics_Associated(so2))      ) RETURN
< 
<     ! Check contents
<     ! ...Dimensions
<     IF ( (so1%n_Angles /= so2%n_Angles) .OR. &
<          (so1%n_Stokes /= so2%n_Stokes) ) RETURN
< 
<     ! Copy the first structure
<     sosum = so1
<     
<     ! And add its components to the second one
<     ! ...The scalar values
<     sosum%Transmittance       = sosum%Transmittance       + so2%Transmittance
<     sosum%Surface_Temperature = sosum%Surface_Temperature + so2%Surface_Temperature
<     ! ...The arrays
<     sosum%Reflectivity        = sosum%Reflectivity        + so2%Reflectivity
<     sosum%Direct_Reflectivity = sosum%Direct_Reflectivity + so2%Direct_Reflectivity
<     sosum%Emissivity          = sosum%Emissivity          + so2%Emissivity    
< 
<   END FUNCTION CRTM_SfcOptics_Add
< 
< 
<   
< !------------------------------------------------------------------------------
< !
< ! NAME:
< !       CRTM_SfcOptics_Subtract
< !
< ! PURPOSE:
< !       Pure function to subtract two CRTM_SfcOptics objects.
< !       Used in OPERATOR(-) interface block.
< !
< ! CALLING SEQUENCE:
< !       sodiff = CRTM_SfcOptics_Subtract( so1, so2 )
< !
< !         or
< !
< !       sodiff = so1 - so2
< !
< ! INPUTS:
< !       so1, so2:      Two CRTM SfcOptics objects to be subtracted.
< !                      UNITS:      N/A
< !                      TYPE:       CRTM_SfcOptics_type
< !                      DIMENSION:  Scalar
< !                      ATTRIBUTES: INTENT(IN OUT)
< !
< ! RESULT:
< !       sodiff:        SfcOptics object containing the differenced components.
< !                      UNITS:      N/A
< !                      TYPE:       CRTM_SfcOptics_type
< !                      DIMENSION:  Scalar
< !
< !------------------------------------------------------------------------------
< 
<   ELEMENTAL FUNCTION CRTM_SfcOptics_Subtract( so1, so2 ) RESULT( sodiff )
<     TYPE(CRTM_SfcOptics_type), INTENT(IN) :: so1, so2
<     TYPE(CRTM_SfcOptics_type) :: sodiff
< 
<     ! Check the structure association status
<     IF ( (.NOT. CRTM_SfcOptics_Associated(so1)) .OR. &
<          (.NOT. CRTM_SfcOptics_Associated(so2))      ) RETURN
< 
<     ! Check contents
<     ! ...Dimensions
<     IF ( (so1%n_Angles /= so2%n_Angles) .OR. &
<          (so1%n_Stokes /= so2%n_Stokes) ) RETURN
< 
<     ! Copy the first structure
<     sodiff = so1
<     
<     ! And subtract the second one from it
<     ! ...The scalar values
<     sodiff%Transmittance       = sodiff%Transmittance       - so2%Transmittance
<     sodiff%Surface_Temperature = sodiff%Surface_Temperature - so2%Surface_Temperature
<     ! ...The arrays
<     sodiff%Reflectivity        = sodiff%Reflectivity        - so2%Reflectivity
<     sodiff%Direct_Reflectivity = sodiff%Direct_Reflectivity - so2%Direct_Reflectivity
<     sodiff%Emissivity          = sodiff%Emissivity          - so2%Emissivity    
< 
<   END FUNCTION CRTM_SfcOptics_Subtract
< 
diff -w ./CRTM_SpcCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_SpcCoeff.f90
diff -w ./CRTM_Surface_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Surface_Define.f90
86d85
<   PUBLIC :: CRTM_Surface_NonVariableCopy
385,434d383
< !       CRTM_Surface_NonVariableCopy
< !
< ! PURPOSE:
< !       Elemental utility subroutine to copy the "non-variable" data (coverages
< !       and surface types) from one instance of a CRTM Surface object to another
< !       (usually a TL or AD one).
< !
< !       NOTE: No error checking is performed in this procedure.
< !
< ! CALLING SEQUENCE:
< !       CALL CRTM_Surface_NonVariableCopy( sfc, modified_sfc )
< !
< ! OBJECTS:
< !       sfc:             Surface object from which to copy.
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Surface_type
< !                        DIMENSION:  Scalar or any rank
< !                        ATTRIBUTES: INTENT(IN)
< !
< ! IN/OUTPUTS:
< !       modified_sfc:    Existing Surface object to be modified.
< !                        UNITS:      N/A
< !                        TYPE:       CRTM_Surface_type
< !                        DIMENSION:  Conformable with sfc input
< !                        ATTRIBUTES: INTENT(IN OUT)
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   ELEMENTAL SUBROUTINE CRTM_Surface_NonVariableCopy( sfc, modified_sfc )
<     TYPE(CRTM_Surface_type), INTENT(IN)     :: sfc
<     TYPE(CRTM_Surface_type), INTENT(IN OUT) :: modified_sfc
< 
<     modified_sfc%Land_Coverage  = sfc%Land_Coverage
<     modified_sfc%Water_Coverage = sfc%Water_Coverage
<     modified_sfc%Snow_Coverage  = sfc%Snow_Coverage
<     modified_sfc%Ice_Coverage   = sfc%Ice_Coverage
<     
<     modified_sfc%Land_Type  = sfc%Land_Type
<     modified_sfc%Water_Type = sfc%Water_Type
<     modified_sfc%Snow_Type  = sfc%Snow_Type
<     modified_sfc%Ice_Type   = sfc%Ice_Type
<     
<   END SUBROUTINE CRTM_Surface_NonVariableCopy
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
1118,1119c1067
<    !ALLOCATE(Surface(n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
<     ALLOCATE(Surface(n_input_profiles), STAT=alloc_stat)
---
>     ALLOCATE(Surface(n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
1166,1167c1114
<        !DEALLOCATE(Surface, STAT=alloc_stat, ERRMSG=alloc_msg)
<         DEALLOCATE(Surface, STAT=alloc_stat)
---
>         DEALLOCATE(Surface, STAT=alloc_stat, ERRMSG=alloc_msg)
1234,1235c1181
<    !ALLOCATE(Surface(n_input_channels, n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
<     ALLOCATE(Surface(n_input_channels, n_input_profiles), STAT=alloc_stat)
---
>     ALLOCATE(Surface(n_input_channels, n_input_profiles), STAT=alloc_stat, ERRMSG=alloc_msg)
1286,1287c1232
<        !DEALLOCATE(Surface, STAT=alloc_stat, ERRMSG=alloc_msg)
<         DEALLOCATE(Surface, STAT=alloc_stat)
---
>         DEALLOCATE(Surface, STAT=alloc_stat, ERRMSG=alloc_msg)
diff -w ./CRTM_Tangent_Linear_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Tangent_Linear_Module.f90
43,45d42
<   USE CRTM_RTSolution_Define,     ONLY: CRTM_RTSolution_type   , &
<                                         CRTM_RTSolution_Destroy, &
<                                         CRTM_RTSolution_Zero
49,53c46
<                                         CRTM_Atmosphere_AddLayers_TL   , &
<                                         CRTM_Atmosphere_IsFractional   , &
<                                         CRTM_Atmosphere_Coverage       , &
<                                         CRTM_Atmosphere_ClearSkyCopy   , &
<                                         CRTM_Atmosphere_ClearSkyCopy_TL
---
>                                         CRTM_Atmosphere_AddLayers_TL
77c70,72
<   USE CRTM_AtmOptics,             ONLY: CRTM_Include_Scattering, &
---
>   USE CRTM_AtmOptics,             ONLY: AOvar_type  , &
>                                         AOvar_Create, &
>                                         CRTM_Include_Scattering, &
80,83c75,76
<                                         CRTM_AtmOptics_Combine         , &
<                                         CRTM_AtmOptics_Combine_TL      , &
<                                         CRTM_AtmOptics_NoScatterCopy   , &
<                                         CRTM_AtmOptics_NoScatterCopy_TL
---
>                                         CRTM_Combine_AtmOptics       , &
>                                         CRTM_Combine_AtmOptics_TL
90c83,84
<   USE CRTM_RTSolution,            ONLY: CRTM_Compute_nStreams     , &
---
>   USE CRTM_RTSolution,            ONLY: CRTM_RTSolution_type      , &
>                                         CRTM_Compute_nStreams     , &
92a87,90
>   USE RTV_Define,                 ONLY: RTV_type      , &
>                                         RTV_Associated, &
>                                         RTV_Destroy   , &
>                                         RTV_Create
97a96
> 
99a99
> 
105a106
> 
107a109
> 
110d111
<   USE CRTM_CloudCover_Define,     ONLY: CRTM_CloudCover_type
113,117d113
<   ! ...AtmOptics
<   USE AOvar_Define, ONLY: AOvar_type, &
<                           AOvar_Associated, &
<                           AOvar_Destroy   , &
<                           AOvar_Create
128,132d123
<   ! ...Radiative transfer
<   USE RTV_Define,   ONLY: RTV_type, &
<                           RTV_Associated, &
<                           RTV_Destroy   , &
<                           RTV_Create
292c283,286
<     LOGICAL :: compute_antenna_correction
---
>     LOGICAL :: Check_Input
>     LOGICAL :: User_Emissivity, User_Direct_Reflectivity, User_N_Streams
>     LOGICAL :: User_AntCorr, Compute_AntCorr
>     LOGICAL :: Apply_NLTE_Correction
293a288
>     INTEGER :: RT_Algorithm_Id
301d295
<     INTEGER :: cloud_coverage_flag
305,306d298
<     REAL(fp) :: transmittance_clear, transmittance_clear_TL
<     REAL(fp) :: r_cloudy
309,310c301,302
<     ! Local options structure for default and use values
<     TYPE(CRTM_Options_type) :: Default_Options, Opt
---
>     ! Local options structure for default values
>     TYPE(CRTM_Options_type) :: Default_Options
313,318d304
<     ! Clear sky structures
<     TYPE(CRTM_Atmosphere_type) :: Atm_Clear       , Atm_Clear_TL
<     TYPE(CRTM_AtmOptics_type)  :: AtmOptics_Clear , AtmOptics_Clear_TL
<     TYPE(CRTM_SfcOptics_type)  :: SfcOptics_Clear , SfcOptics_Clear_TL
<     TYPE(CRTM_RTSolution_type) :: RTSolution_Clear, RTSolution_Clear_TL
<     TYPE(RTV_type)             :: RTV_Clear
333,334d318
<     ! Cloud cover object
<     TYPE(CRTM_CloudCover_type) :: CloudCover, CloudCover_TL
389,392d372
<     ! Reinitialise the output RTSolution
<     CALL CRTM_RTSolution_Zero(RTSolution)
< 
< 
429c409,416
<       Opt = Default_Options
---
>       ! ...Specify default actions
>       Check_Input           = Default_Options%Check_Input
>       User_Emissivity       = Default_Options%Use_Emissivity
>       User_AntCorr          = Default_Options%Use_Antenna_Correction
>       Apply_NLTE_Correction = Default_Options%Apply_NLTE_Correction
>       RT_Algorithm_Id       = Default_Options%RT_Algorithm_Id
>       User_N_Streams        = Default_Options%Use_N_Streams
>       ! ...Check the Options argument
431c418,439
<         Opt = Options(m)
---
>         ! Override input checker with option
>         Check_Input = Options(m)%Check_Input
>         ! Check if the supplied emissivity should be used
>         User_Emissivity = Options(m)%Use_Emissivity
>         IF ( Options(m)%Use_Emissivity ) THEN
>           ! Are the channel dimensions consistent
>           IF ( Options(m)%n_Channels < n_Channels ) THEN
>             Error_Status = FAILURE
>             WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
>                    &"than the number of requested channels (",i0, ")" )' ) &
>                    Options(m)%n_Channels, n_Channels
>             CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
>             RETURN
>           END IF
>           ! Check if the supplied direct reflectivity should be used
>           User_Direct_Reflectivity = Options(m)%Use_Direct_Reflectivity
>         END IF
>         ! Check if antenna correction should be attempted
>         User_AntCorr = Options(m)%Use_Antenna_Correction
>         ! Set NLTE correction option
>         Apply_NLTE_Correction = Options(m)%Apply_NLTE_Correction
> 
434a443,459
>         ! Copy over surface optics input
>         SfcOptics%Use_New_MWSSEM = .NOT. Options(m)%Use_Old_MWSSEM
>         ! Specify the RT algorithm
>         RT_Algorithm_Id = Options(m)%RT_Algorithm_Id
>         ! Check if n_Streams should be used
>         User_N_Streams = Options(m)%Use_N_Streams
>         ! Check value for nstreams
>         IF ( User_N_Streams ) THEN
>           IF ( Options(m)%n_Streams <= 0 .OR. MOD(Options(m)%n_Streams,2) /= 0 .OR. &
>                Options(m)%n_Streams > MAX_N_STREAMS ) THEN
>               Error_Status = FAILURE
>               WRITE( Message,'( "Input Options n_Streams (", i0, ") is invalid" )' ) &
>                      Options(m)%n_Streams
>               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
>               RETURN
>           END IF
>         END IF
436,438d460
<       ! ...Assign the option specific SfcOptics input
<       SfcOptics%Use_New_MWSSEM    = .NOT. Opt%Use_Old_MWSSEM
<       SfcOptics_TL%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
442c464
<       IF ( Opt%Check_Input ) THEN
---
>       IF ( Check_Input ) THEN
462,483d483
<           ! Are the channel dimensions consistent if emissivity is passed?
<           IF ( Options(m)%Use_Emissivity ) THEN
<             IF ( Options(m)%n_Channels < n_Channels ) THEN
<               Error_Status = FAILURE
<               WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
<                      &"than the number of requested channels (",i0, ")" )' ) &
<                      Options(m)%n_Channels, n_Channels
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
<           END IF
<           ! Check value for user-defined n_Streams
<           IF ( Options(m)%Use_N_Streams ) THEN
<             IF ( Options(m)%n_Streams <= 0 .OR. MOD(Options(m)%n_Streams,2) /= 0 .OR. &
<                  Options(m)%n_Streams > MAX_N_STREAMS ) THEN
<                 Error_Status = FAILURE
<                 WRITE( Message,'( "Input Options n_Streams (", i0, ") is invalid" )' ) &
<                        Options(m)%n_Streams
<                 CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                 RETURN
<             END IF
<           END IF
498a499,503
>       ! Average surface skin temperature for multi-surface types
>       CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )
>       CALL CRTM_Compute_SurfaceT_TL( Surface(m), Surface_TL(m), SfcOptics_TL )
> 
> 
524,527c529
< 
< 
<       ! Prepre the atmospheric optics structures
<       ! ...Allocate the AtmOptics structures based on Atm extension
---
>       ! ...Allocate the atmospheric optics structures based on Atm extension
536,537c538,539
<       IF ( (.NOT. CRTM_AtmOptics_Associated( Atmoptics )) .OR. &
<            (.NOT. CRTM_AtmOptics_Associated( Atmoptics_TL )) ) THEN
---
>       IF ( .NOT. CRTM_AtmOptics_Associated( Atmoptics ) .OR. &
>            .NOT. CRTM_AtmOptics_Associated( Atmoptics_TL ) ) THEN
543,545c545,549
<       ! ...Set the scattering switch
<       AtmOptics%Include_Scattering    = Opt%Include_Scattering
<       AtmOptics_TL%Include_Scattering = Opt%Include_Scattering
---
>       IF (Options_Present) THEN
>         ! Set Scattering Switch
>         AtmOptics%Include_Scattering = Options(m)%Include_Scattering
>         AtmOptics_TL%Include_Scattering = Options(m)%Include_Scattering
>       END IF
569,619d572
<       ! Determine the type of cloud coverage
<       cloud_coverage_flag = CRTM_Atmosphere_Coverage( atm )
< 
< 
<       ! Setup for fractional cloud coverage
<       IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<       
<         ! Compute cloudcover
<         Status_FWD = CloudCover%Compute_CloudCover(atm, Overlap = opt%Overlap_Id)
<         Status_TL  = CloudCover_TL%Compute_CloudCover_TL(CloudCover, atm, atm_TL)
<         IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
<           Error_Status = FAILURE
<           WRITE( Message,'("Error computing cloud cover for profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
< 
<         ! Allocate all the CLEAR sky structures for fractional cloud coverage
<         ! ...Clear sky atmosphere
<         Status_FWD = CRTM_Atmosphere_ClearSkyCopy(Atm, Atm_Clear)
<         Status_TL  = CRTM_Atmosphere_ClearSkyCopy_TL(Atm, Atm_TL, Atm_Clear_TL)
<         IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
<           Error_status = FAILURE
<           WRITE( Message,'("Error copying CLEAR SKY Atmopshere structures for profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
<         ! ...Clear sky SfcOptics
<         CALL CRTM_SfcOptics_Create( SfcOptics_Clear   , MAX_N_ANGLES, MAX_N_STOKES )
<         CALL CRTM_SfcOptics_Create( SfcOptics_Clear_TL, MAX_N_ANGLES, MAX_N_STOKES )
<         IF ( (.NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear)) .OR. &
<              (.NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear_TL))) THEN
<           Error_Status = FAILURE
<           WRITE( Message,'("Error allocating CLEAR SKY SfcOptics data structures for profile #",i0)' ) m
<           CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<           RETURN
<         END IF
<         ! ...Copy over surface optics input
<         SfcOptics_Clear%Use_New_MWSSEM    = .NOT. Opt%Use_Old_MWSSEM
<         SfcOptics_Clear_TL%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
<         ! ...CLEAR SKY average surface skin temperature for multi-surface types
<         CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics_Clear )
<         CALL CRTM_Compute_SurfaceT_TL( Surface(m), Surface_TL(m), SfcOptics_Clear_TL )
<       END IF
< 
< 
<       ! Average surface skin temperature for multi-surface types
<       CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )
<       CALL CRTM_Compute_SurfaceT_TL( Surface(m), Surface_TL(m), SfcOptics_TL )
< 
< 
634c587
<         compute_antenna_correction = ( Opt%Use_Antenna_Correction               .AND. &
---
>         IF ( User_AntCorr                             .AND. &
636c589,593
<                                        iFOV /= 0 )
---
>              iFOV /= 0 ) THEN
>           Compute_AntCorr = .TRUE.
>         ELSE
>           Compute_AntCorr = .FALSE.
>         END IF
677,678c634
<               SpcCoeff_IsVisibleSensor(SC(SensorIndex)) ) .AND. &
<             AtmOptics%Include_Scattering ) THEN
---
>             SpcCoeff_IsVisibleSensor( SC(SensorIndex) ) ) .and. AtmOptics%Include_Scattering ) THEN
688c644
<           RTV%RT_Algorithm_Id = Opt%RT_Algorithm_Id
---
>           RTV%RT_Algorithm_Id = RT_Algorithm_Id
693c649
<         IF ( Opt%Apply_NLTE_Correction ) THEN
---
>         IF ( Apply_NLTE_Correction ) THEN
732,735d687
<           CALL CRTM_AtmOptics_Zero( AtmOptics_Clear )
<           CALL CRTM_AtmOptics_Zero( AtmOptics_Clear_TL )
<           CALL CRTM_RTSolution_Zero( RTSolution_Clear )
<           CALL CRTM_RTSolution_Zero( RTSolution_Clear_TL )
739,740c691,692
<           IF ( Opt%Use_N_Streams ) THEN
<             n_Full_Streams = Opt%n_Streams
---
>           IF ( User_N_Streams ) THEN
>             n_Full_Streams = Options(m)%n_Streams
768a721,728
>           ! Compute the total atmospheric transmittance
>           ! for use in FASTEM-X reflection correction
>           CALL CRTM_Compute_Transmittance(AtmOptics,transmittance)
>           SfcOptics%Transmittance = transmittance
>           CALL CRTM_Compute_Transmittance_TL(AtmOptics,AtmOptics_TL,transmittance_TL)
>           SfcOptics_TL%Transmittance = transmittance_TL
> 
> 
809,827d768
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               RTV_Clear%Visible_Flag_true = .FALSE.
<               RTV_Clear%n_Azi = 0
<             END IF
<           END IF
< 
< 
<           ! Copy the clear-sky AtmOptics
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             Status_FWD = CRTM_AtmOptics_NoScatterCopy(    AtmOptics, AtmOptics_Clear )
<             Status_TL  = CRTM_AtmOptics_NoScatterCopy_TL( AtmOptics, AtmOptics_TL, AtmOptics_Clear_TL )
<             IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
<               Error_Status = FAILURE
<               WRITE( Message,'("Error copying CLEAR SKY AtmOptics for ",a,&
<                      &", channel ",i0,", profile #",i0)' ) &
<                      TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<               CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<               RETURN
<             END IF
883,884c824,825
<             CALL CRTM_AtmOptics_Combine( AtmOptics, AOvar )
<             CALL CRTM_AtmOptics_Combine_TL( AtmOptics, AtmOptics_TL, AOvar )
---
>             CALL CRTM_Combine_AtmOptics( AtmOptics, AOvar )
>             CALL CRTM_Combine_AtmOptics_TL( AtmOptics, AtmOptics_TL, AOvar )
888d828
<           RTSolution_TL(ln,m)%SOD = AtmOptics_TL%Scattering_Optical_Depth
891,893c831,834
<           ! Compute the all-sky atmospheric transmittance
<           ! for use in FASTEM-X reflection correction
<           CALL CRTM_Compute_Transmittance(AtmOptics,transmittance)
---
>           ! Turn off FASTEM-X reflection correction for scattering conditions
>           IF ( CRTM_Include_Scattering(AtmOptics) .AND. SpcCoeff_IsMicrowaveSensor( SC(SensorIndex) ) ) THEN
>             SfcOptics%Transmittance = -ONE
>           ELSE
895,902d835
<           CALL CRTM_Compute_Transmittance_TL(AtmOptics,AtmOptics_TL,transmittance_TL)
<           SfcOptics_TL%Transmittance = transmittance_TL
<           ! ...Clear sky for fractional cloud cover
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             CALL CRTM_Compute_Transmittance(AtmOptics_Clear,transmittance_clear)
<             SfcOptics_Clear%Transmittance = transmittance_clear
<             CALL CRTM_Compute_Transmittance_TL(AtmOptics_Clear,AtmOptics_Clear_TL,transmittance_clear_TL)
<             SfcOptics_Clear_TL%Transmittance = transmittance_clear_TL
906a840
>           ! ...Indicate SfcOptics ARE to be computed
908,910c842,843
<           SfcOptics_Clear%Compute = .TRUE.
<           IF ( Opt%Use_Emissivity ) THEN
<             ! ...Cloudy/all-sky case
---
>           ! ...Change SfcOptics emissivity/reflectivity contents/computation status
>           IF ( User_Emissivity ) THEN
912,915c845,848
<             SfcOptics%Emissivity(1,1)       = Opt%Emissivity(ln)
<             SfcOptics%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
<             IF ( Opt%Use_Direct_Reflectivity ) THEN
<               SfcOptics%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
---
>             SfcOptics%Emissivity(1,1)       = Options(m)%Emissivity(ln)
>             SfcOptics%Reflectivity(1,1,1,1) = ONE - Options(m)%Emissivity(ln)
>             IF ( User_Direct_Reflectivity ) THEN
>               SfcOptics%Direct_Reflectivity(1,1) = Options(m)%Direct_Reflectivity(ln)
919,929d851
<             ! ...Repeat for fractional clear-sky case
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               SfcOptics_Clear%Compute = .FALSE.
<               SfcOptics_Clear%Emissivity(1,1)       = Opt%Emissivity(ln)
<               SfcOptics_Clear%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
<               IF ( Opt%Use_Direct_Reflectivity ) THEN
<                 SfcOptics_Clear%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
<               ELSE
<                 SfcOptics_Clear%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
<               END IF
<             END IF
987,1036d908
< 
< 
<             ! Do clear-sky calculation for fractionally cloudy atmospheres
<             IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<               RTV_Clear%mth_Azi = mth_Azi
<               SfcOptics_Clear%mth_Azi = mth_Azi
<               ! ...Forward model
<               Error_Status = CRTM_Compute_RTSolution( &
<                                Atm_Clear       , &  ! Input
<                                Surface(m)      , &  ! Input
<                                AtmOptics_Clear , &  ! Input
<                                SfcOptics_Clear , &  ! Input
<                                GeometryInfo    , &  ! Input
<                                SensorIndex     , &  ! Input
<                                ChannelIndex    , &  ! Input
<                                RTSolution_Clear, &  ! Output
<                                RTV_Clear         )  ! Internal variable output
<               IF ( Error_Status /= SUCCESS ) THEN
<                 WRITE( Message,'( "Error computing CLEAR SKY RTSolution for ", a, &
<                        &", channel ", i0,", profile #",i0)' ) &
<                        TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<                 CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                 RETURN
<               END IF
< 
<               ! ...Tangent-linear model
<               Error_Status = CRTM_Compute_RTSolution_TL( &
<                                Atm_Clear          , &  ! FWD Input
<                                Surface(m)         , &  ! FWD Input
<                                AtmOptics_Clear    , &  ! FWD Input
<                                SfcOptics_Clear    , &  ! FWD Input
<                                RTSolution_Clear   , &  ! FWD Input
<                                Atm_Clear_TL       , &  ! TL  Input
<                                Surface_TL(m)      , &  ! TL  Input
<                                AtmOptics_Clear_TL , &  ! TL  Input
<                                SfcOptics_Clear_TL , &  ! TL  Input
<                                GeometryInfo       , &  ! Input
<                                SensorIndex        , &  ! Input
<                                ChannelIndex       , &  ! Input
<                                RTSolution_Clear_TL, &  ! TL  Output
<                                RTV_Clear            )  ! Internal variable input
<               IF ( Error_Status /= SUCCESS ) THEN
<                 WRITE( Message,'( "Error computing CLEAR SKY RTSolution_TL for ", a, &
<                        &", channel ", i0,", profile #",i0)' ) &
<                        TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
<                 CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
<                 RETURN
<               END IF
<             END IF
< 
1039,1116d910
< 
<           ! Combine cloudy and clear radiances for fractional cloud coverage
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             ! ...Save the 100% cloudy radince (or just reverse the order of calculation?)
<             r_cloudy = RTSolution(ln,m)%Radiance
<             ! ...Forward radiance
<             RTSolution(ln,m)%Radiance = &
<                 ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_Clear%Radiance) + &
<                 (CloudCover%Total_Cloud_Cover * RTSolution(ln,m)%Radiance)
<             ! ...Tangent-linear radince
<             RTSolution_TL(ln,m)%Radiance = &
<                 ((r_cloudy - RTSolution_Clear%Radiance) * CloudCover_TL%Total_Cloud_Cover) + &
<                 ((ONE - CloudCover%Total_Cloud_Cover)   *  RTSolution_Clear_TL%Radiance  ) + &
<                 (CloudCover%Total_Cloud_Cover           * RTSolution_TL(ln,m)%Radiance   )
<             ! ...Save the cloud cover in the output structures
<             RTSolution(ln,m)%Total_Cloud_Cover    =    CloudCover%Total_Cloud_Cover
<             RTSolution_TL(ln,m)%Total_Cloud_Cover = CloudCover_TL%Total_Cloud_Cover
<           END IF
< 
< 
<           ! The radiance post-processing
<           CALL Post_Process_RTSolution(RTSolution(ln,m), RTSolution_TL(ln,m))
<           
<           
<           ! Perform clear-sky post-processing
<           IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
<             CALL Post_Process_RTSolution(RTSolution_Clear, RTSolution_Clear_TL)
<             ! ...Save the results in the output structure
<             RTSolution(ln,m)%R_Clear     = RTSolution_Clear%Radiance
<             RTSolution(ln,m)%Tb_Clear    = RTSolution_Clear%Brightness_Temperature
<             RTSolution_TL(ln,m)%R_Clear  = RTSolution_Clear_TL%Radiance
<             RTSolution_TL(ln,m)%Tb_Clear = RTSolution_Clear_TL%Brightness_Temperature
<           END IF
< 
<         END DO Channel_Loop
< 
<       END DO Sensor_Loop
< 
<     END DO Profile_Loop
< 
< 
<     ! Clean up
<     CALL CRTM_Predictor_Destroy( Predictor )
<     CALL CRTM_Predictor_Destroy( Predictor_TL )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics_TL )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear )
<     CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear_TL )
<     CALL CRTM_SfcOptics_Destroy( SfcOptics )
<     CALL CRTM_SfcOptics_Destroy( SfcOptics_TL )
<     CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear )
<     CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear_TL )
<     CALL CRTM_Atmosphere_Destroy( Atm )
<     CALL CRTM_Atmosphere_Destroy( Atm_TL )
<     CALL CRTM_Atmosphere_Destroy( Atm_Clear )
<     CALL CRTM_Atmosphere_Destroy( Atm_Clear_TL )
<     ! ...Internal variables
<     CALL AOvar_Destroy( AOvar )
<     CALL CSvar_Destroy( CSvar )
<     CALL ASvar_Destroy( ASvar )
<     CALL RTV_Destroy( RTV ) 
< 
< 
<   CONTAINS
< 
< 
<     ! ----------------------------------------------------------------
<     ! Local subroutine to post-process the radiance, as it is the same
<     ! for all-sky and fractional clear-sky cases.
<     !
<     !   1. Apply non-LTE correction to radiances
<     !   2. Convert radiances to brightness temperatures
<     !   3. Apply antenna correction to brightness temperatures
<     ! ----------------------------------------------------------------
< 
<     SUBROUTINE Post_Process_RTSolution(rts, rts_TL)
<       TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: rts, rts_TL
< 
1118c912
<       IF ( Opt%Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
---
>           IF ( Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
1123c917
<                rts%Radiance        )  ! In/Output
---
>                    RTSolution(ln,m)%Radiance  )  ! In/Output
1128c922
<                rts_TL%Radiance     )  ! In/Output
---
>                    RTSolution_TL(ln,m)%Radiance  )  ! In/Output
1129a924
> 
1134,1135c929,930
<              rts%Radiance              , & ! Input
<              rts%Brightness_Temperature  ) ! Output
---
>                  RTSolution(ln,m)%Radiance              , & ! Input
>                  RTSolution(ln,m)%Brightness_Temperature  ) ! Output
1139,1141c934,937
<              rts%Radiance                 , & ! Input
<              rts_TL%Radiance              , & ! Input
<              rts_TL%Brightness_Temperature  ) ! Output
---
>                  RTSolution(ln,m)%Radiance                 , & ! Input
>                  RTSolution_TL(ln,m)%Radiance              , & ! Input
>                  RTSolution_TL(ln,m)%Brightness_Temperature  ) ! Output
> 
1143c939
<       IF ( compute_antenna_correction ) THEN
---
>           IF ( Compute_AntCorr ) THEN
1148c944
<                rts           )  ! Output
---
>                    RTSolution(ln,m)  )  ! Output
1153c949
<                rts_TL       )  ! Output
---
>                    RTSolution_TL(ln,m)  )  ! Output
1154a951,970
>         END DO Channel_Loop
> 
> 
>         ! Deallocate local sensor dependent data structures
>         ! ...RTV structure
>         IF ( RTV_Associated(RTV) ) CALL RTV_Destroy(RTV)
>         ! ...Predictor structures
>         CALL CRTM_Predictor_Destroy( Predictor )
>         CALL CRTM_Predictor_Destroy( Predictor_TL )
> 
>       END DO Sensor_Loop
> 
> 
>       ! Deallocate local sensor independent data structures
>       ! ...Atmospheric optics
>       CALL CRTM_AtmOptics_Destroy( AtmOptics )
>       CALL CRTM_AtmOptics_Destroy( AtmOptics_TL )
> 
>     END DO Profile_Loop
> 
1156c972,976
<     END SUBROUTINE Post_Process_RTSolution
---
>     ! Destroy any remaining structures
>     CALL CRTM_SfcOptics_Destroy( SfcOptics )
>     CALL CRTM_SfcOptics_Destroy( SfcOptics_TL )
>     CALL CRTM_Atmosphere_Destroy( Atm_TL )
>     CALL CRTM_Atmosphere_Destroy( Atm )
diff -w ./CRTM_TauCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_TauCoeff.f90
diff -w ./CRTM_Utility.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Utility.f90
diff -w ./CRTM_VIS_Ice_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_VIS_Ice_SfcOptics.f90
diff -w ./CRTM_VIS_Land_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_VIS_Land_SfcOptics.f90
diff -w ./CRTM_VIS_Snow_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_VIS_Snow_SfcOptics.f90
diff -w ./CRTM_VIS_Water_SfcOptics.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_VIS_Water_SfcOptics.f90
diff -w ./CRTM_VISiceCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_VISiceCoeff.f90
diff -w ./CRTM_VISlandCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_VISlandCoeff.f90
diff -w ./CRTM_VISsnowCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_VISsnowCoeff.f90
diff -w ./CRTM_VISwaterCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_VISwaterCoeff.f90
diff -w ./CSvar_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CSvar_Define.f90
diff -w ./CloudCoeff_Binary_IO.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CloudCoeff_Binary_IO.f90
diff -w ./CloudCoeff_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/CloudCoeff_Define.f90
diff -w ./Common_RTSolution.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Common_RTSolution.f90
240,241d239
<       RTSolution%Single_Scatter_Albedo(1:no) = AtmOptics%Single_Scatter_Albedo(na+1:nt)
<       RTSolution%SSA_Max = MAXVAL(AtmOptics%Single_Scatter_Albedo)      
diff -w ./Compare_Float_Numbers.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Compare_Float_Numbers.f90
30,31d29
<   PUBLIC :: SP_N_SIGFIG
<   PUBLIC :: DP_N_SIGFIG
99,104c97,98
<   ! Default cutoff value for comparisons
<   REAL(Single), PARAMETER :: SP_COMPARE_CUTOFF = TINY(1.0_Single)
<   REAL(Double), PARAMETER :: DP_COMPARE_CUTOFF = TINY(1.0_Double)
<   ! Minimum exponents for precisions
<   INTEGER, PARAMETER :: SP_MIN_EXPONENT = -RANGE(1.0_Single)
<   INTEGER, PARAMETER :: DP_MIN_EXPONENT = -RANGE(1.0_Double)
---
>   REAL(Single), PARAMETER :: SP_COMPARE_CUTOFF = 1.0e-15_Single
>   REAL(Double), PARAMETER :: DP_COMPARE_CUTOFF = 1.0e-15_Double
106,108c100
<   INTEGER, PARAMETER :: SP_N_SIGFIG =  7
<   INTEGER, PARAMETER :: DP_N_SIGFIG = 15
<   INTEGER, PARAMETER :: DEFAULT_N_SIGFIG = SP_N_SIGFIG
---
>   INTEGER, PARAMETER :: DEFAULT_N_SIGFIG = 6
537c529
< !       tol = Tolerance( x, n )
---
> !       Result = Tolerance( x, n )
556c548
< !                    DIMENSION:  Conformable with input x.
---
> !                    DIMENSION:  Scalar or same as input x.
560c552
< !       tol:         The return value is a tolerance value that can be used to
---
> !       Result:      The return value is a tolerance value that can be used to
564c556
< !                    DIMENSION:  Conformable with input x.
---
> !                    DIMENSION:  Same as input x.
574a567
>       Tolerance = SP_TEN**e
576c569
<       e = -n
---
>       Tolerance = SP_ONE
578,579d570
<     e = MAX(e,SP_MIN_EXPONENT)
<     Tolerance = SP_TEN**e
588a580
>       Tolerance = DP_TEN**e
590c582
<       e = -n
---
>       Tolerance = DP_ONE
592,593d583
<     e = MAX(e,DP_MIN_EXPONENT)
<     Tolerance = DP_TEN**e
653c643,644
< !                    If not specified, the default value is TINY().
---
> !                    If not specified, the default value is 1.0e-15 for real
> !                    input, or (1.0e-15,1.0e-15) for complex input.
diff -w ./DateTime_Utility.f90 ~/CRTM/clean/REL-2.2.3/libsrc/DateTime_Utility.f90
diff -w ./Date_Utility.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Date_Utility.f90
diff -w ./Ellison.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Ellison.f90
diff -w ./Emission_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Emission_Module.f90
diff -w ./Endian_Utility.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Endian_Utility.f90
562c562
< ! $Revision$
---
> ! $Revision: 60152 $
diff -w ./File_Utility.f90 ~/CRTM/clean/REL-2.2.3/libsrc/File_Utility.f90
356c356
< ! $Revision$
---
> ! $Revision: 60152 $
diff -w ./FitCoeff_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/FitCoeff_Define.f90
diff -w ./Foam_Utility_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Foam_Utility_Module.f90
diff -w ./Fresnel.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Fresnel.f90
diff -w ./Fundamental_Constants.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Fundamental_Constants.f90
diff -w ./Guillou.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Guillou.f90
diff -w ./Hyperbolic_Step.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Hyperbolic_Step.f90
diff -w ./IRwaterCoeff_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/IRwaterCoeff_Define.f90
diff -w ./LSEatlas_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/LSEatlas_Define.f90
diff -w ./Large_Scale_Correction_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Large_Scale_Correction_Module.f90
diff -w ./Liu.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Liu.f90
diff -w ./MWwaterCoeff_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/MWwaterCoeff_Define.f90
diff -w ./MWwaterLUT_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/MWwaterLUT_Define.f90
diff -w ./Message_Handler.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Message_Handler.f90
diff -w ./NESDIS_AMSRE_SICEEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_AMSRE_SICEEM_Module.f90
diff -w ./NESDIS_AMSRE_SNOWEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_AMSRE_SNOWEM_Module.f90
diff -w ./NESDIS_AMSU_SICEEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_AMSU_SICEEM_Module.f90
342d341
<   save coe
345,355c344,354
<   coe(1:5) = (/ 9.815214e-001_fp,  3.783815e-003_fp,  &
<        6.391155e-004_fp, -9.106375e-005_fp, -4.263206e-003_fp/)
<   coe(21:25) = (/ 9.047181e-001_fp, -2.782826e-004_fp,  &
<        4.664207e-003_fp, -3.121744e-005_fp, -3.976189e-003_fp/)
<   coe(41:45) = (/ 1.163853e+000_fp, -1.419205e-003_fp,  &
<        5.505238e-003_fp,  1.506867e-003_fp, -6.157735e-003_fp/)
<   coe(61:65) = (/  1.020753e+000_fp, -8.666064e-004_fp,  &
<        9.624331e-004_fp,  4.878773e-003_fp, -5.055044e-003_fp/)
<   coe(81:85) = (/ 1.438246e+000_fp,  5.667756e-004_fp, &
<        -2.621972e-003_fp,  5.928146e-003_fp, -5.856687e-003_fp/)
< ! save coe
---
>   data (coe(k),k=1,5)/ 9.815214e-001_fp,  3.783815e-003_fp,  &
>        6.391155e-004_fp, -9.106375e-005_fp, -4.263206e-003_fp/
>   data (coe(k),k=21,25)/ 9.047181e-001_fp, -2.782826e-004_fp,  &
>        4.664207e-003_fp, -3.121744e-005_fp, -3.976189e-003_fp/
>   data (coe(k),k=41,45)/ 1.163853e+000_fp, -1.419205e-003_fp,  &
>        5.505238e-003_fp,  1.506867e-003_fp, -6.157735e-003_fp/
>   data (coe(k),k=61,65)/  1.020753e+000_fp, -8.666064e-004_fp,  &
>        9.624331e-004_fp,  4.878773e-003_fp, -5.055044e-003_fp/
>   data (coe(k),k=81,85)/ 1.438246e+000_fp,  5.667756e-004_fp, &
>        -2.621972e-003_fp,  5.928146e-003_fp, -5.856687e-003_fp/
>   save coe
424d422
<   save coe
427c425
<   coe(1:7) = (/ 2.239429e+000_fp, -2.153967e-002_fp,  &
---
>   data (coe(k),k=1,7)/ 2.239429e+000_fp, -2.153967e-002_fp,  &
429,430c427,428
<        -3.749251e-005_fp, -5.128486e-002_fp, -2.184161e-003_fp/)
<   coe(11:17) = (/ 1.768085e+000_fp, -1.643430e-002_fp,  &
---
>        -3.749251e-005_fp, -5.128486e-002_fp, -2.184161e-003_fp/
>   data (coe(k),k=11,17)/ 1.768085e+000_fp, -1.643430e-002_fp,  &
432,433c430,431
<        -3.628051e-005_fp, -4.751277e-002_fp, -2.580649e-003_fp/)
<   coe(21:27) = (/ 8.910227e-001_fp,  6.170706e-003_fp, &
---
>        -3.628051e-005_fp, -4.751277e-002_fp, -2.580649e-003_fp/
>   data (coe(k),k=21,27)/ 8.910227e-001_fp,  6.170706e-003_fp, &
435,436c433,434
<        -2.208121e-006_fp, -3.163193e-002_fp, -3.863217e-003_fp/)
< ! save coe
---
>        -2.208121e-006_fp, -3.163193e-002_fp, -3.863217e-003_fp/
>   save coe
506c504
<   freq = (/23.8_fp, 31.4_fp, 50.3_fp,89.0_fp, 150.0_fp/)
---
>   data  freq/23.8_fp, 31.4_fp, 50.3_fp,89.0_fp, 150.0_fp/
diff -w ./NESDIS_AMSU_SnowEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_AMSU_SnowEM_Module.f90
787c787
<   DI_coe(1,0:ncoe-1) = (/ &
---
>   data (DI_coe(1,k),k=0,ncoe-1)/ &
791,792c791,792
<        1.680881e-005_fp, -1.708405e-004_fp/)
<   DI_coe(2,0:ncoe-1) = (/ &
---
>        1.680881e-005_fp, -1.708405e-004_fp/
>   data (DI_coe(2,k),k=0,ncoe-1)/ &
796,797c796,797
<        -4.748506e-003_fp,  2.293836e-004_fp/)
<   DI_coe(3,0:ncoe-1) = (/ &
---
>        -4.748506e-003_fp,  2.293836e-004_fp/
>   data (DI_coe(3,k),k=0,ncoe-1)/ &
801,802c801,802
<       -2.312224e-004_fp,  9.498600e-004_fp/)
<   DI_coe(4,0:ncoe-1) = (/ &
---
>       -2.312224e-004_fp,  9.498600e-004_fp/
>   data (DI_coe(4,k),k=0,ncoe-1)/ &
806,807c806,807
<        5.179711e-004_fp,  4.667157e-005_fp/)
<   DI_coe(5,0:ncoe-1) = (/ &
---
>        5.179711e-004_fp,  4.667157e-005_fp/
>   data (DI_coe(5,k),k=0,ncoe-1)/ &
811c811
<        4.517280e-003_fp,  7.204695e-004_fp/)
---
>        4.517280e-003_fp,  7.204695e-004_fp/
814c814
<   LI_coe = (/ &
---
>   data  LI_coe/ &
817c817
<        1.731405e-005_fp, -4.105358e-003_fp/)
---
>        1.731405e-005_fp, -4.105358e-003_fp/
820c820
<   HI_coe = (/ &
---
>   data  HI_coe/ &
826c826
<        1.955293e-006_fp, -4.942230e-003_fp/)
---
>        1.955293e-006_fp, -4.942230e-003_fp/
833,842c833,842
<   threshold(1,1:6) = (/0.88_fp,0.86_fp,-999.9_fp,&
<        0.01_fp,0.01_fp,200._fp/)
<   threshold(2,1:6) = (/0.88_fp,0.85_fp,-999.9_fp,&
<        0.06_fp,0.10_fp,200._fp/)
<   threshold(3,1:6) = (/0.88_fp,0.83_fp,-0.02_fp,&
<        0.12_fp,0.16_fp,204._fp/)
<   threshold(4,1:6) = (/0.90_fp,0.89_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
<   threshold(5,1:6) = (/0.92_fp,0.85_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
---
>   data (threshold(1,k),k=1,6)/0.88_fp,0.86_fp,-999.9_fp,&
>        0.01_fp,0.01_fp,200._fp/
>   data (threshold(2,k),k=1,6)/0.88_fp,0.85_fp,-999.9_fp,&
>        0.06_fp,0.10_fp,200._fp/
>   data (threshold(3,k),k=1,6)/0.88_fp,0.83_fp,-0.02_fp,&
>        0.12_fp,0.16_fp,204._fp/
>   data (threshold(4,k),k=1,6)/0.90_fp,0.89_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
>   data (threshold(5,k),k=1,6)/0.92_fp,0.85_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
845,854c845,854
<   threshold(6,1:6) = (/0.84_fp,0.83_fp,-999.9_fp,&
<        0.08_fp,0.10_fp,195._fp/)
<   threshold(7,1:6) = (/0.85_fp,0.85_fp,-999.9_fp,&
<        0.10_fp,-999.9_fp,190._fp/)
<   threshold(8,1:6) = (/0.86_fp,0.81_fp,-999.9_fp,&
<        0.12_fp,-999.9_fp,200._fp/)
<   threshold(9,1:6) = (/0.86_fp,0.81_fp,0.0_fp,&
<        0.12_fp,-999.9_fp,189._fp/)
<   threshold(10,1:6) = (/0.90_fp,0.81_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,195._fp/)
---
>   data (threshold(6,k),k=1,6)/0.84_fp,0.83_fp,-999.9_fp,&
>        0.08_fp,0.10_fp,195._fp/
>   data (threshold(7,k),k=1,6)/0.85_fp,0.85_fp,-999.9_fp,&
>        0.10_fp,-999.9_fp,190._fp/
>   data (threshold(8,k),k=1,6)/0.86_fp,0.81_fp,-999.9_fp,&
>        0.12_fp,-999.9_fp,200._fp/
>   data (threshold(9,k),k=1,6)/0.86_fp,0.81_fp,0.0_fp,&
>        0.12_fp,-999.9_fp,189._fp/
>   data (threshold(10,k),k=1,6)/0.90_fp,0.81_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,195._fp/
857,862c857,862
<   threshold(11,1:6) = (/0.80_fp,0.76_fp,-999.9_fp,&
<        0.05_fp,-999.9_fp,185._fp/)
<   threshold(12,1:6) = (/0.82_fp,0.78_fp,-999.9_fp,&
<        -999.9_fp,0.25_fp,180._fp/)
<   threshold(13,1:6) = (/0.90_fp,0.76_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,180._fp/)
---
>   data (threshold(11,k),k=1,6)/0.80_fp,0.76_fp,-999.9_fp,&
>        0.05_fp,-999.9_fp,185._fp/
>   data (threshold(12,k),k=1,6)/0.82_fp,0.78_fp,-999.9_fp,&
>        -999.9_fp,0.25_fp,180._fp/
>   data (threshold(13,k),k=1,6)/0.90_fp,0.76_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,180._fp/
865,870c865,870
<   threshold(14,1:6) = (/0.89_fp,0.73_fp,-999.9_fp,&
<        0.20_fp,-999.9_fp,-999.9_fp/)
<   threshold(15,1:6) = (/0.89_fp,0.75_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
<   threshold(16,1:6) = (/0.93_fp,0.72_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
---
>   data (threshold(14,k),k=1,6)/0.89_fp,0.73_fp,-999.9_fp,&
>        0.20_fp,-999.9_fp,-999.9_fp/
>   data (threshold(15,k),k=1,6)/0.89_fp,0.75_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
>   data (threshold(16,k),k=1,6)/0.93_fp,0.72_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
873,876c873,876
<   threshold(17,1:6) = (/0.82_fp,0.70_fp,-999.9_fp,&
<        0.20_fp,-999.9_fp,160._fp/)
<   threshold(18,1:6) = (/0.83_fp,0.70_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,160._fp/)
---
>   data (threshold(17,k),k=1,6)/0.82_fp,0.70_fp,-999.9_fp,&
>        0.20_fp,-999.9_fp,160._fp/
>   data (threshold(18,k),k=1,6)/0.83_fp,0.70_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,160._fp/
879,890c879,890
<   threshold(19,1:6) = (/0.75_fp,0.76_fp,-999.9_fp,&
<        0.08_fp,-999.9_fp,172._fp/)
<   threshold(20,1:6) = (/0.77_fp,0.72_fp,-999.9_fp,&
<        0.12_fp,0.15_fp,175._fp/)
<   threshold(21,1:6) = (/0.78_fp,0.74_fp,-999.9_fp,&
<        -999.9_fp,0.20_fp,172._fp/)
<   threshold(22,1:6) = (/0.80_fp,0.77_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,170._fp/)
<   threshold(23,1:6) = (/0.82_fp,-999.9_fp,-999.9_fp,&
<        0.15_fp,0.22_fp,170._fp/)
<   threshold(24,1:6) = (/0.82_fp,0.73_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,170._fp/)
---
>   data (threshold(19,k),k=1,6)/0.75_fp,0.76_fp,-999.9_fp,&
>        0.08_fp,-999.9_fp,172._fp/
>   data (threshold(20,k),k=1,6)/0.77_fp,0.72_fp,-999.9_fp,&
>        0.12_fp,0.15_fp,175._fp/
>   data (threshold(21,k),k=1,6)/0.78_fp,0.74_fp,-999.9_fp,&
>        -999.9_fp,0.20_fp,172._fp/
>   data (threshold(22,k),k=1,6)/0.80_fp,0.77_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,170._fp/
>   data (threshold(23,k),k=1,6)/0.82_fp,-999.9_fp,-999.9_fp,&
>        0.15_fp,0.22_fp,170._fp/
>   data (threshold(24,k),k=1,6)/0.82_fp,0.73_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,170._fp/
893,905c893,905
<   threshold(25,1:6) = (/0.75_fp,0.70_fp,-999.9_fp,&
<        0.15_fp,0.25_fp,167._fp/)
<   threshold(26,1:6) = (/0.77_fp,0.76_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
<   threshold(27,1:6) = (/0.80_fp,0.72_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
<   threshold(28,1:6) = (/0.77_fp,0.73_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
< 
<   threshold(29,1:6) = (/0.81_fp,0.71_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
<   threshold(30,1:6) = (/0.82_fp,0.69_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
---
>   data (threshold(25,k),k=1,6)/0.75_fp,0.70_fp,-999.9_fp,&
>        0.15_fp,0.25_fp,167._fp/
>   data (threshold(26,k),k=1,6)/0.77_fp,0.76_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
>   data (threshold(27,k),k=1,6)/0.80_fp,0.72_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
>   data (threshold(28,k),k=1,6)/0.77_fp,0.73_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
> 
>   data (threshold(29,k),k=1,6)/0.81_fp,0.71_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
>   data (threshold(30,k),k=1,6)/0.82_fp,0.69_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
908,909c908,909
<   threshold(31,1:6) = (/0.88_fp,0.58_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
---
>   data (threshold(31,k),k=1,6)/0.88_fp,0.58_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
912,913c912,913
<   threshold(32,1:6) = (/0.73_fp,0.67_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
---
>   data (threshold(32,k),k=1,6)/0.73_fp,0.67_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
916,917c916,917
<   threshold(33,1:6) = (/0.83_fp,0.66_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
---
>   data (threshold(33,k),k=1,6)/0.83_fp,0.66_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
920,921c920,921
<   threshold(34,1:6) = (/0.82_fp,0.60_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
---
>   data (threshold(34,k),k=1,6)/0.82_fp,0.60_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
924,925c924,925
<   threshold(35,1:6) = (/0.77_fp,0.60_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
---
>   data (threshold(35,k),k=1,6)/0.77_fp,0.60_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
928,929c928,929
<   threshold(36,1:6) = (/0.77_fp,0.7_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
---
>   data (threshold(36,k),k=1,6)/0.77_fp,0.7_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
932,933c932,933
<   threshold(37,1:6) = (/-999.9_fp,0.55_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
---
>   data (threshold(37,k),k=1,6)/-999.9_fp,0.55_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
936,937c936,937
<   threshold(38,1:6) = (/0.74_fp,-999.9_fp,-999.9_fp,&
<        -999.9_fp,-999.9_fp,-999.9_fp/)
---
>   data (threshold(38,k),k=1,6)/0.74_fp,-999.9_fp,-999.9_fp,&
>        -999.9_fp,-999.9_fp,-999.9_fp/
1170d1169
<   save coe
1173c1172
<   coe(1:7) = (/&
---
>   data (coe(k),k=1,7)/&
1177c1176
<        -4.925512e-005_fp/)
---
>        -4.925512e-005_fp/
1180c1179
<   coe(12:18) = (/ &
---
>   data (coe(k),k=12,18)/ &
1184c1183
<        -4.574811e-005_fp/)
---
>        -4.574811e-005_fp/
1187c1186
<   coe(23:29) = (/  &
---
>   data (coe(k),k=23,29)/  &
1191c1190
<        -7.358356e-005_fp/)
---
>        -7.358356e-005_fp/
1194c1193
<   coe(34:42) = (/ &
---
>   data (coe(k),k=34,42)/ &
1199c1198
<        -6.351148e-006_fp/)
---
>        -6.351148e-006_fp/
1202c1201
<   coe(45:55) = (/&
---
>   data (coe(k),k=45,55)/&
1208c1207
<      1.269419e-005_fp/)
---
>      1.269419e-005_fp/
1210c1209
< ! save coe
---
>   save coe
1283d1281
<   save coe
1286c1284
<   coe(1:6) = (/ &
---
>   data (coe(k),k=1,6)/ &
1289c1287
<        1.809047e-005_fp, -4.206605e-003_fp /)
---
>        1.809047e-005_fp, -4.206605e-003_fp/
1292c1290
<   coe(11:16) = (/ &
---
>   data (coe(k),k=11,16)/ &
1295c1293
<        1.731405e-005_fp, -4.105358e-003_fp /)
---
>        1.731405e-005_fp, -4.105358e-003_fp/
1298c1296
<   coe(21:28) = (/ &
---
>   data (coe(k),k=21,28)/ &
1302c1300
<        -5.581535e-008_fp, -5.413451e-003_fp /)
---
>        -5.581535e-008_fp, -5.413451e-003_fp/
1305c1303
<   coe(31:40) = (/ &
---
>   data (coe(k),k=31,40)/ &
1310c1308
<        9.711384e-006_fp, -4.259102e-003_fp /)
---
>        9.711384e-006_fp, -4.259102e-003_fp/
1313c1311
<   coe(41:50) = (/ &
---
>   data (coe(k),k=41,50)/ &
1318c1316
<        6.190403e-007_fp, -2.944785e-003_fp/)
---
>        6.190403e-007_fp, -2.944785e-003_fp/
1320c1318
< ! save coe
---
>   save coe
1392c1390
<   save coe
---
> 
1395c1393
<   coe(1:7) = (/ &
---
>   data (coe(k),k=1,7)/ &
1398c1396
<        -4.925512e-005_fp/)
---
>        -4.925512e-005_fp/
1401c1399
<   coe(11:17) = (/ &
---
>   data (coe(k),k=11,17)/ &
1404c1402
<        -4.574811e-005_fp/)
---
>        -4.574811e-005_fp/
1407c1405
<   coe(21:27) = (/ &
---
>   data (coe(k),k=21,27)/ &
1410c1408
<        -7.358356e-005_fp/)
---
>        -7.358356e-005_fp/
1413c1411
<   coe(31:39) = (/ &
---
>   data (coe(k),k=31,39)/ &
1416c1414
<        -4.154825e-005_fp,  7.703879e-003_fp, -6.351148e-006_fp/)
---
>        -4.154825e-005_fp,  7.703879e-003_fp, -6.351148e-006_fp/
1419c1417
<   coe(41:49) = (/ &
---
>   data (coe(k),k=41,49)/ &
1422c1420
<        -6.702349e-005_fp, 1.111658e-002_fp, -1.050708e-005_fp/)
---
>        -6.702349e-005_fp, 1.111658e-002_fp, -1.050708e-005_fp/
1424c1422
< ! save coe
---
>   save coe
1506c1504
<   save coe
---
> 
1509,1510c1507,1508
<   coe(1:6) = (/ 3.110967e-001_fp,  1.100175e-002_fp, -1.677626e-005_fp,    &
<        -4.020427e-003_fp,  9.242240e-006_fp, -2.363207e-003_fp/)
---
>   data (coe(k),k=1,6)/ 3.110967e-001_fp,  1.100175e-002_fp, -1.677626e-005_fp,    &
>        -4.020427e-003_fp,  9.242240e-006_fp, -2.363207e-003_fp/
1512,1513c1510,1511
<   coe(11:16) = (/  1.148098e+000_fp,  1.452926e-003_fp,  1.037081e-005_fp, &
<        1.340696e-003_fp, -5.185640e-006_fp, -4.546382e-003_fp /)
---
>   data (coe(k),k=11,16)/  1.148098e+000_fp,  1.452926e-003_fp,  1.037081e-005_fp, &
>        1.340696e-003_fp, -5.185640e-006_fp, -4.546382e-003_fp /
1515,1517c1513,1515
<   coe(21:26) = (/ 1.165323e+000_fp, -1.030435e-003_fp,  4.828009e-006_fp,  &
<        4.851731e-003_fp, -2.588049e-006_fp, -4.990193e-003_fp/)
< ! save coe
---
>   data (coe(k),k=21,26)/ 1.165323e+000_fp, -1.030435e-003_fp,  4.828009e-006_fp,  &
>        4.851731e-003_fp, -2.588049e-006_fp, -4.990193e-003_fp/
>   save coe
1596c1594
<   save coe
---
> 
1599,1600c1597,1598
<   coe(1:5) = (/-4.015636e-001_fp,9.297894e-003_fp, -1.305068e-005_fp, &
<        3.717131e-004_fp, -4.364877e-006_fp/)
---
>   data (coe(k),k=1,5)/-4.015636e-001_fp,9.297894e-003_fp, -1.305068e-005_fp, &
>        3.717131e-004_fp, -4.364877e-006_fp/
1602,1603c1600,1601
<   coe(11:15) = (/-2.229547e-001_fp, -1.828402e-003_fp,1.754807e-005_fp, &
<        9.793681e-003_fp, -3.137189e-005_fp/)
---
>   data (coe(k),k=11,15)/-2.229547e-001_fp, -1.828402e-003_fp,1.754807e-005_fp, &
>        9.793681e-003_fp, -3.137189e-005_fp/
1605,1607c1603,1605
<   coe(21:25) = (/-3.395416e-001_fp,-4.632656e-003_fp,1.270735e-005_fp, &
<        1.413038e-002_fp,-3.133239e-005_fp/)
< ! save coe
---
>   data (coe(k),k=21,25)/-3.395416e-001_fp,-4.632656e-003_fp,1.270735e-005_fp, &
>        1.413038e-002_fp,-3.133239e-005_fp/
>   save coe
1692,1694c1690
<   save freq_3w
< 
<   freq_3w = (/31.4_fp,89.0_fp,150.0_fp/)
---
>   data   freq_3w/31.4_fp,89.0_fp,150.0_fp/
1782c1778
<   dem_coe(1,0:ncoe-1) = (/ 2.306844e+000_Double, -7.287718e-003_Double, &
---
>   data (dem_coe(1,k),k=0,ncoe-1)/ 2.306844e+000_Double, -7.287718e-003_Double, &
1786c1782
<        4.766508e-007_Double, -1.754184e+000_Double/)
---
>        4.766508e-007_Double, -1.754184e+000_Double/
1788c1784
<   dem_coe(2,0:ncoe-1) = (/ 3.152527e+000_Double, -1.823670e-002_Double, &
---
>   data (dem_coe(2,k),k=0,ncoe-1)/ 3.152527e+000_Double, -1.823670e-002_Double, &
1792c1788
<        9.609477e-007_Double, -1.113725e+000_Double/)
---
>        9.609477e-007_Double, -1.113725e+000_Double/
1794c1790
<   dem_coe(3,0:ncoe-1) = (/ 3.492495e+000_Double, -2.184545e-002_Double,  &
---
>   data (dem_coe(3,k),k=0,ncoe-1)/ 3.492495e+000_Double, -2.184545e-002_Double,  &
1798c1794
<        -6.305717e-008_Double, -1.221087e+000_Double/)
---
>        -6.305717e-008_Double, -1.221087e+000_Double/
diff -w ./NESDIS_ATMS_SeaICE_LIB.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_ATMS_SeaICE_LIB.f90
diff -w ./NESDIS_ATMS_SeaICE_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_ATMS_SeaICE_Module.f90
diff -w ./NESDIS_ATMS_SnowEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_ATMS_SnowEM_Module.f90
884c884
<      SAVE coe
---
> 
887,888c887,888
<      coe(1:5) = (/-4.015636e-001_fp,9.297894e-003_fp, -1.305068e-005_fp, &
<           3.717131e-004_fp, -4.364877e-006_fp/)
---
>      DATA (coe(k),k=1,5)/-4.015636e-001_fp,9.297894e-003_fp, -1.305068e-005_fp, &
>           3.717131e-004_fp, -4.364877e-006_fp/
890,891c890,891
<      coe(11:15) = (/-2.229547e-001_fp, -1.828402e-003_fp,1.754807e-005_fp, &
<           9.793681e-003_fp, -3.137189e-005_fp/)
---
>      DATA (coe(k),k=11,15)/-2.229547e-001_fp, -1.828402e-003_fp,1.754807e-005_fp, &
>           9.793681e-003_fp, -3.137189e-005_fp/
893,895c893,895
<      coe(21:25) = (/-3.395416e-001_fp,-4.632656e-003_fp,1.270735e-005_fp, &
<           1.413038e-002_fp,-3.133239e-005_fp/)
< !    SAVE coe
---
>      DATA (coe(k),k=21,25)/-3.395416e-001_fp,-4.632656e-003_fp,1.270735e-005_fp, &
>           1.413038e-002_fp,-3.133239e-005_fp/
>      SAVE coe
994c994
<      nmodel = (/5,10,13,16,18,24,30,31,32,33,34,35,36,37,38/)
---
>      DATA  nmodel/5,10,13,16,18,24,30,31,32,33,34,35,36,37,38/
997c997
<      LI_coe = (/ &
---
>      DATA  LI_coe/ &
1000c1000
<           1.731405e-005_fp, -4.105358e-003_fp/)
---
>           1.731405e-005_fp, -4.105358e-003_fp/
1003c1003
<      HI_coe = (/ &
---
>      DATA  HI_coe/ &
1009c1009
<           1.955293e-006_fp, -4.942230e-003_fp/)
---
>           1.955293e-006_fp, -4.942230e-003_fp/
1465c1465
<      freq_3w = (/31.4_fp,89.0_fp,150.0_fp/)
---
>      DATA   freq_3w/31.4_fp,89.0_fp,150.0_fp/
diff -w ./NESDIS_LandEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_LandEM_Module.f90
diff -w ./NESDIS_MHS_SICEEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_MHS_SICEEM_Module.f90
diff -w ./NESDIS_MHS_SnowEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_MHS_SnowEM_Module.f90
diff -w ./NESDIS_SEAICE_PHYEM_MODULE.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_SEAICE_PHYEM_MODULE.f90
diff -w ./NESDIS_SSMIS_SeaIceEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_SSMIS_SeaIceEM_Module.f90
diff -w ./NESDIS_SSMIS_SnowEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_SSMIS_SnowEM_Module.f90
diff -w ./NESDIS_SSMI_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_SSMI_Module.f90
354c354
<   save  coe_v,coe_h
---
> 
357,365c357,365
<   coe_v(1,1,1:5) = (/ -8.722723e-002_fp,  1.064573e-002_fp, &
<        -5.333843e-003_fp, -1.394910e-003_fp,  4.007640e-004_fp/)
<   coe_v(1,2,1:5) = (/-1.373924e-001_fp,  6.580569e-003_fp, &
<        -9.991220e-004_fp, -1.476022e-003_fp,  4.131816e-004_fp/)
<   coe_v(1,3,1:5) = (/ -2.329867e-001_fp,  6.419856e-003_fp, &
<        -5.260987e-003_fp, 3.342582e-003_fp,  4.139272e-004_fp/)
<   coe_v(1,4,1:5) = (/ -3.528638e-001_fp,  6.342649e-003_fp, &
<        -5.002575e-003_fp, -1.469298e-003_fp,  5.529711e-003_fp/)
<   coe_h(1,1,1:4) = (/ &
---
>   data (coe_v(1,1,k),k=1,5)/ -8.722723e-002_fp,  1.064573e-002_fp, &
>        -5.333843e-003_fp, -1.394910e-003_fp,  4.007640e-004_fp/
>   data (coe_v(1,2,k),k=1,5)/-1.373924e-001_fp,  6.580569e-003_fp, &
>        -9.991220e-004_fp, -1.476022e-003_fp,  4.131816e-004_fp/
>   data (coe_v(1,3,k),k=1,5)/ -2.329867e-001_fp,  6.419856e-003_fp, &
>        -5.260987e-003_fp, 3.342582e-003_fp,  4.139272e-004_fp/
>   data (coe_v(1,4,k),k=1,5)/ -3.528638e-001_fp,  6.342649e-003_fp, &
>        -5.002575e-003_fp, -1.469298e-003_fp,  5.529711e-003_fp/
>   data (coe_h(1,1,k),k=1,4)/ &
367,368c367,368
<        5.706367e-004_fp/)
<   coe_h(1,2,1:4) = (/ &
---
>        5.706367e-004_fp/
>   data (coe_h(1,2,k),k=1,4)/ &
370,371c370,371
<        5.924890e-004_fp/)
<   coe_h(1,3,1:4) = (/ &
---
>        5.924890e-004_fp/
>   data (coe_h(1,3,k),k=1,4)/ &
373c373
<        5.750499e-003_fp/)
---
>        5.750499e-003_fp/
376,383c376,383
<   coe_v(2,1,1:5) = (/  1.109066e-001_fp,  5.449409e-003_fp,  &
<        1.835799e-004_fp, -1.765248e-003_fp, -2.996101e-004_fp/)
<   coe_v(2,2,1:5) = (/ 9.356505e-002_fp,  1.320617e-003_fp,  &
<        4.449195e-003_fp, -1.786960e-003_fp, -3.479687e-004_fp/)
<   coe_v(2,3,1:5) = (/ 6.387097e-002_fp,  1.252447e-003_fp,  &
<        1.998846e-004_fp, 2.680219e-003_fp, -3.740141e-004_fp/)
<   coe_v(2,4,1:5) = (/ 4.150689e-002_fp,  1.420274e-003_fp,  &
<        1.223339e-004_fp, -1.948946e-003_fp,  4.248289e-003_fp/)
---
>   data (coe_v(2,1,k),k=1,5)/  1.109066e-001_fp,  5.449409e-003_fp,  &
>        1.835799e-004_fp, -1.765248e-003_fp, -2.996101e-004_fp/
>   data (coe_v(2,2,k),k=1,5)/ 9.356505e-002_fp,  1.320617e-003_fp,  &
>        4.449195e-003_fp, -1.786960e-003_fp, -3.479687e-004_fp/
>   data (coe_v(2,3,k),k=1,5)/ 6.387097e-002_fp,  1.252447e-003_fp,  &
>        1.998846e-004_fp, 2.680219e-003_fp, -3.740141e-004_fp/
>   data (coe_v(2,4,k),k=1,5)/ 4.150689e-002_fp,  1.420274e-003_fp,  &
>        1.223339e-004_fp, -1.948946e-003_fp,  4.248289e-003_fp/
385c385
<   coe_h(2,1,1:4) = (/ &
---
>   data (coe_h(2,1,k),k=1,4)/ &
387,388c387,388
<        -3.319696e-004_fp/)
<   coe_h(2,2,1:4) = (/ &
---
>        -3.319696e-004_fp/
>   data (coe_h(2,2,k),k=1,4)/ &
390,391c390,391
<        -4.087036e-004_fp/)
<   coe_h(2,3,1:4) = (/ &
---
>        -4.087036e-004_fp/
>   data (coe_h(2,3,k),k=1,4)/ &
393c393
<        4.194914e-003_fp/)
---
>        4.194914e-003_fp/
395c395
< ! save  coe_v,coe_h
---
>   save  coe_v,coe_h
diff -w ./NESDIS_SSMI_SIceEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_SSMI_SIceEM_Module.f90
337d336
<   save  coe_v,coe_h
339,347c338,347
<   coe_v(1,1:5) = (/ -8.722723e-002_fp,  1.064573e-002_fp, &
<        -5.333843e-003_fp, -1.394910e-003_fp,  4.007640e-004_fp/)
<   coe_v(2,1:5) = (/-1.373924e-001_fp,  6.580569e-003_fp, &
<        -9.991220e-004_fp, -1.476022e-003_fp,  4.131816e-004_fp/)
<   coe_v(3,1:5) = (/ -2.329867e-001_fp,  6.419856e-003_fp, &
<        -5.260987e-003_fp, 3.342582e-003_fp,  4.139272e-004_fp/)
<   coe_v(4,1:5) = (/ -3.528638e-001_fp,  6.342649e-003_fp, &
<        -5.002575e-003_fp, -1.469298e-003_fp,  5.529711e-003_fp/)
<   coe_h(1,1:4) = (/ &
---
> 
>   data (coe_v(1,k),k=1,5)/ -8.722723e-002_fp,  1.064573e-002_fp, &
>        -5.333843e-003_fp, -1.394910e-003_fp,  4.007640e-004_fp/
>   data (coe_v(2,k),k=1,5)/-1.373924e-001_fp,  6.580569e-003_fp, &
>        -9.991220e-004_fp, -1.476022e-003_fp,  4.131816e-004_fp/
>   data (coe_v(3,k),k=1,5)/ -2.329867e-001_fp,  6.419856e-003_fp, &
>        -5.260987e-003_fp, 3.342582e-003_fp,  4.139272e-004_fp/
>   data (coe_v(4,k),k=1,5)/ -3.528638e-001_fp,  6.342649e-003_fp, &
>        -5.002575e-003_fp, -1.469298e-003_fp,  5.529711e-003_fp/
>   data (coe_h(1,k),k=1,4)/ &
349,350c349,350
<        5.706367e-004_fp/)
<   coe_h(2,1:4) = (/ &
---
>        5.706367e-004_fp/
>   data (coe_h(2,k),k=1,4)/ &
352,353c352,353
<        5.924890e-004_fp/)
<   coe_h(3,1:4) = (/ &
---
>        5.924890e-004_fp/
>   data (coe_h(3,k),k=1,4)/ &
355c355
<        5.750499e-003_fp/)
---
>        5.750499e-003_fp/
357c357
< ! save  coe_v,coe_h
---
>   save  coe_v,coe_h
diff -w ./NESDIS_SSMI_SnowEM_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_SSMI_SnowEM_Module.f90
340c340
<   save  coe_v,coe_h
---
> 
343,350c343,350
<   coe_v(1,1:5) = (/  1.109066e-001_fp,  5.449409e-003_fp,  &
<        1.835799e-004_fp, -1.765248e-003_fp, -2.996101e-004_fp/)
<   coe_v(2,1:5) = (/ 9.356505e-002_fp,  1.320617e-003_fp,  &
<        4.449195e-003_fp, -1.786960e-003_fp, -3.479687e-004_fp/)
<   coe_v(3,1:5) = (/ 6.387097e-002_fp,  1.252447e-003_fp,  &
<        1.998846e-004_fp, 2.680219e-003_fp, -3.740141e-004_fp/)
<   coe_v(4,1:5) = (/ 4.150689e-002_fp,  1.420274e-003_fp,  &
<        1.223339e-004_fp, -1.948946e-003_fp,  4.248289e-003_fp/)
---
>   data (coe_v(1,k),k=1,5)/  1.109066e-001_fp,  5.449409e-003_fp,  &
>        1.835799e-004_fp, -1.765248e-003_fp, -2.996101e-004_fp/
>   data (coe_v(2,k),k=1,5)/ 9.356505e-002_fp,  1.320617e-003_fp,  &
>        4.449195e-003_fp, -1.786960e-003_fp, -3.479687e-004_fp/
>   data (coe_v(3,k),k=1,5)/ 6.387097e-002_fp,  1.252447e-003_fp,  &
>        1.998846e-004_fp, 2.680219e-003_fp, -3.740141e-004_fp/
>   data (coe_v(4,k),k=1,5)/ 4.150689e-002_fp,  1.420274e-003_fp,  &
>        1.223339e-004_fp, -1.948946e-003_fp,  4.248289e-003_fp/
352c352
<   coe_h(1,1:4) = (/ &
---
>   data (coe_h(1,k),k=1,4)/ &
354,355c354,355
<        -3.319696e-004_fp/)
<   coe_h(2,1:4) = (/ &
---
>        -3.319696e-004_fp/
>   data (coe_h(2,k),k=1,4)/ &
357,358c357,358
<        -4.087036e-004_fp/)
<   coe_h(3,1:4) = (/ &
---
>        -4.087036e-004_fp/
>   data (coe_h(3,k),k=1,4)/ &
360c360
<        4.194914e-003_fp/)
---
>        4.194914e-003_fp/
362c362
< ! save  coe_v,coe_h
---
>   save  coe_v,coe_h
diff -w ./NESDIS_SnowEM_ATMS_Parameters.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_SnowEM_ATMS_Parameters.f90
diff -w ./NESDIS_SnowEM_Parameters.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NESDIS_SnowEM_Parameters.f90
diff -w ./NLTECoeff_Binary_IO.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NLTECoeff_Binary_IO.f90
diff -w ./NLTECoeff_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NLTECoeff_Define.f90
diff -w ./NLTE_Parameters.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NLTE_Parameters.f90
diff -w ./NLTE_Predictor_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NLTE_Predictor_Define.f90
diff -w ./NLTE_Predictor_IO.f90 ~/CRTM/clean/REL-2.2.3/libsrc/NLTE_Predictor_IO.f90
diff -w ./ODAS_AtmAbsorption.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODAS_AtmAbsorption.f90
diff -w ./ODAS_Binary_IO.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODAS_Binary_IO.f90
diff -w ./ODAS_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODAS_Define.f90
diff -w ./ODAS_Predictor.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODAS_Predictor.f90
diff -w ./ODAS_Predictor_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODAS_Predictor_Define.f90
diff -w ./ODAS_TauCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODAS_TauCoeff.f90
diff -w ./ODPS_AtmAbsorption.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODPS_AtmAbsorption.f90
diff -w ./ODPS_Binary_IO.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODPS_Binary_IO.f90
diff -w ./ODPS_CoordinateMapping.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODPS_CoordinateMapping.f90
diff -w ./ODPS_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODPS_Define.f90
diff -w ./ODPS_Predictor.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODPS_Predictor.f90
diff -w ./ODPS_Predictor_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODPS_Predictor_Define.f90
diff -w ./ODPS_TauCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODPS_TauCoeff.f90
diff -w ./ODSSU_AtmAbsorption.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODSSU_AtmAbsorption.f90
diff -w ./ODSSU_Binary_IO.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODSSU_Binary_IO.f90
diff -w ./ODSSU_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODSSU_Define.f90
diff -w ./ODSSU_TauCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODSSU_TauCoeff.f90
diff -w ./ODZeeman_AtmAbsorption.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODZeeman_AtmAbsorption.f90
diff -w ./ODZeeman_Predictor.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODZeeman_Predictor.f90
diff -w ./ODZeeman_TauCoeff.f90 ~/CRTM/clean/REL-2.2.3/libsrc/ODZeeman_TauCoeff.f90
diff -w ./PAFV_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/PAFV_Define.f90
diff -w ./Profile_Utility_Parameters.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Profile_Utility_Parameters.f90
diff -w ./RTV_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/RTV_Define.f90
diff -w ./Reflection_Correction_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Reflection_Correction_Module.f90
diff -w ./SEcategory_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/SEcategory_Define.f90
diff -w ./SOI_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/SOI_Module.f90
diff -w ./SSU_Input_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/SSU_Input_Define.f90
diff -w ./Search_Utility.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Search_Utility.f90
diff -w ./SensorInfo_Parameters.f90 ~/CRTM/clean/REL-2.2.3/libsrc/SensorInfo_Parameters.f90
diff -w ./Slope_Variance.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Slope_Variance.f90
diff -w ./Small_Scale_Correction_Module.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Small_Scale_Correction_Module.f90
diff -w ./Sort_Utility.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Sort_Utility.f90
diff -w ./SpcCoeff_Binary_IO.f90 ~/CRTM/clean/REL-2.2.3/libsrc/SpcCoeff_Binary_IO.f90
diff -w ./SpcCoeff_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/SpcCoeff_Define.f90
diff -w ./Spectral_Units_Conversion.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Spectral_Units_Conversion.f90
diff -w ./String_Utility.f90 ~/CRTM/clean/REL-2.2.3/libsrc/String_Utility.f90
diff -w ./Subset_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Subset_Define.f90
diff -w ./TauCoeff_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/TauCoeff_Define.f90
diff -w ./Timing_Utility.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Timing_Utility.f90
26d25
<   PUBLIC :: Timing_ToString
373d371
< 
diff -w ./Type_Kinds.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Type_Kinds.f90
diff -w ./UnitTest_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/UnitTest_Define.f90
19,20c19
<   USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.), &
<                                    Compares_Within_Tolerance
---
>   USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
24d22
< 
33,34d30
<   ! **** These procedure interfaces are kept for legacy
<   ! **** purposes, but deprecated for new code
46c42
<   PUBLIC :: UnitTest_IsWithinSigFig
---
>   PUBLIC :: UnitTest_DefineVersion
52,90c48
< 
<   ! **** Pre-type-bound procedure interface definitions
<   ! **** Kept for legacy purposes, but deprecated for new code
<   INTERFACE UnitTest_Init
<     MODULE PROCEDURE Init
<   END INTERFACE UnitTest_Init
< 
<   INTERFACE UnitTest_Setup
<     MODULE PROCEDURE Setup
<   END INTERFACE UnitTest_Setup
< 
<   INTERFACE UnitTest_Report
<     MODULE PROCEDURE Report
<   END INTERFACE UnitTest_Report
< 
<   INTERFACE UnitTest_Summary
<     MODULE PROCEDURE Summary
<   END INTERFACE UnitTest_Summary
< 
<   INTERFACE UnitTest_n_Passed
<     MODULE PROCEDURE n_Passed
<   END INTERFACE UnitTest_n_Passed
< 
<   INTERFACE UnitTest_n_Failed
<     MODULE PROCEDURE n_Failed
<   END INTERFACE UnitTest_n_Failed
< 
<   INTERFACE UnitTest_Passed
<     MODULE PROCEDURE Passed
<   END INTERFACE UnitTest_Passed
< 
<   INTERFACE UnitTest_Failed
<     MODULE PROCEDURE Failed
<   END INTERFACE UnitTest_Failed
< 
<   INTERFACE UnitTest_Assert
<     MODULE PROCEDURE Assert
<   END INTERFACE UnitTest_Assert
< 
---
>   ! PUBLIC procedures
93,95c51,53
<     MODULE PROCEDURE intbyte_assert_equal_s
<     MODULE PROCEDURE intbyte_assert_equal_r1
<     MODULE PROCEDURE intbyte_assert_equal_r2
---
>     MODULE PROCEDURE intbyte_isequal_scalar
>     MODULE PROCEDURE intbyte_isequal_rank1
>     MODULE PROCEDURE intbyte_isequal_rank2
97,99c55,57
<     MODULE PROCEDURE intshort_assert_equal_s
<     MODULE PROCEDURE intshort_assert_equal_r1
<     MODULE PROCEDURE intshort_assert_equal_r2
---
>     MODULE PROCEDURE intshort_isequal_scalar
>     MODULE PROCEDURE intshort_isequal_rank1
>     MODULE PROCEDURE intshort_isequal_rank2
101,103c59,61
<     MODULE PROCEDURE intlong_assert_equal_s
<     MODULE PROCEDURE intlong_assert_equal_r1
<     MODULE PROCEDURE intlong_assert_equal_r2
---
>     MODULE PROCEDURE intlong_isequal_scalar
>     MODULE PROCEDURE intlong_isequal_rank1
>     MODULE PROCEDURE intlong_isequal_rank2
105,107c63,65
<     MODULE PROCEDURE realsp_assert_equal_s
<     MODULE PROCEDURE realsp_assert_equal_r1
<     MODULE PROCEDURE realsp_assert_equal_r2
---
>     MODULE PROCEDURE realsp_isequal_scalar
>     MODULE PROCEDURE realsp_isequal_rank1
>     MODULE PROCEDURE realsp_isequal_rank2
109,111c67,69
<     MODULE PROCEDURE realdp_assert_equal_s
<     MODULE PROCEDURE realdp_assert_equal_r1
<     MODULE PROCEDURE realdp_assert_equal_r2
---
>     MODULE PROCEDURE realdp_isequal_scalar
>     MODULE PROCEDURE realdp_isequal_rank1
>     MODULE PROCEDURE realdp_isequal_rank2
113,115c71,73
<     MODULE PROCEDURE complexsp_assert_equal_s
<     MODULE PROCEDURE complexsp_assert_equal_r1
<     MODULE PROCEDURE complexsp_assert_equal_r2
---
>     MODULE PROCEDURE complexsp_isequal_scalar
>     MODULE PROCEDURE complexsp_isequal_rank1
>     MODULE PROCEDURE complexsp_isequal_rank2
117,119c75,77
<     MODULE PROCEDURE complexdp_assert_equal_s
<     MODULE PROCEDURE complexdp_assert_equal_r1
<     MODULE PROCEDURE complexdp_assert_equal_r2
---
>     MODULE PROCEDURE complexdp_isequal_scalar
>     MODULE PROCEDURE complexdp_isequal_rank1
>     MODULE PROCEDURE complexdp_isequal_rank2
121,123c79,81
<     MODULE PROCEDURE char_assert_equal_s
<     MODULE PROCEDURE char_assert_equal_r1
<     MODULE PROCEDURE char_assert_equal_r2
---
>     MODULE PROCEDURE char_isequal_scalar
>     MODULE PROCEDURE char_isequal_rank1
>     MODULE PROCEDURE char_isequal_rank2
128,130c86,88
<     MODULE PROCEDURE realsp_assert_equalwithin_s
<     MODULE PROCEDURE realsp_assert_equalwithin_r1
<     MODULE PROCEDURE realsp_assert_equalwithin_r2
---
>     MODULE PROCEDURE realsp_isequalwithin_scalar
>     MODULE PROCEDURE realsp_isequalwithin_rank1
>     MODULE PROCEDURE realsp_isequalwithin_rank2
132,134c90,92
<     MODULE PROCEDURE realdp_assert_equalwithin_s
<     MODULE PROCEDURE realdp_assert_equalwithin_r1
<     MODULE PROCEDURE realdp_assert_equalwithin_r2
---
>     MODULE PROCEDURE realdp_isequalwithin_scalar
>     MODULE PROCEDURE realdp_isequalwithin_rank1
>     MODULE PROCEDURE realdp_isequalwithin_rank2
136,138c94,96
<     MODULE PROCEDURE complexsp_assert_equalwithin_s
<     MODULE PROCEDURE complexsp_assert_equalwithin_r1
<     MODULE PROCEDURE complexsp_assert_equalwithin_r2
---
>     MODULE PROCEDURE complexsp_isequalwithin_scalar
>     MODULE PROCEDURE complexsp_isequalwithin_rank1
>     MODULE PROCEDURE complexsp_isequalwithin_rank2
140,142c98,100
<     MODULE PROCEDURE complexdp_assert_equalwithin_s
<     MODULE PROCEDURE complexdp_assert_equalwithin_r1
<     MODULE PROCEDURE complexdp_assert_equalwithin_r2
---
>     MODULE PROCEDURE complexdp_isequalwithin_scalar
>     MODULE PROCEDURE complexdp_isequalwithin_rank1
>     MODULE PROCEDURE complexdp_isequalwithin_rank2
145,162c103,108
<   INTERFACE UnitTest_IsWithinSigFig
<     ! REAL(Single) procedures
<     MODULE PROCEDURE realsp_assert_withinsigfig_s
<     MODULE PROCEDURE realsp_assert_withinsigfig_r1
<     MODULE PROCEDURE realsp_assert_withinsigfig_r2
<     ! REAL(Double) procedures
<     MODULE PROCEDURE realdp_assert_withinsigfig_s
<     MODULE PROCEDURE realdp_assert_withinsigfig_r1
<     MODULE PROCEDURE realdp_assert_withinsigfig_r2
<     ! COMPLEX(Single) procedures
<     MODULE PROCEDURE complexsp_assert_withinsigfig_s
<     MODULE PROCEDURE complexsp_assert_withinsigfig_r1
<     MODULE PROCEDURE complexsp_assert_withinsigfig_r2
<     ! COMPLEX(Double) procedures
<     MODULE PROCEDURE complexdp_assert_withinsigfig_s
<     MODULE PROCEDURE complexdp_assert_withinsigfig_r1
<     MODULE PROCEDURE complexdp_assert_withinsigfig_r2
<   END INTERFACE UnitTest_IsWithinSigFig
---
> 
>   ! PRIVATE procedures
>   INTERFACE Get_Multiplier
>     MODULE PROCEDURE realsp_get_multiplier
>     MODULE PROCEDURE realdp_get_multiplier
>   END INTERFACE Get_Multiplier
172a119,120
>   CHARACTER(*), PARAMETER :: RFMT = 'es25.18'
>   CHARACTER(*), PARAMETER :: ZFMT = '"(",'//RFMT//',",",'//RFMT//',")"'
173a122
> 
177a127
> 
194d143
< 
220,374d168
<   CONTAINS
<     PRIVATE
<     ! Public methods
<     PROCEDURE, PUBLIC, PASS(self) :: Init
<     PROCEDURE, PUBLIC, PASS(self) :: Setup
<     PROCEDURE, PUBLIC, PASS(self) :: Report
<     PROCEDURE, PUBLIC, PASS(self) :: Summary
<     PROCEDURE, PUBLIC, PASS(self) :: n_Passed
<     PROCEDURE, PUBLIC, PASS(self) :: n_Failed
<     PROCEDURE, PUBLIC, PASS(self) :: Passed
<     PROCEDURE, PUBLIC, PASS(self) :: Failed
<     PROCEDURE, PUBLIC, PASS(self) :: Assert
<     PROCEDURE, PUBLIC, PASS(self) :: Refute
<     GENERIC, PUBLIC :: Assert_Equal => &
<       intbyte_assert_equal_s, intbyte_assert_equal_r1, intbyte_assert_equal_r2, &
<       intshort_assert_equal_s, intshort_assert_equal_r1, intshort_assert_equal_r2, &
<       intlong_assert_equal_s, intlong_assert_equal_r1, intlong_assert_equal_r2, &
<       realsp_assert_equal_s, realsp_assert_equal_r1, realsp_assert_equal_r2, &
<       realdp_assert_equal_s, realdp_assert_equal_r1, realdp_assert_equal_r2, &
<       complexsp_assert_equal_s, complexsp_assert_equal_r1, complexsp_assert_equal_r2, &
<       complexdp_assert_equal_s, complexdp_assert_equal_r1, complexdp_assert_equal_r2, &
<       char_assert_equal_s, char_assert_equal_r1, char_assert_equal_r2
<     PROCEDURE, PASS(self) :: intbyte_assert_equal_s
<     PROCEDURE, PASS(self) :: intbyte_assert_equal_r1
<     PROCEDURE, PASS(self) :: intbyte_assert_equal_r2
<     PROCEDURE, PASS(self) :: intshort_assert_equal_s
<     PROCEDURE, PASS(self) :: intshort_assert_equal_r1
<     PROCEDURE, PASS(self) :: intshort_assert_equal_r2
<     PROCEDURE, PASS(self) :: intlong_assert_equal_s
<     PROCEDURE, PASS(self) :: intlong_assert_equal_r1
<     PROCEDURE, PASS(self) :: intlong_assert_equal_r2
<     PROCEDURE, PASS(self) :: realsp_assert_equal_s
<     PROCEDURE, PASS(self) :: realsp_assert_equal_r1
<     PROCEDURE, PASS(self) :: realsp_assert_equal_r2
<     PROCEDURE, PASS(self) :: realdp_assert_equal_s
<     PROCEDURE, PASS(self) :: realdp_assert_equal_r1
<     PROCEDURE, PASS(self) :: realdp_assert_equal_r2
<     PROCEDURE, PASS(self) :: complexsp_assert_equal_s
<     PROCEDURE, PASS(self) :: complexsp_assert_equal_r1
<     PROCEDURE, PASS(self) :: complexsp_assert_equal_r2
<     PROCEDURE, PASS(self) :: complexdp_assert_equal_s
<     PROCEDURE, PASS(self) :: complexdp_assert_equal_r1
<     PROCEDURE, PASS(self) :: complexdp_assert_equal_r2
<     PROCEDURE, PASS(self) :: char_assert_equal_s
<     PROCEDURE, PASS(self) :: char_assert_equal_r1
<     PROCEDURE, PASS(self) :: char_assert_equal_r2
<     GENERIC, PUBLIC :: Refute_Equal => &
<       intbyte_refute_equal_s, intbyte_refute_equal_r1, intbyte_refute_equal_r2, &
<       intshort_refute_equal_s, intshort_refute_equal_r1, intshort_refute_equal_r2, &
<       intlong_refute_equal_s, intlong_refute_equal_r1, intlong_refute_equal_r2, &
<       realsp_refute_equal_s, realsp_refute_equal_r1, realsp_refute_equal_r2, &
<       realdp_refute_equal_s, realdp_refute_equal_r1, realdp_refute_equal_r2, &
<       complexsp_refute_equal_s, complexsp_refute_equal_r1, complexsp_refute_equal_r2, &
<       complexdp_refute_equal_s, complexdp_refute_equal_r1, complexdp_refute_equal_r2, &
<       char_refute_equal_s, char_refute_equal_r1, char_refute_equal_r2
<     PROCEDURE, PASS(self) :: intbyte_refute_equal_s
<     PROCEDURE, PASS(self) :: intbyte_refute_equal_r1
<     PROCEDURE, PASS(self) :: intbyte_refute_equal_r2
<     PROCEDURE, PASS(self) :: intshort_refute_equal_s
<     PROCEDURE, PASS(self) :: intshort_refute_equal_r1
<     PROCEDURE, PASS(self) :: intshort_refute_equal_r2
<     PROCEDURE, PASS(self) :: intlong_refute_equal_s
<     PROCEDURE, PASS(self) :: intlong_refute_equal_r1
<     PROCEDURE, PASS(self) :: intlong_refute_equal_r2
<     PROCEDURE, PASS(self) :: realsp_refute_equal_s
<     PROCEDURE, PASS(self) :: realsp_refute_equal_r1
<     PROCEDURE, PASS(self) :: realsp_refute_equal_r2
<     PROCEDURE, PASS(self) :: realdp_refute_equal_s
<     PROCEDURE, PASS(self) :: realdp_refute_equal_r1
<     PROCEDURE, PASS(self) :: realdp_refute_equal_r2
<     PROCEDURE, PASS(self) :: complexsp_refute_equal_s
<     PROCEDURE, PASS(self) :: complexsp_refute_equal_r1
<     PROCEDURE, PASS(self) :: complexsp_refute_equal_r2
<     PROCEDURE, PASS(self) :: complexdp_refute_equal_s
<     PROCEDURE, PASS(self) :: complexdp_refute_equal_r1
<     PROCEDURE, PASS(self) :: complexdp_refute_equal_r2
<     PROCEDURE, PASS(self) :: char_refute_equal_s
<     PROCEDURE, PASS(self) :: char_refute_equal_r1
<     PROCEDURE, PASS(self) :: char_refute_equal_r2
<     GENERIC, PUBLIC :: Assert_EqualWithin => &
<       realsp_assert_equalwithin_s, realsp_assert_equalwithin_r1, realsp_assert_equalwithin_r2, &
<       realdp_assert_equalwithin_s, realdp_assert_equalwithin_r1, realdp_assert_equalwithin_r2, &
<       complexsp_assert_equalwithin_s, complexsp_assert_equalwithin_r1, complexsp_assert_equalwithin_r2, &
<       complexdp_assert_equalwithin_s, complexdp_assert_equalwithin_r1, complexdp_assert_equalwithin_r2
<     PROCEDURE, PASS(self) :: realsp_assert_equalwithin_s
<     PROCEDURE, PASS(self) :: realsp_assert_equalwithin_r1
<     PROCEDURE, PASS(self) :: realsp_assert_equalwithin_r2
<     PROCEDURE, PASS(self) :: realdp_assert_equalwithin_s
<     PROCEDURE, PASS(self) :: realdp_assert_equalwithin_r1
<     PROCEDURE, PASS(self) :: realdp_assert_equalwithin_r2
<     PROCEDURE, PASS(self) :: complexsp_assert_equalwithin_s
<     PROCEDURE, PASS(self) :: complexsp_assert_equalwithin_r1
<     PROCEDURE, PASS(self) :: complexsp_assert_equalwithin_r2
<     PROCEDURE, PASS(self) :: complexdp_assert_equalwithin_s
<     PROCEDURE, PASS(self) :: complexdp_assert_equalwithin_r1
<     PROCEDURE, PASS(self) :: complexdp_assert_equalwithin_r2
<     GENERIC, PUBLIC :: Refute_EqualWithin => &
<       realsp_refute_equalwithin_s, realsp_refute_equalwithin_r1, realsp_refute_equalwithin_r2, &
<       realdp_refute_equalwithin_s, realdp_refute_equalwithin_r1, realdp_refute_equalwithin_r2, &
<       complexsp_refute_equalwithin_s, complexsp_refute_equalwithin_r1, complexsp_refute_equalwithin_r2, &
<       complexdp_refute_equalwithin_s, complexdp_refute_equalwithin_r1, complexdp_refute_equalwithin_r2
<     PROCEDURE, PASS(self) :: realsp_refute_equalwithin_s
<     PROCEDURE, PASS(self) :: realsp_refute_equalwithin_r1
<     PROCEDURE, PASS(self) :: realsp_refute_equalwithin_r2
<     PROCEDURE, PASS(self) :: realdp_refute_equalwithin_s
<     PROCEDURE, PASS(self) :: realdp_refute_equalwithin_r1
<     PROCEDURE, PASS(self) :: realdp_refute_equalwithin_r2
<     PROCEDURE, PASS(self) :: complexsp_refute_equalwithin_s
<     PROCEDURE, PASS(self) :: complexsp_refute_equalwithin_r1
<     PROCEDURE, PASS(self) :: complexsp_refute_equalwithin_r2
<     PROCEDURE, PASS(self) :: complexdp_refute_equalwithin_s
<     PROCEDURE, PASS(self) :: complexdp_refute_equalwithin_r1
<     PROCEDURE, PASS(self) :: complexdp_refute_equalwithin_r2
<     GENERIC, PUBLIC :: Assert_WithinSigfig => &
<       realsp_assert_withinsigfig_s, realsp_assert_withinsigfig_r1, realsp_assert_withinsigfig_r2, &
<       realdp_assert_withinsigfig_s, realdp_assert_withinsigfig_r1, realdp_assert_withinsigfig_r2, &
<       complexsp_assert_withinsigfig_s, complexsp_assert_withinsigfig_r1, complexsp_assert_withinsigfig_r2, &
<       complexdp_assert_withinsigfig_s, complexdp_assert_withinsigfig_r1, complexdp_assert_withinsigfig_r2
<     PROCEDURE, PASS(self) :: realsp_assert_withinsigfig_s
<     PROCEDURE, PASS(self) :: realsp_assert_withinsigfig_r1
<     PROCEDURE, PASS(self) :: realsp_assert_withinsigfig_r2
<     PROCEDURE, PASS(self) :: realdp_assert_withinsigfig_s
<     PROCEDURE, PASS(self) :: realdp_assert_withinsigfig_r1
<     PROCEDURE, PASS(self) :: realdp_assert_withinsigfig_r2
<     PROCEDURE, PASS(self) :: complexsp_assert_withinsigfig_s
<     PROCEDURE, PASS(self) :: complexsp_assert_withinsigfig_r1
<     PROCEDURE, PASS(self) :: complexsp_assert_withinsigfig_r2
<     PROCEDURE, PASS(self) :: complexdp_assert_withinsigfig_s
<     PROCEDURE, PASS(self) :: complexdp_assert_withinsigfig_r1
<     PROCEDURE, PASS(self) :: complexdp_assert_withinsigfig_r2
<     GENERIC, PUBLIC :: Refute_WithinSigfig => &
<       realsp_refute_withinsigfig_s, realsp_refute_withinsigfig_r1, realsp_refute_withinsigfig_r2, &
<       realdp_refute_withinsigfig_s, realdp_refute_withinsigfig_r1, realdp_refute_withinsigfig_r2, &
<       complexsp_refute_withinsigfig_s, complexsp_refute_withinsigfig_r1, complexsp_refute_withinsigfig_r2, &
<       complexdp_refute_withinsigfig_s, complexdp_refute_withinsigfig_r1, complexdp_refute_withinsigfig_r2
<     PROCEDURE, PASS(self) :: realsp_refute_withinsigfig_s
<     PROCEDURE, PASS(self) :: realsp_refute_withinsigfig_r1
<     PROCEDURE, PASS(self) :: realsp_refute_withinsigfig_r2
<     PROCEDURE, PASS(self) :: realdp_refute_withinsigfig_s
<     PROCEDURE, PASS(self) :: realdp_refute_withinsigfig_r1
<     PROCEDURE, PASS(self) :: realdp_refute_withinsigfig_r2
<     PROCEDURE, PASS(self) :: complexsp_refute_withinsigfig_s
<     PROCEDURE, PASS(self) :: complexsp_refute_withinsigfig_r1
<     PROCEDURE, PASS(self) :: complexsp_refute_withinsigfig_r2
<     PROCEDURE, PASS(self) :: complexdp_refute_withinsigfig_s
<     PROCEDURE, PASS(self) :: complexdp_refute_withinsigfig_r1
<     PROCEDURE, PASS(self) :: complexdp_refute_withinsigfig_r2
<     ! Private methods
<     PROCEDURE, PASS(self) :: Set_Property
<     PROCEDURE, PASS(self) :: Get_Property
<     PROCEDURE, PASS(self) :: Test_Passed
<     PROCEDURE, PASS(self) :: Test_Failed
<     PROCEDURE, PASS(self) :: Test_Increment
<     PROCEDURE, PASS(self) :: Display_Message
<     PROCEDURE, PASS(self) :: Test_Info_String
378d171
< 
390c183
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
394c187
< !   UnitTest::Init
---
> !       UnitTest_Init
397c190
< !   UnitTest initialisation method.
---
> !       UnitTest initialisation subroutine.
399c192
< !   This method should be called ONCE, BEFORE ANY tests are performed.
---
> !       This subroutine should be called ONCE, BEFORE ANY tests are performed.
402c195
< !   CALL utest%Init( Verbose=Verbose )
---
> !       CALL UnitTest_Init( UnitTest, Verbose=Verbose )
405c198
< !   utest:    UnitTest object.
---
> !       UnitTest:      UnitTest object.
407c200
< !             CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
422c215
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
424c217
<   SUBROUTINE Init( self, Verbose )
---
>   SUBROUTINE UnitTest_Init( UnitTest, Verbose )
426c219
<     CLASS(UnitTest_type), INTENT(OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(OUT) :: UnitTest
429c222,228
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Init'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_Init'
>     ! Variables
>     LOGICAL :: local_Verbose
> 
>     ! Check optional arguments
>     local_Verbose = DEFAULT_VERBOSE
>     IF ( PRESENT(Verbose) ) local_Verbose = Verbose
433,434c232,233
<       self, &
<       Verbose           = Verbose , &
---
>       UnitTest, &
>       Verbose = local_Verbose, &
442a242,243
>     CALL Display_Message( UnitTest )
>   END SUBROUTINE UnitTest_Init
444,446d244
<     CALL Display_Message( self )
< 
<   END SUBROUTINE Init
448,449c246
< 
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
453c250
< !   UnitTest::Setup
---
> !       UnitTest_Setup
456c253
< !   Individual test setup method.
---
> !       UnitTest individual test setup subroutine.
458c255
< !   This method should be called BEFORE each set of tests performed.
---
> !       This subroutine should be called BEFORE each set of tests performed.
461c258,259
< !   CALL utest_obj&Setup( Title            , &
---
> !       CALL UnitTest_Setup( UnitTest         , &
> !                            Title            , &
466c264
< !   utest_obj:  UnitTest object.
---
> !       UnitTest:      UnitTest object.
468c266
< !               CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
498c296
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
500c298
<   SUBROUTINE Setup( self, Title, Caller, Verbose )
---
>   SUBROUTINE UnitTest_Setup( UnitTest, Title, Caller, Verbose )
502c300
<     CLASS(UnitTest_type)  , INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type)   , INTENT(IN OUT) :: UnitTest
507c305
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Setup'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_Setup'
509,510c307,309
<     CHARACTER(SL) :: the_caller
<     CHARACTER(SL) :: message
---
>     CHARACTER(SL) :: local_Caller
>     LOGICAL       :: local_Verbose
>     CHARACTER(SL) :: Message
512,514c311,315
<     ! Check optional arguments
<     the_caller = ''
<     IF ( PRESENT(Caller) ) the_caller = '; CALLER: '//TRIM(ADJUSTL(Caller))
---
>     ! Check arguments
>     local_Caller = ''
>     IF ( PRESENT(Caller) ) local_Caller = '; CALLER: '//TRIM(ADJUSTL(Caller))
>     local_Verbose = DEFAULT_VERBOSE
>     IF ( PRESENT(Verbose) ) local_Verbose = Verbose
516,517c317,318
<     ! Create setup message
<     message = TRIM(ADJUSTL(Title))//TRIM(the_caller)
---
>     ! Create init message
>     Message = TRIM(Title)//TRIM(local_Caller)
521,524c322,325
<       self, &
<       Title          = Title         , &
<       Caller         = Caller        , &
<       Verbose        = Verbose       , &
---
>       UnitTest, &
>       Title   = ADJUSTL(Title), &
>       Caller  = local_Caller  , &
>       Verbose = local_Verbose , &
527c328
<       Message        = message       , &
---
>       Message = Message, &
530a332
>     CALL Display_Message( UnitTest )
532,534c334
<     CALL Display_Message( self )
< 
<   END SUBROUTINE Setup
---
>   END SUBROUTINE UnitTest_Setup
537c337
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
541c341
< !   UnitTest::Report
---
> !       UnitTest_Report
544c344
< !   Individual test report method.
---
> !       UnitTest individual test report subroutine
546c346
< !   This method should be called AFTER each set of tests performed.
---
> !       This subroutine should be called AFTER each set of tests performed.
549c349
< !   CALL utest_obj%Report()
---
> !       CALL UnitTest_Report( UnitTest )
552c352
< !   utest_obj:  UnitTest object.
---
> !       UnitTest:      UnitTest object.
554c354
< !               CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
559c359
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
561c361
<   SUBROUTINE Report( self )
---
>   SUBROUTINE UnitTest_Report( UnitTest )
563c363
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
565c365
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Report'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_Report'
567,571c367,371
<     INTEGER :: n_tests
<     INTEGER :: n_passed_tests
<     INTEGER :: n_failed_tests
<     CHARACTER(SL) :: message
<     CHARACTER(SL) :: attention
---
>     INTEGER :: n_Tests
>     INTEGER :: n_Passed_Tests
>     INTEGER :: n_Failed_Tests
>     CHARACTER(SL) :: Message
>     CHARACTER(SL) :: Attention
573d372
< 
576,579c375,378
<       self, &
<       n_Tests        = n_tests       , &
<       n_Passed_Tests = n_passed_tests, &
<       n_Failed_Tests = n_failed_tests  )
---
>       UnitTest, &
>       n_Tests        = n_Tests       , &
>       n_Passed_Tests = n_Passed_Tests, &
>       n_Failed_Tests = n_Failed_Tests  )
583,584c382,383
<     attention = ''
<     IF ( n_failed_tests /= 0 ) THEN
---
>     Attention = ''
>     IF ( n_Failed_Tests /= 0 ) THEN
586c385
<       attention = '  <----<<<  **WARNING**'
---
>       Attention = '  <----<<<  **WARNING**'
589,590c388,389
<     ! Generate report message
<     WRITE( message, &
---
>     ! Output results
>     WRITE( Message, &
594c393
<       n_passed_tests, n_tests, &
---
>       n_Passed_Tests, n_Tests, &
596,599c395,396
<       n_failed_tests, n_tests, &
<       TRIM(attention), NO_COLOUR
< 
<     ! Load object with report message
---
>       n_Failed_Tests, n_Tests, &
>       TRIM(Attention), NO_COLOUR
601c398
<       self, &
---
>       UnitTest, &
604a402
>     CALL Display_Message( UnitTest )
606,607c404
<     ! Report!
<     CALL Display_Message( self )
---
>   END SUBROUTINE UnitTest_Report
609d405
<   END SUBROUTINE Report
611,612c407
< 
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
616c411
< !   UnitTest::Summary
---
> !       UnitTest_Summary
619c414
< !   Test suite report summary method.
---
> !       UnitTest test suite report summary subroutine
621c416
< !   This method should be called ONCE, AFTER ALL tests are performed.
---
> !       This subroutine should be called ONCE, AFTER ALL tests are performed.
624c419
< !   CALL utest_obj%Summary()
---
> !       CALL UnitTest_Summary( UnitTest )
627c422
< !   utest_obj:     UnitTest object.
---
> !       UnitTest:      UnitTest object.
629c424
< !                  CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
634c429
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
636c431
<   SUBROUTINE Summary( self )
---
>   SUBROUTINE UnitTest_Summary( UnitTest )
638c433
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
640c435
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Summary'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_Summary'
642,646c437,441
<     INTEGER :: n_alltests
<     INTEGER :: n_passed_alltests
<     INTEGER :: n_failed_alltests
<     CHARACTER(SL) :: message
<     CHARACTER(SL) :: attention
---
>     INTEGER :: n_AllTests
>     INTEGER :: n_Passed_AllTests
>     INTEGER :: n_Failed_AllTests
>     CHARACTER(SL) :: Message
>     CHARACTER(SL) :: Attention
651,654c446,449
<       self, &
<       n_AllTests        = n_alltests       , &
<       n_Passed_AllTests = n_passed_alltests, &
<       n_Failed_AllTests = n_failed_alltests  )
---
>       UnitTest, &
>       n_AllTests        = n_AllTests       , &
>       n_Passed_AllTests = n_Passed_AllTests, &
>       n_Failed_AllTests = n_Failed_AllTests  )
658,659c453,454
<     attention = ''
<     IF ( n_failed_alltests /= 0 ) THEN
---
>     Attention = ''
>     IF ( n_Failed_AllTests /= 0 ) THEN
661c456
<       attention = '  <----<<<  **WARNING**'
---
>       Attention = '  <----<<<  **WARNING**'
664,665c459,460
<     ! Generate summary
<     WRITE( message, &
---
>     ! Output results
>     WRITE( Message, &
669c464
<       n_passed_alltests, n_alltests, &
---
>       n_Passed_AllTests, n_AllTests, &
671,674c466,467
<       n_failed_alltests, n_alltests, &
<       TRIM(attention), NO_COLOUR
< 
<     ! Load object with summary message
---
>       n_Failed_AllTests, n_AllTests, &
>       TRIM(Attention), NO_COLOUR
676c469
<       self, &
---
>       UnitTest, &
679,684c472,474
<       Message   = message )
< 
<     ! Summarise!
<     CALL Display_Message( self )
< 
<   END SUBROUTINE Summary
---
>       Message = Message )
>     CALL Display_Message( UnitTest )
>   END SUBROUTINE UnitTest_Summary
687c477
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
691c481
< !   UnitTest::n_Passed
---
> !       UnitTest_n_Passed
694c484
< !   Method to return the number of tests passed.
---
> !       Utility function to return the number of tests passed.
697c487
< !   n = utest_obj%n_Passed()
---
> !       n = UnitTest_n_Passed( UnitTest )
700c490
< !   utest_obj:  UnitTest object.
---
> !       UnitTest:      UnitTest object.
702c492
< !               CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
707c497
< !   n:          The number of exercised unit tests that have passed.
---
> !       n:             The number of unit tests that have currently passed.
713c503
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
715,718c505,509
<   PURE INTEGER FUNCTION n_Passed( self )
<     CLASS(UnitTest_type), INTENT(IN) :: self
<     CALL Get_Property( self, n_Passed_Tests = n_Passed )
<   END FUNCTION n_Passed
---
>   PURE FUNCTION UnitTest_n_Passed( UnitTest ) RESULT( n )
>     TYPE(UnitTest_type), INTENT(IN) :: UnitTest
>     INTEGER :: n
>     CALL Get_Property( UnitTest, n_Passed_Tests = n )
>   END FUNCTION UnitTest_n_Passed
721c512
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
725c516
< !   UnitTest::n_Failed
---
> !       UnitTest_n_Failed
728c519
< !   Method to return the number of tests failed.
---
> !       Utility function to return the number of tests failed.
731c522
< !   n = utest_obj%n_Failed()
---
> !       n = UnitTest_n_Failed( UnitTest )
734c525
< !   utest_obj:  UnitTest object.
---
> !       UnitTest:      UnitTest object.
736c527
< !               CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
741c532
< !   n:          The number of exercised unit tests that have failed.
---
> !       n:             The number of unit tests that have currently failed.
747c538
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
749,752c540,544
<   PURE INTEGER FUNCTION n_Failed( self )
<     CLASS(UnitTest_type), INTENT(IN) :: self
<     CALL Get_Property( self, n_Failed_Tests = n_Failed )
<   END FUNCTION n_Failed
---
>   PURE FUNCTION UnitTest_n_Failed( UnitTest ) RESULT( n )
>     TYPE(UnitTest_type), INTENT(IN) :: UnitTest
>     INTEGER :: n
>     CALL Get_Property( UnitTest, n_Failed_Tests = n )
>   END FUNCTION UnitTest_n_Failed
755c547
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
759c551
< !   UnitTest::Passed
---
> !       UnitTest_Passed
762c554
< !   Method to inform if the last test performed passed.
---
> !       Function to inform if the last test performed passed.
765c557
< !   result = utest_obj%Passed()
---
> !       result = UnitTest_Passed( UnitTest )
768c560
< !   utest_obj:  UnitTest object.
---
> !       UnitTest:      UnitTest object.
770c562
< !               CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
783c575
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
785,788c577,581
<   PURE LOGICAL FUNCTION Passed( self )
<     CLASS(UnitTest_type), INTENT(IN) :: self
<     CALL Get_Property( self, Test_Result = Passed )
<   END FUNCTION Passed
---
>   PURE FUNCTION UnitTest_Passed( UnitTest ) RESULT( Passed )
>     TYPE(UnitTest_type), INTENT(IN) :: UnitTest
>     LOGICAL :: Passed
>     CALL Get_Property( UnitTest, Test_Result = Passed )
>   END FUNCTION UnitTest_Passed
791c584
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
795c588
< !   UnitTest::Failed
---
> !       UnitTest_Failed
798c591
< !   Method to inform if the last test performed failed.
---
> !       Function to inform if the last test performed failed.
803c596
< !   result = utest_obj%Failed()
---
> !       result = UnitTest_Failed( UnitTest )
806c599
< !   utest_obj:  UnitTest object.
---
> !       UnitTest:      UnitTest object.
808c601
< !               CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
821c614
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
823,826c616,620
<   PURE LOGICAL FUNCTION Failed( self )
<     CLASS(UnitTest_type), INTENT(IN) :: self
<     Failed = .NOT. self%Passed()
<   END FUNCTION Failed
---
>   PURE FUNCTION UnitTest_Failed( UnitTest ) RESULT( Failed )
>     TYPE(UnitTest_type), INTENT(IN) :: UnitTest
>     LOGICAL :: Failed
>     Failed = .NOT. UnitTest_Passed( UnitTest )
>   END FUNCTION UnitTest_Failed
829c623
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
833c627
< !   UnitTest::Assert
---
> !       UnitTest_Assert
836c630
< !   Method to assert its logical argument as true.
---
> !       Subroutine to assert its test argument
839c633
< !   CALL utest_obj%Assert( boolean )
---
> !       CALL UnitTest_Assert(UnitTest, Test)
842c636
< !   utest_obj:  UnitTest object.
---
> !       UnitTest:      UnitTest object.
844c638
< !               CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
849,850c643
< !   boolean:    The logical expression to assert. The test passes if the
< !               expression is .TRUE.
---
> !       Test:          The logical expression to assert.
857c650
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
859c652
<   SUBROUTINE Assert(self, boolean)
---
>   SUBROUTINE UnitTest_Assert(UnitTest, Test)
861,862c654,655
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     LOGICAL,              INTENT(IN)     :: boolean
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     LOGICAL,             INTENT(IN)     :: Test
864c657
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_Assert'
866,867c659,660
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
870c663
<     message = ''
---
>     Message = ''
873,875c666,668
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. boolean)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
878,950c671,672
<     IF ( boolean ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
< 
<     ! Generate the assertion message
<     CALL Test_Info_String( self, message )
<     
<     ! Load the object with message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL    , &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message         )
<       
<     ! Output the assertion result
<     IF ( verbose ) CALL Display_Message( self )
< 
<   END SUBROUTINE Assert
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !   UnitTest::Refute
< !
< ! PURPOSE:
< !   Method to refute its logical argument as false
< !
< ! CALLING SEQUENCE:
< !   CALL utest_obj%Assert( boolean )
< !
< ! OBJECTS:
< !   utest_obj:  UnitTest object.
< !               UNITS:      N/A
< !               CLASS:      UnitTest_type
< !               DIMENSION:  Scalar
< !               ATTRIBUTES: INTENT(IN OUT)
< !
< ! INPUTS:
< !   boolean:    The logical expression to refute. The test passes if the
< !               expression is .FALSE.
< !               UNITS:      N/A
< !               TYPE:       LOGICAL
< !               DIMENSION:  Scalar
< !               ATTRIBUTES: INTENT(IN)
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   SUBROUTINE Refute(self, boolean)
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     LOGICAL,              INTENT(IN)     :: boolean
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute'
<     ! Variables
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
< 
<     ! Setup
<     message = ''
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. boolean  ! Always output test failure
< 
<     ! Refute the test
<     IF ( .NOT. boolean ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
952c674
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
955,958c677,678
<     ! Generate the refutation message
<     CALL Test_Info_String( self, message )
<     
<     ! Load the object with message
---
>     ! Output message
>     CALL Test_Info_String( UnitTest, Message )
960c680
<       self, &
---
>       UnitTest, &
963,966c683,684
<       Message   = message         )
<       
<     ! Output the refuation result
<     IF ( verbose ) CALL Display_Message( self )
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
968c686
<   END SUBROUTINE Refute
---
>   END SUBROUTINE UnitTest_Assert
971c689
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
975c693
< !   UnitTest::Assert_Equal
---
> !       UnitTest_IsEqual
978c696
< !   Method to assert that two arguments are equal.
---
> !       Subroutine to assert that two arguments are equal.
981c699
< !   CALL utest_obj%Assert_Equal( Expected, Actual )
---
> !       CALL UnitTest_IsEqual( UnitTest, Expected, Actual )
984c702
< !   utest_obj:     UnitTest object.
---
> !       UnitTest:      UnitTest object.
986c704
< !                  CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
1013c731
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
1015c733
<   SUBROUTINE intbyte_assert_equal_s( self, Expected, Actual )
---
>   SUBROUTINE intbyte_isequal_scalar( UnitTest, Expected, Actual )
1017c735
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1020c738
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Byte)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Byte)]'
1022,1024c740,743
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     LOGICAL :: Test
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
> 
1027c746
<     test = (Expected == Actual)
---
>     Test = (Expected == Actual)
1030,1032c749,753
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
1034,1035c755,756
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
1037c758
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1039,1043c760,762
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",i0,a,&
<                        &7x,"And got:  ",i0)') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
---
> 
>     ! Output message
>     WRITE( Message,'("Expected ",i0," and got ",i0)') Expected, Actual
1045c764
<       self, &
---
>       UnitTest, &
1048,1051c767,769
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE intbyte_assert_equal_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE intbyte_isequal_scalar
1054c772
<   SUBROUTINE intbyte_assert_equal_r1( self, Expected, Actual )
---
>   SUBROUTINE intbyte_isequal_rank1( UnitTest, Expected, Actual )
1056c774
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1059c777
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Byte)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Byte)]'
1062a781
> 
1066,1067c785,787
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
---
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
1070c790
<         self, &
---
>         UnitTest, &
1073,1074c793,794
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
1076a797
> 
1079c800
<       CALL self%Assert_Equal( Expected(i), Actual(i) )
---
>       CALL intbyte_isequal_scalar( UnitTest, Expected(i), Actual(i) )
1081c802
<   END SUBROUTINE intbyte_assert_equal_r1
---
>   END SUBROUTINE intbyte_isequal_rank1
1084c805
<   SUBROUTINE intbyte_assert_equal_r2( self, Expected, Actual )
---
>   SUBROUTINE intbyte_isequal_rank2( UnitTest, Expected, Actual )
1086c807
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1089c810
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Byte)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Byte)]'
1092a814
> 
1097c819
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1103c825
<         self, &
---
>         UnitTest, &
1107c829
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
1109a832
> 
1113c836
<         CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
---
>         CALL intbyte_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
1116c839
<   END SUBROUTINE intbyte_assert_equal_r2
---
>   END SUBROUTINE intbyte_isequal_rank2
1119c842
<   SUBROUTINE intshort_assert_equal_s( self, Expected, Actual )
---
>   SUBROUTINE intshort_isequal_scalar( UnitTest, Expected, Actual )
1121c844
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1124c847
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Short)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Short)]'
1126,1128c849,852
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     LOGICAL :: Test
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
> 
1131c855
<     test = (Expected == Actual)
---
>     Test = (Expected == Actual)
1134,1136c858,862
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
1138,1139c864,865
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
1141c867
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1143,1147c869,871
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",i0,a,&
<                        &7x,"And got:  ",i0)') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
---
> 
>     ! Output message
>     WRITE( Message,'("Expected ",i0," and got ",i0)') Expected, Actual
1149c873
<       self, &
---
>       UnitTest, &
1152,1155c876,878
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE intshort_assert_equal_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE intshort_isequal_scalar
1158c881
<   SUBROUTINE intshort_assert_equal_r1( self, Expected, Actual )
---
>   SUBROUTINE intshort_isequal_rank1( UnitTest, Expected, Actual )
1160c883
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1163c886
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Short)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Short)]'
1166a890
> 
1170,1171c894,896
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
---
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
1174c899
<         self, &
---
>         UnitTest, &
1177,1178c902,903
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
1180a906
> 
1183c909
<       CALL self%Assert_Equal( Expected(i), Actual(i) )
---
>       CALL intshort_isequal_scalar( UnitTest, Expected(i), Actual(i) )
1185c911
<   END SUBROUTINE intshort_assert_equal_r1
---
>   END SUBROUTINE intshort_isequal_rank1
1188c914
<   SUBROUTINE intshort_assert_equal_r2( self, Expected, Actual )
---
>   SUBROUTINE intshort_isequal_rank2( UnitTest, Expected, Actual )
1190c916
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1193c919
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Short)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Short)]'
1196a923
> 
1201c928
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1207c934
<         self, &
---
>         UnitTest, &
1211c938
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
1213a941
> 
1217c945
<         CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
---
>         CALL intshort_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
1220c948
<   END SUBROUTINE intshort_assert_equal_r2
---
>   END SUBROUTINE intshort_isequal_rank2
1223c951
<   SUBROUTINE intlong_assert_equal_s( self, Expected, Actual )
---
>   SUBROUTINE intlong_isequal_scalar( UnitTest, Expected, Actual )
1225c953
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1228c956
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Long)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Long)]'
1230,1232c958,961
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     LOGICAL :: Test
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
> 
1235c964
<     test = (Expected == Actual)
---
>     Test = (Expected == Actual)
1238,1240c967,971
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
1242,1243c973,974
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
1245c976
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1247,1251c978,980
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",i0,a,&
<                        &7x,"And got:  ",i0)') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
---
> 
>     ! Output message
>     WRITE( Message,'("Expected ",i0," and got ",i0)') Expected, Actual
1253c982
<       self, &
---
>       UnitTest, &
1256,1259c985,987
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE intlong_assert_equal_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE intlong_isequal_scalar
1262c990
<   SUBROUTINE intlong_assert_equal_r1( self, Expected, Actual )
---
>   SUBROUTINE intlong_isequal_rank1( UnitTest, Expected, Actual )
1264c992
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1267c995
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Long)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Long)]'
1270a999
> 
1274,1275c1003,1005
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
---
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
1278c1008
<         self, &
---
>         UnitTest, &
1281,1282c1011,1012
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
1284a1015
> 
1287c1018
<       CALL self%Assert_Equal( Expected(i), Actual(i) )
---
>       CALL intlong_isequal_scalar( UnitTest, Expected(i), Actual(i) )
1289c1020
<   END SUBROUTINE intlong_assert_equal_r1
---
>   END SUBROUTINE intlong_isequal_rank1
1292c1023
<   SUBROUTINE intlong_assert_equal_r2( self, Expected, Actual )
---
>   SUBROUTINE intlong_isequal_rank2( UnitTest, Expected, Actual )
1294c1025
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1297c1028
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[INTEGER(Long)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[INTEGER(Long)]'
1300a1032
> 
1305c1037
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1311c1043
<         self, &
---
>         UnitTest, &
1315c1047
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
1317a1050
> 
1321c1054
<         CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
---
>         CALL intlong_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
1324c1057
<   END SUBROUTINE intlong_assert_equal_r2
---
>   END SUBROUTINE intlong_isequal_rank2
1327c1060
<   SUBROUTINE realsp_assert_equal_s( self, Expected, Actual )
---
>   SUBROUTINE realsp_isequal_scalar( UnitTest, Expected, Actual )
1329c1062
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1332c1065
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Single)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Single)]'
1334,1336c1067,1070
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     LOGICAL :: Test
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
> 
1339c1073
<     test = (Expected .EqualTo. Actual)
---
>     Test = (Expected .EqualTo. Actual)
1342,1344c1076,1080
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
1346,1347c1082,1083
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
1349c1085
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1351,1353c1087,1091
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",es25.18,a,&
<                        &7x,"And got:  ",es25.18)') &
---
> 
>     ! Output message
>     WRITE( Message, &
>       '(a,7x,"Expected: ",'//RFMT//',a,&
>          &7x,"And got:  ",'//RFMT//')') &
1355d1092
<     ! Load the object with the message
1357c1094
<       self, &
---
>       UnitTest, &
1360,1363c1097,1099
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realsp_assert_equal_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE realsp_isequal_scalar
1366c1102
<   SUBROUTINE realsp_assert_equal_r1( self, Expected, Actual )
---
>   SUBROUTINE realsp_isequal_rank1( UnitTest, Expected, Actual )
1368c1104
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1371c1107
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Single)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Single)]'
1374a1111
> 
1378,1379c1115,1117
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
---
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
1382c1120
<         self, &
---
>         UnitTest, &
1385,1386c1123,1124
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
1388a1127
> 
1391c1130
<       CALL self%Assert_Equal( Expected(i), Actual(i) )
---
>       CALL realsp_isequal_scalar( UnitTest, Expected(i), Actual(i) )
1393c1132
<   END SUBROUTINE realsp_assert_equal_r1
---
>   END SUBROUTINE realsp_isequal_rank1
1396c1135
<   SUBROUTINE realsp_assert_equal_r2( self, Expected, Actual )
---
>   SUBROUTINE realsp_isequal_rank2( UnitTest, Expected, Actual )
1398c1137
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1401c1140
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Single)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Single)]'
1404a1144
> 
1409c1149
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1415c1155
<         self, &
---
>         UnitTest, &
1419c1159
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
1421a1162
> 
1425c1166
<         CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
---
>         CALL realsp_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
1428c1169
<   END SUBROUTINE realsp_assert_equal_r2
---
>   END SUBROUTINE realsp_isequal_rank2
1431c1172
<   SUBROUTINE realdp_assert_equal_s( self, Expected, Actual )
---
>   SUBROUTINE realdp_isequal_scalar( UnitTest, Expected, Actual )
1433c1174
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1436c1177
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Double)]'
1438,1440c1179,1182
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     LOGICAL :: Test
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
> 
1443c1185
<     test = (Expected .EqualTo. Actual)
---
>     Test = (Expected .EqualTo. Actual)
1446,1448c1188,1192
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
1450,1451c1194,1195
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
1453c1197
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1455,1457c1199,1203
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",es25.18,a,&
<                        &7x,"And got:  ",es25.18)') &
---
> 
>     ! Output message
>     WRITE( Message, &
>       '(a,7x,"Expected: ",'//RFMT//',a,&
>          &7x,"And got:  ",'//RFMT//')') &
1459d1204
<     ! Load the object with the message
1461c1206
<       self, &
---
>       UnitTest, &
1464,1467c1209,1211
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realdp_assert_equal_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE realdp_isequal_scalar
1470c1214
<   SUBROUTINE realdp_assert_equal_r1( self, Expected, Actual )
---
>   SUBROUTINE realdp_isequal_rank1( UnitTest, Expected, Actual )
1472c1216
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1475c1219
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Double)]'
1478a1223
> 
1482,1483c1227,1229
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
---
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
1486c1232
<         self, &
---
>         UnitTest, &
1489,1490c1235,1236
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
1492a1239
> 
1495c1242
<       CALL self%Assert_Equal( Expected(i), Actual(i) )
---
>       CALL realdp_isequal_scalar( UnitTest, Expected(i), Actual(i) )
1497c1244
<   END SUBROUTINE realdp_assert_equal_r1
---
>   END SUBROUTINE realdp_isequal_rank1
1500c1247
<   SUBROUTINE realdp_assert_equal_r2( self, Expected, Actual )
---
>   SUBROUTINE realdp_isequal_rank2( UnitTest, Expected, Actual )
1502c1249
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1505c1252
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[REAL(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[REAL(Double)]'
1508a1256
> 
1513c1261
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1519c1267
<         self, &
---
>         UnitTest, &
1523c1271
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
1525a1274
> 
1529c1278
<         CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
---
>         CALL realdp_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
1532c1281
<   END SUBROUTINE realdp_assert_equal_r2
---
>   END SUBROUTINE realdp_isequal_rank2
1535c1284
<   SUBROUTINE complexsp_assert_equal_s( self, Expected, Actual )
---
>   SUBROUTINE complexsp_isequal_scalar( UnitTest, Expected, Actual )
1537c1286
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1540c1289
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Single)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Single)]'
1542,1544c1291,1294
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     LOGICAL :: Test
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
> 
1547c1297
<     test = (Expected .EqualTo. Actual)
---
>     Test = (Expected .EqualTo. Actual)
1550,1552c1300,1304
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
1554,1555c1306,1307
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
1557c1309
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1559,1561c1311,1315
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ","(",es25.18,",",es25.18,")",a,&
<                        &7x,"And got:  ","(",es25.18,",",es25.18,")")') &
---
> 
>     ! Output message
>     WRITE( Message, &
>       '(a,7x,"Expected: ",'//ZFMT//',a,&
>          &7x,"And got:  ",'//ZFMT//')') &
1563d1316
<     ! Load the object with the message
1565c1318
<       self, &
---
>       UnitTest, &
1568,1571c1321,1323
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexsp_assert_equal_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE complexsp_isequal_scalar
1574c1326
<   SUBROUTINE complexsp_assert_equal_r1( self, Expected, Actual )
---
>   SUBROUTINE complexsp_isequal_rank1( UnitTest, Expected, Actual )
1576c1328
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1579c1331
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Single)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Single)]'
1582a1335
> 
1586,1587c1339,1341
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
---
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
1590c1344
<         self, &
---
>         UnitTest, &
1593,1594c1347,1348
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
1596a1351
> 
1599c1354
<       CALL self%Assert_Equal( Expected(i), Actual(i) )
---
>       CALL complexsp_isequal_scalar( UnitTest, Expected(i), Actual(i) )
1601c1356
<   END SUBROUTINE complexsp_assert_equal_r1
---
>   END SUBROUTINE complexsp_isequal_rank1
1604c1359
<   SUBROUTINE complexsp_assert_equal_r2( self, Expected, Actual )
---
>   SUBROUTINE complexsp_isequal_rank2( UnitTest, Expected, Actual )
1606c1361
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1609c1364
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Single)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Single)]'
1612a1368
> 
1617c1373
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1623c1379
<         self, &
---
>         UnitTest, &
1627c1383
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
1629a1386
> 
1633c1390
<         CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
---
>         CALL complexsp_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
1636c1393
<   END SUBROUTINE complexsp_assert_equal_r2
---
>   END SUBROUTINE complexsp_isequal_rank2
1639c1396
<   SUBROUTINE complexdp_assert_equal_s( self, Expected, Actual )
---
>   SUBROUTINE complexdp_isequal_scalar( UnitTest, Expected, Actual )
1641c1398
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1644c1401
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Double)]'
1646,1648c1403,1406
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     LOGICAL :: Test
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
> 
1651c1409
<     test = (Expected .EqualTo. Actual)
---
>     Test = (Expected .EqualTo. Actual)
1654,1656c1412,1416
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
1658,1659c1418,1419
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
1661c1421
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1663,1665c1423,1427
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ","(",es25.18,",",es25.18,")",a,&
<                        &7x,"And got:  ","(",es25.18,",",es25.18,")")') &
---
> 
>     ! Output message
>     WRITE( Message, &
>       '(a,7x,"Expected: ",'//ZFMT//',a,&
>          &7x,"And got:  ",'//ZFMT//')') &
1667d1428
<     ! Load the object with the message
1669c1430
<       self, &
---
>       UnitTest, &
1672,1675c1433,1435
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexdp_assert_equal_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE complexdp_isequal_scalar
1678c1438
<   SUBROUTINE complexdp_assert_equal_r1( self, Expected, Actual )
---
>   SUBROUTINE complexdp_isequal_rank1( UnitTest, Expected, Actual )
1680c1440
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1683c1443
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Double)]'
1686a1447
> 
1690,1691c1451,1453
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
---
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
1694c1456
<         self, &
---
>         UnitTest, &
1697,1698c1459,1460
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
1700a1463
> 
1703c1466
<       CALL self%Assert_Equal( Expected(i), Actual(i) )
---
>       CALL complexdp_isequal_scalar( UnitTest, Expected(i), Actual(i) )
1705c1468
<   END SUBROUTINE complexdp_assert_equal_r1
---
>   END SUBROUTINE complexdp_isequal_rank1
1708c1471
<   SUBROUTINE complexdp_assert_equal_r2( self, Expected, Actual )
---
>   SUBROUTINE complexdp_isequal_rank2( UnitTest, Expected, Actual )
1710c1473
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1713c1476
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[COMPLEX(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[COMPLEX(Double)]'
1716a1480
> 
1721c1485
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1727c1491
<         self, &
---
>         UnitTest, &
1731c1495
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
1733a1498
> 
1737c1502
<         CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
---
>         CALL complexdp_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
1740c1505
<   END SUBROUTINE complexdp_assert_equal_r2
---
>   END SUBROUTINE complexdp_isequal_rank2
1743c1508
<   SUBROUTINE char_assert_equal_s( self, Expected, Actual )
---
>   SUBROUTINE char_isequal_scalar( UnitTest, Expected, Actual )
1745c1510
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1748c1513
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[CHARACTER(*)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[CHARACTER]'
1750,1752c1515,1518
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     LOGICAL :: Test
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
> 
1755c1521
<     test = (Expected == Actual)
---
>     Test = (Expected == Actual)
1758,1760c1524,1528
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
1762,1763c1530,1531
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
1765c1533
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1767,1771c1535,1537
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",">",a,"<",a,&
<                        &7x,"And got:  ",">",a,"<")') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
---
> 
>     ! Output message
>     WRITE( Message,'("Expected >",a,"< and got >",a,"<")') Expected, Actual
1773c1539
<       self, &
---
>       UnitTest, &
1776,1779c1542,1544
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE char_assert_equal_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE char_isequal_scalar
1782c1547
<   SUBROUTINE char_assert_equal_r1( self, Expected, Actual )
---
>   SUBROUTINE char_isequal_rank1( UnitTest, Expected, Actual )
1784c1549
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1787c1552
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[CHARACTER(*)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[CHARACTER]'
1790a1556
> 
1794,1795c1560,1562
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
---
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
1798c1565
<         self, &
---
>         UnitTest, &
1801,1802c1568,1569
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
1804a1572
> 
1807c1575
<       CALL self%Assert_Equal( Expected(i), Actual(i) )
---
>       CALL char_isequal_scalar( UnitTest, Expected(i), Actual(i) )
1809c1577
<   END SUBROUTINE char_assert_equal_r1
---
>   END SUBROUTINE char_isequal_rank1
1812c1580
<   SUBROUTINE char_assert_equal_r2( self, Expected, Actual )
---
>   SUBROUTINE char_isequal_rank2( UnitTest, Expected, Actual )
1814c1582
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
1817c1585
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_Equal[CHARACTER(*)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqual[CHARACTER]'
1820a1589
> 
1825c1594
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
1831c1600
<         self, &
---
>         UnitTest, &
1835c1604
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
1837a1607
> 
1841c1611
<         CALL self%Assert_Equal( Expected(i,j), Actual(i,j) )
---
>         CALL char_isequal_scalar( UnitTest, Expected(i,j), Actual(i,j) )
1844,1845c1614
<   END SUBROUTINE char_assert_equal_r2
<   
---
>   END SUBROUTINE char_isequal_rank2
1848c1617
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
1852c1621
< !   UnitTest::Refute_Equal
---
> !       UnitTest_IsEqualWithin
1855c1624,1625
< !   Method to refute that two arguments are equal.
---
> !       Subroutine to assert that two floating point arguments are equal to
> !       within the specified tolerance.
1858c1628,1632
< !   CALL utest_obj%Refute_Equal( Expected, Actual )
---
> !       CALL UnitTest_IsEqualWithin( UnitTest , &
> !                                    Expected , &
> !                                    Actual   , &
> !                                    Tolerance, &
> !                                    Epsilon_Scale = Epsilon_Scale )
1861c1635
< !   utest_obj:     UnitTest object.
---
> !       UnitTest:      UnitTest object.
1863c1637
< !                  CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
1870,1873c1644
< !                  TYPE:       INTEGER(Byte)  , or
< !                              INTEGER(Short) , or
< !                              INTEGER(Long)  , or
< !                              REAL(Single)   , or
---
> !                      TYPE:       REAL(Single)   , or
1876,1877c1647
< !                              COMPLEX(Double), or
< !                              CHARACTER(*)
---
> !                                  COMPLEX(Double)
1888a1659,1680
> !       Tolerance:     The tolerance to within which the Expected and Actual
> !                      values must agree. If negative, the value of
> !                        EPSILON(Expected)
> !                      is used.
> !                      This argument is ignored if the EPSILON_SCALE optional
> !                      argument is specified
> !                      UNITS:      N/A
> !                      TYPE:       Same as Expected input
> !                      DIMENSION:  Same as Expected input
> !                      ATTRIBUTES: INTENT(IN)
> !
> ! OPTIONAL INPUTS:
> !       Epsilon_Scale: Set this logical flag to compute and use the tolerance
> !                      value:
> !                        EPSILON(Expected) * Scale_Factor
> !                      where the scaling factor is the exponent value of the
> !                      input argument Expected.
> !                      UNITS:      N/A
> !                      TYPE:       LOGICAL.
> !                      DIMENSION:  Scalar
> !                      ATTRIBUTES: INTENT(IN)
> !
1890c1682
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
1892c1684,1689
<   SUBROUTINE intbyte_refute_equal_s( self, Expected, Actual )
---
>   SUBROUTINE realsp_isequalwithin_scalar( &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
1894,1895c1691,1693
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     INTEGER(Byte), INTENT(IN) :: Expected, Actual
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     REAL(Single),        INTENT(IN)     :: Expected, Actual, Tolerance
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
1897c1695
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Byte)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Single)]'
1899,4241c1697,1699
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Assign the test
<     test = .NOT.(Expected == Actual)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",i0,a,&
<                        &7x,"And got:  ",i0)') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE intbyte_refute_equal_s
<   
<   
<   SUBROUTINE intbyte_refute_equal_r1( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     INTEGER(Byte), INTENT(IN) :: Expected(:), Actual(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Byte)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_Equal( Expected(i), Actual(i) )
<     END DO
<   END SUBROUTINE intbyte_refute_equal_r1
<   
<   
<   SUBROUTINE intbyte_refute_equal_r2( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     INTEGER(Byte), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Byte)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE intbyte_refute_equal_r2
<   
<   
<   SUBROUTINE intshort_refute_equal_s( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     INTEGER(Short), INTENT(IN) :: Expected, Actual
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Short)]'
<     ! Variables
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Assign the test
<     test = .NOT.(Expected == Actual)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",i0,a,&
<                        &7x,"And got:  ",i0)') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE intshort_refute_equal_s
<   
<   
<   SUBROUTINE intshort_refute_equal_r1( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     INTEGER(Short), INTENT(IN) :: Expected(:), Actual(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Short)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_Equal( Expected(i), Actual(i) )
<     END DO
<   END SUBROUTINE intshort_refute_equal_r1
<   
<   
<   SUBROUTINE intshort_refute_equal_r2( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     INTEGER(Short), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Short)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE intshort_refute_equal_r2
<   
<   
<   SUBROUTINE intlong_refute_equal_s( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     INTEGER(Long), INTENT(IN) :: Expected, Actual
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Long)]'
<     ! Variables
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Assign the test
<     test = .NOT.(Expected == Actual)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",i0,a,&
<                        &7x,"And got:  ",i0)') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE intlong_refute_equal_s
<   
<   
<   SUBROUTINE intlong_refute_equal_r1( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     INTEGER(Long), INTENT(IN) :: Expected(:), Actual(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Long)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_Equal( Expected(i), Actual(i) )
<     END DO
<   END SUBROUTINE intlong_refute_equal_r1
<   
<   
<   SUBROUTINE intlong_refute_equal_r2( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     INTEGER(Long), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[INTEGER(Long)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE intlong_refute_equal_r2
<   
<   
<   SUBROUTINE realsp_refute_equal_s( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected, Actual
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Single)]'
<     ! Variables
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Assign the test
<     test = .NOT.(Expected .EqualTo. Actual)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",es25.18,a,&
<                        &7x,"And got:  ",es25.18)') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realsp_refute_equal_s
<   
<   
<   SUBROUTINE realsp_refute_equal_r1( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected(:), Actual(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Single)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_Equal( Expected(i), Actual(i) )
<     END DO
<   END SUBROUTINE realsp_refute_equal_r1
<   
<   
<   SUBROUTINE realsp_refute_equal_r2( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Single)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE realsp_refute_equal_r2
<   
<   
<   SUBROUTINE realdp_refute_equal_s( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected, Actual
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Double)]'
<     ! Variables
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Assign the test
<     test = .NOT.(Expected .EqualTo. Actual)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",es25.18,a,&
<                        &7x,"And got:  ",es25.18)') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realdp_refute_equal_s
<   
<   
<   SUBROUTINE realdp_refute_equal_r1( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected(:), Actual(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Double)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_Equal( Expected(i), Actual(i) )
<     END DO
<   END SUBROUTINE realdp_refute_equal_r1
<   
<   
<   SUBROUTINE realdp_refute_equal_r2( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[REAL(Double)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE realdp_refute_equal_r2
<   
<   
<   SUBROUTINE complexsp_refute_equal_s( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected, Actual
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Single)]'
<     ! Variables
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Assign the test
<     test = .NOT.(Expected .EqualTo. Actual)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ","(",es25.18,",",es25.18,")",a,&
<                        &7x,"And got:  ","(",es25.18,",",es25.18,")")') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexsp_refute_equal_s
<   
<   
<   SUBROUTINE complexsp_refute_equal_r1( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected(:), Actual(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Single)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_Equal( Expected(i), Actual(i) )
<     END DO
<   END SUBROUTINE complexsp_refute_equal_r1
<   
<   
<   SUBROUTINE complexsp_refute_equal_r2( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Single)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE complexsp_refute_equal_r2
<   
<   
<   SUBROUTINE complexdp_refute_equal_s( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected, Actual
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Double)]'
<     ! Variables
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Assign the test
<     test = .NOT.(Expected .EqualTo. Actual)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ","(",es25.18,",",es25.18,")",a,&
<                        &7x,"And got:  ","(",es25.18,",",es25.18,")")') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexdp_refute_equal_s
<   
<   
<   SUBROUTINE complexdp_refute_equal_r1( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected(:), Actual(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Double)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_Equal( Expected(i), Actual(i) )
<     END DO
<   END SUBROUTINE complexdp_refute_equal_r1
<   
<   
<   SUBROUTINE complexdp_refute_equal_r2( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[COMPLEX(Double)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE complexdp_refute_equal_r2
<   
<   
<   SUBROUTINE char_refute_equal_s( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     CHARACTER(*), INTENT(IN) :: Expected, Actual
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[CHARACTER(*)]'
<     ! Variables
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Assign the test
<     test = .NOT.(Expected == Actual)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( message, '(a,7x,"Expected: ",">",a,"<",a,&
<                        &7x,"And got:  ",">",a,"<")') &
<                     CRLF, Expected, CRLF, Actual
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE char_refute_equal_s
<   
<   
<   SUBROUTINE char_refute_equal_r1( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     CHARACTER(*), INTENT(IN) :: Expected(:), Actual(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[CHARACTER(*)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_Equal( Expected(i), Actual(i) )
<     END DO
<   END SUBROUTINE char_refute_equal_r1
<   
<   
<   SUBROUTINE char_refute_equal_r2( self, Expected, Actual )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     CHARACTER(*), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_Equal[CHARACTER(*)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_Equal( Expected(i,j), Actual(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE char_refute_equal_r2
<   
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !   UnitTest::Assert_EqualWithin
< !
< ! PURPOSE:
< !   Method to assert that two floating point arguments are equal to
< !   within the specified tolerance.
< !
< ! CALLING SEQUENCE:
< !   CALL utest_obj%Assert_EqualWithin( Expected, Actual, Tolerance )
< !
< ! OBJECTS:
< !   utest_obj:     UnitTest object.
< !                  UNITS:      N/A
< !                  CLASS:      UnitTest_type
< !                  DIMENSION:  Scalar
< !                  ATTRIBUTES: INTENT(IN OUT)
< !
< ! INPUTS:
< !   Expected:      The expected value of the variable being tested.
< !                  UNITS:      N/A
< !                  TYPE:       REAL(Single)   , or
< !                              REAL(Double)   , or
< !                              COMPLEX(Single), or
< !                              COMPLEX(Double)
< !                  DIMENSION:  Scalar, or
< !                              Rank-1, or
< !                              Rank-2
< !                  ATTRIBUTES: INTENT(IN)
< !
< !   Actual:        The actual value of the variable being tested.
< !                  UNITS:      N/A
< !                  TYPE:       Same as Expected input
< !                  DIMENSION:  Same as Expected input
< !                  ATTRIBUTES: INTENT(IN)
< !
< !   Tolerance:     The tolerance to within which the Expected and Actual
< !                  values must agree. If negative, the value of
< !                    EPSILON(Expected)
< !                  is used.
< !                  UNITS:      N/A
< !                  TYPE:       Same as Expected input
< !                  DIMENSION:  Same as Expected input
< !                  ATTRIBUTES: INTENT(IN)
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   SUBROUTINE realsp_assert_equalwithin_s( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected, Actual, Tolerance
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Single)]'
<     ! Variables
<     REAL(Single) :: delta
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Local delta for test
<     delta = Tolerance
<     IF ( delta < 0.0_Single ) delta = EPSILON(Expected)
<     ! ...Assign the test
<     test = (ABS(Expected-Actual) < delta)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ",es25.18,a,&
<          &7x,"To within    : ",es25.18,a,&
<          &7x,"And got      : ",es25.18,a,&
<          &7x,"|Difference| : ",es25.18)') &
<       CRLF, Expected, CRLF, delta, CRLF, Actual, CRLF, ABS(Expected-Actual)
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realsp_assert_equalwithin_s
<   
<   
<   SUBROUTINE realsp_assert_equalwithin_r1( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Single)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Assert_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
<     END DO
<   END SUBROUTINE realsp_assert_equalwithin_r1
<   
<   
<   SUBROUTINE realsp_assert_equalwithin_r2( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Single)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Assert_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE realsp_assert_equalwithin_r2
<   
<   
<   SUBROUTINE realdp_assert_equalwithin_s( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected, Actual, Tolerance
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Double)]'
<     ! Variables
<     REAL(Double) :: delta
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Local delta for test
<     delta = Tolerance
<     IF ( delta < 0.0_Double ) delta = EPSILON(Expected)
<     ! ...Assign the test
<     test = (ABS(Expected-Actual) < delta)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ",es25.18,a,&
<          &7x,"To within    : ",es25.18,a,&
<          &7x,"And got      : ",es25.18,a,&
<          &7x,"|Difference| : ",es25.18)') &
<       CRLF, Expected, CRLF, delta, CRLF, Actual, CRLF, ABS(Expected-Actual)
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realdp_assert_equalwithin_s
<   
<   
<   SUBROUTINE realdp_assert_equalwithin_r1( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Double)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Assert_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
<     END DO
<   END SUBROUTINE realdp_assert_equalwithin_r1
<   
<   
<   SUBROUTINE realdp_assert_equalwithin_r2( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[REAL(Double)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Assert_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE realdp_assert_equalwithin_r2
<   
<   
<   SUBROUTINE complexsp_assert_equalwithin_s( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected, Actual, Tolerance
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Single)]'
<     ! Variables
<     REAL(Single) :: deltar, deltai
<     REAL(Single) :: zr, zi
<     REAL(Single) :: dzr, dzi
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Split expected into real and imag
<     zr = REAL(Expected,Single)
<     zi = AIMAG(Expected)
<     ! ...Local delta for test
<     deltar = REAL(Tolerance,Single)
<     IF ( deltar < 0.0_Single ) deltar = EPSILON(zr)
<     deltai = AIMAG(Tolerance)
<     IF ( deltai < 0.0_Single ) deltai = EPSILON(zi)
<     ! ...Assign the test
<     dzr = ABS(zr - REAL(Actual,Single))
<     dzi = ABS(zi - AIMAG(Actual))
<     test = ((dzr < deltar) .AND. (dzi < deltai))
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"To within    : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
<       CRLF, Expected, CRLF, CMPLX(deltar,deltai,Single), CRLF, Actual, CRLF, dzr, dzi
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexsp_assert_equalwithin_s
<   
<   
<   SUBROUTINE complexsp_assert_equalwithin_r1( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Single)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Assert_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
<     END DO
<   END SUBROUTINE complexsp_assert_equalwithin_r1
<   
<   
<   SUBROUTINE complexsp_assert_equalwithin_r2( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Single)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Assert_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE complexsp_assert_equalwithin_r2
<   
<   
<   SUBROUTINE complexdp_assert_equalwithin_s( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected, Actual, Tolerance
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Double)]'
<     ! Variables
<     REAL(Double) :: deltar, deltai
<     REAL(Double) :: zr, zi
<     REAL(Double) :: dzr, dzi
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Split expected into real and imag
<     zr = REAL(Expected,Double)
<     zi = AIMAG(Expected)
<     ! ...Local delta for test
<     deltar = REAL(Tolerance,Double)
<     IF ( deltar < 0.0_Double ) deltar = EPSILON(zr)
<     deltai = AIMAG(Tolerance)
<     IF ( deltai < 0.0_Double ) deltai = EPSILON(zi)
<     ! ...Assign the test
<     dzr = ABS(zr - REAL(Actual,Double))
<     dzi = ABS(zi - AIMAG(Actual))
<     test = ((dzr < deltar) .AND. (dzi < deltai))
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"To within    : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
<       CRLF, Expected, CRLF, CMPLX(deltar,deltai,Double), CRLF, Actual, CRLF, dzr, dzi
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexdp_assert_equalwithin_s
<   
<   
<   SUBROUTINE complexdp_assert_equalwithin_r1( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Double)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Assert_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
<     END DO
<   END SUBROUTINE complexdp_assert_equalwithin_r1
<   
<   
<   SUBROUTINE complexdp_assert_equalwithin_r2( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_EqualWithin[COMPLEX(Double)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Assert_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE complexdp_assert_equalwithin_r2
<   
<   
<   
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !   UnitTest::Refute_EqualWithin
< !
< ! PURPOSE:
< !   Method to refute that two floating point arguments are equal to
< !   within the specified tolerance.
< !
< ! CALLING SEQUENCE:
< !   CALL utest_obj%Refute_EqualWithin( Expected, Actual, Tolerance )
< !
< ! OBJECTS:
< !   utest_obj:     UnitTest object.
< !                  UNITS:      N/A
< !                  CLASS:      UnitTest_type
< !                  DIMENSION:  Scalar
< !                  ATTRIBUTES: INTENT(IN OUT)
< !
< ! INPUTS:
< !   Expected:      The expected value of the variable being tested.
< !                  UNITS:      N/A
< !                  TYPE:       REAL(Single)   , or
< !                              REAL(Double)   , or
< !                              COMPLEX(Single), or
< !                              COMPLEX(Double)
< !                  DIMENSION:  Scalar, or
< !                              Rank-1, or
< !                              Rank-2
< !                  ATTRIBUTES: INTENT(IN)
< !
< !   Actual:        The actual value of the variable being tested.
< !                  UNITS:      N/A
< !                  TYPE:       Same as Expected input
< !                  DIMENSION:  Same as Expected input
< !                  ATTRIBUTES: INTENT(IN)
< !
< !   Tolerance:     The tolerance to within which the Expected and Actual
< !                  values must agree. If negative, the value of
< !                    EPSILON(Expected)
< !                  is used.
< !                  UNITS:      N/A
< !                  TYPE:       Same as Expected input
< !                  DIMENSION:  Same as Expected input
< !                  ATTRIBUTES: INTENT(IN)
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   SUBROUTINE realsp_refute_equalwithin_s( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected, Actual, Tolerance
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Single)]'
<     ! Variables
<     REAL(Single) :: delta
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Local delta for test
<     delta = Tolerance
<     IF ( delta < 0.0_Single ) delta = EPSILON(Expected)
<     ! ...Assign the test
<     test = .NOT.(ABS(Expected-Actual) < delta)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ",es25.18,a,&
<          &7x,"Outside of   : ",es25.18,a,&
<          &7x,"And got      : ",es25.18,a,&
<          &7x,"|Difference| : ",es25.18)') &
<       CRLF, Expected, CRLF, delta, CRLF, Actual, CRLF, ABS(Expected-Actual)
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realsp_refute_equalwithin_s
<   
<   
<   SUBROUTINE realsp_refute_equalwithin_r1( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Single)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
<     END DO
<   END SUBROUTINE realsp_refute_equalwithin_r1
<   
<   
<   SUBROUTINE realsp_refute_equalwithin_r2( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Single)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE realsp_refute_equalwithin_r2
<   
<   
<   SUBROUTINE realdp_refute_equalwithin_s( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected, Actual, Tolerance
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Double)]'
<     ! Variables
<     REAL(Double) :: delta
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Local delta for test
<     delta = Tolerance
<     IF ( delta < 0.0_Double ) delta = EPSILON(Expected)
<     ! ...Assign the test
<     test = .NOT.(ABS(Expected-Actual) < delta)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ",es25.18,a,&
<          &7x,"Outside of   : ",es25.18,a,&
<          &7x,"And got      : ",es25.18,a,&
<          &7x,"|Difference| : ",es25.18)') &
<       CRLF, Expected, CRLF, delta, CRLF, Actual, CRLF, ABS(Expected-Actual)
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realdp_refute_equalwithin_s
<   
<   
<   SUBROUTINE realdp_refute_equalwithin_r1( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Double)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
<     END DO
<   END SUBROUTINE realdp_refute_equalwithin_r1
<   
<   
<   SUBROUTINE realdp_refute_equalwithin_r2( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[REAL(Double)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE realdp_refute_equalwithin_r2
<   
<   
<   SUBROUTINE complexsp_refute_equalwithin_s( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected, Actual, Tolerance
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Single)]'
<     ! Variables
<     REAL(Single) :: deltar, deltai
<     REAL(Single) :: zr, zi
<     REAL(Single) :: dzr, dzi
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Split expected into real and imag
<     zr = REAL(Expected,Single)
<     zi = AIMAG(Expected)
<     ! ...Local delta for test
<     deltar = REAL(Tolerance,Single)
<     IF ( deltar < 0.0_Single ) deltar = EPSILON(zr)
<     deltai = AIMAG(Tolerance)
<     IF ( deltai < 0.0_Single ) deltai = EPSILON(zi)
<     ! ...Assign the test
<     dzr = ABS(zr - REAL(Actual,Single))
<     dzi = ABS(zi - AIMAG(Actual))
<     test = .NOT.((dzr < deltar) .AND. (dzi < deltai))
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"Outside of   : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
<       CRLF, Expected, CRLF, CMPLX(deltar,deltai,Single), CRLF, Actual, CRLF, dzr, dzi
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexsp_refute_equalwithin_s
<   
<   
<   SUBROUTINE complexsp_refute_equalwithin_r1( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Single)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
<     END DO
<   END SUBROUTINE complexsp_refute_equalwithin_r1
<   
<   
<   SUBROUTINE complexsp_refute_equalwithin_r2( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Single)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE complexsp_refute_equalwithin_r2
<   
<   
<   SUBROUTINE complexdp_refute_equalwithin_s( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected, Actual, Tolerance
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Double)]'
<     ! Variables
<     REAL(Double) :: deltar, deltai
<     REAL(Double) :: zr, zi
<     REAL(Double) :: dzr, dzi
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Split expected into real and imag
<     zr = REAL(Expected,Double)
<     zi = AIMAG(Expected)
<     ! ...Local delta for test
<     deltar = REAL(Tolerance,Double)
<     IF ( deltar < 0.0_Double ) deltar = EPSILON(zr)
<     deltai = AIMAG(Tolerance)
<     IF ( deltai < 0.0_Double ) deltai = EPSILON(zi)
<     ! ...Assign the test
<     dzr = ABS(zr - REAL(Actual,Double))
<     dzi = ABS(zi - AIMAG(Actual))
<     test = .NOT.((dzr < deltar) .AND. (dzi < deltai))
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"Outside of   : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
<       CRLF, Expected, CRLF, CMPLX(deltar,deltai,Double), CRLF, Actual, CRLF, dzr, dzi
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexdp_refute_equalwithin_s
<   
<   
<   SUBROUTINE complexdp_refute_equalwithin_r1( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected(:), Actual(:), Tolerance(:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Double)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Refute_EqualWithin( Expected(i), Actual(i), Tolerance(i) )
<     END DO
<   END SUBROUTINE complexdp_refute_equalwithin_r1
<   
<   
<   SUBROUTINE complexdp_refute_equalwithin_r2( self, Expected, Actual, Tolerance )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected(:,:), Actual(:,:), Tolerance(:,:)
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_EqualWithin[COMPLEX(Double)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Refute_EqualWithin( Expected(i,j), Actual(i,j), Tolerance(i,j) )
<       END DO
<     END DO
<   END SUBROUTINE complexdp_refute_equalwithin_r2
<   
< 
< 
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !   UnitTest::Assert_WithinSigFig
< !
< ! PURPOSE:
< !   Method to assert that two floating point arguments are equal to
< !   within the specified number of significant figures.
< !
< ! CALLING SEQUENCE:
< !   CALL utest_obj%Assert_WithinSigFig( Expected, Actual, n_SigFig )
< !
< ! OBJECTS:
< !   utest_obj:     UnitTest object.
< !                  UNITS:      N/A
< !                  CLASS:      UnitTest_type
< !                  DIMENSION:  Scalar
< !                  ATTRIBUTES: INTENT(IN OUT)
< !
< ! INPUTS:
< !   Expected:      The expected value of the variable being tested.
< !                  UNITS:      N/A
< !                  TYPE:       REAL(Single)   , or
< !                              REAL(Double)   , or
< !                              COMPLEX(Single), or
< !                              COMPLEX(Double)
< !                  DIMENSION:  Scalar, or
< !                              Rank-1, or
< !                              Rank-2
< !                  ATTRIBUTES: INTENT(IN)
< !
< !   Actual:        The actual value of the variable being tested.
< !                  UNITS:      N/A
< !                  TYPE:       Same as Expected input
< !                  DIMENSION:  Same as Expected input
< !                  ATTRIBUTES: INTENT(IN)
< !
< !   n_SigFig:      The number of sgnificant figures within which the
< !                  expected and actual numbers are to be compared.
< !                  UNITS:      N/A
< !                  TYPE:       INTEGER
< !                  DIMENSION:  Scalar
< !                  ATTRIBUTES: INTENT(IN)
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
< 
<   SUBROUTINE realsp_assert_withinsigfig_s( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected, Actual
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Single)]'
<     ! Variables
<     REAL(Single) :: epsilon_delta
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Compute the test cutoff
<     epsilon_delta = EPSILON(Expected) * REAL(RADIX(Expected),Single)**(EXPONENT(Expected)-1)
<     ! ...Assign the test
<     test = Compares_Within_Tolerance(Expected, Actual, n_SigFig, cutoff=epsilon_delta)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ",es25.18,a,&
<          &7x,"To within    : ",i0," significant figures",a,&
<          &7x,"And got      : ",es25.18,a,&
<          &7x,"|Difference| : ",es25.18)') &
<       CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, ABS(Expected-Actual)
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realsp_assert_withinsigfig_s
<   
<   
<   SUBROUTINE realsp_assert_withinsigfig_r1( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected(:), Actual(:)
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Single)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Assert_WithinSigfig( Expected(i), Actual(i), n_SigFig )
<     END DO
<   END SUBROUTINE realsp_assert_withinsigfig_r1
<   
<   
<   SUBROUTINE realsp_assert_withinsigfig_r2( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Single)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Assert_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
<       END DO
<     END DO
<   END SUBROUTINE realsp_assert_withinsigfig_r2
<   
<   
<   SUBROUTINE realdp_assert_withinsigfig_s( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected, Actual
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Double)]'
<     ! Variables
<     REAL(Double) :: epsilon_delta
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Compute the test cutoff
<     epsilon_delta = EPSILON(Expected) * REAL(RADIX(Expected),Double)**(EXPONENT(Expected)-1)
<     ! ...Assign the test
<     test = Compares_Within_Tolerance(Expected, Actual, n_SigFig, cutoff=epsilon_delta)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ",es25.18,a,&
<          &7x,"To within    : ",i0," significant figures",a,&
<          &7x,"And got      : ",es25.18,a,&
<          &7x,"|Difference| : ",es25.18)') &
<       CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, ABS(Expected-Actual)
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realdp_assert_withinsigfig_s
<   
<   
<   SUBROUTINE realdp_assert_withinsigfig_r1( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected(:), Actual(:)
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Double)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Assert_WithinSigfig( Expected(i), Actual(i), n_SigFig )
<     END DO
<   END SUBROUTINE realdp_assert_withinsigfig_r1
<   
<   
<   SUBROUTINE realdp_assert_withinsigfig_r2( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[REAL(Double)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Assert_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
<       END DO
<     END DO
<   END SUBROUTINE realdp_assert_withinsigfig_r2
<   
<   
<   SUBROUTINE complexsp_assert_withinsigfig_s( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected, Actual
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Single)]'
<     ! Variables
<     REAL(Single) :: ezr, ezi
<     REAL(Single) :: azr, azi
<     REAL(Single) :: epsilon_delta_r, epsilon_delta_i
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Split expected into real and imag
<     ezr = REAL(Expected,Single)
<     ezi = AIMAG(Expected)
<     azr = REAL(Actual,Single)
<     azi = AIMAG(Actual)
<     ! ...Compute the test cutoffs
<     epsilon_delta_r = EPSILON(ezr) * REAL(RADIX(ezr),Single)**(EXPONENT(ezr)-1)
<     epsilon_delta_i = EPSILON(ezi) * REAL(RADIX(ezi),Single)**(EXPONENT(ezi)-1)
<     ! ...Assign the test
<     test = Compares_Within_Tolerance(ezr, azr, n_SigFig, cutoff=epsilon_delta_r) .AND. &
<            Compares_Within_Tolerance(ezi, azi, n_SigFig, cutoff=epsilon_delta_i)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"To within    : ",i0," significant figures",a,&
<          &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
<       CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, CMPLX(ezr-azr,ezi-azi,Single)
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexsp_assert_withinsigfig_s
<   
<   
<   SUBROUTINE complexsp_assert_withinsigfig_r1( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected(:), Actual(:)
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Single)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Assert_WithinSigfig( Expected(i), Actual(i), n_SigFig )
<     END DO
<   END SUBROUTINE complexsp_assert_withinsigfig_r1
<   
<   
<   SUBROUTINE complexsp_assert_withinsigfig_r2( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Single)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Assert_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
<       END DO
<     END DO
<   END SUBROUTINE complexsp_assert_withinsigfig_r2
<   
<   
<   SUBROUTINE complexdp_assert_withinsigfig_s( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected, Actual
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Double)]'
<     ! Variables
<     REAL(Double) :: ezr, ezi
<     REAL(Double) :: azr, azi
<     REAL(Double) :: epsilon_delta_r, epsilon_delta_i
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
<     ! Setup
<     ! ...Split expected into real and imag
<     ezr = REAL(Expected,Double)
<     ezi = AIMAG(Expected)
<     azr = REAL(Actual,Double)
<     azi = AIMAG(Actual)
<     ! ...Compute the test cutoffs
<     epsilon_delta_r = EPSILON(ezr) * REAL(RADIX(ezr),Double)**(EXPONENT(ezr)-1)
<     epsilon_delta_i = EPSILON(ezi) * REAL(RADIX(ezi),Double)**(EXPONENT(ezi)-1)
<     ! ...Assign the test
<     test = Compares_Within_Tolerance(ezr, azr, n_SigFig, cutoff=epsilon_delta_r) .AND. &
<            Compares_Within_Tolerance(ezi, azi, n_SigFig, cutoff=epsilon_delta_i)
<     ! ...Locally modify properties for this test
<     CALL Get_Property( &
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
<     ! Assert the test
<     IF ( test ) THEN
<       CALL Test_Passed( self )
<     ELSE
<       CALL Test_Failed( self )
<     END IF
<     ! Generate the test message
<     WRITE( Message, &
<       '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"To within    : ",i0," significant figures",a,&
<          &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
<       CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, CMPLX(ezr-azr,ezi-azi,Single)
<     ! Load the object with the message
<     CALL Set_Property( &
<       self, &
<       Level     = TEST_LEVEL, &
<       Procedure = PROCEDURE_NAME, &
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexdp_assert_withinsigfig_s
<   
<   
<   SUBROUTINE complexdp_assert_withinsigfig_r1( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected(:), Actual(:)
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Double)]'
<     ! Variables
<     INTEGER :: i, isize
<     CHARACTER(SL) :: Message
<     ! Check array sizes
<     isize = SIZE(Expected)
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO i = 1, isize
<       CALL self%Assert_WithinSigfig( Expected(i), Actual(i), n_SigFig )
<     END DO
<   END SUBROUTINE complexdp_assert_withinsigfig_r1
<   
<   
<   SUBROUTINE complexdp_assert_withinsigfig_r2( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Assert_WithinSigfig[COMPLEX(Double)]'
<     ! Variables
<     INTEGER :: i, j, isize, jsize
---
>     REAL(Single) :: tol
>     LOGICAL :: Test
>     LOGICAL :: Verbose
4243,4316d1700
<     ! Check array sizes
<     isize = SIZE(Expected,DIM=1); jsize = SIZE(Expected,DIM=2)
<     IF ( SIZE(Actual,DIM=1) /= isize .OR. &
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message, &
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
<         isize, jsize, &
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
<       CALL Set_Property( &
<         self, &
<         Level     = TEST_LEVEL, &
<         Procedure = PROCEDURE_NAME, &
<         Message   = Message )
<       CALL Display_Message( self )
<       RETURN
<     ENDIF
<     ! Loop over elements
<     DO j = 1, jsize
<       DO i = 1, isize
<         CALL self%Assert_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
<       END DO
<     END DO
<   END SUBROUTINE complexdp_assert_withinsigfig_r2
<   
<   
<   
< !--------------------------------------------------------------------------------
< !:sdoc+:
< !
< ! NAME:
< !   UnitTest::Refute_WithinSigFig
< !
< ! PURPOSE:
< !   Method to refute that two floating point arguments are equal to
< !   within the specified number of significant figures.
< !
< ! CALLING SEQUENCE:
< !   CALL utest_obj%Refute_WithinSigFig( Expected, Actual, n_SigFig )
< !
< ! OBJECTS:
< !   utest_obj:     UnitTest object.
< !                  UNITS:      N/A
< !                  CLASS:      UnitTest_type
< !                  DIMENSION:  Scalar
< !                  ATTRIBUTES: INTENT(IN OUT)
< !
< ! INPUTS:
< !   Expected:      The expected value of the variable being tested.
< !                  UNITS:      N/A
< !                  TYPE:       REAL(Single)   , or
< !                              REAL(Double)   , or
< !                              COMPLEX(Single), or
< !                              COMPLEX(Double)
< !                  DIMENSION:  Scalar, or
< !                              Rank-1, or
< !                              Rank-2
< !                  ATTRIBUTES: INTENT(IN)
< !
< !   Actual:        The actual value of the variable being tested.
< !                  UNITS:      N/A
< !                  TYPE:       Same as Expected input
< !                  DIMENSION:  Same as Expected input
< !                  ATTRIBUTES: INTENT(IN)
< !
< !   n_SigFig:      The number of sgnificant figures within which the
< !                  expected and actual numbers are to be compared.
< !                  UNITS:      N/A
< !                  TYPE:       INTEGER
< !                  DIMENSION:  Scalar
< !                  ATTRIBUTES: INTENT(IN)
< !
< !:sdoc-:
< !--------------------------------------------------------------------------------
4318,4329d1701
<   SUBROUTINE realsp_refute_withinsigfig_s( self, Expected, Actual, n_SigFig )
<     ! Arguments
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected, Actual
<     INTEGER, INTENT(IN) :: n_SigFig
<     ! Parameters
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Single)]'
<     ! Variables
<     REAL(Single) :: epsilon_delta
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
4331,4332c1703,1708
<     ! ...Compute the test cutoff
<     epsilon_delta = EPSILON(Expected) * REAL(RADIX(Expected),Single)**(EXPONENT(Expected)-1)
---
>     ! ...Default tolerance
>     tol = Tolerance
>     ! ...Check optional arguments
>     IF ( PRESENT(Epsilon_Scale) ) THEN
>       IF ( Epsilon_Scale ) tol = EPSILON(Expected) * Get_Multiplier( Expected )
>     END IF
4334c1710
<     test = .NOT.(Compares_Within_Tolerance(Expected, Actual, n_SigFig, cutoff=epsilon_delta))
---
>     Test = (ABS(Expected-Actual) < tol)
4337,4339c1713,1717
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
4341,4342c1719,1720
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
4344c1722
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
4346c1724,1725
<     ! Generate the test message
---
> 
>     ! Output message
4348,4353c1727,1731
<       '(a,7x,"Expected     : ",es25.18,a,&
<          &7x,"Outside of   : ",i0," significant figures",a,&
<          &7x,"And got      : ",es25.18,a,&
<          &7x,"|Difference| : ",es25.18)') &
<       CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, ABS(Expected-Actual)
<     ! Load the object with the message
---
>       '(a,7x,"Expected:     ",'//RFMT//',a,&
>          &7x,"To within:    ",'//RFMT//',a,&
>          &7x,"And got:      ",'//RFMT//',a,&
>          &7x,"|Difference|: ",'//RFMT//')') &
>       CRLF, Expected, CRLF, tol, CRLF, Actual, CRLF, ABS(Expected-Actual)
4355c1733
<       self, &
---
>       UnitTest, &
4358,4361c1736,1738
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realsp_refute_withinsigfig_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE realsp_isequalwithin_scalar
4364c1741,1746
<   SUBROUTINE realsp_refute_withinsigfig_r1( self, Expected, Actual, n_SigFig )
---
>   SUBROUTINE realsp_isequalwithin_rank1(  &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
4366,4368c1748,1750
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected(:), Actual(:)
<     INTEGER, INTENT(IN) :: n_SigFig
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     REAL(Single),        INTENT(IN)     :: Expected(:), Actual(:), Tolerance(:)
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
4370c1752
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Single)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Single)]'
4373a1756
> 
4376,4379c1759,1764
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
---
>     IF ( SIZE(Actual)    /= isize .OR. &
>          SIZE(Tolerance) /= isize ) THEN
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0,"; Tolerance:",i0)') &
>         isize, SIZE(Actual), SIZE(Tolerance)
4381c1766
<         self, &
---
>         UnitTest, &
4384,4385c1769,1770
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
4387a1773
> 
4390c1776,1781
<       CALL self%Refute_WithinSigfig( Expected(i), Actual(i), n_SigFig )
---
>       CALL realsp_isequalwithin_scalar( &
>         UnitTest    , &
>         Expected(i) , &
>         Actual(i)   , &
>         Tolerance(i), &
>         Epsilon_Scale = Epsilon_Scale )
4392c1783
<   END SUBROUTINE realsp_refute_withinsigfig_r1
---
>   END SUBROUTINE realsp_isequalwithin_rank1
4395c1786,1791
<   SUBROUTINE realsp_refute_withinsigfig_r2( self, Expected, Actual, n_SigFig )
---
>   SUBROUTINE realsp_isequalwithin_rank2( &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
4397,4399c1793,1795
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     INTEGER, INTENT(IN) :: n_SigFig
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     REAL(Single),        INTENT(IN)     :: Expected(:,:), Actual(:,:), Tolerance(:,:)
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
4401c1797
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Single)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Single)]'
4404a1801
> 
4408,4409c1805,1808
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
---
>          SIZE(Actual,DIM=2) /= jsize .OR. &
>          SIZE(Tolerance,DIM=1) /= isize .OR. &
>          SIZE(Tolerance,DIM=2) /= jsize ) THEN
>       CALL Test_Failed( UnitTest )
4411c1810,1811
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
---
>         '("Array sizes are diffferent -- ",&
>         &"Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,"); Tolerance:(",i0,",",i0,")")') &
4413c1813,1814
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
---
>         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2), &
>         SIZE(Tolerance,DIM=1), SIZE(Tolerance,DIM=2)
4415c1816
<         self, &
---
>         UnitTest, &
4419c1820
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
4421a1823
> 
4425c1827,1832
<         CALL self%Refute_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
---
>         CALL realsp_isequalwithin_scalar( &
>           UnitTest      , &
>           Expected(i,j) , &
>           Actual(i,j)   , &
>           Tolerance(i,j), &
>           Epsilon_Scale = Epsilon_Scale )
4428c1835
<   END SUBROUTINE realsp_refute_withinsigfig_r2
---
>   END SUBROUTINE realsp_isequalwithin_rank2
4431c1838,1843
<   SUBROUTINE realdp_refute_withinsigfig_s( self, Expected, Actual, n_SigFig )
---
>   SUBROUTINE realdp_isequalwithin_scalar( &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
4433,4435c1845,1847
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected, Actual
<     INTEGER, INTENT(IN) :: n_SigFig
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     REAL(Double),        INTENT(IN)     :: Expected, Actual, Tolerance
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
4437c1849
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Double)]'
4439,4442c1851,1855
<     REAL(Double) :: epsilon_delta
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     REAL(Double) :: tol
>     LOGICAL :: Test
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
> 
4444,4445c1857,1862
<     ! ...Compute the test cutoff
<     epsilon_delta = EPSILON(Expected) * REAL(RADIX(Expected),Double)**(EXPONENT(Expected)-1)
---
>     ! ...Default tolerance
>     tol = Tolerance
>     ! ...Check optional arguments
>     IF ( PRESENT(Epsilon_Scale) ) THEN
>       IF ( Epsilon_Scale ) tol = EPSILON(Expected) * Get_Multiplier( Expected )
>     END IF
4447c1864
<     test = .NOT.(Compares_Within_Tolerance(Expected, Actual, n_SigFig, cutoff=epsilon_delta))
---
>     Test = (ABS(Expected-Actual) < tol)
4450,4452c1867,1871
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
4454,4455c1873,1874
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
4457c1876
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
4459c1878,1879
<     ! Generate the test message
---
> 
>     ! Output message
4461,4466c1881,1885
<       '(a,7x,"Expected     : ",es25.18,a,&
<          &7x,"Outside of   : ",i0," significant figures",a,&
<          &7x,"And got      : ",es25.18,a,&
<          &7x,"|Difference| : ",es25.18)') &
<       CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, ABS(Expected-Actual)
<     ! Load the object with the message
---
>       '(a,7x,"Expected:     ",'//RFMT//',a,&
>          &7x,"To within:    ",'//RFMT//',a,&
>          &7x,"And got:      ",'//RFMT//',a,&
>          &7x,"|Difference|: ",'//RFMT//')') &
>       CRLF, Expected, CRLF, tol, CRLF, Actual, CRLF, ABS(Expected-Actual)
4468c1887
<       self, &
---
>       UnitTest, &
4471,4474c1890,1892
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE realdp_refute_withinsigfig_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE realdp_isequalwithin_scalar
4477c1895,1900
<   SUBROUTINE realdp_refute_withinsigfig_r1( self, Expected, Actual, n_SigFig )
---
>   SUBROUTINE realdp_isequalwithin_rank1(  &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
4479,4481c1902,1904
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected(:), Actual(:)
<     INTEGER, INTENT(IN) :: n_SigFig
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     REAL(Double),        INTENT(IN)     :: Expected(:), Actual(:), Tolerance(:)
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
4483c1906
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Double)]'
4486a1910
> 
4489,4492c1913,1918
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
---
>     IF ( SIZE(Actual)    /= isize .OR. &
>          SIZE(Tolerance) /= isize ) THEN
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0,"; Tolerance:",i0)') &
>         isize, SIZE(Actual), SIZE(Tolerance)
4494c1920
<         self, &
---
>         UnitTest, &
4497,4498c1923,1924
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
4500a1927
> 
4503c1930,1935
<       CALL self%Refute_WithinSigfig( Expected(i), Actual(i), n_SigFig )
---
>       CALL realdp_isequalwithin_scalar( &
>         UnitTest    , &
>         Expected(i) , &
>         Actual(i)   , &
>         Tolerance(i), &
>         Epsilon_Scale = Epsilon_Scale )
4505c1937
<   END SUBROUTINE realdp_refute_withinsigfig_r1
---
>   END SUBROUTINE realdp_isequalwithin_rank1
4508c1940,1945
<   SUBROUTINE realdp_refute_withinsigfig_r2( self, Expected, Actual, n_SigFig )
---
>   SUBROUTINE realdp_isequalwithin_rank2( &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
4510,4512c1947,1949
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     REAL(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     INTEGER, INTENT(IN) :: n_SigFig
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     REAL(Double),        INTENT(IN)     :: Expected(:,:), Actual(:,:), Tolerance(:,:)
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
4514c1951
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[REAL(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[REAL(Double)]'
4517a1955
> 
4521,4522c1959,1962
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
---
>          SIZE(Actual,DIM=2) /= jsize .OR. &
>          SIZE(Tolerance,DIM=1) /= isize .OR. &
>          SIZE(Tolerance,DIM=2) /= jsize ) THEN
>       CALL Test_Failed( UnitTest )
4524c1964,1965
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
---
>         '("Array sizes are diffferent -- ",&
>         &"Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,"); Tolerance:(",i0,",",i0,")")') &
4526c1967,1968
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
---
>         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2), &
>         SIZE(Tolerance,DIM=1), SIZE(Tolerance,DIM=2)
4528c1970
<         self, &
---
>         UnitTest, &
4532c1974
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
4534a1977
> 
4538c1981,1986
<         CALL self%Refute_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
---
>         CALL realdp_isequalwithin_scalar( &
>           UnitTest      , &
>           Expected(i,j) , &
>           Actual(i,j)   , &
>           Tolerance(i,j), &
>           Epsilon_Scale = Epsilon_Scale )
4541c1989
<   END SUBROUTINE realdp_refute_withinsigfig_r2
---
>   END SUBROUTINE realdp_isequalwithin_rank2
4544c1992,1997
<   SUBROUTINE complexsp_refute_withinsigfig_s( self, Expected, Actual, n_SigFig )
---
>   SUBROUTINE complexsp_isequalwithin_scalar( &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
4546,4548c1999,2001
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected, Actual
<     INTEGER, INTENT(IN) :: n_SigFig
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     COMPLEX(Single),     INTENT(IN)     :: Expected, Actual, Tolerance
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
4550c2003
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Single)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Single)]'
4552,4557c2005,2011
<     REAL(Single) :: ezr, ezi
<     REAL(Single) :: azr, azi
<     REAL(Single) :: epsilon_delta_r, epsilon_delta_i
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     REAL(Single) :: tolr, toli
>     REAL(Single) :: zr, zi
>     REAL(Single) :: dzr, dzi
>     LOGICAL :: Test
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
> 
4560,4566c2014,2025
<     ezr = REAL(Expected,Single)
<     ezi = AIMAG(Expected)
<     azr = REAL(Actual,Single)
<     azi = AIMAG(Actual)
<     ! ...Compute the test cutoffs
<     epsilon_delta_r = EPSILON(ezr) * REAL(RADIX(ezr),Single)**(EXPONENT(ezr)-1)
<     epsilon_delta_i = EPSILON(ezi) * REAL(RADIX(ezi),Single)**(EXPONENT(ezi)-1)
---
>     zr = REAL(Expected,Single)
>     zi = AIMAG(Expected)
>     ! ...Default tolerance
>     tolr = REAL(Tolerance,Single)
>     toli = AIMAG(Tolerance)
>     ! ...Check optional arguments
>     IF ( PRESENT(Epsilon_Scale) ) THEN
>       IF ( Epsilon_Scale ) THEN
>         tolr = EPSILON(zr) * Get_Multiplier(zr)
>         toli = EPSILON(zi) * Get_Multiplier(zi)
>       END IF
>     END IF
4568,4569c2027,2029
<     test = .NOT.(Compares_Within_Tolerance(ezr, azr, n_SigFig, cutoff=epsilon_delta_r) .AND. &
<            Compares_Within_Tolerance(ezi, azi, n_SigFig, cutoff=epsilon_delta_i))
---
>     dzr = ABS(zr - REAL(Actual,Single))
>     dzi = ABS(zi - AIMAG(Actual))
>     Test = (dzr < tolr) .AND. (dzi < toli)
4572,4574c2032,2036
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
4576,4577c2038,2039
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
4579c2041
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
4581c2043,2044
<     ! Generate the test message
---
> 
>     ! Output message
4583,4588c2046,2050
<       '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"Outside of   : ",i0," significant figures",a,&
<          &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
<       CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, CMPLX(ezr-azr,ezi-azi,Single)
<     ! Load the object with the message
---
>       '(a,7x,"Expected:     ",'//ZFMT//',a,&
>          &7x,"To within:    ",'//ZFMT//',a,&
>          &7x,"And got:      ",'//ZFMT//',a,&
>          &7x,"|Difference|: ",'//ZFMT//')') &
>       CRLF, Expected, CRLF, CMPLX(tolr,toli,Single), CRLF, Actual, CRLF, dzr, dzi
4590c2052
<       self, &
---
>       UnitTest, &
4593,4596c2055,2057
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexsp_refute_withinsigfig_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE complexsp_isequalwithin_scalar
4599c2060,2065
<   SUBROUTINE complexsp_refute_withinsigfig_r1( self, Expected, Actual, n_SigFig )
---
>   SUBROUTINE complexsp_isequalwithin_rank1( &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
4601,4603c2067,2069
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected(:), Actual(:)
<     INTEGER, INTENT(IN) :: n_SigFig
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     COMPLEX(Single),     INTENT(IN)     :: Expected(:), Actual(:), Tolerance(:)
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
4605c2071
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Single)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Single)]'
4608a2075
> 
4611,4614c2078,2083
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
---
>     IF ( SIZE(Actual)    /= isize .OR. &
>          SIZE(Tolerance) /= isize ) THEN
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0,"; Tolerance:",i0)') &
>         isize, SIZE(Actual), SIZE(Tolerance)
4616c2085
<         self, &
---
>         UnitTest, &
4619,4620c2088,2089
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
4622a2092
> 
4625c2095,2100
<       CALL self%Refute_WithinSigfig( Expected(i), Actual(i), n_SigFig )
---
>       CALL complexsp_isequalwithin_scalar( &
>         UnitTest    , &
>         Expected(i) , &
>         Actual(i)   , &
>         Tolerance(i), &
>         Epsilon_Scale = Epsilon_Scale )
4627c2102
<   END SUBROUTINE complexsp_refute_withinsigfig_r1
---
>   END SUBROUTINE complexsp_isequalwithin_rank1
4630c2105,2110
<   SUBROUTINE complexsp_refute_withinsigfig_r2( self, Expected, Actual, n_SigFig )
---
>   SUBROUTINE complexsp_isequalwithin_rank2( &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
4632,4634c2112,2114
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Single), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     INTEGER, INTENT(IN) :: n_SigFig
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     COMPLEX(Single),     INTENT(IN)     :: Expected(:,:), Actual(:,:), Tolerance(:,:)
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
4636c2116
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Single)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Single)]'
4639a2120
> 
4643,4644c2124,2127
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
---
>          SIZE(Actual,DIM=2) /= jsize .OR. &
>          SIZE(Tolerance,DIM=1) /= isize .OR. &
>          SIZE(Tolerance,DIM=2) /= jsize ) THEN
>       CALL Test_Failed( UnitTest )
4646c2129,2130
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
---
>         '("Array sizes are diffferent -- ",&
>         &"Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,"); Tolerance:(",i0,",",i0,")")') &
4648c2132,2133
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
---
>         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2), &
>         SIZE(Tolerance,DIM=1), SIZE(Tolerance,DIM=2)
4650c2135
<         self, &
---
>         UnitTest, &
4654c2139
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
4656a2142
> 
4660c2146,2151
<         CALL self%Refute_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
---
>         CALL complexsp_isequalwithin_scalar( &
>           UnitTest      , &
>           Expected(i,j) , &
>           Actual(i,j)   , &
>           Tolerance(i,j), &
>           Epsilon_Scale = Epsilon_Scale )
4663c2154
<   END SUBROUTINE complexsp_refute_withinsigfig_r2
---
>   END SUBROUTINE complexsp_isequalwithin_rank2
4666c2157,2162
<   SUBROUTINE complexdp_refute_withinsigfig_s( self, Expected, Actual, n_SigFig )
---
>   SUBROUTINE complexdp_isequalwithin_scalar( &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
4668,4670c2164,2166
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected, Actual
<     INTEGER, INTENT(IN) :: n_SigFig
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     COMPLEX(Double),     INTENT(IN)     :: Expected, Actual, Tolerance
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
4672c2168
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Double)]'
4674,4679c2170,2176
<     REAL(Double) :: ezr, ezi
<     REAL(Double) :: azr, azi
<     REAL(Double) :: epsilon_delta_r, epsilon_delta_i
<     LOGICAL :: test
<     LOGICAL :: verbose
<     CHARACTER(SL) :: message
---
>     REAL(Double) :: tolr, toli
>     REAL(Double) :: zr, zi
>     REAL(Double) :: dzr, dzi
>     LOGICAL :: Test
>     LOGICAL :: Verbose
>     CHARACTER(SL) :: Message
> 
4682,4688c2179,2190
<     ezr = REAL(Expected,Double)
<     ezi = AIMAG(Expected)
<     azr = REAL(Actual,Double)
<     azi = AIMAG(Actual)
<     ! ...Compute the test cutoffs
<     epsilon_delta_r = EPSILON(ezr) * REAL(RADIX(ezr),Double)**(EXPONENT(ezr)-1)
<     epsilon_delta_i = EPSILON(ezi) * REAL(RADIX(ezi),Double)**(EXPONENT(ezi)-1)
---
>     zr = REAL(Expected,Double)
>     zi = AIMAG(Expected)
>     ! ...Default tolerance
>     tolr = REAL(Tolerance,Double)
>     toli = AIMAG(Tolerance)
>     ! ...Check optional arguments
>     IF ( PRESENT(Epsilon_Scale) ) THEN
>       IF ( Epsilon_Scale ) THEN
>         tolr = EPSILON(zr) * Get_Multiplier(zr)
>         toli = EPSILON(zi) * Get_Multiplier(zi)
>       END IF
>     END IF
4690,4691c2192,2194
<     test = .NOT.(Compares_Within_Tolerance(ezr, azr, n_SigFig, cutoff=epsilon_delta_r) .AND. &
<            Compares_Within_Tolerance(ezi, azi, n_SigFig, cutoff=epsilon_delta_i))
---
>     dzr = ABS(zr - REAL(Actual,Double))
>     dzi = ABS(zi - AIMAG(Actual))
>     Test = (dzr < tolr) .AND. (dzi < toli)
4694,4696c2197,2201
<       self, &
<       Verbose = verbose )
<     verbose = verbose .OR. (.NOT. test)  ! Always output test failure
---
>       UnitTest, &
>       Verbose = Verbose )
>     Verbose = Verbose .OR. (.NOT. Test)  ! Always output test failure
> 
> 
4698,4699c2203,2204
<     IF ( test ) THEN
<       CALL Test_Passed( self )
---
>     IF ( Test ) THEN
>       CALL Test_Passed( UnitTest )
4701c2206
<       CALL Test_Failed( self )
---
>       CALL Test_Failed( UnitTest )
4703c2208,2209
<     ! Generate the test message
---
> 
>     ! Output message
4705,4710c2211,2215
<       '(a,7x,"Expected     : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"Outside of   : ",i0," significant figures",a,&
<          &7x,"And got      : ","(",es25.18,",",es25.18,")",a,&
<          &7x,"|Difference| : ","(",es25.18,",",es25.18,")")') &
<       CRLF, Expected, CRLF, n_SigFig, CRLF, Actual, CRLF, CMPLX(ezr-azr,ezi-azi,Single)
<     ! Load the object with the message
---
>       '(a,7x,"Expected:     ",'//ZFMT//',a,&
>          &7x,"To within:    ",'//ZFMT//',a,&
>          &7x,"And got:      ",'//ZFMT//',a,&
>          &7x,"|Difference|: ",'//ZFMT//')') &
>       CRLF, Expected, CRLF, CMPLX(tolr,toli,Double), CRLF, Actual, CRLF, dzr, dzi
4712c2217
<       self, &
---
>       UnitTest, &
4715,4718c2220,2222
<       Message   = message )
<     ! Output the result
<     IF ( verbose ) CALL Display_Message( self )
<   END SUBROUTINE complexdp_refute_withinsigfig_s
---
>       Message = Message )
>     IF ( Verbose ) CALL Display_Message( UnitTest )
>   END SUBROUTINE complexdp_isequalwithin_scalar
4721c2225,2230
<   SUBROUTINE complexdp_refute_withinsigfig_r1( self, Expected, Actual, n_SigFig )
---
>   SUBROUTINE complexdp_isequalwithin_rank1( &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
4723,4725c2232,2234
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected(:), Actual(:)
<     INTEGER, INTENT(IN) :: n_SigFig
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     COMPLEX(Double),     INTENT(IN)     :: Expected(:), Actual(:), Tolerance(:)
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
4727c2236
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Double)]'
4730a2240
> 
4733,4736c2243,2248
<     IF ( SIZE(Actual) /= isize ) THEN
<       CALL Test_Failed( self )
<       WRITE( Message,'("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0)') &
<                      isize, SIZE(Actual)
---
>     IF ( SIZE(Actual)    /= isize .OR. &
>          SIZE(Tolerance) /= isize ) THEN
>       CALL Test_Failed( UnitTest )
>       WRITE( Message, &
>         '("Array sizes are diffferent -- Expected:",i0,"; Actual:",i0,"; Tolerance:",i0)') &
>         isize, SIZE(Actual), SIZE(Tolerance)
4738c2250
<         self, &
---
>         UnitTest, &
4741,4742c2253,2254
<         Message   = message )
<       CALL Display_Message( self )
---
>         Message = Message )
>       CALL Display_Message( UnitTest )
4744a2257
> 
4747c2260,2265
<       CALL self%Refute_WithinSigfig( Expected(i), Actual(i), n_SigFig )
---
>       CALL complexdp_isequalwithin_scalar( &
>         UnitTest    , &
>         Expected(i) , &
>         Actual(i)   , &
>         Tolerance(i), &
>         Epsilon_Scale = Epsilon_Scale )
4749c2267
<   END SUBROUTINE complexdp_refute_withinsigfig_r1
---
>   END SUBROUTINE complexdp_isequalwithin_rank1
4752c2270,2275
<   SUBROUTINE complexdp_refute_withinsigfig_r2( self, Expected, Actual, n_SigFig )
---
>   SUBROUTINE complexdp_isequalwithin_rank2( &
>     UnitTest     , &
>     Expected     , &
>     Actual       , &
>     Tolerance    , &
>     Epsilon_Scale  )
4754,4756c2277,2279
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
<     COMPLEX(Double), INTENT(IN) :: Expected(:,:), Actual(:,:)
<     INTEGER, INTENT(IN) :: n_SigFig
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
>     COMPLEX(Double),     INTENT(IN)     :: Expected(:,:), Actual(:,:), Tolerance(:,:)
>     LOGICAL,   OPTIONAL, INTENT(IN)     :: Epsilon_Scale
4758c2281
<     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest::Refute_WithinSigfig[COMPLEX(Double)]'
---
>     CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'UnitTest_IsEqualWithin[COMPLEX(Double)]'
4761a2285
> 
4765,4766c2289,2292
<          SIZE(Actual,DIM=2) /= jsize ) THEN
<       CALL Test_Failed( self )
---
>          SIZE(Actual,DIM=2) /= jsize .OR. &
>          SIZE(Tolerance,DIM=1) /= isize .OR. &
>          SIZE(Tolerance,DIM=2) /= jsize ) THEN
>       CALL Test_Failed( UnitTest )
4768c2294,2295
<         '("Array sizes are diffferent -- Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,")")') &
---
>         '("Array sizes are diffferent -- ",&
>         &"Expected:(",i0,",",i0,"); Actual:(",i0,",",i0,"); Tolerance:(",i0,",",i0,")")') &
4770c2297,2298
<         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2)
---
>         SIZE(Actual,DIM=1), SIZE(Actual,DIM=2), &
>         SIZE(Tolerance,DIM=1), SIZE(Tolerance,DIM=2)
4772c2300
<         self, &
---
>         UnitTest, &
4776c2304
<       CALL Display_Message( self )
---
>       CALL Display_Message( UnitTest )
4778a2307
> 
4782c2311,2316
<         CALL self%Refute_WithinSigfig( Expected(i,j), Actual(i,j), n_SigFig )
---
>         CALL complexdp_isequalwithin_scalar( &
>           UnitTest      , &
>           Expected(i,j) , &
>           Actual(i,j)   , &
>           Tolerance(i,j), &
>           Epsilon_Scale = Epsilon_Scale )
4785c2319,2320
<   END SUBROUTINE complexdp_refute_withinsigfig_r2
---
>   END SUBROUTINE complexdp_isequalwithin_rank2
> 
4786a2322,2343
> !--------------------------------------------------------------------------------
> !:sdoc+:
> !
> ! NAME:
> !       UnitTest_DefineVersion
> !
> ! PURPOSE:
> !       Subroutine to return the module version information.
> !
> ! CALLING SEQUENCE:
> !       CALL UnitTest_DefineVersion( Id )
> !
> ! OUTPUTS:
> !       Id:    Character string containing the version Id information
> !              for the module.
> !              UNITS:      N/A
> !              TYPE:       CHARACTER(*)
> !              DIMENSION:  Scalar
> !              ATTRIBUTES: INTENT(OUT)
> !
> !:sdoc-:
> !--------------------------------------------------------------------------------
4787a2345,2348
>   SUBROUTINE UnitTest_DefineVersion( Id )
>     CHARACTER(*), INTENT(OUT) :: Id
>     Id = MODULE_VERSION_ID
>   END SUBROUTINE UnitTest_DefineVersion
4798c2359,2363
< !--------------------------------------------------------------------------------
---
> !===================
> ! METHOD PROCEDURES
> !===================
> 
> !------------------------------------------------------------------------------
4801c2366
< !   UnitTest::Set_Property
---
> !       Set_Property
4804c2369
< !   Private method to set the properties of a UnitTest object.
---
> !       Private subroutine to set the properties of a UnitTest object.
4807c2372
< !   done using this method.
---
> !       done using this subroutine.
4810c2375,2377
< !   CALL utest_obj%Set_Property( Verbose           = Verbose          , &
---
> !       CALL Set_Property( &
> !         UnitTest, &
> !         Verbose           = Verbose          , &
4825c2392
< !   utest_obj:          UnitTest object.
---
> !       UnitTest:           UnitTest object.
4827c2394
< !                       CLASS:      UnitTest_type
---
> !                           TYPE:       TYPE(UnitTest_type)
4927c2494
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
4930c2497
<     self             , & ! Object
---
>     UnitTest         , & ! Object
4945c2512
<     CLASS(UnitTest_type)  , INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type)   , INTENT(IN OUT) :: UnitTest
4960,4972c2527,2539
<     IF ( PRESENT(Verbose          ) ) self%Verbose           = Verbose
<     IF ( PRESENT(Title            ) ) self%Title             = Title
<     IF ( PRESENT(Caller           ) ) self%Caller            = Caller
<     IF ( PRESENT(Level            ) ) self%Level             = Level
<     IF ( PRESENT(Procedure        ) ) self%Procedure         = Procedure
<     IF ( PRESENT(Message          ) ) self%Message           = Message
<     IF ( PRESENT(Test_Result      ) ) self%Test_Result       = Test_Result
<     IF ( PRESENT(n_Tests          ) ) self%n_Tests           = n_Tests
<     IF ( PRESENT(n_Passed_Tests   ) ) self%n_Passed_Tests    = n_Passed_Tests
<     IF ( PRESENT(n_Failed_Tests   ) ) self%n_Failed_Tests    = n_Failed_Tests
<     IF ( PRESENT(n_AllTests       ) ) self%n_AllTests        = n_AllTests
<     IF ( PRESENT(n_Passed_AllTests) ) self%n_Passed_AllTests = n_Passed_AllTests
<     IF ( PRESENT(n_Failed_AllTests) ) self%n_Failed_AllTests = n_Failed_AllTests
---
>     IF ( PRESENT(Verbose          ) ) UnitTest%Verbose           = Verbose
>     IF ( PRESENT(Title            ) ) UnitTest%Title             = Title
>     IF ( PRESENT(Caller           ) ) UnitTest%Caller            = Caller
>     IF ( PRESENT(Level            ) ) UnitTest%Level             = Level
>     IF ( PRESENT(Procedure        ) ) UnitTest%Procedure         = Procedure
>     IF ( PRESENT(Message          ) ) UnitTest%Message           = Message
>     IF ( PRESENT(Test_Result      ) ) UnitTest%Test_Result       = Test_Result
>     IF ( PRESENT(n_Tests          ) ) UnitTest%n_Tests           = n_Tests
>     IF ( PRESENT(n_Passed_Tests   ) ) UnitTest%n_Passed_Tests    = n_Passed_Tests
>     IF ( PRESENT(n_Failed_Tests   ) ) UnitTest%n_Failed_Tests    = n_Failed_Tests
>     IF ( PRESENT(n_AllTests       ) ) UnitTest%n_AllTests        = n_AllTests
>     IF ( PRESENT(n_Passed_AllTests) ) UnitTest%n_Passed_AllTests = n_Passed_AllTests
>     IF ( PRESENT(n_Failed_AllTests) ) UnitTest%n_Failed_AllTests = n_Failed_AllTests
4976c2543
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
4979c2546
< !   UnitTest::Get_Property
---
> !       Get_Property
4982c2549
< !   Private method to get the properties of a UnitTest object.
---
> !       Private subroutine to get the properties of a UnitTest object.
4985c2552
< !   done using this method.
---
> !       done using this subroutine.
4988c2555,2557
< !   CALL utest_obj%Get_Property( Verbose           = Verbose          , &
---
> !       CALL Get_Property( &
> !         UnitTest, &
> !         Verbose           = Verbose          , &
5003c2572
< !   utest_obj:          UnitTest object.
---
> !       UnitTest:           UnitTest object.
5005c2574
< !                       CLASS:      UnitTest_type
---
> !                           TYPE:       TYPE(UnitTest_type)
5108c2677
<     self             , & ! Object
---
>     UnitTest         , & ! Object
5123c2692
<     CLASS(UnitTest_type)  , INTENT(IN)  :: self
---
>     TYPE(UnitTest_type)   , INTENT(IN)  :: UnitTest
5138,5150c2707,2719
<     IF ( PRESENT(Verbose          ) ) Verbose           = self%Verbose
<     IF ( PRESENT(Title            ) ) Title             = self%Title
<     IF ( PRESENT(Caller           ) ) Caller            = self%Caller
<     IF ( PRESENT(Level            ) ) Level             = self%Level
<     IF ( PRESENT(Procedure        ) ) Procedure         = self%Procedure
<     IF ( PRESENT(Message          ) ) Message           = self%Message
<     IF ( PRESENT(Test_Result      ) ) Test_Result       = self%Test_Result
<     IF ( PRESENT(n_Tests          ) ) n_Tests           = self%n_Tests
<     IF ( PRESENT(n_Passed_Tests   ) ) n_Passed_Tests    = self%n_Passed_Tests
<     IF ( PRESENT(n_Failed_Tests   ) ) n_Failed_Tests    = self%n_Failed_Tests
<     IF ( PRESENT(n_AllTests       ) ) n_AllTests        = self%n_AllTests
<     IF ( PRESENT(n_Passed_AllTests) ) n_Passed_AllTests = self%n_Passed_AllTests
<     IF ( PRESENT(n_Failed_AllTests) ) n_Failed_AllTests = self%n_Failed_AllTests
---
>     IF ( PRESENT(Verbose          ) ) Verbose           = UnitTest%Verbose
>     IF ( PRESENT(Title            ) ) Title             = UnitTest%Title
>     IF ( PRESENT(Caller           ) ) Caller            = UnitTest%Caller
>     IF ( PRESENT(Level            ) ) Level             = UnitTest%Level
>     IF ( PRESENT(Procedure        ) ) Procedure         = UnitTest%Procedure
>     IF ( PRESENT(Message          ) ) Message           = UnitTest%Message
>     IF ( PRESENT(Test_Result      ) ) Test_Result       = UnitTest%Test_Result
>     IF ( PRESENT(n_Tests          ) ) n_Tests           = UnitTest%n_Tests
>     IF ( PRESENT(n_Passed_Tests   ) ) n_Passed_Tests    = UnitTest%n_Passed_Tests
>     IF ( PRESENT(n_Failed_Tests   ) ) n_Failed_Tests    = UnitTest%n_Failed_Tests
>     IF ( PRESENT(n_AllTests       ) ) n_AllTests        = UnitTest%n_AllTests
>     IF ( PRESENT(n_Passed_AllTests) ) n_Passed_AllTests = UnitTest%n_Passed_AllTests
>     IF ( PRESENT(n_Failed_AllTests) ) n_Failed_AllTests = UnitTest%n_Failed_AllTests
5154c2723
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
5157c2726
< !   UnitTest::Test_Passed
---
> !       Test_Passed
5160c2729
< !   Private method to increment passed test counters.
---
> !       Subroutine to increment passed test counters.
5163c2732
< !   CALL utest_obj%Test_Passed()
---
> !       CALL Test_Passed( UnitTest )
5166c2735
< !   utest_obj:     UnitTest object.
---
> !       UnitTest:      UnitTest object.
5168c2737
< !                  CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
5172c2741
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
5174c2743
<   SUBROUTINE Test_Passed( self )
---
>   SUBROUTINE Test_Passed( UnitTest )
5176c2745
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
5181c2750
<     CALL self%Test_Increment()
---
>     CALL Test_Increment( UnitTest )
5185c2754,2755
<     CALL self%Get_Property( &
---
>     CALL Get_Property( &
>       UnitTest, &
5192c2762,2763
<     CALL self%Set_Property( &
---
>     CALL Set_Property( &
>       UnitTest, &
5199c2770
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
5202c2773
< !   UnitTest::Test_Failed
---
> !       Test_Failed
5205c2776
< !   Private method to increment failed test counters.
---
> !       Subroutine to increment failed test counters.
5208c2779
< !   CALL utest_obj%Test_Failed()
---
> !       CALL Test_Failed( UnitTest )
5211c2782
< !   utest_obj:     UnitTest object.
---
> !       UnitTest:      UnitTest object.
5213c2784
< !                  CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
5217c2788
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
5219c2790
<   SUBROUTINE Test_Failed( self )
---
>   SUBROUTINE Test_Failed( UnitTest )
5221c2792
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
5226c2797
<     CALL self%Test_Increment()
---
>     CALL Test_Increment( UnitTest )
5230c2801,2802
<     CALL self%Get_Property( &
---
>     CALL Get_Property( &
>       UnitTest, &
5237c2809,2810
<     CALL self%Set_Property( &
---
>     CALL Set_Property( &
>       UnitTest, &
5244c2817
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
5247c2820
< !   UnitTest::Test_Increment
---
> !       Test_Increment
5250c2823
< !   Private method to increment the test total counters.
---
> !       Subroutine to increment the test total counters.
5253c2826
< !   CALL utest_obj%Test_Increment()
---
> !       CALL Test_Increment( UnitTest )
5256c2829
< !   utest_obj:     UnitTest object.
---
> !       UnitTest:      UnitTest object.
5258c2831
< !                  CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
5262c2835
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
5264,5265c2837,2838
<   SUBROUTINE Test_Increment( self )
<     CLASS(UnitTest_type), INTENT(IN OUT) :: self
---
>   SUBROUTINE Test_Increment( UnitTest )
>     TYPE(UnitTest_type), INTENT(IN OUT) :: UnitTest
5268c2841,2842
<     CALL self%Get_Property( &
---
>     CALL Get_Property( &
>       UnitTest, &
5275c2849,2850
<     CALL self%Set_Property( &
---
>     CALL Set_Property( &
>       UnitTest, &
5281c2856
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
5284c2859
< !   UnitTest::Display_Message
---
> !       Display_Message
5287c2862
< !   Private method to display the unit test messages to stdout.
---
> !       Subroutine to display the unit test messages to stdout.
5290c2865
< !   CALL utest_obj%Display_Message()
---
> !       CALL Display_Message( UnitTest )
5293c2868
< !   utest_obj:     UnitTest object.
---
> !       UnitTest:      UnitTest object.
5295c2870
< !                  CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
5299c2874
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
5301,5302c2876,2877
<   SUBROUTINE Display_Message( self )
<     CLASS(UnitTest_type), INTENT(IN) :: self
---
>   SUBROUTINE Display_Message( UnitTest )
>     TYPE(UnitTest_type), INTENT(IN) :: UnitTest
5304,5315c2879,2891
<     INTEGER :: level
<     CHARACTER(SL) :: procedure
<     CHARACTER(SL) :: message
<     CHARACTER(SL) :: fmt
<     CHARACTER(SL) :: prefix
<     CHARACTER(SL) :: test_info
<     INTEGER :: n_spaces
< 
<     CALL self%Get_Property( &
<       Level     = level, &
<       Procedure = procedure, &
<       Message   = message )
---
>     INTEGER :: Level
>     CHARACTER(SL) :: Procedure
>     CHARACTER(SL) :: Message
>     CHARACTER(SL) :: Fmt
>     CHARACTER(SL) :: Prefix
>     CHARACTER(SL) :: Test_Info
>     INTEGER :: n_Spaces
> 
>     CALL Get_Property( &
>       UnitTest, &
>       Level = Level, &
>       Procedure = Procedure, &
>       Message = Message )
5318,5319c2894,2895
<     test_info = ''
<     SELECT CASE(level)
---
>     Test_Info = ''
>     SELECT CASE(Level)
5321,5322c2897,2898
<         prefix = '/'
<         n_spaces = 1
---
>         Prefix = '/'
>         n_Spaces = 1
5324,5325c2900,2901
<         prefix = '/,3x,14("-"),/'
<         n_spaces = 3
---
>         Prefix = '/,3x,14("-"),/'
>         n_Spaces = 3
5327,5329c2903,2905
<         prefix = ''
<         n_spaces = 5
<         CALL self%Test_Info_String( test_info )
---
>         Prefix = ''
>         n_Spaces = 5
>         CALL Test_Info_String( UnitTest, Test_Info )
5331,5332c2907,2908
<         prefix = ''
<         n_spaces = 3
---
>         Prefix = ''
>         n_Spaces = 3
5334,5335c2910,2911
<         prefix = '/,1x,16("="),/'
<         n_spaces = 1
---
>         Prefix = '/,1x,16("="),/'
>         n_Spaces = 1
5337,5339c2913,2915
<         level = INTERNAL_FAIL_LEVEL
<         prefix = '/,"INVALID MESSAGE LEVEL!!",/'
<         n_spaces = 15
---
>         Level = INTERNAL_FAIL_LEVEL
>         Prefix = '/,"INVALID MESSAGE LEVEL!!",/'
>         n_Spaces = 15
5343,5344c2919,2920
<     WRITE(fmt, '("(",a,i0,"x,""("",a,"") "",a,"": "",a,1x,a)")') TRIM(prefix), n_spaces
<     WRITE( *,FMT=fmt ) TRIM(MESSAGE_LEVEL(level)), TRIM(procedure), TRIM(test_info), TRIM(message)
---
>     WRITE(Fmt, '("(",a,i0,"x,""("",a,"") "",a,"": "",a,1x,a)")') TRIM(Prefix), n_Spaces
>     WRITE( *,FMT=Fmt ) TRIM(MESSAGE_LEVEL(Level)), TRIM(Procedure), TRIM(Test_Info), TRIM(Message)
5349c2925
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
5352c2928
< !   UnitTest::Test_Info_String
---
> !       Test_Info_String
5355c2931
< !   Private method to construct an info string for message output.
---
> !       Subroutine to construct an info string for message output.
5358c2934
< !   CALL utest_obj%Test_Info_String( info )
---
> !       CALL Test_Info_String( UnitTest, info )
5361c2937
< !   utest_obj:     UnitTest object.
---
> !       UnitTest:      UnitTest object.
5363c2939
< !                  CLASS:      UnitTest_type
---
> !                      TYPE:       TYPE(UnitTest_type)
5375c2951
< !--------------------------------------------------------------------------------
---
> !------------------------------------------------------------------------------
5377,5378c2953,2954
<   SUBROUTINE Test_Info_String( self, info )
<     CLASS(UnitTest_Type), INTENT(IN)  :: self
---
>   SUBROUTINE Test_Info_String( UnitTest, info )
>     TYPE(UnitTest_Type), INTENT(IN) :: UnitTest
5380,5384c2956,2960
<     INTEGER :: n_tests
<     CHARACTER(6) :: passfail
<     CALL self%Get_Property( n_Tests = n_Tests )
<     IF ( self%Passed() ) THEN
<       passfail = 'PASSED'
---
>     INTEGER :: n_Tests
>     CHARACTER(6) :: PassFail
>     CALL Get_Property( UnitTest, n_Tests = n_Tests )
>     IF ( UnitTest_Passed( UnitTest ) ) THEN
>       PassFail = 'PASSED'
5386c2962
<       passfail = 'FAILED'
---
>       PassFail = 'FAILED'
5388c2964
<     WRITE( info,'("Test#",i0,1x,a,".")') n_tests, passfail
---
>     WRITE( info,'("Test#",i0,1x,a,".")') n_Tests, PassFail
5390a2967,3019
> 
> !====================
> ! UTILITY PROCEDURES
> !====================
> 
> !------------------------------------------------------------------------------
> !
> ! NAME:
> !       Get_Multiplier
> !
> ! PURPOSE:
> !       Elemental function to compute the exponent multiplier of an input
> !       for use in scaling tolerance values for floating point comparisons.
> !
> ! CALLING SEQUENCE:
> !       e = Get_Multiplier(x)
> !
> ! INPUTS:
> !       x:             Number for which the exponent multiplier is required.
> !                      UNITS:      N/A
> !                      TYPE:       REAL(Single)   , or
> !                                  REAL(Double)
> !                      DIMENSION:  Scalar or any rank
> !                      ATTRIBUTES: INTENT(IN)
> !
> ! FUNCTION RESULT:
> !       e:             Exponent multiplier to use in scaling tolerance values.
> !                      UNITS:      N/A
> !                      TYPE:       Same as input x.
> !                      DIMENSION:  Same as input x.
> !
> !------------------------------------------------------------------------------
> 
>   ELEMENTAL FUNCTION realsp_get_multiplier(x) RESULT(e)
>     REAL(Single), INTENT(IN) :: x
>     REAL(Single) :: e
>     IF (x > 0.0_Single) THEN
>       e = 10.0_Single**FLOOR(LOG10(x))
>     ELSE
>       e = 1.0_Single
>     END IF
>   END FUNCTION realsp_get_multiplier
> 
>   ELEMENTAL FUNCTION realdp_get_multiplier(x) RESULT(e)
>     REAL(Double), INTENT(IN) :: x
>     REAL(Double) :: e
>     IF (x > 0.0_Double) THEN
>       e = 10.0_Double**FLOOR(LOG10(x))
>     ELSE
>       e = 1.0_Double
>     END IF
>   END FUNCTION realdp_get_multiplier
> 
diff -w ./Zeeman_Input_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Zeeman_Input_Define.f90
diff -w ./Zeeman_Utility.f90 ~/CRTM/clean/REL-2.2.3/libsrc/Zeeman_Utility.f90
diff -w ./iAtm_Define.f90 ~/CRTM/clean/REL-2.2.3/libsrc/iAtm_Define.f90
diff -w ./CRTM_Module.fpp ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Module.fpp
diff -w ./CRTM_Version.inc ~/CRTM/clean/REL-2.2.3/libsrc/CRTM_Version.inc
1c1
< 'v2.3.0'
---
> 'v2.2.3'
diff -w ./FitCoeff_Destroy.inc ~/CRTM/clean/REL-2.2.3/libsrc/FitCoeff_Destroy.inc
diff -w ./FitCoeff_Equal.inc ~/CRTM/clean/REL-2.2.3/libsrc/FitCoeff_Equal.inc
diff -w ./FitCoeff_Info.inc ~/CRTM/clean/REL-2.2.3/libsrc/FitCoeff_Info.inc
diff -w ./FitCoeff_ReadFile.inc ~/CRTM/clean/REL-2.2.3/libsrc/FitCoeff_ReadFile.inc
diff -w ./FitCoeff_SetValue.inc ~/CRTM/clean/REL-2.2.3/libsrc/FitCoeff_SetValue.inc
diff -w ./FitCoeff_WriteFile.inc ~/CRTM/clean/REL-2.2.3/libsrc/FitCoeff_WriteFile.inc


[Files not present in CRTM REL-2.2.3]

diff: /Users/bjohns/CRTM/clean/REL-2.2.3/libsrc/CRTM_CloudCover_Define.f90: No such file or directory
diff: /Users/bjohns/CRTM/clean/REL-2.2.3/libsrc/NESDIS_ATMS_SeaICE_LIB.f90: No such file or directory
diff: /Users/bjohns/CRTM/clean/REL-2.2.3/libsrc/NESDIS_ATMS_SeaICE_Module.f90: No such file or directory
