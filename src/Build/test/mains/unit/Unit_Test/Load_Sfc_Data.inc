  !
  ! Include file containing an internal subprogam to load some test surface data
  !
  SUBROUTINE Load_Sfc_Data()
    
    
    ! 4a.0 Surface type definitions for default SfcOptics definitions
    !      For IR and VIS, this is the NPOESS reflectivities.
    ! ---------------------------------------------------------------
    INTEGER, PARAMETER :: TUNDRA_SURFACE_TYPE         = 10  ! NPOESS Land surface type for IR/VIS Land SfcOptics
    INTEGER, PARAMETER :: SCRUB_SURFACE_TYPE          =  7  ! NPOESS Land surface type for IR/VIS Land SfcOptics
    INTEGER, PARAMETER :: COARSE_SOIL_TYPE            =  1  ! Soil type                for MW land SfcOptics
    INTEGER, PARAMETER :: GROUNDCOVER_VEGETATION_TYPE =  7  ! Vegetation type          for MW Land SfcOptics
    INTEGER, PARAMETER :: BARE_SOIL_VEGETATION_TYPE   = 11  ! Vegetation type          for MW Land SfcOptics
    INTEGER, PARAMETER :: SEA_WATER_TYPE              =  1  ! Water type               for all SfcOptics
    INTEGER, PARAMETER :: FRESH_SNOW_TYPE             =  2  ! NPOESS Snow type         for IR/VIS SfcOptics
    INTEGER, PARAMETER :: FRESH_ICE_TYPE              =  1  ! NPOESS Ice type          for IR/VIS SfcOptics



    ! 4a.1 Profile #1
    ! ---------------
    ! ...Land surface characteristics
    sfc(1)%Land_Coverage     = 0.1_fp
    sfc(1)%Land_Type         = TUNDRA_SURFACE_TYPE
    sfc(1)%Land_Temperature  = 272.0_fp
    sfc(1)%Lai               = 0.17_fp
    sfc(1)%Soil_Type         = COARSE_SOIL_TYPE
    sfc(1)%Vegetation_Type   = GROUNDCOVER_VEGETATION_TYPE
    ! ...Water surface characteristics
    sfc(1)%Water_Coverage    = 0.5_fp
    sfc(1)%Water_Type        = SEA_WATER_TYPE
    sfc(1)%Water_Temperature = 275.0_fp
    ! ...Snow coverage characteristics
    sfc(1)%Snow_Coverage    = 0.25_fp
    sfc(1)%Snow_Type        = FRESH_SNOW_TYPE
    sfc(1)%Snow_Temperature = 265.0_fp
    ! ...Ice surface characteristics
    sfc(1)%Ice_Coverage    = 0.15_fp
    sfc(1)%Ice_Type        = FRESH_ICE_TYPE
    sfc(1)%Ice_Temperature = 269.0_fp



    ! 4a.2 Profile #2
    ! ---------------
    ! Surface data
    sfc(2)%Land_Coverage    = 1.0_fp
    sfc(2)%Land_Type        = SCRUB_SURFACE_TYPE
    sfc(2)%Land_Temperature = 318.0_fp
    sfc(2)%Lai              = 0.65_fp
    sfc(2)%Soil_Type        = COARSE_SOIL_TYPE
    sfc(2)%Vegetation_Type  = BARE_SOIL_VEGETATION_TYPE

  END SUBROUTINE Load_Sfc_Data
