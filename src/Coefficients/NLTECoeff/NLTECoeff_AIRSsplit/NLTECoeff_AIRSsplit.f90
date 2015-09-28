!
! NLTECoeff_AIRSsplit
!
! Program to split the current single AIRS NLTE coefficient file into its
! respective module files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Sep-2015
!                       paul.vandelst@noaa.gov
!

PROGRAM NLTECoeff_AIRSsplit

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility       , ONLY: File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, &
                                 Program_Message, Display_Message
  USE NLTECoeff_Define   , ONLY: NLTECoeff_type, &
                                 NLTECoeff_Associated, &
                                 NLTECoeff_Destroy, &
                                 NLTECoeff_Create, &
                                 NLTECoeff_Inspect
  USE NLTECoeff_netCDF_IO, ONLY: NLTECoeff_netCDF_ReadFile, &
                                 NLTECoeff_netCDF_WriteFile
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'NLTECoeff_AIRSsplit'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  '$Id$'

  ! Module channel ranges
  INTEGER, PARAMETER :: M1B_CHANNEL_BEGIN = 2015
  INTEGER, PARAMETER :: M1B_CHANNEL_END   = 2144
  INTEGER, PARAMETER :: M2B_CHANNEL_BEGIN = 1865
  INTEGER, PARAMETER :: M2B_CHANNEL_END   = 2014

  ! Module sensor ids
  CHARACTER(*), PARAMETER :: M1B_SENSOR_ID   = 'airsM1b_aqua'
  CHARACTER(*), PARAMETER :: M2B_SENSOR_ID   = 'airsM2b_aqua'

  ! Input filename is fixed
  CHARACTER(*), PARAMETER :: INPUT_FILENAME = 'airs_aqua.NLTECoeff.nc'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(5000) :: history
  CHARACTER(256) :: msg, output_filename
  CHARACTER(20) :: sid
  INTEGER :: err_stat
  INTEGER :: i, i1, i2, n
  INTEGER :: n_module_nlte_channels, n_module_channels
  INTEGER :: min_c_index
  INTEGER, ALLOCATABLE :: idx_nlte(:)
  TYPE(NLTECoeff_type) :: sensor_nltecoeff, module_nltecoeff

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to split the current single AIRS NLTE coefficient '//&
                        'file into its respective module files.', &
                        '$Revision$' )

  ! Read the input file
  err_stat = NLTECoeff_netCDF_ReadFile( &
    INPUT_FILENAME   , &
    sensor_nltecoeff , &
    History = history  )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading NLTECoeff file '//INPUT_FILENAME
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Loop over the number of modules to extract
  DO n = 1, 2  ! Only two modules


    ! Set the module parameters
    IF ( n == 1 ) THEN
      i1 = M1B_CHANNEL_BEGIN
      i2 = M1B_CHANNEL_END
      sid = 'airsM1b_aqua'
    ELSE
      i1 = M2B_CHANNEL_BEGIN
      i2 = M2B_CHANNEL_END
      sid = 'airsM2b_aqua'
    END IF
    n_module_channels = i2 - i1 + 1
    output_filename = TRIM(sid)//'.NLTECoeff.nc'


    ! How many NLTE channels for the current?
    n_module_nlte_channels = COUNT(sensor_nltecoeff%Is_NLTE_Channel(i1:i2))

    WRITE(*,'(/5x,"Extracting ",i0," NLTE channels out of ",i0," module channels for ",a)' ) &
            n_module_nlte_channels, n_module_channels, TRIM(sid)


    ! Create the module NLTECoeff object
    CALL NLTECoeff_Create( &
      module_nltecoeff                , &
      sensor_nltecoeff%n_Predictors   , &
      sensor_nltecoeff%n_Sensor_Angles, &
      sensor_nltecoeff%n_Solar_Angles , &
      n_module_nlte_channels          , &
      n_module_channels                  )
    IF ( .NOT. NLTECoeff_Associated(module_nltecoeff) ) THEN
      msg = 'Error allocating '//TRIM(sid)//' NLTECoeff object'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Copy over the channel independent data
    module_nltecoeff%Version              = sensor_nltecoeff%Version
    module_nltecoeff%Sensor_Id            = sid
    module_nltecoeff%WMO_Satellite_ID     = sensor_nltecoeff%WMO_Satellite_ID
    module_nltecoeff%WMO_Sensor_ID        = sensor_nltecoeff%WMO_Sensor_ID
    module_nltecoeff%Upper_Plevel         = sensor_nltecoeff%Upper_Plevel
    module_nltecoeff%Lower_Plevel         = sensor_nltecoeff%Lower_Plevel
    module_nltecoeff%Min_Tm               = sensor_nltecoeff%Min_Tm
    module_nltecoeff%Max_Tm               = sensor_nltecoeff%Max_Tm
    module_nltecoeff%Mean_Tm              = sensor_nltecoeff%Mean_Tm
    module_nltecoeff%Secant_Sensor_Zenith = sensor_nltecoeff%Secant_Sensor_Zenith
    module_nltecoeff%Secant_Solar_Zenith  = sensor_nltecoeff%Secant_Solar_Zenith

    ! Copy over the subset of the full sensor range of channel data
    module_nltecoeff%Sensor_Channel  = sensor_nltecoeff%Sensor_Channel(i1:i2)
    module_nltecoeff%Is_NLTE_Channel = sensor_nltecoeff%Is_NLTE_Channel(i1:i2)
    module_nltecoeff%C_Index         = sensor_nltecoeff%C_Index(i1:i2)


    ! C_index needs special treatment because it should start from 1 within a module
    ! ...First find the smallest +ve, non-zero value
    min_c_index = MINVAL(module_nltecoeff%C_Index, DIM=1, MASK=(module_nltecoeff%C_Index > 0))
    WHERE(module_nltecoeff%C_Index > 0)
      module_nltecoeff%C_Index = module_nltecoeff%C_Index - min_c_index + 1
    END WHERE


    ! Copy over the non-LTE channel data
    ! ...Allocsate and fill a non-LTE index array
    ALLOCATE(idx_nlte(n_module_nlte_channels))
    idx_nlte = PACK([(i,i=1,sensor_nltecoeff%n_NLTE_Channels)], &
                    (sensor_nltecoeff%NLTE_Channel >= i1) .AND. &
                    (sensor_nltecoeff%NLTE_Channel <= i2) )
    ! ...Copy over the data
    module_nltecoeff%NLTE_Channel = sensor_nltecoeff%NLTE_Channel(idx_nlte)
    module_nltecoeff%C            = sensor_nltecoeff%C(:,:,:,idx_nlte)


    ! Output inspect result
    CALL NLTECoeff_Inspect(module_nltecoeff)


    ! Write the output datafile
    err_stat = NLTECoeff_netCDF_WriteFile( &
      output_filename, &
      module_nltecoeff, &
      Title   = 'non-LTE correction coefficients for '//TRIM(sid), &
      History = PROGRAM_VERSION_ID//'; '//TRIM(history), &
      Comment = 'Individual AIRS module data extracted from the all-channel file '//&
                INPUT_FILENAME )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing NLTECoeff file '//TRIM(output_filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Clean up
    DEALLOCATE( idx_nlte )
    CALL NLTECoeff_Destroy( module_nltecoeff )


    ! Pause
    IF ( n < 2 ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to continue...")')
      READ(*,*)
    END IF

  END DO


  ! Final clean up
  CALL NLTECoeff_Destroy( sensor_nltecoeff )

END PROGRAM NLTECoeff_AIRSsplit
