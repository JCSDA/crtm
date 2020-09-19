!
! LBLRTM_Fhdr_netCDF_IO
!
! Module containing routine to read and write LBLRTM Fhdr objects as
! groups to a netCDF format file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 19-Feb-2014
!                       paul.vandelst@noaa.gov
!

MODULE LBLRTM_Fhdr_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds        , ONLY: FP, IP, DP => Double, Long
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE String_Utility    , ONLY: StrClean
  USE LBLRTM_Parameters , ONLY: N_MOL => LBLRTM_MAX_N_MOLECULES
  USE LBLRTM_Fhdr_Define, ONLY: LBLRTM_Fhdr_type    , &
                                LBLRTM_Fhdr_IsValid , &
                                LBLRTM_Fhdr_SetValid, &
                                LBLRTM_Fhdr_Destroy , &
                                LBLRTM_Fhdr_Inspect
  USE netcdf
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Procedures
  PUBLIC :: LBLRTM_Fhdr_netCDF_ReadGroup
  PUBLIC :: LBLRTM_Fhdr_netCDF_WriteGroup
  PUBLIC :: LBLRTM_Fhdr_netCDF_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Default message string length
  INTEGER, PARAMETER :: ML = 1024
  ! Literal constants
  REAL(DP), PARAMETER :: ZERO = 0.0_DP
  REAL(DP), PARAMETER :: ONE  = 1.0_DP
  ! Extra parameters not in netCDF(?)
  INTEGER, PARAMETER :: MAX_N_GROUPS = 8096

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME   = 'Title'
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME = 'History'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME = 'Comment'


  ! Dimension names
  CHARACTER(*), PARAMETER :: MOLECULE_DIMNAME   = 'n_molecules'
  CHARACTER(*), PARAMETER :: ANCILLARY_DIMNAME  = 'n_ancillary'
  CHARACTER(*), PARAMETER :: UID_STRLEN_DIMNAME = 'uid_strlen'
  CHARACTER(*), PARAMETER :: SL_STRLEN_DIMNAME  = 'sl_strlen'


  ! Variable names
  CHARACTER(*), PARAMETER :: USER_ID_VARNAME          = 'User_ID'
  CHARACTER(*), PARAMETER :: COL_SCALE_FACTOR_VARNAME = 'Column_Scale_Factor'
  CHARACTER(*), PARAMETER :: AVG_LAYER_PRES_VARNAME   = 'Average_Layer_Pressure'
  CHARACTER(*), PARAMETER :: AVG_LAYER_TEMP_VARNAME   = 'Average_Layer_Temperature'
  CHARACTER(*), PARAMETER :: MOL_ID_VARNAME           = 'Molecule_Id'
  CHARACTER(*), PARAMETER :: MOL_COL_DENS_VARNAME     = 'Molecule_Column_Density'
  CHARACTER(*), PARAMETER :: BROAD_COL_DENS_VARNAME   = 'Broadening_Gas_Column_Density'
  CHARACTER(*), PARAMETER :: FREQ_INTERVAL_VARNAME    = 'Frequency_Interval'
  CHARACTER(*), PARAMETER :: BEGIN_FREQ_VARNAME       = 'Begin_Frequency'
  CHARACTER(*), PARAMETER :: END_FREQ_VARNAME         = 'End_Frequency'
  CHARACTER(*), PARAMETER :: BDRY_TEMP_VARNAME        = 'Boundary_Temperature'
  CHARACTER(*), PARAMETER :: BDRY_EMIS_VARNAME        = 'Boundary_Emissivity'
  CHARACTER(*), PARAMETER :: N_MOLECULES_VARNAME      = 'n_Molecules'
  CHARACTER(*), PARAMETER :: N_LAYER_VARNAME          = 'n_Layer'
  CHARACTER(*), PARAMETER :: OD_LAYER_FLAG_VARNAME    = 'OD_Layering_Control_Flag'
  CHARACTER(*), PARAMETER :: CALC_DATE_VARNAME        = 'Calculation_Date'
  CHARACTER(*), PARAMETER :: CALC_TIME_VARNAME        = 'Calculation_Time'
  CHARACTER(*), PARAMETER :: ANCILLARY_VARNAME        = 'ancillary'
  ! ...The run flags
  CHARACTER(*), PARAMETER :: HIRAC_VARNAME = 'hirac'
  CHARACTER(*), PARAMETER :: LBLF4_VARNAME = 'lblf4'
  CHARACTER(*), PARAMETER :: XSCNT_VARNAME = 'xscnt'
  CHARACTER(*), PARAMETER :: AERSL_VARNAME = 'aersl'
  CHARACTER(*), PARAMETER :: EMIT_VARNAME  = 'emit'
  CHARACTER(*), PARAMETER :: SCAN_VARNAME  = 'scan'
  CHARACTER(*), PARAMETER :: PLOT_VARNAME  = 'plot'
  CHARACTER(*), PARAMETER :: PATH_VARNAME  = 'path'
  CHARACTER(*), PARAMETER :: JRAD_VARNAME  = 'jrad'
  CHARACTER(*), PARAMETER :: TEST_VARNAME  = 'test'
  CHARACTER(*), PARAMETER :: MERGE_VARNAME = 'merge'
  CHARACTER(*), PARAMETER :: SCNID_VARNAME = 'scnid'
  CHARACTER(*), PARAMETER :: HWHM_VARNAME  = 'hwhm'
  CHARACTER(*), PARAMETER :: IDABS_VARNAME = 'idabs'
  CHARACTER(*), PARAMETER :: ATM_VARNAME   = 'atm'
  CHARACTER(*), PARAMETER :: LAYR1_VARNAME = 'layr1'
  CHARACTER(*), PARAMETER :: NLAYR_VARNAME = 'nlayr'


  ! Variable long name attribute
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: USER_ID_LONGNAME          = 'User ID'
  CHARACTER(*), PARAMETER :: COL_SCALE_FACTOR_LONGNAME = 'Column Scale Factor'
  CHARACTER(*), PARAMETER :: AVG_LAYER_PRES_LONGNAME   = 'Average Layer Pressure'
  CHARACTER(*), PARAMETER :: AVG_LAYER_TEMP_LONGNAME   = 'Average Layer Temperature'
  CHARACTER(*), PARAMETER :: MOL_ID_LONGNAME           = 'Molecule Id'
  CHARACTER(*), PARAMETER :: MOL_COL_DENS_LONGNAME     = 'Molecule Column Density'
  CHARACTER(*), PARAMETER :: BROAD_COL_DENS_LONGNAME   = 'Broadening Gas Column Density'
  CHARACTER(*), PARAMETER :: FREQ_INTERVAL_LONGNAME    = 'Frequency Interval'
  CHARACTER(*), PARAMETER :: BEGIN_FREQ_LONGNAME       = 'Begin Frequency'
  CHARACTER(*), PARAMETER :: END_FREQ_LONGNAME         = 'End Frequency'
  CHARACTER(*), PARAMETER :: BDRY_TEMP_LONGNAME        = 'Boundary Temperature'
  CHARACTER(*), PARAMETER :: BDRY_EMIS_LONGNAME        = 'Boundary Emissivity'
  CHARACTER(*), PARAMETER :: N_MOLECULES_LONGNAME      = 'Number of molecules'
  CHARACTER(*), PARAMETER :: N_LAYER_LONGNAME          = 'Number of layer'
  CHARACTER(*), PARAMETER :: OD_LAYER_FLAG_LONGNAME    = 'OD Layering Control Flag'
  CHARACTER(*), PARAMETER :: CALC_DATE_LONGNAME        = 'Calculation Date'
  CHARACTER(*), PARAMETER :: CALC_TIME_LONGNAME        = 'Calculation Time'
  CHARACTER(*), PARAMETER :: ANCILLARY_LONGNAME        = 'ancillary'
  ! ...The run flags
  CHARACTER(*), PARAMETER :: HIRAC_LONGNAME = 'hirac run flag'
  CHARACTER(*), PARAMETER :: LBLF4_LONGNAME = 'lblf4 run flag'
  CHARACTER(*), PARAMETER :: XSCNT_LONGNAME = 'xscnt run flag'
  CHARACTER(*), PARAMETER :: AERSL_LONGNAME = 'aersl run flag'
  CHARACTER(*), PARAMETER :: EMIT_LONGNAME  = 'emit run flag'
  CHARACTER(*), PARAMETER :: SCAN_LONGNAME  = 'scan run flag'
  CHARACTER(*), PARAMETER :: PLOT_LONGNAME  = 'plot run flag'
  CHARACTER(*), PARAMETER :: PATH_LONGNAME  = 'path run flag'
  CHARACTER(*), PARAMETER :: JRAD_LONGNAME  = 'jrad run flag'
  CHARACTER(*), PARAMETER :: TEST_LONGNAME  = 'test run flag'
  CHARACTER(*), PARAMETER :: MERGE_LONGNAME = 'merge run flag'
  CHARACTER(*), PARAMETER :: SCNID_LONGNAME = 'scnid run flag'
  CHARACTER(*), PARAMETER :: HWHM_LONGNAME  = 'hwhm run flag'
  CHARACTER(*), PARAMETER :: IDABS_LONGNAME = 'idabs run flag'
  CHARACTER(*), PARAMETER :: ATM_LONGNAME   = 'atm run flag'
  CHARACTER(*), PARAMETER :: LAYR1_LONGNAME = 'layr1 run flag'
  CHARACTER(*), PARAMETER :: NLAYR_LONGNAME = 'nlayr run flag'


  ! Variable description attribute
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: USER_ID_DESCRIPTION          = 'User Identification string'
  CHARACTER(*), PARAMETER :: COL_SCALE_FACTOR_DESCRIPTION = 'Column profile amount scaling factor'
  CHARACTER(*), PARAMETER :: AVG_LAYER_PRES_DESCRIPTION   = 'Average layer pressure'
  CHARACTER(*), PARAMETER :: AVG_LAYER_TEMP_DESCRIPTION   = 'Average layer temperature'
  CHARACTER(*), PARAMETER :: MOL_ID_DESCRIPTION           = 'Molecule identification string'
  CHARACTER(*), PARAMETER :: MOL_COL_DENS_DESCRIPTION     = 'Molecule column density'
  CHARACTER(*), PARAMETER :: BROAD_COL_DENS_DESCRIPTION   = 'Broadening gas column density'
  CHARACTER(*), PARAMETER :: FREQ_INTERVAL_DESCRIPTION    = 'Calculation frequency interval'
  CHARACTER(*), PARAMETER :: BEGIN_FREQ_DESCRIPTION       = 'Calculation begin frequency'
  CHARACTER(*), PARAMETER :: END_FREQ_DESCRIPTION         = 'Calculation end frequency'
  CHARACTER(*), PARAMETER :: BDRY_TEMP_DESCRIPTION        = 'Boundary temperature'
  CHARACTER(*), PARAMETER :: BDRY_EMIS_DESCRIPTION        = 'Boundary emissivity'
  CHARACTER(*), PARAMETER :: N_MOLECULES_DESCRIPTION      = 'Number of gaseous absorbers included used in calculation'
  CHARACTER(*), PARAMETER :: N_LAYER_DESCRIPTION          = 'Number of atmospheric layer'
  CHARACTER(*), PARAMETER :: OD_LAYER_FLAG_DESCRIPTION    = 'Optical depth layering control flag'
  CHARACTER(*), PARAMETER :: CALC_DATE_DESCRIPTION        = 'Calculation date'
  CHARACTER(*), PARAMETER :: CALC_TIME_DESCRIPTION        = 'Calculation time'
  CHARACTER(*), PARAMETER :: ANCILLARY_DESCRIPTION        = 'ancillary'
  ! ...The run flags
  CHARACTER(*), PARAMETER :: HIRAC_DESCRIPTION = 'LBLRTM control - hirac run flag'
  CHARACTER(*), PARAMETER :: LBLF4_DESCRIPTION = 'LBLRTM control - lblf4 run flag'
  CHARACTER(*), PARAMETER :: XSCNT_DESCRIPTION = 'LBLRTM control - xscnt run flag'
  CHARACTER(*), PARAMETER :: AERSL_DESCRIPTION = 'LBLRTM control - aersl run flag'
  CHARACTER(*), PARAMETER :: EMIT_DESCRIPTION  = 'LBLRTM control - emit run flag'
  CHARACTER(*), PARAMETER :: SCAN_DESCRIPTION  = 'LBLRTM control - scan run flag'
  CHARACTER(*), PARAMETER :: PLOT_DESCRIPTION  = 'LBLRTM control - plot run flag'
  CHARACTER(*), PARAMETER :: PATH_DESCRIPTION  = 'LBLRTM control - path run flag'
  CHARACTER(*), PARAMETER :: JRAD_DESCRIPTION  = 'LBLRTM control - jrad run flag'
  CHARACTER(*), PARAMETER :: TEST_DESCRIPTION  = 'LBLRTM control - test run flag'
  CHARACTER(*), PARAMETER :: MERGE_DESCRIPTION = 'LBLRTM control - merge run flag'
  CHARACTER(*), PARAMETER :: SCNID_DESCRIPTION = 'LBLRTM control - scnid run flag'
  CHARACTER(*), PARAMETER :: HWHM_DESCRIPTION  = 'LBLRTM control - hwhm run flag'
  CHARACTER(*), PARAMETER :: IDABS_DESCRIPTION = 'LBLRTM control - idabs run flag'
  CHARACTER(*), PARAMETER :: ATM_DESCRIPTION   = 'LBLRTM control - atm run flag'
  CHARACTER(*), PARAMETER :: LAYR1_DESCRIPTION = 'LBLRTM control - layr1 run flag'
  CHARACTER(*), PARAMETER :: NLAYR_DESCRIPTION = 'LBLRTM control - nlayr run flag'


  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: USER_ID_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: COL_SCALE_FACTOR_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: AVG_LAYER_PRES_UNITS   = 'hPa'
  CHARACTER(*), PARAMETER :: AVG_LAYER_TEMP_UNITS   = 'K'
  CHARACTER(*), PARAMETER :: MOL_ID_UNITS           = 'N/A'
  CHARACTER(*), PARAMETER :: MOL_COL_DENS_UNITS     = 'mol/cm^2'
  CHARACTER(*), PARAMETER :: BROAD_COL_DENS_UNITS   = 'mol/cm^2'
  CHARACTER(*), PARAMETER :: FREQ_INTERVAL_UNITS    = 'cm^-1'
  CHARACTER(*), PARAMETER :: BEGIN_FREQ_UNITS       = 'cm^-1'
  CHARACTER(*), PARAMETER :: END_FREQ_UNITS         = 'cm^-1'
  CHARACTER(*), PARAMETER :: BDRY_TEMP_UNITS        = 'K'
  CHARACTER(*), PARAMETER :: BDRY_EMIS_UNITS        = 'dimensionless'
  CHARACTER(*), PARAMETER :: N_MOLECULES_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: N_LAYER_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: OD_LAYER_FLAG_UNITS    = 'N/A'
  CHARACTER(*), PARAMETER :: CALC_DATE_UNITS        = 'N/A'
  CHARACTER(*), PARAMETER :: CALC_TIME_UNITS        = 'N/A'
  CHARACTER(*), PARAMETER :: ANCILLARY_UNITS        = 'N/A'
  ! ...The run flags
  CHARACTER(*), PARAMETER :: HIRAC_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: LBLF4_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: XSCNT_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: AERSL_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: EMIT_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: SCAN_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: PLOT_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: PATH_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: JRAD_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: TEST_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: MERGE_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: SCNID_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: HWHM_UNITS  = 'N/A'
  CHARACTER(*), PARAMETER :: IDABS_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: ATM_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: LAYR1_UNITS = 'N/A'
  CHARACTER(*), PARAMETER :: NLAYR_UNITS = 'N/A'


  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  CHARACTER(*), PARAMETER :: USER_ID_FILLVALUE          = NF90_FILL_CHAR
  REAL(DP)    , PARAMETER :: COL_SCALE_FACTOR_FILLVALUE = 0.0_DP
  REAL(FP)    , PARAMETER :: AVG_LAYER_PRES_FILLVALUE   = 0.0_FP
  REAL(FP)    , PARAMETER :: AVG_LAYER_TEMP_FILLVALUE   = 0.0_FP
  CHARACTER(*), PARAMETER :: MOL_ID_FILLVALUE           = NF90_FILL_CHAR
  REAL(FP)    , PARAMETER :: MOL_COL_DENS_FILLVALUE     = 0.0_FP
  REAL(FP)    , PARAMETER :: BROAD_COL_DENS_FILLVALUE   = 0.0_FP
  REAL(FP)    , PARAMETER :: FREQ_INTERVAL_FILLVALUE    = 0.0_FP
  REAL(DP)    , PARAMETER :: BEGIN_FREQ_FILLVALUE       = 0.0_DP
  REAL(DP)    , PARAMETER :: END_FREQ_FILLVALUE         = 0.0_DP
  REAL(FP)    , PARAMETER :: BDRY_TEMP_FILLVALUE        = 0.0_FP
  REAL(FP)    , PARAMETER :: BDRY_EMIS_FILLVALUE        = 0.0_FP
  INTEGER(IP) , PARAMETER :: N_MOLECULES_FILLVALUE      = 0_IP
  INTEGER(IP) , PARAMETER :: N_LAYER_FILLVALUE          = 0_IP
  INTEGER(IP) , PARAMETER :: OD_LAYER_FLAG_FILLVALUE    = 0_IP
  CHARACTER(*), PARAMETER :: CALC_DATE_FILLVALUE        = NF90_FILL_CHAR
  CHARACTER(*), PARAMETER :: CALC_TIME_FILLVALUE        = NF90_FILL_CHAR
  CHARACTER(*), PARAMETER :: ANCILLARY_FILLVALUE        = NF90_FILL_CHAR
  ! ...The run flags
  INTEGER(IP), PARAMETER :: HIRAC_FILLVALUE = 0_IP
  INTEGER(IP), PARAMETER :: LBLF4_FILLVALUE = 0_IP
  INTEGER(IP), PARAMETER :: XSCNT_FILLVALUE = 0_IP
  INTEGER(IP), PARAMETER :: AERSL_FILLVALUE = 0_IP
  INTEGER(IP), PARAMETER :: EMIT_FILLVALUE  = 0_IP
  INTEGER(IP), PARAMETER :: SCAN_FILLVALUE  = 0_IP
  INTEGER(IP), PARAMETER :: PLOT_FILLVALUE  = 0_IP
  INTEGER(IP), PARAMETER :: PATH_FILLVALUE  = 0_IP
  INTEGER(IP), PARAMETER :: JRAD_FILLVALUE  = 0_IP
  INTEGER(IP), PARAMETER :: TEST_FILLVALUE  = 0_IP
  INTEGER(IP), PARAMETER :: MERGE_FILLVALUE = 0_IP
  REAL(FP)   , PARAMETER :: SCNID_FILLVALUE = 0.0_FP
  REAL(FP)   , PARAMETER :: HWHM_FILLVALUE  = 0.0_FP
  INTEGER(IP), PARAMETER :: IDABS_FILLVALUE = 0_IP
  INTEGER(IP), PARAMETER :: ATM_FILLVALUE   = 0_IP
  INTEGER(IP), PARAMETER :: LAYR1_FILLVALUE = 0_IP
  INTEGER(IP), PARAMETER :: NLAYR_FILLVALUE = 0_IP


  ! Variable netCDF datatypes
  INTEGER(Long), PARAMETER :: USER_ID_TYPE          = NF90_CHAR
  INTEGER(Long), PARAMETER :: COL_SCALE_FACTOR_TYPE = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: AVG_LAYER_PRES_TYPE   = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: AVG_LAYER_TEMP_TYPE   = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: MOL_ID_TYPE           = NF90_CHAR
  INTEGER(Long), PARAMETER :: MOL_COL_DENS_TYPE     = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: BROAD_COL_DENS_TYPE   = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: FREQ_INTERVAL_TYPE    = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: BEGIN_FREQ_TYPE       = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: END_FREQ_TYPE         = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: BDRY_TEMP_TYPE        = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: BDRY_EMIS_TYPE        = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: N_MOLECULES_TYPE      = NF90_INT
  INTEGER(Long), PARAMETER :: N_LAYER_TYPE          = NF90_INT
  INTEGER(Long), PARAMETER :: OD_LAYER_FLAG_TYPE    = NF90_INT
  INTEGER(Long), PARAMETER :: CALC_DATE_TYPE        = NF90_CHAR
  INTEGER(Long), PARAMETER :: CALC_TIME_TYPE        = NF90_CHAR
  INTEGER(Long), PARAMETER :: ANCILLARY_TYPE        = NF90_CHAR
  ! ...The run flags
  INTEGER(Long), PARAMETER :: HIRAC_TYPE = NF90_INT
  INTEGER(Long), PARAMETER :: LBLF4_TYPE = NF90_INT
  INTEGER(Long), PARAMETER :: XSCNT_TYPE = NF90_INT
  INTEGER(Long), PARAMETER :: AERSL_TYPE = NF90_INT
  INTEGER(Long), PARAMETER :: EMIT_TYPE  = NF90_INT
  INTEGER(Long), PARAMETER :: SCAN_TYPE  = NF90_INT
  INTEGER(Long), PARAMETER :: PLOT_TYPE  = NF90_INT
  INTEGER(Long), PARAMETER :: PATH_TYPE  = NF90_INT
  INTEGER(Long), PARAMETER :: JRAD_TYPE  = NF90_INT
  INTEGER(Long), PARAMETER :: TEST_TYPE  = NF90_INT
  INTEGER(Long), PARAMETER :: MERGE_TYPE = NF90_INT
  INTEGER(Long), PARAMETER :: SCNID_TYPE = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: HWHM_TYPE  = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: IDABS_TYPE = NF90_INT
  INTEGER(Long), PARAMETER :: ATM_TYPE   = NF90_INT
  INTEGER(Long), PARAMETER :: LAYR1_TYPE = NF90_INT
  INTEGER(Long), PARAMETER :: NLAYR_TYPE = NF90_INT
        
        
CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PUBLIC MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

  !----------------------------------------------------------
  ! Function to write an LBLRTM File Header object as a group
  !----------------------------------------------------------
  FUNCTION LBLRTM_Fhdr_netCDF_WriteGroup( &
    Fhdr     , &  ! Input
    FileId   , &  ! Input
    GroupName, &  ! Optional input
    Quiet    , &  ! Optional input
    Debug    ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(LBLRTM_Fhdr_type), INTENT(IN) :: Fhdr
    INTEGER(Long),          INTENT(IN) :: FileId
    CHARACTER(*), OPTIONAL, INTENT(IN) :: GroupName
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Fhdr_netCDF_IO::WriteGroup'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: group_name
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER(Long) :: nf90_stat
    INTEGER(Long) :: groupid
    INTEGER(Long) :: n_mol_dimid, n_ancillary_dimid
    INTEGER(Long) :: uid_strlen_dimid, sl_strlen_dimid

    ! Setup
    err_stat = SUCCESS
    ! ...Check structure
    IF ( .NOT. (LBLRTM_Fhdr_IsValid( Fhdr )) ) THEN
      msg = 'LBLRTM Fhdr object is invalid. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check GroupName argument, defining default.
    group_name = 'Fhdr'
    IF ( PRESENT(GroupName) ) THEN
      group_name = ADJUSTL(GroupName)
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(debug) ) debug_output = debug
    IF ( debug_output ) THEN
      CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)
      noisy = .TRUE.
    END IF


    ! Create a new group for the file header data
    nf90_stat = NF90_DEF_GRP( &
      fileid, &
      group_name, &
      groupid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(group_name)//' group - '//&
            ' - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Define the dimensions for the group
    err_stat = DefineDimensions( &
      Fhdr             , &
      groupid          , &
      n_mol_dimid      , &
      n_ancillary_dimid, &
      uid_strlen_dimid , &
      sl_strlen_dimid    )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error defining dimensions for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Define the variables for the group
    err_stat = DefineVariables( &
      groupid          , &
      n_mol_dimid      , &
      n_ancillary_dimid, &
      uid_strlen_dimid , &
      sl_strlen_dimid    )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error defining variables for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Take netCDF file out of define mode
    nf90_stat = NF90_ENDDEF( fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error taking file out of define mode to write the '//&
            TRIM(group_name)//' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the variables for the group
    err_stat = WriteVariables( Fhdr, groupid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing variables for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Put netCDF file back into define mode
    nf90_stat = NF90_REDEF( fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error putting file back into define mode after writing the '//&
            TRIM(group_name)//' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      nf90_stat = NF90_CLOSE( fileid )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION LBLRTM_Fhdr_netCDF_WriteGroup


  !---------------------------------------------------------
  ! Function to read an LBLRTM File Header object as a group
  !---------------------------------------------------------
  FUNCTION LBLRTM_Fhdr_netCDF_ReadGroup( &
    Fhdr     , &  ! Output
    FileId   , &  ! Input
    GroupName, &  ! Optional input
    Quiet    , &  ! Optional input
    Debug    ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(LBLRTM_Fhdr_type), INTENT(OUT) :: Fhdr
    INTEGER(Long),          INTENT(IN)  :: FileId
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: GroupName
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Fhdr_netCDF_IO::ReadGroup'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: group_name
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER(Long) :: nf90_stat
    INTEGER(Long) :: groupid

    ! Setup
    err_stat = SUCCESS
    ! ...Check GroupName argument, defining default.
    group_name = 'Fhdr'
    IF ( PRESENT(GroupName) ) THEN
      group_name = ADJUSTL(GroupName)
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(debug) ) debug_output = debug
    IF ( debug_output ) THEN
      CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)
      noisy = .TRUE.
    END IF


    ! Get the group id
    nf90_stat = NF90_INQ_GRP_NCID(fileid, group_name, groupid)
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(group_name)//' group for its group id - '//&
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the variables for the group
    err_stat = ReadVariables( Fhdr, groupid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading variables for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Tag object as valid
    CALL LBLRTM_Fhdr_SetValid(Fhdr)
    IF ( debug_output ) CALL LBLRTM_Fhdr_Inspect(fhdr)

  CONTAINS

    SUBROUTINE Read_CleanUp()
      CALL LBLRTM_Fhdr_Destroy(Fhdr)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION LBLRTM_Fhdr_netCDF_ReadGroup


  !------------------------------------------------
  ! Subroutine to return module version information
  !------------------------------------------------
  SUBROUTINE LBLRTM_Fhdr_netCDF_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LBLRTM_Fhdr_netCDF_IOVersion


!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE MODULE ROUTINES ##                     ##
!##                                                                            ##
!################################################################################
!################################################################################

  INCLUDE 'LBLRTM_Fhdr_netCDF_IO.inc'

END MODULE LBLRTM_Fhdr_netCDF_IO
