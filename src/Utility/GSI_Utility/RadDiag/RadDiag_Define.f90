!
! RadDiag_Define
!
! Module defining the RadDiag header and data structures
! and containing routines to manipulate them
!

MODULE RadDiag_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds,      ONLY: sp=>Single
  USE Message_Handler, ONLY: FAILURE, SUCCESS, WARNING, &
                             Display_Message
  ! Disable implicit typing
  IMPLICIT NONE


  ! ---------------------
  ! Explicit visibilities
  ! ---------------------
  PRIVATE
  ! Module parameters
  PUBLIC :: RadDiag_nFPelements
  PUBLIC :: RadDiag_nCHelements
  PUBLIC :: RadDiag_nPRelements
  ! Module derived type definitions
  PUBLIC :: RadDiag_Hdr_Scalar_type,  RadDiag_Hdr_Channel_type,  RadDiag_Hdr_type
  PUBLIC :: RadDiag_Data_Scalar_type, RadDiag_Data_Channel_type, RadDiag_Data_type
  ! Module subprograms
  PUBLIC :: Associated_RadDiag_Hdr
  PUBLIC :: Destroy_RadDiag_Hdr
  PUBLIC :: Allocate_RadDiag_Hdr
  PUBLIC :: Assign_RadDiag_Hdr
  PUBLIC :: Associated_RadDiag_Data
  PUBLIC :: Destroy_RadDiag_Data
  PUBLIC :: Allocate_RadDiag_Data
  PUBLIC :: Assign_RadDiag_Data

  ! -----------------
  ! Module parameters
  ! -----------------
  INTEGER, PARAMETER :: RadDiag_nFPelements = 26 ! Number of floating point elements
  INTEGER, PARAMETER :: RadDiag_nCHelements = 7  ! Number of channel elements
  INTEGER, PARAMETER :: RadDiag_nPRelements = 5  ! Number of bias correction terms
  INTEGER, PARAMETER :: SET = 1
  REAL,    PARAMETER :: ZERO = 0.0_sp
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'


  ! ---------------------------
  ! Header structure definition
  ! ---------------------------
  ! Scalar part of header
  TYPE :: RadDiag_Hdr_Scalar_type
    CHARACTER(20) :: isis    = ' '  ! sat and sensor type
    CHARACTER(10) :: id      = ' '  ! sat type
    CHARACTER(10) :: obstype = ' '  ! observation type
    INTEGER       :: jiter   = 0    ! outer loop counter
    INTEGER       :: nchan   = 0    ! number of channels in the sensor
    INTEGER       :: npred   = 0    ! number of updating bias correction predictors
    INTEGER       :: idate   = 0    ! time (yyyymmddhh)
    INTEGER       :: ireal   = 0    ! # of real elements in the fix part of a data record
    INTEGER       :: ipchan  = 0    ! # of elements for each channel except for bias correction terms
    INTEGER       :: iextra  = 0    ! # of extra elements for each channel
    INTEGER       :: jextra  = 0    ! # of extra elements
  END TYPE RadDiag_Hdr_Scalar_type

  ! Channel dependent part of header
  TYPE :: RadDiag_Hdr_Channel_type
    REAL(sp) :: freq     = ZERO  ! frequency (Hz)
    REAL(sp) :: polar    = ZERO  ! polarization
    REAL(sp) :: wave     = ZERO  ! wave number (cm^-1)
    REAL(sp) :: varch    = ZERO  ! error variance (or SD error?)
    REAL(sp) :: tlapmean = ZERO  ! mean lapse rate
    INTEGER  :: iuse     = -1    ! use flag
    INTEGER  :: nuchan   = -1    ! sensor relative channel number
    INTEGER  :: iochan   = -1    ! satinfo relative channel number
  END TYPE RadDiag_Hdr_Channel_type

  ! The complete header
  TYPE :: RadDiag_Hdr_type
    INTEGER :: nAllocates = 0  ! Allocation counter
    INTEGER :: nChannels  = 0  ! Structure dimensions
    TYPE(RadDiag_Hdr_Scalar_type) :: Scalar
    TYPE(RadDiag_Hdr_Channel_type), DIMENSION(:), POINTER :: Channel => NULL()
  END TYPE RadDiag_Hdr_type


  ! -------------------------
  ! Data structure definition
  ! -------------------------
  ! Scalar part of data
  TYPE :: RadDiag_Data_Scalar_type
    REAL(sp) :: lat        = ZERO  ! latitude (deg)
    REAL(sp) :: lon        = ZERO  ! longitude (deg)
    REAL(sp) :: zsges      = ZERO  ! guess elevation at obs location (m)
    REAL(sp) :: obstime    = ZERO  ! observation time relative to analysis
    REAL(sp) :: senscn_pos = ZERO  ! sensor scan position (integer)
    REAL(sp) :: satzen_ang = ZERO  ! satellite zenith angle (deg)
    REAL(sp) :: satazm_ang = ZERO  ! satellite azimuth angle (deg)
    REAL(sp) :: solzen_ang = ZERO  ! solar zenith angle (deg)
    REAL(sp) :: solazm_ang = ZERO  ! solar azimumth angle (deg)
    REAL(sp) :: sungln_ang = ZERO  ! sun glint angle (deg)
    REAL(sp) :: water_frac = ZERO  ! fractional coverage by water
    REAL(sp) :: land_frac  = ZERO  ! fractional coverage by land
    REAL(sp) :: ice_frac   = ZERO  ! fractional coverage by ice
    REAL(sp) :: snow_frac  = ZERO  ! fractional coverage by snow
    REAL(sp) :: water_temp = ZERO  ! surface temperature over water (K)
    REAL(sp) :: land_temp  = ZERO  ! surface temperature over land (K)
    REAL(sp) :: ice_temp   = ZERO  ! surface temperature over ice (K)
    REAL(sp) :: snow_temp  = ZERO  ! surface temperature over snow (K)
    REAL(sp) :: soil_temp  = ZERO  ! soil temperature (K)
    REAL(sp) :: soil_mois  = ZERO  ! soil moisture 
    REAL(sp) :: land_type  = ZERO  ! land type (integer)
    REAL(sp) :: veg_frac   = ZERO  ! vegetation fraction
    REAL(sp) :: snow_depth = ZERO  ! snow depth
    REAL(sp) :: sfc_wndspd = ZERO  ! surface wind speed
    REAL(sp) :: qcdiag1    = ZERO  ! ir=cloud fraction, mw=cloud liquid water
    REAL(sp) :: qcdiag2    = ZERO  ! ir=cloud top pressure, mw=total column water
  END TYPE RadDiag_Data_Scalar_type

  ! Channel dependent part of data
  TYPE :: RadDiag_Data_Channel_type
    REAL(sp) :: tbobs  = ZERO  ! Tb (obs) (K)
    REAL(sp) :: omgbc  = ZERO  ! Tb_(obs) - Tb_(simulated w/ bc)  (K)
    REAL(sp) :: omgnbc = ZERO  ! Tb_(obs) - Tb_(simulated_w/o bc) (K)
    REAL(sp) :: errinv = ZERO  ! inverse error (K**(-1))
    REAL(sp) :: qcmark = ZERO  ! quality control mark
    REAL(sp) :: emiss  = ZERO  ! surface emissivity
    REAL(sp) :: tlap   = ZERO  ! temperature lapse rate
    REAL(sp) :: bifix  = ZERO  ! fixed angle dependent bias
    REAL(sp) :: bilap  = ZERO  ! lapse rate bias correction term
    REAL(sp) :: bilap2 = ZERO  ! square lapse rate bias correction term
    REAL(sp) :: bicons = ZERO  ! constant bias correction term
    REAL(sp) :: biang  = ZERO  ! scan angle bias correction term
    REAL(sp) :: biclw  = ZERO  ! CLW bias correction term
  END TYPE RadDiag_Data_Channel_type

  ! The complete data structure
  TYPE :: RadDiag_Data_type
    INTEGER :: nAllocates = 0  ! Allocation counter
    INTEGER :: nChannels  = 0  ! Structure dimensions
    TYPE(RadDiag_Data_Scalar_type) :: Scalar
    TYPE(RadDiag_Data_Channel_type), DIMENSION(:), POINTER :: Channel => NULL()
  END TYPE RadDiag_Data_type


CONTAINS


  ! Subroutine to clear the scalar elements of
  ! the radiance diagnostic header structure
  SUBROUTINE Clear_RadDiag_Hdr( RadDiag_Hdr )
    TYPE( RadDiag_Hdr_type ), INTENT(IN OUT) :: RadDiag_Hdr
    TYPE( RadDiag_Hdr_Scalar_type ) :: Scalar
    RadDiag_Hdr%Scalar%isis    = Scalar%isis   
    RadDiag_Hdr%Scalar%id      = Scalar%id     
    RadDiag_Hdr%Scalar%obstype = Scalar%obstype
    RadDiag_Hdr%Scalar%jiter   = Scalar%jiter  
    RadDiag_Hdr%Scalar%nchan   = Scalar%nchan  
    RadDiag_Hdr%Scalar%npred   = Scalar%npred  
    RadDiag_Hdr%Scalar%idate   = Scalar%idate  
    RadDiag_Hdr%Scalar%ireal   = Scalar%ireal  
    RadDiag_Hdr%Scalar%ipchan  = Scalar%ipchan 
    RadDiag_Hdr%Scalar%iextra  = Scalar%iextra 
    RadDiag_Hdr%Scalar%jextra  = Scalar%jextra 
  END SUBROUTINE Clear_RadDiag_Hdr

  
  ! Subroutine to clear the scalar elements of
  ! the radiance diagnostic data structure
  SUBROUTINE Clear_RadDiag_Data( RadDiag_Data )
    TYPE( RadDiag_Data_type ), INTENT(IN OUT) :: RadDiag_Data
    TYPE( RadDiag_Data_Scalar_type ) :: Scalar
    RadDiag_Data%Scalar%lat        = Scalar%lat       
    RadDiag_Data%Scalar%lon        = Scalar%lon       
    RadDiag_Data%Scalar%zsges      = Scalar%zsges     
    RadDiag_Data%Scalar%obstime    = Scalar%obstime   
    RadDiag_Data%Scalar%senscn_pos = Scalar%senscn_pos
    RadDiag_Data%Scalar%satzen_ang = Scalar%satzen_ang
    RadDiag_Data%Scalar%satazm_ang = Scalar%satazm_ang
    RadDiag_Data%Scalar%solzen_ang = Scalar%solzen_ang
    RadDiag_Data%Scalar%solazm_ang = Scalar%solazm_ang
    RadDiag_Data%Scalar%sungln_ang = Scalar%sungln_ang
    RadDiag_Data%Scalar%water_frac = Scalar%water_frac
    RadDiag_Data%Scalar%land_frac  = Scalar%land_frac 
    RadDiag_Data%Scalar%ice_frac   = Scalar%ice_frac  
    RadDiag_Data%Scalar%snow_frac  = Scalar%snow_frac 
    RadDiag_Data%Scalar%water_temp = Scalar%water_temp
    RadDiag_Data%Scalar%land_temp  = Scalar%land_temp 
    RadDiag_Data%Scalar%ice_temp   = Scalar%ice_temp  
    RadDiag_Data%Scalar%snow_temp  = Scalar%snow_temp 
    RadDiag_Data%Scalar%soil_temp  = Scalar%soil_temp 
    RadDiag_Data%Scalar%soil_mois  = Scalar%soil_mois 
    RadDiag_Data%Scalar%land_type  = Scalar%land_type 
    RadDiag_Data%Scalar%veg_frac   = Scalar%veg_frac  
    RadDiag_Data%Scalar%snow_depth = Scalar%snow_depth
    RadDiag_Data%Scalar%sfc_wndspd = Scalar%sfc_wndspd
    RadDiag_Data%Scalar%qcdiag1    = Scalar%qcdiag1   
    RadDiag_Data%Scalar%qcdiag2    = Scalar%qcdiag2   
  END SUBROUTINE Clear_RadDiag_Data



! Function to test the association status of the RadDiag_Hdr structure
!
! CALLING SEQUENCE:
!   Association_Status = Associated_RadDiag_Hdr( RadDiag_Hdr,        &  ! Input
!                                                ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!   RadDiag_Hdr:         RadDiag_Hdr structure which is to have its
!                        pointer member's association status tested.
!                        UNITS:      N/A
!                        TYPE:       RadDiag_Hdr_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!   ANY_Test:            Set this argument to test if ANY of the
!                        RadDiag_Hdr structure pointer members are
!                        associated.
!                        The default is to test if ALL the pointer members
!                        are associated.
!                        If ANY_Test = 0, test if ALL the pointer members
!                                         are associated.  (DEFAULT)
!                           ANY_Test = 1, test if ANY of the pointer members
!                                         are associated.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! FUNCTION RESULT:
!   Association_Status:  The return value is a logical value indicating
!                        the association status of the RadDiag_Hdr
!                        pointer members.
!                        .TRUE.  - if ALL the RadDiag_Hdr pointer
!                                  members are associated, or if the
!                                  ANY_Test argument is set and ANY of the
!                                  RadDiag_Hdr pointer members are
!                                  associated.
!                        .FALSE. - some or all of the RadDiag_Hdr
!                                  pointer members are NOT associated.
!                        UNITS:      N/A
!                        TYPE:       LOGICAL
!                        DIMENSION:  Scalar
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, CIMSS/SSEC 23-Mar-2006
!                   paul.vandelst@ssec.wisc.edu

  FUNCTION Associated_RadDiag_Hdr( RadDiag_Hdr, &  ! Output
                                   ANY_Test )   &  ! Optional input
                                 RESULT( Association_Status )
    ! Arguments
    TYPE( RadDiag_Hdr_type ), INTENT( IN ) :: RadDiag_Hdr
    INTEGER,        OPTIONAL, INTENT( IN ) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test


    ! ------
    ! Set up
    ! ------
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF


    ! ---------------------------------------------
    ! Test the structure pointer member association
    ! NOTE: Currently the ANY vs ALL test is moot
    ! since there is only one non-scalar component.
    ! This may change.
    ! ---------------------------------------------
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( RadDiag_Hdr%Channel ) ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( RadDiag_Hdr%Channel ) ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_RadDiag_Hdr


! Function to re-initialize the scalar and pointer members of
! a RadDiag_Hdr data structure.
!
! CALLING SEQUENCE:
!   Error_Status = Destroy_RadDiag_Hdr( RadDiag_Hdr,              &  ! Output
!                                       RCS_Id = RCS_Id,          &  ! Revision control
!                                       Message_Log = Message_Log )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:        Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!   RadDiag_Hdr:        Re-initialized RadDiag_Hdr structure.
!                       UNITS:      N/A
!                       TYPE:       RadDiag_Hdr_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:             Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:       The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the structure re-initialisation was successful
!                          == FAILURE - an error occurred, or
!                                     - the structure internal allocation counter
!                                       is not equal to zero (0) upon exiting this
!                                       function. This value is incremented and
!                                       decremented for every structure allocation
!                                       and deallocation respectively.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!   Note the INTENT on the output RadDiag_Hdr argument is IN OUT 
!   rather than just OUT. This is necessary because the argument may be
!   defined upon input. To prevent memory leaks, the IN OUT INTENT is
!   a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Mar-2006
!                       paul.vandelst@ssec.wisc.edu

  FUNCTION Destroy_RadDiag_Hdr( RadDiag_Hdr,  &  ! Output
                                No_Clear,     &  ! Optional input
                                RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(RadDiag_Hdr_type), INTENT(IN OUT) :: RadDiag_Hdr
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_RadDiag_Hdr'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    ! ------------------------
    ! Perform reinitialisation
    ! ------------------------
    ! Initialise the scalar members
    IF ( Clear ) CALL Clear_RadDiag_Hdr( RadDiag_Hdr )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_RadDiag_Hdr( RadDiag_Hdr ) ) RETURN

    ! Deallocate the RadDiag_Hdr Channel member
    IF ( ASSOCIATED( RadDiag_Hdr%Channel ) ) THEN
      DEALLOCATE( RadDiag_Hdr%Channel, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating RadDiag_Hdr Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -------------------------------------
    ! Decrement and test allocation counter
    ! -------------------------------------
    RadDiag_Hdr%nAllocates = RadDiag_Hdr%nAllocates - 1
    IF ( RadDiag_Hdr%nAllocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      RadDiag_Hdr%nAllocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_RadDiag_Hdr


! Function to allocate the pointer members of the RadDiag_Hdr
! data structure.
!
! CALLING SEQUENCE:
!   Error_Status = Allocate_RadDiag_Hdr( nChannels,                &  ! Input
!                                        RadDiag_Hdr,              &  ! Output
!                                        RCS_Id = RCS_Id,          &  ! Revision control
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   nChannels:              Dimension of RadDiag_Hdr structure pointer
!                           members.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:            Character string specifying a filename in
!                           which any messages will be logged. If not
!                           specified, or if an error occurs opening the
!                           log file, the default action is to output
!                           messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!   RadDiag_Hdr:            RadDiag_Hdr structure with allocated pointer
!                           members
!                           UNITS:      N/A
!                           TYPE:       RadDiag_Hdr_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:                 Character string containing the Revision
!                           Control System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:           The return value is an integer defining the
!                           error status. The error codes are defined in
!                           the ERROR_HANDLER module.
!                           If == SUCCESS the structure pointer allocations
!                                         were successful
!                              == FAILURE - an error occurred, or
!                                         - the structure internal allocation
!                                           counter is not equal to one (1)
!                                           upon exiting this function. This
!                                           value is incremented and decre-
!                                           mented for every structure
!                                           allocation and deallocation
!                                           respectively.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! COMMENTS:
!   Note the INTENT on the output RadDiag_Hdr argument is IN OUT 
!   rather than just OUT. This is necessary because the argument may be
!   defined upon input. To prevent memory leaks, the IN OUT INTENT is
!   a must.
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, CIMSS/SSEC 23-Mar-2006
!                   paul.vandelst@ssec.wisc.edu

  FUNCTION Allocate_RadDiag_Hdr( nChannels,    &  ! Input
                                 RadDiag_Hdr,  &  ! Output
                                 RCS_Id,       &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)     :: nChannels
    TYPE(RadDiag_Hdr_type), INTENT(IN OUT) :: RadDiag_Hdr
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_RadDiag_Hdr'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check dimensions
    IF ( nChannels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input nChannels must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_RadDiag_Hdr( RadDiag_Hdr, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_RadDiag_Hdr( RadDiag_Hdr, &
                                          No_Clear=SET, &
                                          Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating RadDiag_Hdr pointer members.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! ----------------------
    ! Perform the allocation
    ! ----------------------
    ALLOCATE( RadDiag_Hdr%Channel( nChannels ), &
              STAT = Allocate_Status )           
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating RadDiag_Hdr data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Assign the dimensions
    ! ---------------------
    RadDiag_Hdr%nChannels = nChannels


    ! -----------------------------------------
    ! Increment and test the allocation counter
    ! -----------------------------------------
    RadDiag_Hdr%nAllocates = RadDiag_Hdr%nAllocates + 1
    IF ( RadDiag_Hdr%nAllocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      RadDiag_Hdr%nAllocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_RadDiag_Hdr


! Function to copy valid RadDiag_Hdr structures.
!
! CALLING SEQUENCE:
!   Error_Status = Assign_RadDiag_Hdr( RadDiag_Hdr_in,           &  ! Input           
!                                      RadDiag_Hdr_out,          &  ! Output          
!                                      RCS_Id = RCS_Id,          &  ! Revision control
!                                      Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   RadDiag_Hdr_in:         RadDiag_Hdr structure which is to be copied.
!                           UNITS:      N/A
!                           TYPE:       RadDiag_Hdr_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:            Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!   RadDiag_Hdr_out:        Copy of the input structure, RadDiag_Hdr_in.
!                           UNITS:      N/A
!                           TYPE:       Same as RadDiag_Hdr_in
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:                 Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status: The return value is an integer defining the error status.
!                 The error codes are defined in the ERROR_HANDLER module.
!                 If == SUCCESS the structure assignment was successful
!                    == FAILURE an error occurred
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
! COMMENTS:
!   Note the INTENT on the output RadDiag_Hdr argument is IN OUT 
!   rather than just OUT. This is necessary because the argument may be
!   defined upon input. To prevent memory leaks, the IN OUT INTENT is
!   a must.
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, CIMSS/SSEC 23-Mar-2006
!                   paul.vandelst@ssec.wisc.edu

  FUNCTION Assign_RadDiag_Hdr( RadDiag_Hdr_in,  &  ! Input
                               RadDiag_Hdr_out, &  ! Output
                               RCS_Id,          &  ! Revision control
                               Message_Log )    &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    TYPE(RadDiag_Hdr_type), INTENT(IN)     :: RadDiag_Hdr_in
    TYPE(RadDiag_Hdr_type), INTENT(IN OUT) :: RadDiag_Hdr_out
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_RadDiag_Hdr'


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated.
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    IF ( .NOT. Associated_RadDiag_Hdr( RadDiag_Hdr_In ) ) THEN
      Error_Status = Destroy_RadDiag_Hdr( RadDiag_Hdr_Out, &
                                                Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating output RadDiag_Hdr_out pointer members.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
      RETURN
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------
    Error_Status = Allocate_RadDiag_Hdr( RadDiag_Hdr_in%nChannels, &
                                         RadDiag_Hdr_out, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output RadDiag_Hdr arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------
    RadDiag_Hdr_out%Scalar = RadDiag_Hdr_in%Scalar


    ! -----------------
    ! Assign array data
    ! -----------------
    RadDiag_Hdr_out%Channel = RadDiag_Hdr_in%Channel

  END FUNCTION Assign_RadDiag_Hdr


! Function to test the association status of the RadDiag_Data structure
!
! CALLING SEQUENCE:
!   Association_Status = Associated_RadDiag_Data( RadDiag_Data,  &  ! Input
!                                                 ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!   RadDiag_Data:        RadDiag_Data structure which is to have its
!                        pointer member's association status tested.
!                        UNITS:      N/A
!                        TYPE:       RadDiag_Data_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!   ANY_Test:            Set this argument to test if ANY of the
!                        RadDiag_Data structure pointer members are
!                        associated.
!                        The default is to test if ALL the pointer members
!                        are associated.
!                        If ANY_Test = 0, test if ALL the pointer members
!                                         are associated.  (DEFAULT)
!                           ANY_Test = 1, test if ANY of the pointer members
!                                         are associated.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! FUNCTION RESULT:
!   Association_Status:  The return value is a logical value indicating
!                        the association status of the RadDiag_Data
!                        pointer members.
!                        .TRUE.  - if ALL the RadDiag_Data pointer
!                                  members are associated, or if the
!                                  ANY_Test argument is set and ANY of the
!                                  RadDiag_Data pointer members are
!                                  associated.
!                        .FALSE. - some or all of the RadDiag_Data
!                                  pointer members are NOT associated.
!                        UNITS:      N/A
!                        TYPE:       LOGICAL
!                        DIMENSION:  Scalar
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, CIMSS/SSEC 23-Mar-2006
!                   paul.vandelst@ssec.wisc.edu

  FUNCTION Associated_RadDiag_Data( RadDiag_Data, &  ! Output
                                    ANY_Test )    &  ! Optional input
                                  RESULT( Association_Status )
    ! Arguments
    TYPE( RadDiag_Data_type ), INTENT( IN ) :: RadDiag_Data
    INTEGER,         OPTIONAL, INTENT( IN ) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test


    ! ------
    ! Set up
    ! ------
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF


    ! ---------------------------------------------
    ! Test the structure pointer member association
    ! NOTE: Currently the ANY vs ALL test is moot
    ! since there is only one non-scalar component.
    ! This may change.
    ! ---------------------------------------------
    Association_Status = .FALSE.
    IF ( ALL_Test ) THEN
      IF ( ASSOCIATED( RadDiag_Data%Channel ) ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED( RadDiag_Data%Channel ) ) THEN
        Association_Status = .TRUE.
      END IF
    END IF

  END FUNCTION Associated_RadDiag_Data


! Function to re-initialize the scalar and pointer members of
! a RadDiag_Data data structure.
!
! CALLING SEQUENCE:
!   Error_Status = Destroy_RadDiag_Data( RadDiag_Data,             &  ! Output
!                                        RCS_Id = RCS_Id,          &  ! Revision control
!                                        Message_Log = Message_Log )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:        Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!   RadDiag_Data:       Re-initialized RadDiag_Data structure.
!                       UNITS:      N/A
!                       TYPE:       RadDiag_Data_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:             Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:       The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the structure re-initialisation was successful
!                          == FAILURE - an error occurred, or
!                                     - the structure internal allocation counter
!                                       is not equal to zero (0) upon exiting this
!                                       function. This value is incremented and
!                                       decremented for every structure allocation
!                                       and deallocation respectively.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!   Note the INTENT on the output RadDiag_Data argument is IN OUT 
!   rather than just OUT. This is necessary because the argument may be
!   defined upon input. To prevent memory leaks, the IN OUT INTENT is
!   a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Mar-2006
!                       paul.vandelst@ssec.wisc.edu

  FUNCTION Destroy_RadDiag_Data( RadDiag_Data, &  ! Output
                                 No_Clear,     &  ! Optional input
                                 RCS_Id,       &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT( Error_Status )
    ! Arguments
    TYPE(RadDiag_Data_type), INTENT(IN OUT) :: RadDiag_Data
    INTEGER,       OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_RadDiag_Data'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    ! ------------------------
    ! Perform reinitialisation
    ! ------------------------
    ! Initialise the scalar members
    IF ( Clear ) CALL Clear_RadDiag_Data( RadDiag_Data )

    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_RadDiag_Data( RadDiag_Data ) ) RETURN

    ! Deallocate the RadDiag_Data Channel member
    IF ( ASSOCIATED( RadDiag_Data%Channel ) ) THEN
      DEALLOCATE( RadDiag_Data%Channel, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating RadDiag_Data Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -------------------------------------
    ! Decrement and test allocation counter
    ! -------------------------------------
    RadDiag_Data%nAllocates = RadDiag_Data%nAllocates - 1
    IF ( RadDiag_Data%nAllocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      RadDiag_Data%nAllocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_RadDiag_Data


! Function to allocate the pointer members of the RadDiag_Data
! data structure.
!
! CALLING SEQUENCE:
!   Error_Status = Allocate_RadDiag_Data( nChannels,                &  ! Input           
!                                         RadDiag_Data,             &  ! Output          
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   nChannels:              Dimension of RadDiag_Data structure pointer
!                           members.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:            Character string specifying a filename in
!                           which any messages will be logged. If not
!                           specified, or if an error occurs opening the
!                           log file, the default action is to output
!                           messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!   RadDiag_Data:           RadDiag_Data structure with allocated pointer
!                           members
!                           UNITS:      N/A
!                           TYPE:       RadDiag_Data_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:                 Character string containing the Revision
!                           Control System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:           The return value is an integer defining the
!                           error status. The error codes are defined in
!                           the ERROR_HANDLER module.
!                           If == SUCCESS the structure pointer allocations
!                                         were successful
!                              == FAILURE - an error occurred, or
!                                         - the structure internal allocation
!                                           counter is not equal to one (1)
!                                           upon exiting this function. This
!                                           value is incremented and decre-
!                                           mented for every structure
!                                           allocation and deallocation
!                                           respectively.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! COMMENTS:
!   Note the INTENT on the output RadDiag_Data argument is IN OUT 
!   rather than just OUT. This is necessary because the argument may be
!   defined upon input. To prevent memory leaks, the IN OUT INTENT is
!   a must.
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, CIMSS/SSEC 23-Mar-2006
!                   paul.vandelst@ssec.wisc.edu

  FUNCTION Allocate_RadDiag_Data( nChannels,    &  ! Input
                                  RadDiag_Data, &  ! Output
                                  RCS_Id,       &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: nChannels
    TYPE(RadDiag_Data_type), INTENT(IN OUT) :: RadDiag_Data
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_RadDiag_Data'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: Allocate_Status


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check dimensions
    IF ( nChannels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input nChannels must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_RadDiag_Data( RadDiag_Data, ANY_Test=SET ) ) THEN
      Error_Status = Destroy_RadDiag_Data( RadDiag_Data, &
                                           No_Clear=SET, &
                                           Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating RadDiag_Data pointer members.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! ----------------------
    ! Perform the allocation
    ! ----------------------
    ALLOCATE( RadDiag_Data%Channel( nChannels ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating RadDiag_Data data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------------------
    ! Assign the dimensions and intialise arrays
    ! ------------------------------------------
    RadDiag_Data%nChannels = nChannels


    ! -----------------------------------------
    ! Increment and test the allocation counter
    ! -----------------------------------------
    RadDiag_Data%nAllocates = RadDiag_Data%nAllocates + 1
    IF ( RadDiag_Data%nAllocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      RadDiag_Data%nAllocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_RadDiag_Data


! Function to copy valid RadDiag_Data structures.
!
! CALLING SEQUENCE:
!   Error_Status = Assign_RadDiag_Data( RadDiag_Data_in,          &  ! Input
!                                       RadDiag_Data_out,         &  ! Output
!                                       RCS_Id = RCS_Id,          &  ! Revision control
!                                       Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   RadDiag_Data_in:        RadDiag_Data structure which is to be copied.
!                           UNITS:      N/A
!                           TYPE:       RadDiag_Data_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:            Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!   RadDiag_Data_out:       Copy of the input structure, RadDiag_Data_in.
!                           UNITS:      N/A
!                           TYPE:       Same as RadDiag_Data_in
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:                 Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status: The return value is an integer defining the error status.
!                 The error codes are defined in the ERROR_HANDLER module.
!                 If == SUCCESS the structure assignment was successful
!                    == FAILURE an error occurred
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
! COMMENTS:
!   Note the INTENT on the output RadDiag_Data argument is IN OUT 
!   rather than just OUT. This is necessary because the argument may be
!   defined upon input. To prevent memory leaks, the IN OUT INTENT is
!   a must.
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, CIMSS/SSEC 23-Mar-2006
!                   paul.vandelst@ssec.wisc.edu

  FUNCTION Assign_RadDiag_Data( RadDiag_Data_in,  &  ! Input
                                RadDiag_Data_out, &  ! Output
                                RCS_Id,           &  ! Revision control
                                Message_Log )     &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    TYPE(RadDiag_Data_type), INTENT(IN)     :: RadDiag_Data_in
    TYPE(RadDiag_Data_type), INTENT(IN OUT) :: RadDiag_Data_out
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_RadDiag_Data'


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated.
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    IF ( .NOT. Associated_RadDiag_Data( RadDiag_Data_In ) ) THEN
      Error_Status = Destroy_RadDiag_Data( RadDiag_Data_Out, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating output RadDiag_Data_Out pointer members.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
      RETURN
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------
    Error_Status = Allocate_RadDiag_Data( RadDiag_Data_in%nChannels, &
                                          RadDiag_Data_out, &
                                          Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output RadDiag_Data arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------
    RadDiag_Data_out%Scalar = RadDiag_Data_in%Scalar


    ! -----------------
    ! Assign array data
    ! -----------------
    RadDiag_Data_out%Channel = RadDiag_Data_in%Channel

  END FUNCTION Assign_RadDiag_Data

END MODULE RadDiag_Define
