!
! LBLRTM_Fhdr_Define
!
! Module containing the definition of the LBLRTM File header object, as
! well as procedures to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 23-Jan-2000
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_Fhdr_Define

  ! ------------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: FP, IP, DP => Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE LBLRTM_Parameters    , ONLY: N_MOL => LBLRTM_MAX_N_MOLECULES
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: LBLRTM_Fhdr_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(/=)
  ! Procedures
  PUBLIC :: LBLRTM_Fhdr_SetValid
  PUBLIC :: LBLRTM_Fhdr_IsValid
  PUBLIC :: LBLRTM_Fhdr_Destroy
  PUBLIC :: LBLRTM_Fhdr_Inspect
  PUBLIC :: LBLRTM_Fhdr_DefineVersion
  PUBLIC :: LBLRTM_Fhdr_Compare


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE LBLRTM_Fhdr_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(/=)
    MODULE PROCEDURE LBLRTM_Fhdr_NotEqual
  END INTERFACE OPERATOR(/=)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: LBLRTM_Fhdr_Define.f90 37070 2014-02-21 14:23:37Z paul.vandelst@noaa.gov $'
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512


  ! ------------------------
  ! Derived type definitions
  ! ------------------------
  !:tdoc+:
  TYPE :: LBLRTM_Fhdr_type
    LOGICAL :: Is_Valid = .FALSE.
    CHARACTER(80) :: User_ID                        = ''
    REAL(DP)      :: Column_Scale_Factor            = 0.0_DP
    REAL(FP)      :: Average_Layer_Pressure         = 0.0_FP
    REAL(FP)      :: Average_Layer_Temperature      = 0.0_FP
    CHARACTER(8)  :: Molecule_Id(N_MOL)             = ''
    REAL(FP)      :: Molecule_Column_Density(N_MOL) = 0.0_FP
    REAL(FP)      :: Broadening_Gas_Column_Density  = 0.0_FP
    REAL(FP)      :: Frequency_Interval             = 0.0_FP
    REAL(DP)      :: Begin_Frequency                = 0.0_DP
    REAL(DP)      :: End_Frequency                  = 0.0_DP
    REAL(FP)      :: Boundary_Temperature           = 0.0_FP
    REAL(FP)      :: Boundary_Emissivity            = 0.0_FP
    INTEGER(IP)   :: n_Molecules                    = 0_IP
    INTEGER(IP)   :: n_Layer                        = 0_IP
    INTEGER(IP)   :: OD_Layering_Control_Flag       = 0_IP
    CHARACTER(8)  :: Calculation_Date               = ''
    CHARACTER(8)  :: Calculation_Time               = ''
    CHARACTER(8)  :: ancillary(8)                   = ''
    ! The RunFlags
    INTEGER(IP) :: hirac   = -1_IP
    INTEGER(IP) :: lblf4   = -1_IP
    INTEGER(IP) :: xscnt   = -1_IP
    INTEGER(IP) :: aersl   = -1_IP
    INTEGER(IP) :: emit    = -1_IP
    INTEGER(IP) :: scan    = -1_IP
    INTEGER(IP) :: plot    = -1_IP
    INTEGER(IP) :: path    = -1_IP
    INTEGER(IP) :: jrad    = -1_IP
    INTEGER(IP) :: test    = -1_IP
    INTEGER(IP) :: merge   = -1_IP
    REAL(FP)    :: scnid   = 0.0_FP
    REAL(FP)    :: hwhm    = 0.0_FP
    INTEGER(IP) :: idabs   = -1_IP
    INTEGER(IP) :: atm     = -1_IP
    INTEGER(IP) :: layr1   = -1_IP
    INTEGER(IP) :: nlayr   = -1_IP
  END TYPE LBLRTM_Fhdr_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Fhdr_SetValid
!
! PURPOSE:
!       Elemental subroutine to mark an instance of an LBLRTM_Fhdr object
!       as containing valid data.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Fhdr_SetValid( LBLRTM_Fhdr )
!
! OBJECTS:
!       LBLRTM_Fhdr:   Instance which is to have its validity set.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Fhdr_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LBLRTM_Fhdr_SetValid(self)
    TYPE(LBLRTM_Fhdr_type), INTENT(IN OUT) :: self
    self%Is_Valid = .TRUE.
  END SUBROUTINE LBLRTM_Fhdr_SetValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Fhdr_IsValid
!
! PURPOSE:
!       Elemental function to test if the LBLRTM_Fhdr object contains
!       valid data.
!
! CALLING SEQUENCE:
!       Status = LBLRTM_Fhdr_IsValid( LBLRTM_Fhdr )
!
! OBJECTS:
!       LBLRTM_Fhdr:   Instance which is to have its status tested.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Fhdr_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating
!                      if the object contains valid data.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION LBLRTM_Fhdr_IsValid( self ) RESULT( Status )
    TYPE(LBLRTM_Fhdr_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Valid
  END FUNCTION LBLRTM_Fhdr_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Fhdr_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize LBLRTM_Fhdr objects.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Fhdr_Destroy( LBLRTM_Fhdr )
!
! OBJECTS:
!       LBLRTM_Fhdr:  Re-initialized LBLRTM_Fhdr instance.
!                     UNITS:      N/A
!                     TYPE:       LBLRTM_Fhdr_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LBLRTM_Fhdr_Destroy( self )
    TYPE(LBLRTM_Fhdr_type), INTENT(OUT) :: self
    self%Is_Valid = .FALSE.
  END SUBROUTINE LBLRTM_Fhdr_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Fhdr_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of an instance of an LBLRTM_Fhdr
!       object to stdout.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Fhdr_Inspect( LBLRTM_Fhdr )
!
! OBJECTS:
!       LBLRTM_Fhdr:   LBLRTM_Fhdr object to display.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Fhdr_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LBLRTM_Fhdr_Inspect( self, offset )
    TYPE(LBLRTM_Fhdr_type), INTENT(IN) :: self
    INTEGER,      OPTIONAL, INTENT(IN) :: offset
    CHARACTER(*), PARAMETER :: FMT_STRING = 'es22.15'
    INTEGER      :: n_spaces(3)
    CHARACTER(3) :: sp(3), cr
    IF ( .NOT. LBLRTM_Fhdr_IsValid(self) ) RETURN
    ! Compute indent for stand-alone, or embedded, object
    n_spaces = [1,3,5]
    cr       = '/'
    IF ( PRESENT(offset) ) THEN
      n_spaces = n_spaces + ABS(offset)
      cr       = ''
    END IF
    WRITE(sp,'(i0,"x")') n_spaces
    ! Output data
    WRITE(*,'('//cr//sp(1)//',"LBLRTM_Fhdr OBJECT")')
    WRITE(*,'('//sp(2)//',"Kind types")')
    WRITE(*,'('//sp(3)//',"Default REAL    : ",i0)') FP
    WRITE(*,'('//sp(3)//',"Default INTEGER : ",i0)') IP
    WRITE(*,'('//sp(2)//',"Data")')
    WRITE(*,'('//sp(3)//',"User Id : ",/,a)') TRIM(self%User_Id)
    WRITE(*,'('//sp(3)//',"Column_Scale_Factor           : ",'//FMT_STRING//')') self%Column_Scale_Factor
    WRITE(*,'('//sp(3)//',"Average_Layer_Pressure        : ",'//FMT_STRING//')') self%Average_Layer_Pressure
    WRITE(*,'('//sp(3)//',"Average_Layer_Temperature     : ",'//FMT_STRING//')') self%Average_Layer_Temperature
    WRITE(*,'('//sp(3)//',"Molecule_Id                   : ")')
    WRITE(*,'(10a8)') self%Molecule_Id
    WRITE(*,'('//sp(3)//',"Molecule_Column_Density       : ")')
    WRITE(*,'(4(1x,'//FMT_STRING//'))') self%Molecule_Column_Density
    WRITE(*,'('//sp(3)//',"Broadening_Gas_Column_Density : ",'//FMT_STRING//')') self%Broadening_Gas_Column_Density
    WRITE(*,'('//sp(3)//',"Frequency_Interval            : ",'//FMT_STRING//')') self%Frequency_Interval
    WRITE(*,'('//sp(3)//',"Begin_Frequency               : ",'//FMT_STRING//')') self%Begin_Frequency
    WRITE(*,'('//sp(3)//',"End_Frequency                 : ",'//FMT_STRING//')') self%End_Frequency
    WRITE(*,'('//sp(3)//',"Boundary_Temperature          : ",'//FMT_STRING//')') self%Boundary_Temperature
    WRITE(*,'('//sp(3)//',"Boundary_Emissivity           : ",'//FMT_STRING//')') self%Boundary_Emissivity
    WRITE(*,'('//sp(3)//',"n_Molecules                   : ",i0)') self%n_Molecules
    WRITE(*,'('//sp(3)//',"n_Layer                       : ",i0)') self%n_Layer
    WRITE(*,'('//sp(3)//',"OD_Layering_Control_Flag      : ",i0)') self%OD_Layering_Control_Flag
    WRITE(*,'('//sp(3)//',"Calculation_Date              : ",a8)') self%Calculation_Date
    WRITE(*,'('//sp(3)//',"Calculation_Time              : ",a8)') self%Calculation_Time
    WRITE(*,'('//sp(3)//',"ancillary                     : ")')
    WRITE(*,'(10a8)') self%ancillary
    WRITE(*,'('//sp(2)//',"RunFlags")')
    WRITE(*,'('//sp(3)//',"hirac : ",i0)') self%hirac
    WRITE(*,'('//sp(3)//',"lblf4 : ",i0)') self%lblf4
    WRITE(*,'('//sp(3)//',"xscnt : ",i0)') self%xscnt
    WRITE(*,'('//sp(3)//',"aersl : ",i0)') self%aersl
    WRITE(*,'('//sp(3)//',"emit  : ",i0)') self%emit
    WRITE(*,'('//sp(3)//',"scan  : ",i0)') self%scan
    WRITE(*,'('//sp(3)//',"plot  : ",i0)') self%plot
    WRITE(*,'('//sp(3)//',"path  : ",i0)') self%path
    WRITE(*,'('//sp(3)//',"jrad  : ",i0)') self%jrad
    WRITE(*,'('//sp(3)//',"test  : ",i0)') self%test
    WRITE(*,'('//sp(3)//',"merge : ",i0)') self%merge
    WRITE(*,'('//sp(3)//',"scnid : ",'//FMT_STRING//')') self%scnid
    WRITE(*,'('//sp(3)//',"hwhm  : ",'//FMT_STRING//')') self%hwhm
    WRITE(*,'('//sp(3)//',"idabs : ",i0)') self%idabs
    WRITE(*,'('//sp(3)//',"atm   : ",i0)') self%atm
    WRITE(*,'('//sp(3)//',"layr1 : ",i0)') self%layr1
    WRITE(*,'('//sp(3)//',"nlayr : ",i0)') self%nlayr
  END SUBROUTINE LBLRTM_Fhdr_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Fhdr_DefineVersion
!
! PURPOSE:
!       Subroutine to return the version information for the
!       definition module(s).
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Fhdr_DefineVersion( Id )
!
! OUTPUTS:
!       Id:     Character string containing the version Id information for the
!               definition module(s).
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LBLRTM_Fhdr_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LBLRTM_Fhdr_DefineVersion



!------------------------------------------------------------------------------
!
! NAME:
!       LBLRTM_Fhdr_Compare
!
! PURPOSE:
!       Function to test the equality of two LBLRTM_Fhdr objects.
!
!       This procedure is basically a copy of the LBLRTM_Fhdr_Equal function
!       but non-elemental to allow for informational output when a difference
!       is found between the two structures.
!
!       Used for debugging only.
!
! CALLING SEQUENCE:
!       is_equal = LBLRTM_Fhdr_Equal( x, y )
!
! OBJECTS:
!       x, y:      Two LBLRTM_Fhdr objects to be compared.
!                  UNITS:      N/A
!                  TYPE:       LBLRTM_Fhdr_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:  Logical value indicating whether the inputs are equal.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  FUNCTION LBLRTM_Fhdr_Compare( x, y ) RESULT( is_equal )
    TYPE(LBLRTM_Fhdr_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Fhdr_Define::Compare'
    ! Local variable
    CHARACTER(ML) :: msg

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( LBLRTM_Fhdr_IsValid(x) .NEQV. LBLRTM_Fhdr_IsValid(y) ) THEN
      msg = 'Object association statuses are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF

    ! Check the data
    IF ( x%User_ID /= y%User_ID ) THEN
      msg = 'User_ID components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%Column_Scale_Factor .EqualTo. y%Column_Scale_Factor) ) THEN
      msg = 'Column_Scale_Factor components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%Average_Layer_Pressure .EqualTo. y%Average_Layer_Pressure) ) THEN
      msg = 'Average_Layer_Pressure components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%Average_Layer_Temperature .EqualTo. y%Average_Layer_Temperature) ) THEN
      msg = 'Average_Layer_Temperature components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( ANY(x%Molecule_Id /= y%Molecule_Id) ) THEN
      msg = 'Molecule_Id components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. ALL(x%Molecule_Column_Density .EqualTo. y%Molecule_Column_Density) ) THEN
      msg = 'Molecule_Column_Density components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%Broadening_Gas_Column_Density .EqualTo. y%Broadening_Gas_Column_Density) ) THEN
      msg = 'Broadening_Gas_Column_Density components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%Frequency_Interval .EqualTo. y%Frequency_Interval) ) THEN
      msg = 'Frequency_Interval components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%Begin_Frequency .EqualTo. y%Begin_Frequency) ) THEN
      msg = 'Begin_Frequency components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%End_Frequency .EqualTo. y%End_Frequency) ) THEN
      msg = 'End_Frequency components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%Boundary_Temperature .EqualTo. y%Boundary_Temperature) ) THEN
      msg = 'Boundary_Temperature components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%Boundary_Emissivity .EqualTo. y%Boundary_Emissivity) ) THEN
      msg = 'Boundary_Emissivity components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%n_Molecules == y%n_Molecules) ) THEN
      msg = 'n_Molecules components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%n_Layer == y%n_Layer) ) THEN
      msg = 'n_Layer components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%OD_Layering_Control_Flag == y%OD_Layering_Control_Flag) ) THEN
      msg = 'OD_Layering_Control_Flag components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%Calculation_Date /= y%Calculation_Date ) THEN
      msg = 'Calculation_Date components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%Calculation_Time /= y%Calculation_Time ) THEN
      msg = 'Calculation_Time components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( ANY(x%ancillary /= y%ancillary) ) THEN
      msg = 'ancillary components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    
    ! Check the run flags
    IF ( x%hirac /= y%hirac ) THEN
      msg = 'hirac components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%lblf4 /= y%lblf4 ) THEN
      msg = 'lblf4 components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%xscnt /= y%xscnt ) THEN
      msg = 'xscnt components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%aersl /= y%aersl ) THEN
      msg = 'aersl components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%emit /= y%emit ) THEN
      msg = 'emit components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%scan /= y%scan ) THEN
      msg = 'scan components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%plot /= y%plot ) THEN
      msg = 'plot components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%path /= y%path ) THEN
      msg = 'path components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%jrad /= y%jrad ) THEN
      msg = 'jrad components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%test /= y%test ) THEN
      msg = 'test components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%merge /= y%merge ) THEN
      msg = 'merge components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%scnid .EqualTo. y%scnid) ) THEN
      msg = 'scnid components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%hwhm .EqualTo. y%hwhm) ) THEN
      msg = 'hwhm components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%idabs /= y%idabs ) THEN
      msg = 'idabs components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%atm /= y%atm ) THEN
      msg = 'atm components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%layr1 /= y%layr1 ) THEN
      msg = 'layr1 components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%nlayr /= y%nlayr ) THEN
      msg = 'nlayr components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF

    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION LBLRTM_Fhdr_Compare



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       LBLRTM_Fhdr_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two LBLRTM_Fhdr objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = LBLRTM_Fhdr_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:      Two LBLRTM_Fhdr objects to be compared.
!                  UNITS:      N/A
!                  TYPE:       LBLRTM_Fhdr_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:  Logical value indicating whether the inputs are equal.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION LBLRTM_Fhdr_Equal( x, y ) RESULT( is_equal )
    TYPE(LBLRTM_Fhdr_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( LBLRTM_Fhdr_IsValid(x) .NEQV. LBLRTM_Fhdr_IsValid(y) ) RETURN

    ! Check contents
    IF ( .NOT.((x%User_ID                           ==    y%User_ID                      ) .AND. &
               (x%Column_Scale_Factor           .EqualTo. y%Column_Scale_Factor          ) .AND. &
               (x%Average_Layer_Pressure        .EqualTo. y%Average_Layer_Pressure       ) .AND. &
               (x%Average_Layer_Temperature     .EqualTo. y%Average_Layer_Temperature    ) .AND. &
               ALL(x%Molecule_Id                    ==    y%Molecule_Id                  ) .AND. &
               ALL(x%Molecule_Column_Density    .EqualTo. y%Molecule_Column_Density      ) .AND. &
               (x%Broadening_Gas_Column_Density .EqualTo. y%Broadening_Gas_Column_Density) .AND. &
               (x%Frequency_Interval            .EqualTo. y%Frequency_Interval           ) .AND. &
               (x%Begin_Frequency               .EqualTo. y%Begin_Frequency              ) .AND. &
               (x%End_Frequency                 .EqualTo. y%End_Frequency                ) .AND. &
               (x%Boundary_Temperature          .EqualTo. y%Boundary_Temperature         ) .AND. &
               (x%Boundary_Emissivity           .EqualTo. y%Boundary_Emissivity          ) .AND. &
               (x%n_Molecules                       ==    y%n_Molecules                  ) .AND. &
               (x%n_Layer                           ==    y%n_Layer                      ) .AND. &
               (x%OD_Layering_Control_Flag          ==    y%OD_Layering_Control_Flag     ) .AND. &
               (x%Calculation_Date                  ==    y%Calculation_Date             ) .AND. &
               (x%Calculation_Time                  ==    y%Calculation_Time             ) .AND. &
               ALL(x%ancillary                      ==    y%ancillary                    ) .AND. &
               (x%hirac     ==    y%hirac) .AND. &
               (x%lblf4     ==    y%lblf4) .AND. &
               (x%xscnt     ==    y%xscnt) .AND. &
               (x%aersl     ==    y%aersl) .AND. &
               (x%emit      ==    y%emit ) .AND. &
               (x%scan      ==    y%scan ) .AND. &
               (x%plot      ==    y%plot ) .AND. &
               (x%path      ==    y%path ) .AND. &
               (x%jrad      ==    y%jrad ) .AND. &
               (x%test      ==    y%test ) .AND. &
               (x%merge     ==    y%merge) .AND. &
               (x%scnid .EqualTo. y%scnid) .AND. &
               (x%hwhm  .EqualTo. y%hwhm ) .AND. &
               (x%idabs     ==    y%idabs) .AND. &
               (x%atm       ==    y%atm  ) .AND. &
               (x%layr1     ==    y%layr1) .AND. &
               (x%nlayr     ==    y%nlayr)) ) RETURN

    ! If we get here, then...
    is_equal = .TRUE.
  END FUNCTION LBLRTM_Fhdr_Equal


!------------------------------------------------------------------------------
!
! NAME:
!       LBLRTM_Fhdr_NotEqual
!
! PURPOSE:
!       Elemental function to test the inequality of two LBLRTM_Fhdr objects.
!       Used in OPERATOR(/=) interface block.
!
!       This function is syntactic sugar.
!
! CALLING SEQUENCE:
!       not_equal = LBLRTM_Fhdr_NotEqual( x, y )
!
!         or
!
!       IF ( x /= y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two LBLRTM_Fhdr objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Fhdr_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       not_equal:     Logical value indicating whether the inputs are not equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION LBLRTM_Fhdr_NotEqual( x, y ) RESULT( not_equal )
    TYPE(LBLRTM_Fhdr_type), INTENT(IN) :: x, y
    LOGICAL :: not_equal
    not_equal = .NOT. (x == y)
  END FUNCTION LBLRTM_Fhdr_NotEqual

END MODULE LBLRTM_Fhdr_Define
