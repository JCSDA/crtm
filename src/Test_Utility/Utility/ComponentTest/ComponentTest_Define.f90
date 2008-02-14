!--------------------------------------------------------------------------------
!M+
! NAME:
!       ComponentTest_Define
!
! PURPOSE:
!       Module defining the structures to hold CRTM component model
!       (e.g. Forward/Tangent-Linear, Tangent-Linear/Adjoint, Adjoint/K-matrix)
!       test results and containing routines to manipulate it.
!       
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE ComponentTest_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
! CONTAINS:
!       Associated_ComponentTest:  Function to test the association status
!                                  of the pointer members of a ComponentTest
!                                  structure.
!
!       Destroy_ComponentTest:     Function to re-initialize an ComponentTest
!                                  structure.
!
!       Allocate_ComponentTest:    Function to allocate the pointer members
!                                  of an ComponentTest structure.
!
!
! DERIVED TYPES:
!       ComponentTest_type:   Definition of the ComponentTest data structure. Fields
!                             are...
!
!
!       *!IMPORTANT!*
!       -------------
!       Note that the ComponentTest_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user operate on
!       the structure using only the routines in this module where
!       possible to eliminate -- or at least minimise -- the possibility
!       of memory leakage since most of the structure members are
!       pointers.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2006 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!--------------------------------------------------------------------------------

MODULE ComponentTest_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public module parameters
  PUBLIC :: COMPONENTTEST_TESTTYPE    ! All the test types
  PUBLIC :: COMPONENTTEST_FWDTL_TESTTYPE
  PUBLIC :: COMPONENTTEST_TLAD_TESTTYPE
  PUBLIC :: COMPONENTTEST_ADK_TESTTYPE

  PUBLIC :: COMPONENTTEST_DATATYPE ! All the data types
  PUBLIC :: COMPONENTTEST_POLY_DATATYPE
  PUBLIC :: COMPONENTTEST_MONO_DATATYPE

  ! -- The ComponentTest derived type definition
  PUBLIC :: ComponentTest_type

  ! -- Public procedures to manipulate the ComponentTest structure
  PUBLIC :: Associated_ComponentTest
  PUBLIC :: Destroy_ComponentTest
  PUBLIC :: Allocate_ComponentTest


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id for the module
  CHARACTER( * ), PARAMETER :: MODULE_RCS_ID = &
  '$Id: ComponentTest_Define.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $'

  ! -- Keyword set value
  INTEGER, PARAMETER :: SET = 1

  ! -- Variable name string length
  INTEGER, PARAMETER :: SL = 64

  ! -- Initialisation values for pointer components
  INTEGER,         PARAMETER :: IP_INIT = -1
  REAL( fp_kind ), PARAMETER :: FP_INIT = -999.0_fp_kind

  ! -- The test types
  INTEGER, PARAMETER :: N_COMPONENTTEST_TESTTYPE = 3
  INTEGER, PARAMETER :: COMPONENTTEST_FWDTL_TESTTYPE = 1  ! FWD/TL model test
  INTEGER, PARAMETER :: COMPONENTTEST_TLAD_TESTTYPE  = 2  ! TL/AD model test
  INTEGER, PARAMETER :: COMPONENTTEST_ADK_TESTTYPE   = 3  ! AD/K model test
  INTEGER, PARAMETER, DIMENSION( N_COMPONENTTEST_TESTTYPE ) :: &
    COMPONENTTEST_TESTTYPE = (/ COMPONENTTEST_FWDTL_TESTTYPE, &
                                COMPONENTTEST_TLAD_TESTTYPE,  &
                                COMPONENTTEST_ADK_TESTTYPE   /)


  ! -- The data types
  INTEGER, PARAMETER :: N_COMPONENTTEST_DATATYPE = 2
  INTEGER, PARAMETER :: COMPONENTTEST_POLY_DATATYPE  = 1  ! Polychromatic data
  INTEGER, PARAMETER :: COMPONENTTEST_MONO_DATATYPE  = 2  ! Monochromatic data
  INTEGER, PARAMETER, DIMENSION( N_COMPONENTTEST_DATATYPE ) :: &
    COMPONENTTEST_DATATYPE = (/ COMPONENTTEST_POLY_DATATYPE,   &
                                COMPONENTTEST_MONO_DATATYPE /)


  ! -----------------------------------
  ! ComponentTest data type definitions
  ! -----------------------------------

  TYPE :: ComponentTest_type
    INTEGER :: n_Allocates = 0

    ! -- The character string length
    INTEGER :: StrLen = SL

    ! -- Dimensions
    INTEGER :: nK  = 0  ! Layer dimension - For surface code test, this should be set to 1
    INTEGER :: nL  = 0  ! Spectral dimension
    INTEGER :: nP  = 0  ! Perturbation dimension - For non-FWD/TL tests, this should be set to 1
    INTEGER :: nIV = 0  ! Input variable dimension
    INTEGER :: nOV = 0  ! Output variable dimension

    ! -- The instance for a given atmospheric profile (dataset) and its identifier
    INTEGER :: nM = 0
    CHARACTER( SL ) :: nM_Name

    ! -- The component test type (FWD/TL, TL/AD, or AD/K)
    INTEGER :: TestType = 0

    ! -- The data type (Sensor or Spectral)
    INTEGER :: DataType = 0

    ! -- Pressure, spectral dimension, and perturbation amount identifiers.
    ! -- For POLY data type, Spectral == Instrument channel number
    ! --     MONO data type, Spectral == Frequency (either cm^-1 or GHz)
    REAL( fp_kind ),     POINTER, DIMENSION( : ) :: Pressure     => NULL()  ! nK
    REAL( fp_kind ),     POINTER, DIMENSION( : ) :: Spectral     => NULL()  ! nL
    REAL( fp_kind ),     POINTER, DIMENSION( : ) :: Perturbation => NULL()  ! nP

    ! -- The input variable names and units
    CHARACTER( SL ), POINTER, DIMENSION( : ) :: Input_Variable_Name  => NULL()  ! nIV
    CHARACTER( SL ), POINTER, DIMENSION( : ) :: Input_Variable_Units => NULL()  ! nIV

    ! -- The output variable names and units
    CHARACTER( SL ), POINTER, DIMENSION( : ) :: Output_Variable_Name  => NULL()  ! nOV
    CHARACTER( SL ), POINTER, DIMENSION( : ) :: Output_Variable_Units => NULL()  ! nOV

    ! -- The test results
    REAL( fp_kind ), POINTER, DIMENSION( :,:,:,:,: ) :: d1 => NULL()  ! K x L x nP x nIV x nOV
    REAL( fp_kind ), POINTER, DIMENSION( :,:,:,:,: ) :: d2 => NULL()  ! K x L x nP x nIV x nOV

  END TYPE ComponentTest_type


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Clear_ComponentTest
!
! PURPOSE:
!       Subroutine to clear the scalar members of ComponentTest structures.
!
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_ComponentTest( ComponentTest) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       ComponentTest:  ComponentTest structure for which the scalar members have
!                       been cleared.
!                       UNITS:      N/A
!                       TYPE:       ComponentTest_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output ComponentTest argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_ComponentTest( ComponentTest )

    TYPE( ComponentTest_type ), INTENT( IN OUT ) :: ComponentTest

    ComponentTest%nK  = 0
    ComponentTest%nL  = 0
    ComponentTest%nP  = 0
    ComponentTest%nIV = 0
    ComponentTest%nOV = 0

    ComponentTest%nM      = 0
    ComponentTest%nM_Name = ' '
 
    ComponentTest%TestType = 0
    ComponentTest%DataType = 0

  END SUBROUTINE Clear_ComponentTest


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       Associated_ComponentTest
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       ComponentTest structure.
!
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_ComponentTest( ComponentTest,      &  ! Input
!                                                      ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       ComponentTest:       ComponentTest structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       ComponentTest_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            ComponentTest structure pointer members are associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the pointer members
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the ComponentTest pointer members.
!                            .TRUE.  - if ALL the ComponentTest pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the ComponentTest pointer
!                                      members are associated.
!                            .FALSE. - some or all of the ComponentTest pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_ComponentTest( ComponentTest, & ! Input
                                     ANY_Test )     & ! Optional input
                                   RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( ComponentTest_type ), INTENT( IN ) :: ComponentTest

    ! -- Optional input
    INTEGER,          OPTIONAL, INTENT( IN ) :: ANY_Test


    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Default is to test ALL the pointer members
    ! -- for a true association status....
    ALL_Test = .TRUE.

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( ComponentTest%Pressure              ) .AND. &
           ASSOCIATED( ComponentTest%Spectral              ) .AND. &
           ASSOCIATED( ComponentTest%Perturbation          ) .AND. &
           ASSOCIATED( ComponentTest%Input_Variable_Name   ) .AND. &
           ASSOCIATED( ComponentTest%Input_Variable_Units  ) .AND. &
           ASSOCIATED( ComponentTest%Output_Variable_Name  ) .AND. &
           ASSOCIATED( ComponentTest%Output_Variable_Units ) .AND. &
           ASSOCIATED( ComponentTest%d1                    ) .AND. &
           ASSOCIATED( ComponentTest%d2                    )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( ComponentTest%Pressure              ) .OR. &
           ASSOCIATED( ComponentTest%Spectral              ) .OR. &
           ASSOCIATED( ComponentTest%Perturbation          ) .OR. &
           ASSOCIATED( ComponentTest%Input_Variable_Name   ) .OR. &
           ASSOCIATED( ComponentTest%Input_Variable_Units  ) .OR. &
           ASSOCIATED( ComponentTest%Output_Variable_Name  ) .OR. &
           ASSOCIATED( ComponentTest%Output_Variable_Units ) .OR. &
           ASSOCIATED( ComponentTest%d1                    ) .OR. &
           ASSOCIATED( ComponentTest%d2                    )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_ComponentTest


!--------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_ComponentTest
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of ComponentTest
!       data structures.
!
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_ComponentTest( ComponentTest,            &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ComponentTest:  Re-initialized ComponentTest structure.
!                       UNITS:      N/A
!                       TYPE:       ComponentTest_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
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
! CALLS:
!       Display_Message:    Subroutine to output messages
!                           SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output ComponentTest argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_ComponentTest( ComponentTest, &  ! Output
                                  No_Clear,      &  ! Optional input
                                  RCS_Id,        &  ! Revision control
                                  Message_Log )  &  ! Error messaging
                                RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( ComponentTest_type ), INTENT( IN OUT ) :: ComponentTest

    ! -- Optional input
    INTEGER,          OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),   OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),   OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_ComponentTest'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -- Default is to clear scalar members...
    Clear = .TRUE.
    ! -- ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    
    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_ComponentTest( ComponentTest )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_ComponentTest( ComponentTest ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the pressure array
    IF ( ASSOCIATED( ComponentTest%Pressure ) ) THEN

      DEALLOCATE( ComponentTest%Pressure, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ComponentTest Pressure ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Spectral array
    IF ( ASSOCIATED( ComponentTest%Spectral ) ) THEN

      DEALLOCATE( ComponentTest%Spectral, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ComponentTest Spectral dimension ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Perturbation
    IF ( ASSOCIATED( ComponentTest%Perturbation ) ) THEN

      DEALLOCATE( ComponentTest%Perturbation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ComponentTest Perturbation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Input_Variable_Name
    IF ( ASSOCIATED( ComponentTest%Input_Variable_Name ) ) THEN

      DEALLOCATE( ComponentTest%Input_Variable_Name, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ComponentTest Input_Variable_Name ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Input_Variable_Units
    IF ( ASSOCIATED( ComponentTest%Input_Variable_Units ) ) THEN

      DEALLOCATE( ComponentTest%Input_Variable_Units, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ComponentTest Input_Variable_Units ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Output_Variable_Name
    IF ( ASSOCIATED( ComponentTest%Output_Variable_Name ) ) THEN

      DEALLOCATE( ComponentTest%Output_Variable_Name, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ComponentTest Output_Variable_Name ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Output_Variable_Units
    IF ( ASSOCIATED( ComponentTest%Output_Variable_Units ) ) THEN

      DEALLOCATE( ComponentTest%Output_Variable_Units, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ComponentTest Output_Variable_Units ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate d1
    IF ( ASSOCIATED( ComponentTest%d1 ) ) THEN

      DEALLOCATE( ComponentTest%d1, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ComponentTest d1 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate d_TL
    IF ( ASSOCIATED( ComponentTest%d2 ) ) THEN

      DEALLOCATE( ComponentTest%d2, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating ComponentTest d2 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF




    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    ComponentTest%n_Allocates = ComponentTest%n_Allocates - 1

    IF ( ComponentTest%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      ComponentTest%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_ComponentTest





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_ComponentTest
! 
! PURPOSE:
!       Function to allocate the pointer members of the ComponentTest
!       data structures.
!
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_ComponentTest( nK, nL, nP, nIV, nOv,     &  ! Input
!                                              ComponentTest,            &  ! Output
!                                              RCS_Id = RCS_Id,          &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       nK:                   The number of layers dimension of the
!                             ComponentTest data. For surface component tests,
!                             should be set to 1.
!                             Must be > 0.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( IN )
!
!       nL:                   The spectral dimension (channels/frequencies) of
!                             the ComponentTest data.
!                             Must be > 0.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( IN )
!
!       nP:                   The number of perturbations dimension of the
!                             ComponentTest data. For non-FWD/TL component tests,
!                             should be set to 1.
!                             Must be > 0.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( IN )
!
!       nIV:                  The number of input variables dimension of the
!                             ComponentTest data.
!                             Must be > 0.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( IN )
!
!       nOV:                  The number of output variables dimension of the
!                             ComponentTest data.
!                             Must be > 0.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:          Character string specifying a filename in which any
!                             messages will be logged. If not specified, or if an
!                             error occurs opening the log file, the default action
!                             is to output messages to standard output.
!                             UNITS:      None
!                             TYPE:       CHARACTER(*)
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ComponentTest:        ComponentTest structure with allocated pointer members
!                             UNITS:      N/A
!                             TYPE:       ComponentTest_type
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:               Character string containing the Revision Control
!                             System Id field for the module.
!                             UNITS:      None
!                             TYPE:       CHARACTER(*)
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:         The return value is an integer defining the error status.
!                             The error codes are defined in the ERROR_HANDLER module.
!                             If == SUCCESS the structure pointer allocations were
!                                           successful
!                                == FAILURE - an error occurred, or
!                                           - the structure internal allocation counter
!                                             is not equal to one (1) upon exiting this
!                                             function. This value is incremented and
!                                             decremented for every structure allocation
!                                             and deallocation respectively.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!
! CALLS:
!       Associated_ComponentTest:  Function to test the association status of the
!                                  pointer members of a ComponentTest structure.
!
!       Destroy_ComponentTest:     Function to re-initialize the scalar and pointer
!                                  members of ComponentTest data structures.
!
!       Display_Message:           Subroutine to output messages
!                                  SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output ComponentTest argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_ComponentTest( nK,            &  ! Input
                                   nL,            &  ! Input
                                   nP,            &  ! Input
                                   nIV,           &  ! Input
                                   nOV,           &  ! Input
                                   ComponentTest, &  ! Output
                                   RCS_Id,        &  ! Revision control
                                   Message_Log )  &  ! Error messaging
                                 RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                    INTENT( IN )     :: nK           
    INTEGER,                    INTENT( IN )     :: nL         
    INTEGER,                    INTENT( IN )     :: nP    
    INTEGER,                    INTENT( IN )     :: nIV  
    INTEGER,                    INTENT( IN )     :: nOV  

    ! -- Output
    TYPE( ComponentTest_type ), INTENT( IN OUT ) :: ComponentTest

    ! -- Revision control
    CHARACTER( * ),   OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),   OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_ComponentTest'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    IF ( nK  < 1 .OR. &
         nL  < 1 .OR. &
         nP  < 1 .OR. &
         nIV < 1 .OR. &
         nOV < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_ComponentTest( ComponentTest, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_ComponentTest( ComponentTest, &
                                            No_Clear = SET, &
                                            Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating ComponentTest pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( ComponentTest%Pressure( nK ), &
              ComponentTest%Spectral( nL ), &
              ComponentTest%Perturbation( nP ),   &
              ComponentTest%Input_Variable_Name( nIV ),  &
              ComponentTest%Input_Variable_Units( nIV ),  &
              ComponentTest%Output_Variable_Name( nOV ),  &
              ComponentTest%Output_Variable_Units( nOV ),  &
              ComponentTest%d1( nK, nL, nP, nIV, nOV ), &
              ComponentTest%d2( nK, nL, nP, nIV, nOV ), &
              STAT = Allocate_Status   )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating ComponentTest data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#          -- ASSIGN THE DIMENSIONS AND INITIALISE THE ARRAYS --           #
    !#--------------------------------------------------------------------------#

    ComponentTest%nK  = nK
    ComponentTest%nL  = nL
    ComponentTest%nP  = nP
    ComponentTest%nIV = nIV
    ComponentTest%nOV = nOV

    ComponentTest%Pressure     = FP_INIT
    ComponentTest%Spectral     = FP_INIT
    ComponentTest%Perturbation = FP_INIT
    ComponentTest%Input_Variable_Name   = ' '
    ComponentTest%Input_Variable_Units  = ' '
    ComponentTest%Output_Variable_Name  = ' '
    ComponentTest%Output_Variable_Units = ' '
    ComponentTest%d1 = FP_INIT
    ComponentTest%d2 = FP_INIT



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    ComponentTest%n_Allocates = ComponentTest%n_Allocates + 1

    IF ( ComponentTest%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      ComponentTest%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_ComponentTest

END MODULE ComponentTest_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: ComponentTest_Define.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: ComponentTest_Define.f90,v $
! Revision 1.2  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2006/03/06 19:24:32  paulv
! Initial checkin. Untested.
!
!
!
