!------------------------------------------------------------------------------
!M+
! NAME:
!       TauCoeff_SARTA_Subset_Define
!
! PURPOSE:
!       Module defining the TauCoeff_SARTA data structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE TauCoeff_SARTA_Subset_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
! CONTAINS:
!       Associated_TauCoeff_Subset:    Function to test the association status
!                                      of the pointer members of a TauCoeff_SARTA_Subset
!                                      structure.
!
!       Destroy_TauCoeff_Subset:       Function to re-initialize a TauCoeff_SARTA_Subset
!                                      structure.
!
!       Allocate_TauCoeff_Subset:      Function to allocate the pointer members
!                                      of a TauCoeff_SARTA_Subset structure.
!
!       Assign_TauCoeff_Subset:        Function to copy a valid TauCoeff_SARTA_Subset structure.
!
!
!       Check_TauCoeff_Subset_Release: Function to check the TauCoeff_SARTA_Subset Release value.
!
!       Version_TauCoeff_Subset:       Subroutine to return a string containing
!                                      version and dimension information about
!                                      the TauCoeff_SARTA_Subset data structure.
!
! DERIVED TYPES:
!       TauCoeff_SARTA_Subset_type:   Definition of the public TauCoeff_SARTA_Subset data structure. Fields
!                        are...
!
!         Release:             Coefficient data file release number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         Version:             Coefficient data file version number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Layers:            Maximum layers for the aborber coefficients.
!                              "Ilayers" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Total_Predictors:  Number of predictors used in the
!                              gas absorption regression.
!                              "Iuse" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Absorbers:         Number of gaseous absorbers.
!                              "J" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Channels:          Total number of spectral channels.
!                              "L" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         Channel_Index:       This is the sensor channel number associated
!                              with the data in the coefficient file. Helps
!                              in identifying channels where the numbers are
!                              not contiguous (e.g. AIRS).
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Rank-1 (n_Channels)
!                              ATTRIBUTES: POINTER
!
!         Absorber_ID:         Array containing a list of absorber ID
!                              values. Used to identify the individual
!                              or collective molecular species for which
!                              the gas absorption coefficients were
!                              generated.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  J (n_Absorbers)
!                              ATTRIBUTES: POINTER
!
!         Absorber_Predictor:  Array containing the predictor numbers
!                              used to identify how many predictors to use
!                              for each gases in the gas absorption model.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  J (n_Absorbers)
!                              ATTRIBUTES: POINTER
!
!         C:                   Array containing the gas absorption
!                              model coefficients.
!                              UNITS:      Variable
!                              TYPE:       REAL( Double )
!                              DIMENSION:  Iuse x Ilayer x L
!                              ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the TauCoeff_SARTA_Subset_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user initialize,
!       destroy, allocate, assign, and concatenate the structure
!       using only the routines in this module where possible to
!       eliminate -- or at least minimise -- the possibility of 
!       memory leakage since most of the structure members are
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
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!
!  Copyright (C) 2006 Yong Chen
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
!------------------------------------------------------------------------------

MODULE TauCoeff_SARTA_Subset_Define


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

  ! -- Public procedures to manipulate the TauCoeff_SARTA_Subset structure
  PUBLIC :: Associated_TauCoeff_Subset
  PUBLIC :: Destroy_TauCoeff_Subset
  PUBLIC :: Allocate_TauCoeff_Subset
  PUBLIC :: Assign_TauCoeff_Subset
  PUBLIC :: Zero_TauCoeff_Subset
  PUBLIC :: Check_TauCoeff_Subset_Release
  PUBLIC :: Version_TauCoeff_Subset

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Destroy_TauCoeff_Subset 
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE Destroy_TauCoeff_Subset 

  INTERFACE Allocate_TauCoeff_Subset 
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank1
  END INTERFACE Allocate_TauCoeff_Subset 

  INTERFACE Assign_TauCoeff_Subset 
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE Assign_TauCoeff_Subset 

  INTERFACE Zero_TauCoeff_Subset 
    MODULE PROCEDURE Zero_Scalar
    MODULE PROCEDURE Zero_Rank1
  END INTERFACE Zero_TauCoeff_Subset 


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- TauCoeff_SARTA_Subset valid values
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1
  INTEGER, PRIVATE, PARAMETER ::   VALID =  1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Current valid release and version numbers
  INTEGER, PRIVATE, PARAMETER :: TauCoeff_Subset_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PRIVATE, PARAMETER :: TauCoeff_Subset_VERSION = 1  ! This is just the data version.


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! ------------------------------
  ! TauCoeff_SARTA_Subset data type definition
  ! ------------------------------
 
  TYPE, PUBLIC :: TauCoeff_SARTA_Subset_type
    INTEGER :: n_Allocates = 0

    ! -- Release and version information
    INTEGER( Long ) :: Release = TAUCOEFF_SUBSET_RELEASE
    INTEGER( Long ) :: Version = TAUCOEFF_SUBSET_VERSION

    ! -- Array dimensions
    INTEGER( Long ) :: n_Layers           = 0    ! Ilayer
    INTEGER( Long ) :: n_Total_Predictors = 0    ! Iuse
    INTEGER( Long ) :: n_Absorbers        = 0    ! J
    INTEGER( Long ) :: n_Channels         = 0    ! L

    ! -- The actual sensor channel numbers
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_Index => NULL()    ! L  value from 1-2378

    ! -- The absorber ID
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Absorber_ID => NULL()     ! J

    ! -- The predictor numbers for each absorber
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Absorber_Predictor => NULL()  ! J

    ! -- The array of coefficients
    REAL( Single ),  POINTER, DIMENSION( :, :, : ) :: C => NULL() ! Iuse x Ilayer x L
  END TYPE TauCoeff_SARTA_Subset_type

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
!       Clear_TauCoeff_Subset
!
! PURPOSE:
!       Subroutine to clear the scalar members of a TauCoeff_SARTA_Subset structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_TauCoeff_Subset( TauCoeff_Subset ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       TauCoeff_Subset:    TauCoeff_SARTA_Subset structure for which the scalar members have
!                           been cleared.
!                           UNITS:	N/A
!                           TYPE:	TauCoeff_SARTA_Subset_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
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
!       Note the INTENT on the output TauCoeff_SARTA_Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_TauCoeff_Subset( TauCoeff_Subset )

    TYPE( TauCoeff_SARTA_Subset_type ), INTENT( IN OUT ) :: TauCoeff_Subset

    TauCoeff_Subset%n_Layers           = 0
    TauCoeff_Subset%n_Total_Predictors = 0
    TauCoeff_Subset%n_Absorbers        = 0
    TauCoeff_Subset%n_Channels         = 0

  END SUBROUTINE Clear_TauCoeff_Subset





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
!       Associated_TauCoeff_Subset
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       TauCoeff_SARTA_Subset structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_TauCoeff_Subset( TauCoeff_Subset,           &  ! Input
!                                                 ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       TauCoeff_Subset:    TauCoeff_SARTA_Subset structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       TauCoeff_SARTA_Subset_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:    Set this argument to test if ANY of the
!                    TauCoeff_SARTA_Subset structure pointer members are associated.
!                    The default is to test if ALL the pointer members
!                    are associated.
!                    If ANY_Test = 0, test if ALL the pointer members
!                                     are associated.  (DEFAULT)
!                       ANY_Test = 1, test if ANY of the pointer members
!                                     are associated.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the TauCoeff_SARTA_Subset pointer members.
!                            .TRUE.  - if ALL the TauCoeff_SARTA_Subset pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the TauCoeff_SARTA_Subset pointer
!                                      members are associated.
!                            .FALSE. - some or all of the TauCoeff_SARTA_Subset pointer
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
!       This function tests the association status of the TauCoeff_SARTA_Subset
!       structure pointer members. Therefore this function must only
!       be called after the input TauCoeff_SARTA_Subset structure has, at least,
!       had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_TauCoeff_Subset( TauCoeff_Subset,  & ! Input
                                       ANY_Test )        & ! Optional input
                                     RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_Subset_type ), INTENT( IN ) :: TauCoeff_Subset

    ! -- Optional input
    INTEGER,     OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( TauCoeff_Subset%Channel_Index     ) .AND. &
           ASSOCIATED( TauCoeff_Subset%Absorber_ID       ) .AND. &
           ASSOCIATED( TauCoeff_Subset%Absorber_Predictor) .AND. &
           ASSOCIATED( TauCoeff_Subset%C                 )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( TauCoeff_Subset%Channel_Index     ) .OR. &
           ASSOCIATED( TauCoeff_Subset%Absorber_ID       ) .OR. &
           ASSOCIATED( TauCoeff_Subset%Absorber_Predictor) .OR. &
           ASSOCIATED( TauCoeff_Subset%C                 )       ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_TauCoeff_Subset





!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_TauCoeff_Subset
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of TauCoeff_SARTA_Subset
!       data structures.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_TauCoeff_Subset( TauCoeff_Subset,                 &  ! Output
!                                               RCS_Id = RCS_Id,	  &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauCoeff_Subset:Re-initialized TauCoeff_SARTA_Subset structure.
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_SARTA_Subset_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to zero (0) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_TauCoeff_Subset:  Function to test the association status of the
!                             pointer members of a TauCoeff_SARTA_Subset structure.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output TauCoeff_SARTA_Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( TauCoeff_Subset,  &  ! Output
                           No_Clear,	     &  ! Optional input  
                           RCS_Id,	     &  ! Revision control
                           Message_Log )     &  ! Error messaging 
                          RESULT( Error_Status )	        



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN OUT ) :: TauCoeff_Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_TauCoeff_SARTA_Subset(Scalar)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message
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
    !#                     -- PERFORM RE-INITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_TauCoeff_Subset( TauCoeff_Subset )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_TauCoeff_Subset( TauCoeff_Subset ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the sensor channel number array
    IF ( ASSOCIATED( TauCoeff_Subset%Channel_Index ) ) THEN

      DEALLOCATE( TauCoeff_Subset%Channel_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_Subset Channel_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Absorber_ID
    IF ( ASSOCIATED( TauCoeff_Subset%Absorber_ID ) ) THEN

      DEALLOCATE( TauCoeff_Subset%Absorber_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_Subset Absorber_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Absorber predictor numbers for each absorber
    IF ( ASSOCIATED( TauCoeff_Subset%Absorber_Predictor ) ) THEN

      DEALLOCATE( TauCoeff_Subset%Absorber_Predictor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_Subset Absorber_Predictor ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate coefficients
    IF ( ASSOCIATED( TauCoeff_Subset%C ) ) THEN

      DEALLOCATE( TauCoeff_Subset%C, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating TauCoeff_SARTA_Subset coefficients ", &
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

    TauCoeff_Subset%n_Allocates = TauCoeff_Subset%n_Allocates - 1

    IF ( TauCoeff_Subset%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      TauCoeff_Subset%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar 


  FUNCTION Destroy_Rank1( TauCoeff_Subset,  &  ! Output
                           No_Clear,	     &  ! Optional input  
                           RCS_Id,	     &  ! Revision control
                           Message_Log )     &  ! Error messaging 
                          RESULT( Error_Status )	        


    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ), DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_TauCoeff_SARTA_Subset(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message
    INTEGER :: Scalar_Status
    INTEGER :: n



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
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    DO n = 1, SIZE( TauCoeff_Subset )

      Scalar_Status = Destroy_Scalar( TauCoeff_Subset(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of TauCoeff_SARTA_Subset structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_Rank1



!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_TauCoeff_Subset
! 
! PURPOSE:
!       Function to allocate the pointer members of the TauCoeff_SARTA_Subset
!       data structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_TauCoeff_Subset( n_Layers,                &  ! Input	        
!                                                n_Total_Predictors,	  &  ! Input	        
!                                                n_Absorbers,		  &  ! Input	        
!                                                n_Channels,		  &  ! Input	        
!                                                TauCoeff_Subset,	  &  ! Output	        
!                                                RCS_Id      = RCS_Id,    &  ! Revision control
!                                                Message_Log = Message_Log)  ! Error messaging 
!
! INPUT ARGUMENTS:
!         n_Layers:            Maximum layers for the aborber coefficients.
!                              "Ilayers" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Total_Predictors:  Number of predictors used in the
!                              gas absorption regression.
!                              "Iuse" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Absorbers:         Number of gaseous absorbers.
!                              "J" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Channels:          Total number of spectral channels.
!                              "L" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in
!                     which any messages will be logged. If not
!                     specified, or if an error occurs opening
!                     the log file, the default action is to
!                     output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauCoeff_Subset:     TauCoeff_SARTA_Subset structure with allocated
!                     pointer members
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_SARTA_Subset_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to one (1) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_TauCoeff_Subset:  Function to test the association status of the
!                             pointer members of a TauCoeff_SARTA_Subset structure.
!
!       Destroy_TauCoeff_Subset:     Function to re-initialize the scalar and pointer
!                             members of TauCoeff_SARTA_Subset data structures.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output TauCoeff_SARTA_Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar(n_Layers,		    &  ! Input	       
                  	   n_Total_Predictors,      &  ! Input  	          
                  	   n_Absorbers, 	    &  ! Input  	          
                  	   n_Channels,  	    &  ! Input  	          
                  	   TauCoeff_Subset,	    &  ! Output 	          
                  	   RCS_Id,                  &  ! Revision control         
                  	   Message_Log )            &  ! Error messaging          
                  	RESULT( Error_Status )  			          



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )     :: n_Layers 
    INTEGER,                  INTENT( IN )     :: n_Total_Predictors
    INTEGER,                  INTENT( IN )     :: n_Absorbers
    INTEGER,                  INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN OUT ) :: TauCoeff_Subset

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_TauCoeff_SARTA_Subset(Scalar)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

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

    IF ( n_Layers           < 1 .OR. &
         n_Total_Predictors < 1 .OR. &
         n_Absorbers        < 1 .OR. &
         n_Channels         < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauCoeff_SARTA_Subset dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_TauCoeff_Subset( TauCoeff_Subset, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_TauCoeff_Subset( TauCoeff_Subset, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating TauCoeff_SARTA_Subset pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( TauCoeff_Subset%Channel_Index( n_Channels ), &
              TauCoeff_Subset%Absorber_ID( n_Absorbers ), &
              TauCoeff_Subset%Absorber_Predictor( n_Absorbers ), &
              TauCoeff_Subset%C( n_Total_Predictors, n_Layers, n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating TauCoeff_SARTA_Subset data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                        -- ASSIGN THE DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    TauCoeff_Subset%n_Layers     = n_Layers
    TauCoeff_Subset%n_Total_Predictors = n_Total_Predictors
    TauCoeff_Subset%n_Absorbers  = n_Absorbers
    TauCoeff_Subset%n_Channels   = n_Channels



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    TauCoeff_Subset%n_Allocates = TauCoeff_Subset%n_Allocates + 1

    IF ( TauCoeff_Subset%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      TauCoeff_Subset%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Scalar 


  FUNCTION Allocate_Rank1(n_Layers,		    &  ! Input	       
                  	   n_Total_Predictors,      &  ! Input  	          
                  	   n_Absorbers, 	    &  ! Input  	          
                  	   n_Channels,  	    &  ! Input  	          
                  	   TauCoeff_Subset,	    &  ! Output 	          
                  	   RCS_Id,                  &  ! Revision control         
                  	   Message_Log )            &  ! Error messaging          
                  	RESULT( Error_Status )  			          



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )     :: n_Layers 
    INTEGER,                  INTENT( IN )     :: n_Total_Predictors
    INTEGER,                  INTENT( IN )     :: n_Absorbers
    INTEGER,                  INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ), DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_Subset

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_TauCoeff_SARTA_Subset(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: Scalar_Status
    INTEGER :: i



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
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, SIZE( TauCoeff_Subset )

      Scalar_Status = Allocate_Scalar( n_Layers, &
                                       n_Total_Predictors, &
                                       n_Absorbers, &
                                       n_Channels, &
				       TauCoeff_Subset(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of TauCoeff_SARTA_Subset structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank1



!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_TauCoeff_Subset
!
! PURPOSE:
!       Function to copy valid TauCoeff_SARTA_Subset structures.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_TauCoeff_Subset( TauCoeff_Subset_in,       &  ! Input
!                                              TauCoeff_Subset_out,	 &  ! Output    
!                                              RCS_Id	   = RCS_Id,	 &  ! Revision control 
!                                              Message_Log = Message_Log )  ! Error messaging  
!
! INPUT ARGUMENTS:
!       TauCoeff_Subset_in:   TauCoeff_SARTA_Subset structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_SARTA_Subset_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauCoeff_Subset_out:  Copy of the input structure, TauCoeff_SARTA_Subset_in.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_SARTA_Subset_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_TauCoeff_Subset:  Function to test the association status of the
!                             pointer members of a TauCoeff_SARTA_Subset structure.
!
!       Allocate_TauCoeff_Subset:    Function to allocate the pointer members of
!                             the TauCoeff_SARTA_Subset data structure.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output TauCoeff_SARTA_Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( TauCoeff_Subset_in,   &  ! Input     
                          TauCoeff_Subset_out,  &  ! Output      
                          RCS_Id,	 &  ! Revision control   
                          Message_Log )  &  ! Error messaging    
                        RESULT( Error_Status )		        



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN )     :: TauCoeff_Subset_in

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN OUT ) :: TauCoeff_Subset_out

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_TauCoeff_SARTA_Subset(Scalar)'



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! ALL *input* pointers must be associated
    !
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    ! ---------------------------------------

    IF ( .NOT. Associated_TauCoeff_Subset( TauCoeff_Subset_In ) ) THEN

      Error_Status = Destroy_TauCoeff_Subset( TauCoeff_Subset_Out, &
                                         Message_Log = Message_Log )
       CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT TauCoeff_SARTA_Subset pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_TauCoeff_Subset( TauCoeff_Subset_In%n_Layers, &
                                             TauCoeff_Subset_In%n_Total_Predictors, &
                                             TauCoeff_Subset_In%n_Absorbers, &
                                             TauCoeff_Subset_In%n_Channels, &
                                             TauCoeff_Subset_Out, &
                                             Message_Log = Message_Log ) 
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output TauCoeff_SARTA_Subset arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    TauCoeff_Subset_out%Release = TauCoeff_Subset_in%Release
    TauCoeff_Subset_out%Version = TauCoeff_Subset_in%Version

 
    ! -----------------
    ! Assign array data
    ! -----------------
    TauCoeff_Subset_out%Channel_Index      = TauCoeff_Subset_in%Channel_Index
    TauCoeff_Subset_out%Absorber_ID        = TauCoeff_Subset_in%Absorber_ID
    TauCoeff_Subset_out%Absorber_Predictor = TauCoeff_Subset_in%Absorber_Predictor
    TauCoeff_Subset_out%C                  = TauCoeff_Subset_in%C

  END FUNCTION Assign_Scalar 



  FUNCTION Assign_Rank1(  TauCoeff_Subset_in,   &  ! Input     
                          TauCoeff_Subset_out,  &  ! Output      
                          RCS_Id,	 &  ! Revision control   
                          Message_Log )  &  ! Error messaging    
                        RESULT( Error_Status )		        



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_Subset_type ),  DIMENSION( : ), INTENT( IN )     :: TauCoeff_Subset_in

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ),  DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_Subset_out

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_TauCoeff_SARTA_Subset(Rank-1)'

    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: Scalar_Status
    INTEGER :: i, n

    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


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
    !#                               -- TEST THE INPUT --                       #
    !#--------------------------------------------------------------------------#

    n = SIZE( TauCoeff_Subset_in )

    IF ( SIZE( TauCoeff_Subset_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input TauCoeff_SARTA_Subset_in and TauCoeff_SARTA_Subset_out '//&
			    'arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n 

      Scalar_Status = Assign_Scalar(   TauCoeff_Subset_in(i), & 
				       TauCoeff_Subset_out(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of TauCoeff_SARTA_Subset structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO
  END FUNCTION Assign_Rank1

!--------------------------------------------------------------------------------
!S+
! NAME:
!       Zero_TauCoeff_Subset
! 
! PURPOSE:
!       Subroutine to zero-out all members of a TauCoeff_SARTA_Subset structure - both
!       scalar and pointer.
!
! CATEGORY:
!       Optical Depth : Coefficients: subset
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE: 
!       CALL Zero_TauCoeff_Subset( TauCoeff_Subset)
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       TauCoeff_Subset:Zero TauCoeff_SARTA_Subset structure.
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_SARTA_Subset_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank1 array
!                     ATTRIBUTES: INTENT( IN OUT )
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
!       - No checking of the input structure is performed, so there are no
!         tests for pointer member association status. This means the Cloud
!         structure must have allocated pointer members upon entry to this
!         routine.
!
!       - The dimension components of the structure are *NOT*
!         set to zero.
!
!       - The cloud type component is *NOT* reset.
!
! COMMENTS:
!       Note the INTENT on the output TauCoeff_SARTA_Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument must be defined upon
!       input.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Zero_Scalar( TauCoeff_Subset )  ! Output
    TYPE( TauCoeff_SARTA_Subset_type ),  INTENT( IN OUT ) :: TauCoeff_Subset

    ! -- Reset the array components
    TauCoeff_Subset%Channel_Index      = 0  
    TauCoeff_Subset%Absorber_ID        = 0
    TauCoeff_Subset%Absorber_Predictor = 0
    TauCoeff_Subset%C		       = 0.0
 
  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( TauCoeff_Subset )  ! Output

    TYPE( TauCoeff_SARTA_Subset_type ), DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_Subset
    INTEGER :: n

    DO n = 1, SIZE( TauCoeff_Subset )
      CALL Zero_Scalar( TauCoeff_Subset(n) )
    END DO

  END SUBROUTINE Zero_Rank1



!----------------------------------------------------------------------------------
!S+
! NAME:
!       Check_TauCoeff_Subset_Release
!
! PURPOSE:
!       Function to check the TauCoeff_SARTA_Subset Release value.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Check_TauCoeff_Subset_Release( TauCoeff_Subset,                 &  ! Input
!                                              RCS_Id      = RCS_Id,     &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       TauCoeff_Subset:      TauCoeff_SARTA_Subset structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_SARTA_Subset_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structure Release value is valid.
!                         == FAILURE the structure Release value is NOT valid
!                                    and either a data file file or software
!                                    update is required.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:      Subroutine to output messages
!                             SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!S-
!----------------------------------------------------------------------------------

  FUNCTION Check_TauCoeff_Subset_Release( TauCoeff_Subset,     &  ! Input
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN )  :: TauCoeff_Subset

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Check_TauCoeff_SARTA_Subset_Release'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
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
    !#               -- CHECK THAT THE RELEASE IS NOT TOO OLD --                #
    !#--------------------------------------------------------------------------#

    IF ( TauCoeff_Subset%Release < TAUCOEFF_SUBSET_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A TauCoeff_SARTA_Subset data update is needed. ", &
                        &"TauCoeff_SARTA_Subset release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      TauCoeff_Subset%Release, TAUCOEFF_SUBSET_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#               -- CHECK THAT THE RELEASE IS NOT TOO NEW --                #
    !#--------------------------------------------------------------------------#

    IF ( TauCoeff_Subset%Release > TAUCOEFF_SUBSET_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A TauCoeff_SARTA_Subset software update is needed. ", &
                        &"TauCoeff_Subset release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      TauCoeff_Subset%Release, TAUCOEFF_SUBSET_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Check_TauCoeff_Subset_Release


 
!------------------------------------------------------------------------------
!S+
! NAME:
!       Version_TauCoeff_Subset
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the TauCoeff_SARTA_Subset data structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Version_TauCoeff_Subset( TauCoeff_Subset,       &  ! Input
!                              Version_Info,   &  ! Output
!                              RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       TauCoeff_Subset:      Filled TauCoeff_SARTA_Subset structure.
!                      UNITS:      N/A
!                      TYPE:       TauCoeff_SARTA_Subset_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Version_Info:  String containing version and dimension information
!                      about the passed TauCoeff_SARTA_Subset data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
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
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Version_TauCoeff_Subset( TauCoeff_Subset,     &  ! Input
                               Version_Info, &  ! Output
                               RCS_Id        )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN )  :: TauCoeff_Subset

    ! -- Output
    CHARACTER( * ),           INTENT( OUT ) :: Version_Info

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id


    ! ----------
    ! Parameters
    ! ----------

    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 512 ) :: Long_String



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- FILL THE VERSION INFO STRING --                   #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------
    ! Write the required data to the local string
    ! -------------------------------------------

    WRITE( Long_String, '( a,1x,"TauCoeff_SARTA_Subset RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
                           &"N_LAYERS=",i2,2x,&
                           &"N_TOTAL_PREDICTORS=",i2,2x,&
                           &"N_ABSORBERS=",i2,2x,&
                           &"N_CHANNELS=",i4)' ) & 
                         ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                         TauCoeff_Subset%Release, TauCoeff_Subset%Version, &
                         TauCoeff_Subset%n_Layers, &
                         TauCoeff_Subset%n_Total_Predictors, &
                         TauCoeff_Subset%n_Absorbers, &
                         TauCoeff_Subset%n_Channels
 

    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Version_Info = Long_String(1:MIN( LEN( Version_Info ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Version_TauCoeff_Subset

END MODULE TauCoeff_SARTA_Subset_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/05/03 19:42:09 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! Revision 1.1  2006/05/03 20:06:03  ychen
! Initial checkin.
!
!
!
!

