!------------------------------------------------------------------------------
!M+
! NAME:
!       Tau_OPTRAN_SARTA_Coeff_Define
!
! PURPOSE:
!       Module defining the Tau_OPTRAN_SARTA_Coeff data structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Tau_OPTRAN_SARTA_Coeff_Define
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
!       Associated_Tau_OPTRAN_Coeff:            Function to test the association status
!                                               of the pointer members of a Tau_OPTRAN_SARTA_Coeff
!                                               structure.
!
!       Destroy_Tau_OPTRAN_Coeff:               Function to re-initialize a Tau_OPTRAN_SARTA_Coeff
!                                               structure.
!
!       Allocate_Tau_OPTRAN_Coeff:              Function to allocate the pointer members
!                                               of a Tau_OPTRAN_SARTA_Coeff structure.
!
!       Assign_Tau_OPTRAN_Coeff:                Function to copy a valid Tau_OPTRAN_SARTA_Coeff structure.
!
!
! DERIVED TYPES:
!       Tau_OPTRAN_SARTA_Coeff_type:   Definition of the public Tau_OPTRAN_SARTA_Coeff data structure.
!       Fields are...
!
!         n_Layers:            Maximum layers for the aborber coefficients.
!                              "Ilayers" dimension.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( Long )
!                              DIMENSION:  Scalar
!
!         n_Predictors:        Number of predictors used in the
!                              gas absorption regression.
!                              "Iuse" dimension.
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
!         n_ProfAves:          Number of predictors for water vapor of OPTRAN in the
!                              data structure.
!                              "J" dimension  
!                              UNITS:      N/A
!                              TYPE:       INTEGER
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
!         Water_Amount:        Array containing the The OPTRAN water amount 
!                              UNITS:      Kilomoles/cm^2      
!                              TYPE:       REAL( Single )
!                              DIMENSION:  Ilayers  
!                              ATTRIBUTES: POINTER
!
!         Water_ProfAve:       Array containing the gas absorption
!                              model coefficients.
!                              UNITS:      Variable
!                              TYPE:       REAL( Single )
!                              DIMENSION:  J x Ilayers 
!                              ATTRIBUTES: POINTER
!
!         Water_Coeff:         Array containing the water vapor absorption
!                              model coefficients.
!                              UNITS:      Variable
!                              TYPE:       REAL( Single )
!                              DIMENSION:  Iuse x Ilayers x L
!                              ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the Tau_OPTRAN_SARTA_Coeff_type is PUBLIC and its members are not
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
!       Written by:     Yong Chen, CSU/CIRA 02-May-2006
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

MODULE Tau_OPTRAN_SARTA_Coeff_Define


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

  ! -- Public procedures to manipulate the Tau_OPTRAN_SARTA_Coeff structure
  PUBLIC :: Associated_Tau_OPTRAN_Coeff
  PUBLIC :: Destroy_Tau_OPTRAN_Coeff
  PUBLIC :: Allocate_Tau_OPTRAN_Coeff
  PUBLIC :: Assign_Tau_OPTRAN_Coeff

  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! ------------------------------
  ! Tau_OPTRAN_SARTA_Coeff data type definition
  ! ------------------------------

  TYPE, PUBLIC :: Tau_OPTRAN_SARTA_Coeff_type
    INTEGER :: n_Allocates = 0

    ! -- Array dimensions
    INTEGER( Long ) :: n_Layers      = 0    ! Ilayer   
    INTEGER( Long ) :: n_Predictors  = 0    ! Iuse     
    INTEGER( Long ) :: n_Channels    = 0    ! L        
    INTEGER( Long ) :: n_ProfAves    = 0    ! J        

    ! -- The actual sensor channel numbers
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Channel_Index => NULL()    ! L

    ! -- The OPTRAN water amount ( Kilomoles/cm^2)
    REAL( Single ), POINTER, DIMENSION( : ) ::  Water_Amount  => NULL()  ! Ilayer

    ! -- The OPTRAN water average profile values
    REAL( Single ), POINTER, DIMENSION( :, : ) :: Water_ProfAve  => NULL()  ! J x Ilayer
    
    ! -- The array of coefficients
    REAL( Single ), POINTER, DIMENSION( :, :, : ) :: Water_Coeff => NULL() ! Iuse x Ilayer x L

  END TYPE Tau_OPTRAN_SARTA_Coeff_type


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
!       Clear_Tau_OPTRAN_Coeff
!
! PURPOSE:
!       Subroutine to clear the scalar members of a Tau_OPTRAN_Coeff structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Tau_OPTRAN_Coeff: Tau_OPTRAN_SARTA_Coeff structure for which the 
!                         scalar members have been cleared.
!                         UNITS:      N/A
!                         TYPE:       Tau_OPTRAN_SARTA_Coeff_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
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
!       Note the INTENT on the output Tau_OPTRAN_SARTA_Coeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 02-May-2006
!                       Yong.Chen@noaa.gov
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff )

    TYPE( Tau_OPTRAN_SARTA_Coeff_type ), INTENT( IN OUT ) :: Tau_OPTRAN_Coeff

    Tau_OPTRAN_Coeff%n_Layers     = 0
    Tau_OPTRAN_Coeff%n_Predictors = 0
    Tau_OPTRAN_Coeff%n_Channels   = 0
    Tau_OPTRAN_Coeff%n_ProfAves    = 0
    
  END SUBROUTINE Clear_Tau_OPTRAN_Coeff


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
!       Associated_Tau_OPTRAN_Coeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       Tau_OPTRAN_SARTA_Coeff structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff,           &  ! Input
!                                                         ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Tau_OPTRAN_Coeff:    Tau_OPTRAN_SARTA_Coeff structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:	 N/A
!                            TYPE:	 Tau_OPTRAN_SARTA_Coeff_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Tau_OPTRAN_SARTA_Coeff structure pointer members are associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                            		      are associated.  (DEFAULT)
!                            	ANY_Test = 1, test if ANY of the pointer members
!                            		      are associated.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the Tau_OPTRAN_SARTA_Coeff pointer members.
!                            .TRUE.  - if ALL the Tau_OPTRAN_SARTA_Coeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the Tau_OPTRAN_SARTA_Coeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the Tau_OPTRAN_SARTA_Coeff pointer
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
!       This function tests the association status of the Tau_OPTRAN_SARTA_Coeff
!       structure pointer members. Therefore this function must only
!       be called after the input Tau_OPTRAN_SARTA_Coeff structure has, at least,
!       had its pointer members initialized.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 02-May-2006
!                       Yong.Chen@noaa.gov
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff,  & ! Input
                                        ANY_Test ) & ! Optional input
                                        RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( Tau_OPTRAN_SARTA_Coeff_type ), INTENT( IN ) :: Tau_OPTRAN_Coeff

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

      IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Channel_Index    ) .AND. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_Amount     ) .AND. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_ProfAve    ) .AND. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_Coeff      )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Channel_Index    ) .OR. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_Amount     ) .OR. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_ProfAve    ) .OR. &
           ASSOCIATED( Tau_OPTRAN_Coeff%Water_Coeff      )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_Tau_OPTRAN_Coeff



!------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_Tau_OPTRAN_Coeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of Tau_OPTRAN_SARTA_Coeff
!       data structures.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff,                 &  ! Output
!                                                RCS_Id = RCS_Id,	   &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
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
!       Tau_OPTRAN_Coeff: Re-initialized Tau_OPTRAN_SARTA_Coeff structure.
!                         UNITS:      N/A
!                         TYPE:       Tau_OPTRAN_SARTA_Coeff_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the structure re-initialisation was successful
!                            == FAILURE - an error occurred, or
!                         		- the structure internal allocation counter
!                         		  is not equal to zero (0) upon exiting this
!                         		  function. This value is incremented and
!                         		  decremented for every structure allocation
!                         		  and deallocation respectively.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       Associated_Tau_OPTRAN_Coeff:  Function to test the association status of the
!                                     pointer members of a Tau_OPTRAN_SARTA_Coeff structure.
!
!       Display_Message:   Subroutine to output messages   
!                          SOURCE: Message_Handler module    
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output Tau_OPTRAN_SARTA_Coeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 02-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff,     &  ! Output
                                     No_Clear,     &  ! Optional input
                                     RCS_Id,	   &  ! Revision control
                                     Message_Log ) &  ! Error messaging

                                    RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( Tau_OPTRAN_SARTA_Coeff_type ),    INTENT( IN OUT ) :: Tau_OPTRAN_Coeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_Tau_OPTRAN_Coeff'


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

    IF ( Clear ) CALL Clear_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the sensor channel number array
    IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Channel_Index ) ) THEN

      DEALLOCATE( Tau_OPTRAN_Coeff%Channel_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Tau_OPTRAN_SARTA_Coeff Channel_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Water amount
    IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Water_Amount ) ) THEN

      DEALLOCATE( Tau_OPTRAN_Coeff%Water_Amount, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Tau_OPTRAN_SARTA_Coeff Water_Amount ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate water average profile values
    IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Water_ProfAve ) ) THEN

      DEALLOCATE( Tau_OPTRAN_Coeff%Water_ProfAve, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Tau_OPTRAN_SARTA_Coeff Water_ProfAve ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate coefficients
    IF ( ASSOCIATED( Tau_OPTRAN_Coeff%Water_Coeff ) ) THEN

      DEALLOCATE( Tau_OPTRAN_Coeff%Water_Coeff, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating Tau_OPTRAN_SARTA_Coeff Water_Coeff ", &
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

    Tau_OPTRAN_Coeff%n_Allocates = Tau_OPTRAN_Coeff%n_Allocates - 1

    IF ( Tau_OPTRAN_Coeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Tau_OPTRAN_Coeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Tau_OPTRAN_Coeff



!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_Tau_OPTRAN_Coeff
! 
! PURPOSE:
!       Function to allocate the pointer members of the Tau_OPTRAN_SARTA_Coeff
!       data structure.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_Tau_OPTRAN_Coeff( n_Layers,                  &  ! Input
!                                                 n_Predictors, 	     &  ! Input
!                                                 n_Channels,		     &  ! Input
!                                                 n_ProfAves ,                &  ! Input
!                                                 Tau_OPTRAN_Coeff,	     &  ! Output
!                                                 RCS_Id      = RCS_Id,      &  ! Revision control
!                                                 Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:     Maximum layers for the aborber coefficients.
!                     "Ilayers" dimension.
!                     UNITS:      N/A
!                     TYPE:	  INTEGER( Long )   	  
!                     DIMENSION:  Scalar	    	  
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Predictors: Maximum number of predictors dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Channels:   Number of channels dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       n_ProfAves:   Number of water profile average value.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
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
!       Tau_OPTRAN_Coeff:   Tau_OPTRAN_SARTA_Coeff structure with allocated
!                           pointer members
!                           UNITS:	N/A
!                           TYPE:	Tau_OPTRAN_SARTA_Coeff_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT )
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
!       Associated_Tau_OPTRAN_Coeff:  Function to test the association status of the
!                                     pointer members of a Tau_OPTRAN_SARTA_Coeff structure.
!
!       Destroy_Tau_OPTRAN_Coeff:     Function to re-initialize the scalar and pointer
!                                     members of Tau_OPTRAN_SARTA_Coeff data structures.
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
!       Note the INTENT on the output Tau_OPTRAN_SARTA_Coeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 02-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Allocate_Tau_OPTRAN_Coeff( n_Layers,     &  ! Input
                                      n_Predictors, &  ! Input
                                      n_Channels,   &  ! Input
                                      n_ProfAves ,   &  ! Input
                                      Tau_OPTRAN_Coeff,     &  ! Output
                                      RCS_Id,	    &  ! Revision control
                                      Message_Log ) &  ! Error messaging
                                    RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )     :: n_Layers
    INTEGER,                  INTENT( IN )     :: n_Predictors
    INTEGER,                  INTENT( IN )     :: n_Channels
    INTEGER,                  INTENT( IN )     :: n_ProfAves

    ! -- Output
    TYPE( Tau_OPTRAN_SARTA_Coeff_type ),    INTENT( IN OUT ) :: Tau_OPTRAN_Coeff

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_Tau_OPTRAN_Coeff'


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

    IF ( n_Layers     < 1 .OR. &
         n_Predictors < 1 .OR. &
         n_Channels   < 1 .OR. &
	 n_ProfAves   < 1  ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Tau_OPTRAN_SARTA_Coeff dimensions must all be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating Tau_OPTRAN_SARTA_Coeff pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Tau_OPTRAN_Coeff%Channel_Index( n_Channels ), &
              Tau_OPTRAN_Coeff%Water_Amount( n_Layers ), &
              Tau_OPTRAN_Coeff%Water_ProfAve( n_ProfAves, n_Layers ), &
              Tau_OPTRAN_Coeff%Water_Coeff( n_Predictors, n_Layers, n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Tau_OPTRAN_SARTA_Coeff data arrays. STAT = ", i5 )' ) &
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

    Tau_OPTRAN_Coeff%n_Layers     = n_Layers
    Tau_OPTRAN_Coeff%n_Predictors = n_Predictors
    Tau_OPTRAN_Coeff%n_Channels   = n_Channels
    Tau_OPTRAN_Coeff%n_ProfAves   = n_ProfAves


    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    Tau_OPTRAN_Coeff%n_Allocates = Tau_OPTRAN_Coeff%n_Allocates + 1

    IF ( Tau_OPTRAN_Coeff%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Tau_OPTRAN_Coeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Tau_OPTRAN_Coeff



!------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_Tau_OPTRAN_Coeff
!
! PURPOSE:
!       Function to copy valid Tau_OPTRAN_SARTA_Coeff structures.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff_in,              &  ! Input
!                                            	Tau_OPTRAN_Coeff_out,		  &  ! Output
!                                            	RCS_Id      = RCS_Id,	  &  ! Revision control
!                                            	Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Tau_OPTRAN_Coeff_in: Tau_OPTRAN_SARTA_Coeff structure which is to be copied.
!                            UNITS:	 N/A
!                            TYPE:	 Tau_OPTRAN_SARTA_Coeff_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
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
!       Tau_OPTRAN_Coeff_out: Copy of the input structure, Tau_OPTRAN_SARTA_Coeff_in.
!                             UNITS:	  N/A
!                             TYPE:	  Tau_OPTRAN_SARTA_Coeff_type
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( IN OUT )
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
!       Associated_Tau_OPTRAN_Coeff:  Function to test the association status of the
!                                     pointer members of a Tau_OPTRAN_SARTA_Coeff structure.
!
!       Allocate_Tau_OPTRAN_Coeff:    Function to allocate the pointer members of
!                                     the Tau_OPTRAN_SARTA_Coeff data structure.
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
!       Note the INTENT on the output Tau_OPTRAN_SARTA_Coeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 02-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Assign_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff_in,   &  ! Input
                                    Tau_OPTRAN_Coeff_out,  &  ! Output
                                    RCS_Id,	   &  ! Revision control
                                    Message_Log )  &  ! Error messaging
                                  RESULT( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( Tau_OPTRAN_SARTA_Coeff_type ),    INTENT( IN )     :: Tau_OPTRAN_Coeff_in

    ! -- Output
    TYPE( Tau_OPTRAN_SARTA_Coeff_type ),    INTENT( IN OUT ) :: Tau_OPTRAN_Coeff_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_Tau_OPTRAN_Coeff'



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
    ! ---------------------------------------

    IF ( .NOT. Associated_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT Tau_OPTRAN_SARTA_Coeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- Allocate data arrays
    Error_Status = Allocate_Tau_OPTRAN_Coeff( Tau_OPTRAN_Coeff_in%n_Layers, &
                                              Tau_OPTRAN_Coeff_in%n_Predictors, &
                                              Tau_OPTRAN_Coeff_in%n_Channels, &
                                              Tau_OPTRAN_Coeff_in%n_ProfAves, &
                                              Tau_OPTRAN_Coeff_out, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output Tau_OPTRAN_SARTA_Coeff arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Copy array data
    Tau_OPTRAN_Coeff_out%Channel_Index    = Tau_OPTRAN_Coeff_in%Channel_Index
    Tau_OPTRAN_Coeff_out%Water_Amount     = Tau_OPTRAN_Coeff_in%Water_Amount 
    Tau_OPTRAN_Coeff_out%Water_ProfAve    = Tau_OPTRAN_Coeff_in%Water_ProfAve
    Tau_OPTRAN_Coeff_out%Water_Coeff      = Tau_OPTRAN_Coeff_in%Water_Coeff

  END FUNCTION Assign_Tau_OPTRAN_Coeff

END MODULE Tau_OPTRAN_SARTA_Coeff_Define 
!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/05/02 19:42:09 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! Revision 1.1  2006/05/02 20:06:03  ychen
! Initial checkin.
!
!
!
!
