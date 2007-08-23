!------------------------------------------------------------------------------
!M+
! NAME:
!       Date_Utility
!
! PURPOSE:
!       Module containing date conversion routines
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Date_Utility
!
! MODULES:
!       Message_Handler: Module to define error codes and handle
!                        error conditions
!
! CONTAINS:
!       Is_Leap_year:            Function to test if a year is a leap year.
!
!       Day_of_Year_to_Date:     Function to convert the day of year into 
!                                either a numeric or character string date.
!
!       Date_to_Day_of_Year:     Function to convert input numeric date
!                                (e.g. DD,MM,YYYY) date information to
!                                a day of year.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Apr-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2004 Paul van Delst
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


MODULE Date_Utility


  ! ------------
  ! Modules used
  ! ------------

  USE Message_Handler


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Is_Leap_Year
  PUBLIC :: Day_of_Year_to_Date
  PUBLIC :: Date_to_Day_of_Year


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

  ! -- Number of Months in a Year
  INTEGER, PRIVATE, PARAMETER :: N_MONTHS = 12

  ! -- Days per Month in a non leap Year
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_MONTHS ) :: DAYS_PER_MONTH_IN_NONLEAP = &
                                                        (/ 31, 28, 31, 30, 31, 30,  &
                                                           31, 31, 30, 31, 30, 31 /)

  ! -- Month names
  CHARACTER( * ), PRIVATE, PARAMETER, DIMENSION( N_MONTHS ) :: MONTH_NAME = &
                                                               (/ 'January  ', 'February ', &
                                                                  'March    ', 'April    ', &
                                                                  'May      ', 'June     ', &
                                                                  'July     ', 'August   ', &
                                                                  'September', 'October  ', &
                                                                  'November ', 'December ' /)


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       Is_Leap_Year
!
! PURPOSE:
!       Routine to determine if a specified year is a leap year.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Result = Is_Leap_Year( Year )
!
! INPUT ARGUMENTS:
!       Year:     Integer specifying the Year in the format YYYY.
!                 Thus, 96 == 96 C.E. *NOT* 1996.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Result:    The return value is a logical value indicating whether
!                  the specified year is a leap year.
!                  .TRUE.  - it is a leap year.
!                  .FALSE. - it is NOT a leap year.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Apr-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Is_Leap_Year( Year ) RESULT ( Its_a_Leap_Year )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    INTEGER, INTENT( IN ) :: Year


    ! ------
    ! Result
    ! ------

    LOGICAL :: Its_a_Leap_Year



    !#--------------------------------------------------------------------------#
    !#                       -- DETERMINE LEAP-YEARNESS --                      #
    !#--------------------------------------------------------------------------#

    Its_a_Leap_Year = .FALSE.

    IF ( ( MOD( Year,   4 ) == 0 .AND. MOD( Year, 100 ) /= 0 ) .OR. &
           MOD( Year, 400 ) == 0 ) Its_a_Leap_Year = .TRUE.

  END FUNCTION Is_Leap_Year





!------------------------------------------------------------------------------
!S+
! NAME:
!       Date_to_Day_of_Year
!
! PURPOSE:
!       Routine to convert input numeric (e.g. DD,MM,YYYY) date information
!       to a day of Year.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Date_to_Day_of_Year ( Day_of_Month,             &  ! Input
!                                            Month,                    &  ! Input           
!                                            Year,                     &  ! Input           
!                                            Day_of_Year,              &  ! Output          
!                                            Message_Log = Message_Log )  ! Optional input  
!
! INPUT ARGUMENTS:
!       Day_of_Month:  Integer defining the number of the day in the
!                      specified Month.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
!       Month:         Integer defining the Month from 1-12.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
!       Year:          Integer specifying the Year in the format YYYY.
!                      Thus, 96 == 96 C.E. *NOT* 1996.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output message to the screen.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER( * )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Day_of_Year:   Integer defining the day-of-Year.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the date conversion was successful.
!                        == FAILURE input data was invalid.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:  Subroutine to output messages
!                         SOURCE: Message_Handler module
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Apr-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Date_to_Day_of_Year ( Day_of_Month,  &  ! Input
                                 Month,         &  ! Input
                                 Year,          &  ! Input
                                 Day_of_Year,   &  ! Output
                                 Message_Log  ) &  ! Error messaging
                               RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: Day_of_Month
    INTEGER,                  INTENT( IN )  :: Month
    INTEGER,                  INTENT( IN )  :: Year

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: Day_of_Year

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Date_to_Day_of_Year'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER, DIMENSION( N_MONTHS ) :: Days_per_Month



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! Check the Year
    ! --------------

    IF ( Year < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid YEAR input argument. Must be > 0', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------
    ! Check the Month
    ! ---------------

    IF ( Month < 1 .OR. Month > 12 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid MONTH input argument', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------
    ! Check the Day_of_Month
    ! ----------------------

    ! -- Set the number of days per Month
    Days_per_Month( : ) = DAYS_PER_MONTH_IN_NONLEAP( : )

    ! -- Add an extra day onto February if we have a leap Year
    IF ( Is_Leap_Year( Year ) ) Days_per_Month( 2 ) = 29

    ! -- Check the validity of the specified Day_of_Month
    IF ( Day_of_Month > Days_per_Month( Month ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid DAY_OF_MONTH input argument', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- COMPUTE DAY OF YEAR --                          #
    !#--------------------------------------------------------------------------#

    Day_of_Year = SUM( Days_per_Month( 1 : Month - 1 ) ) + Day_of_Month

  END FUNCTION Date_to_Day_of_Year





!------------------------------------------------------------------------------
!S+
! NAME:
!       Day_of_Year_to_Date
!
! PURPOSE:
!       Routine to convert the day of year into either a numeric or character
!       string date. 
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Day_of_Year_to_Date( Day_of_Year,                 &  ! Input
!                                           Year,                        &  ! Input
!                                           Day_of_Month = Day_of_Month, &  ! Optional output
!                                           Month        = Month,        &  ! Optional output
!                                           Date_String  = Date_String,  &  ! Optional output
!                                           Message_Log  = Message_Log   )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Day_of_Year:   Integer defining the day-of-year.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
!       Year:          Integer specifying the year in the format YYYY.
!                      Thus, 96 == 96 C.E. *NOT* 1996.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output message to the screen.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER( * )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Day_of_Month:  Integer defining the number of the day in the
!                      specified month.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Month:         Integer defining the month from 1-12.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Date_String:   Character string containing the date in DD-MMM-YYYY
!                      format, e.g. 18-Jul-2000. If string length is less
!                      than 11 characters, the output is truncated.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER( * )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the date conversion was successful.
!                        == FAILURE input data was invalid.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:  Subroutine to output messages
!                         SOURCE: Message_Handler module
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Apr-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Day_of_Year_to_Date ( Day_of_Year,  &  ! Input
                                 Year,         &  ! Input
                                 Day_of_Month, &  ! Optional output
                                 Month,        &  ! Optional output
                                 Date_String,  &  ! Optional output
                                 Message_Log ) &  ! Error messaging
                             RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: Day_of_Year
    INTEGER,                  INTENT( IN )  :: Year

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Day_of_Month
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Month
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Date_String

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME      = 'Day_of_Year_to_Date'
    INTEGER,        PARAMETER :: MIN_STRING_LENGTH = 11


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER, DIMENSION( N_MONTHS ) :: Days_per_Month

    INTEGER :: Local_Day_of_Month
    INTEGER :: Local_Month
    INTEGER :: Input_String_Length, Output_String_Length

    CHARACTER( LEN = MIN_STRING_LENGTH ) :: Local_Date_String



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! Check the year
    ! --------------

    IF ( Year < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid YEAR input argument. Must be > 0', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Check the Day_of_Year
    ! ---------------------

    ! -- Set the number of days per Month
    Days_per_Month( : ) = DAYS_PER_MONTH_IN_NONLEAP( : )

    ! -- Add an extra day onto February if we have a leap Year
    IF ( Is_Leap_Year( Year ) ) Days_per_Month( 2 ) = 29

    ! -- Check input day with sum of all days
    IF ( Day_of_Year < 1 .OR. Day_of_Year > SUM( Days_per_Month ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid DAY_OF_YEAR input argument.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- FILL THE OUTPUT ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Determine the month
    ! -------------------

    DO Local_Month = 1, N_MONTHS
      IF ( Day_of_Year <= SUM( Days_per_Month( 1 : Local_Month ) ) ) EXIT
    END DO

    IF ( PRESENT( Month ) ) Month = Local_Month


    ! -----------------
    ! Determine the day
    ! -----------------

    Local_Day_of_Month = Day_of_Year - SUM( Days_per_Month( 1 : Local_Month - 1 ) )

    IF ( PRESENT( Day_of_Month ) ) Day_of_Month = Local_Day_of_Month


    ! -------------------------
    ! Construct the date string
    ! -------------------------

    WRITE( Local_Date_String, '( i2.2, "-", a, "-", i4.4 )' )   &
                              Local_Day_of_Month,               &
                              MONTH_NAME( Local_Month )( 1:3 ), &
                              Year

    IF ( PRESENT( Date_String ) ) THEN

      ! -- Clear the string (just in case)
      Date_String = ' '

      ! -- Get the DATE_STRING length
      Input_String_Length = LEN( Date_String )

      ! -- Check length with minimum
      IF ( Input_String_Length < MIN_STRING_LENGTH ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Output DATE_STRING too short. Truncated output.', &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

      ! -- Define usable string length
      Output_String_Length = MIN( MIN_STRING_LENGTH, Input_String_Length )

      ! -- Assign date string.
      Date_String( 1:Output_String_Length ) = Local_Date_String( 1:Output_String_Length )

    END IF

  END FUNCTION Day_of_Year_to_Date

END MODULE Date_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id$
!
! $Date: 2006/07/26 21:37:50 $
!
! $Revision$
!
! $State: Exp $
!
! $Log: Date_Utility.f90,v $
! Revision 3.3  2006/07/26 21:37:50  wd20pd
! Additional replacement of "Error_Handler" string with "Message_Handler"
! in documentaiton blocks.
!
! Revision 3.2  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 3.1  2004/08/13 17:03:01  paulv
! - Removed old comment header for module variable.
!
! Revision 3.0  2004/08/11 22:18:25  paulv
! - New version. Made subroutines functions that return error status.
! - MAde Is_Leap_Year() function public.
!
! Revision 2.2  2000/12/08 21:46:43  paulv
! - Updated header documentation.
!
! Revision 2.1  2000/11/21 13:41:33  paulv
! - New version with updated documentation.
! - Function names changed.
! - Input argument error checking performed.
!
!
!
