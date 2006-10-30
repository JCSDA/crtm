!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_Date_Utility
!
! PURPOSE:
!       Program to test the routines in the Date_Utility module
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Message_Handler:Module to define simple error codes and
!                       handle error conditions
!                       USEs: FILE_UTILITY module
!
!       Date_Utility:   Module containing Date conversion routines
!                       USEs: ERROR_HANDLER module
!
! CONTAINS:
!       None.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Nov-2000
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
!P-
!------------------------------------------------------------------------------

PROGRAM Test_Date_Utility

  ! $Id: Test_Date_Utility.f90,v 1.4 2006/05/02 16:58:03 dgroff Exp $


  ! ------------
  ! Module usage
  ! ------------

  USE Message_Handler
  USE Date_Utility


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Date_Utility'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Date_Utility.f90,v 1.4 2006/05/02 16:58:03 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: DAYS_IN_A_YEAR = 365


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Message
  CHARACTER( 256 ) :: Answer

  INTEGER :: Error_Status

  INTEGER :: Max_DoY
  INTEGER :: DoY, Year, Re_DoY
  INTEGER :: DoM, Month

  CHARACTER( 80 ) :: Date


  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x,a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the Date_Utility module routines." )' )
  WRITE( *, '(/5x, " $Revision: 1.4 $")' )
  WRITE( *, '( 5x, a )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                            -- GET USER INPUT --                            #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Enter a year [YYYY]: " )', ADVANCE = 'NO' )
  READ( *, * ) Year



  !#----------------------------------------------------------------------------#
  !#                              -- BEGIN TEST --                              #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------------------------
  ! Determine the number of days in the year in question
  ! ----------------------------------------------------

  IF ( Is_Leap_Year( Year ) ) THEN
    Max_DoY = DAYS_IN_A_YEAR + 1
  ELSE
    Max_DoY = DAYS_IN_A_YEAR
  END IF


  ! ------------------------------  
  ! Loop over the days of the year
  ! ------------------------------  

  DO DoY = 1, Max_DoY

    ! -- Convert DoY to Date
    Error_Status = Day_of_Year_to_Date( DoY, Year, &
                                        DoM, Month, &
                                        Date )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error converting day-of-year ", i3, " to date." )' ) DoY
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( MEssage ), &
                            Error_Status )
      STOP
    END IF

    ! -- Convert Date back to DoY
    Error_Status = Date_to_Day_of_Year( DoM, Month, Year, &
                                        Re_DoY )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error converting date ", a, " to day-of-year." )' ) TRIM( Date )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( MEssage ), &
                            Error_Status )
      STOP
    END IF

    ! -- Output result
    WRITE( *, '( 2x, "Input DoY: ", i3, " -->DoM/Month/Date: ", 2(1x,i2),1x,a, &
                    &" --> Output DoY: ", i3 )' ) &
              DoY, DoM, Month, TRIM( Date ), Re_DoY

    IF ( MOD( DoY, 40 ) == 0 ) THEN
      WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
      READ( *, '(a)' ) Answer
    END IF

  END DO

END PROGRAM Test_Date_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_Date_Utility.f90,v 1.4 2006/05/02 16:58:03 dgroff Exp $
!
! $Date: 2006/05/02 16:58:03 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_Date_Utility.f90,v $
! Revision 1.4  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 1.3  2004/11/30 15:35:40  paulv
! - Added program header.
!
!
!
