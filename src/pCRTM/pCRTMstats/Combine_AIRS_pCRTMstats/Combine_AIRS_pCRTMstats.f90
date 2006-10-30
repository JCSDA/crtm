!------------------------------------------------------------------------------
!M+
! NAME:
!       Combine_AIRS_RTMstats
!
! PURPOSE:
!       Program to cobine the separate AIRS module netCDF RTMstats files.
!
! CATEGORY:
!       NCEP RTM : RTMstats
!
! LANGUAGE:
!       Fortran-90
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       RTMstats_Define:        Module defining the RTMstats data structure
!                               and containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!
!       RTMstats_netCDF_IO:     Module containing routines to read and
!                               write netCDF format RTMstats files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     RTMstats_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
!
!       AIRS_Define:            Module containing AIRS channel and module
!                               definitions.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
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
!       Input:  netCDF format individual AIRS module RTMstats datafiles.
!
!       Output: netCDF format AIRS combined module RTMstats datafile.
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! RESTRICTIONS:
!       *ALL* of the required data must be present for the output file to
!       be successfully written.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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

PROGRAM Combine_AIRS_RTMstats


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE RTMstats_Define
  USE RTMstats_netCDF_IO

  USE AIRS_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Combine_AIRS_RTMstats'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Combine_AIRS_pCRTMstats.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Input_Filename
  CHARACTER( 256 ) :: Output_Filename

  CHARACTER( 5000 ) :: History
  CHARACTER(  256 ) :: Sensor_Name
  CHARACTER(  256 ) :: Platform_Name
  CHARACTER( 5000 ) :: Comment

  INTEGER :: Error_Status

  INTEGER :: n
  
  TYPE( RTMstats_type ), TARGET  :: RTMstats_In
  TYPE( RTMstats_type ), TARGET  :: RTMstats_Out
  TYPE( RTMstats_type ), POINTER :: RTMstats



  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC ADJUSTL, &
            LEN,     &
            MINVAL,  &
            MAXVAL,  &
            TRIM



  !#----------------------------------------------------------------------------#
  !#                        -- INITIALISE STRUCTURES --                         #
  !#----------------------------------------------------------------------------#

  CALL Initialize_RTMstats( RTMstats_In  )
  CALL Initialize_RTMstats( RTMstats_Out )

  NULLIFY( RTMstats )



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to combine the separate AIRS module netCDF")' )
  WRITE( *, '( 5x, "   RTMstats files.")' )
  WRITE( *, '(/5x, " $Revision: 1.2 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                      -- LOOP OVER MODULE DATA FILES  --                    #
  !#----------------------------------------------------------------------------#

  Concatenate_Loop: DO n = 1, N_AIRS_MODULES



    !#--------------------------------------------------------------------------#
    !#                    -- READ THE INPUT RTMstats FILE --                    #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Define the filename
    ! -------------------

    Input_Filename = 'airs'//&
                     TRIM( AIRS_MODULE( n ) )//&
                     '_aqua.RTMstats.nc'


    ! --------------------------
    ! Get some global attributes
    ! --------------------------

    IF ( n == 1 ) THEN

      ! -- Inquire the file
      Error_Status = Inquire_RTMstats_netCDF( TRIM( Input_Filename ), &

                                              History       = History, &
                                              Sensor_Name   = Sensor_Name, &
                                              Platform_Name = Platform_Name, &
                                              Comment       = Comment )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring the input netCDF RTMstats file '//&
                              TRIM( Input_Filename ), &
                              Error_Status )
        STOP
      END IF

    END IF

    ! -------------------------------------------------
    ! Depending on the module number, read the RTMstats
    ! data into the appropriate structure
    ! -------------------------------------------------

    IF ( n > 1 ) THEN
      RTMstats => RTMstats_In
    ELSE
      RTMstats => RTMstats_Out
    END IF


    ! --------------------------------------
    ! Read the data into the INPUT structure
    ! --------------------------------------

    Error_Status = Read_RTMstats_netCDF( TRIM( Input_Filename ), &
                                         RTMstats )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( PROGRAM_NAME, &
                            'Error reading RTMstats file '//TRIM( Input_Filename ), &
                            Error_Status )
      STOP
    END IF


    ! -----------------------------------------
    ! Concatenate the OUTPUT and INPUT RTMstats
    ! structures along the CHANNEL dimension
    ! -----------------------------------------

    IF ( n > 1 ) THEN

      Error_Status = Concatenate_RTMstats( RTMstats_Out, &
                                           RTMstats_In )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error concatenating RTMstats structures at file '//&
                              TRIM( Input_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! -- Destroy the INPUT RTMstats structure
      Error_Status = Destroy_RTMstats( RTMstats_In )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error destroying the RTMstats_In structure at file '//&
                              TRIM( Input_Filename ), &
                              Error_Status )
        STOP
      END IF

    END IF

  END DO Concatenate_Loop



  !#----------------------------------------------------------------------------#
  !#                     -- WRITE THE OUTPUT RTMstats FILE --                   #
  !#----------------------------------------------------------------------------#

  ! -----------------------
  ! Set the output filename
  ! -----------------------

  Output_Filename = 'airs_aqua.RTMstats.nc'


  ! --------------
  ! Write the data
  ! --------------

  WRITE( *, '(/10x, "Creating the output file..." )' )

  Error_Status = Write_RTMstats_netCDF( TRIM( Output_Filename ), &
                                        RTMstats_Out, &

                                        Title         = 'RTM statistics for ALL '//&
                                                        TRIM( Platform_Name )//' '//&
                                                        TRIM( Sensor_Name )//&
                                                        ' channels.', &
                                        History       = PROGRAM_RCS_ID//'; '//&
                                                        TRIM( History ), &
                                        Sensor_Name   = TRIM( Sensor_Name ), &
                                        Platform_Name = TRIM( Platform_Name ), &
                                        Comment       = 'Data concatenated from the individual '//&
                                                        'AIRS module RTMstats datafiles. '//&
                                                        TRIM( Comment ) )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing the output RTMstats file '//&
                          TRIM( Output_Filename ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                 -- DESTROY THE OUTPUT RTMstats STRUCTURE --                #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_RTMstats( RTMstats_Out )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying RTMstats structure for output to '//&
                          TRIM( Output_Filename ), &
                          Error_Status )
  END IF

END PROGRAM Combine_AIRS_RTMstats


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Combine_AIRS_pCRTMstats.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Combine_AIRS_pCRTMstats.f90,v $
! Revision 1.2  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2004/02/12 19:56:56  paulv
! Initial checkin.
!
!
!
!
