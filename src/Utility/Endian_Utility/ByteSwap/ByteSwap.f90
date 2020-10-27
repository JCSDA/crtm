!
! ByteSwap
!
! Simple program to byte swap user input numbers
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 10-Dec-2014
!                       paul.vandelst@noaa.gov
!

PROGRAM ByteSwap

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds     ! For future updates using different types
  USE Message_Handler, ONLY: FAILURE, &
                             Display_Message, &
                             Program_Message
  USE Endian_Utility , ONLY: Big_Endian, &
                             Swap_Endian
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'ByteSwap'
  CHARACTER( * ), PARAMETER :: PROGRAM_VERSION_ID = &
  INTEGER, PARAMETER :: ML = 256


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(ML) :: err_msg
  CHARACTER(ML) :: io_msg
  INTEGER :: io_stat
  INTEGER :: number
  INTEGER :: swapped_number


  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Simple program to byte swap integers', &
                        '$Revision$' )


  ! Determine the endian-ness of the platform
  WRITE(*,'(/,2x,"Current platform is ")', ADVANCE = 'NO')
  IF ( Big_Endian() ) THEN
    WRITE(*,'( "big-endian." )')
  ELSE
    WRITE(*,'( "little-endian." )')
  END IF


  ! Get the input number
  WRITE(*,'(/,2x,"Enter the integer to byte-swap : ")', ADVANCE='NO')
  READ(*,*,IOSTAT=io_stat,IOMSG=io_msg) number
  IF ( io_stat /= 0 ) THEN
    err_msg = 'Invalid input - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE )
    STOP
  END IF


  ! Output the byte-swapped number
  WRITE(*,'(/,2x,"The input number is        : ",i12," (",b32.32,")")') number, number
  swapped_number = Swap_Endian(number)
  WRITE(*,'(  2x,"The byte-swapped number is : ",i12," (",b32.32,")")') swapped_number, swapped_number

END PROGRAM ByteSwap
