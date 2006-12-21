!
! Test_Type_Kinds
!
! Program to display the kind types and sizes defined in the
! Type_Kinds module.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_Type_Kinds

  
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler, ONLY: Program_Message
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Type_Kinds'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Type_Kinds.f90,v 1.3 2004/11/30 19:19:34 paulv Exp $'


  ! Output program header
  CALL Program_Message(PROGRAM_NAME,&
                       'Program to display kind type values and sizes defined '//&
                       'in the Type_Kinds module.', &
                       '$Revision: 1.3 $' )

  ! Display the integer kind types and byte-sizes
  !
  ! Kind types
  WRITE( *, '(  /5x, "Integer Kind types: ", &
              &/10x, "Byte   integer kind type: ", i5, &
              &/10x, "Short  integer kind type: ", i5, &
              &/10x, "Long   integer kind type: ", i5, &
              &/10x, "LLong  integer kind type: ", i5, &
              &/10x, "ip     integer kind type: ", i5 )' ) &
             Byte, Short, Long, LLong, ip
  ! Byte sizes
  WRITE( *, '(  /5x, "Expected Integer 8-bit byte sizes: ", &
              &/10x, "Byte   integer kind type size: ", i5, " byte", &
              &/10x, "Short  integer kind type size: ", i5, " bytes", &
              &/10x, "Long   integer kind type size: ", i5, " bytes", &
              &/10x, "LLong  integer kind type size: ", i5, " bytes", &
              &/10x, "ip     integer kind type size: ", i5, " bytes" )' ) &
             n_Bytes_Byte, n_Bytes_Short, n_Bytes_Long, n_Bytes_LLong, n_Bytes_IP


  ! Display the real kind types and byte-sizes
  !
  ! Kind types
  WRITE( *, '( //5x, "Real Kind types: ", &
              &/10x, "Single float kind type: ", i5, &
              &/10x, "Double float kind type: ", i5, &
              &/10x, "Quad   float kind type: ", i5, &
              &/10x, "fp     float kind type: ", i5 )' ) &
             Single, Double, Quad, fp
  ! Byte sizes
  WRITE( *, '(  /5x, "Expected Real 8-bit byte sizes: ", &
              &/10x, "Single float kind type size: ", i5, " bytes", &
              &/10x, "Double float kind type size: ", i5, " bytes", &
              &/10x, "Quad   float kind type size: ", i5, " bytes", &
              &/10x, "fp     float kind type size: ", i5, " bytes" )' ) &
             n_Bytes_Single, n_Bytes_Double, n_Bytes_Quad, n_Bytes_FP

END PROGRAM Test_Type_Kinds
