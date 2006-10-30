PROGRAM Test_Sort_Utility
  USE Type_Kinds
  USE Sort_Utility
  IMPLICIT NONE

  INTEGER, PARAMETER :: N = 6
  INTEGER :: Idx(N)
  INTEGER(Long), DIMENSION(N) :: lArray, LongArray
  CHARACTER(16), DIMENSION(N) :: cArray, CharArray

  lArray = (/ 30, 10, -40, 20, 20, 0 /)
  cArray = (/ 'imgr_g13        ','abi_gr          ','windsat_coriolis',&
              'imgr_g10        ','amsua_metop-b   ','abi_gr          '/)

  WRITE(*,'(/5x,"Testing long array inplace sort")')
  LongArray=lArray
  WRITE(*,'("Unsorted: ",12(1x,i3))') LongArray
  CALL InsertionSort( LongArray )
  WRITE(*,'("Sorted  : ",12(1x,i3))') LongArray

  WRITE(*,'(/5x,"Testing long array index sort")')
  LongArray=lArray
  WRITE(*,'("Unsorted: ",12(1x,i3))') LongArray
  CALL InsertionSort( LongArray, Idx )
  WRITE(*,'("Sorted  : ",12(1x,i3))') LongArray(Idx)
  WRITE(*,'("Idx     : ",12(1x,i3))') Idx

  WRITE(*,'(/5x,"Testing character array inplace sort")')
  CharArray = cArray
  WRITE(*,'("Unsorted: ",6(a,1x))') CharArray
  CALL InsertionSort( CharArray )
  WRITE(*,'("Sorted  : ",6(a,1x))') CharArray

  WRITE(*,'(/5x,"Testing character array index sort")')
  CharArray = cArray
  WRITE(*,'("Unsorted: ",6(a,1x))') CharArray
  CALL InsertionSort( CharArray, Idx )
  WRITE(*,'("Sorted  : ",6(a,1x))') CharArray(Idx)
  WRITE(*,'("Idx     : ",6(i3,14x))') Idx

  PRINT *, MAX(COUNT(CharArray(Idx) /= CSHIFT(CharArray(Idx),1)),1)

  CharArray = 'abi_gr'
  PRINT *, MAX(COUNT(CharArray /= CSHIFT(CharArray,1)),1)

END PROGRAM Test_Sort_Utility
