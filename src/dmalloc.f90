MODULE allocs
  
USE param, ONLY : ZERO, ONE, FALSE, TRUE

INTEGER(8) :: nbytesf  = 0

INTERFACE dmalloc
MODULE PROCEDURE mallocf1
MODULE PROCEDURE mallocd1
MODULE PROCEDURE malloci1
MODULE PROCEDURE mallocl1
MODULE PROCEDURE mallocf2
MODULE PROCEDURE mallocd2
MODULE PROCEDURE malloci2
MODULE PROCEDURE mallocl2
MODULE PROCEDURE mallocf3
MODULE PROCEDURE mallocd3
MODULE PROCEDURE malloci3
MODULE PROCEDURE mallocf4
MODULE PROCEDURE mallocd4
MODULE PROCEDURE malloci4
MODULE PROCEDURE mallocf5
MODULE PROCEDURE mallocd5
MODULE PROCEDURE malloci5
MODULE PROCEDURE mallocf6
MODULE PROCEDURE mallocd6
MODULE PROCEDURE malloci6
MODULE PROCEDURE mallocf7
MODULE PROCEDURE mallocd7
MODULE PROCEDURE malloci7
END INTERFACE dmalloc

INTERFACE dmalloc0
MODULE PROCEDURE mallocf01
MODULE PROCEDURE mallocd01
MODULE PROCEDURE malloci01
MODULE PROCEDURE mallocf02
MODULE PROCEDURE mallocd02
MODULE PROCEDURE malloci02
MODULE PROCEDURE mallocf03
MODULE PROCEDURE mallocd03
MODULE PROCEDURE malloci03
MODULE PROCEDURE mallocf04
MODULE PROCEDURE mallocd04
MODULE PROCEDURE malloci04
MODULE PROCEDURE mallocf05
MODULE PROCEDURE mallocd05
MODULE PROCEDURE malloci05
MODULE PROCEDURE mallocf06
MODULE PROCEDURE mallocd06
MODULE PROCEDURE malloci06
END INTERFACE dmalloc0

INTERFACE dmalloc1
MODULE PROCEDURE mallocf11
MODULE PROCEDURE mallocd11
MODULE PROCEDURE malloci11
MODULE PROCEDURE mallocl11
MODULE PROCEDURE mallocf12
MODULE PROCEDURE mallocd12
MODULE PROCEDURE malloci12
MODULE PROCEDURE mallocl12
MODULE PROCEDURE mallocf13
MODULE PROCEDURE mallocd13
MODULE PROCEDURE malloci13
MODULE PROCEDURE mallocf14
MODULE PROCEDURE mallocd14
MODULE PROCEDURE malloci14
MODULE PROCEDURE mallocf15
MODULE PROCEDURE mallocd15
MODULE PROCEDURE malloci15
END INTERFACE dmalloc1

CONTAINS
! --------------------------------------------------------------------------------------------------
!                                   01. dmalloc
! --------------------------------------------------------------------------------------------------
! ------------------------------------------------
!            f1
! ------------------------------------------------
SUBROUTINE mallocf1(a, n1)

IMPLICIT NONE

INTEGER :: n1
REAL(4), POINTER :: a(:)

ALLOCATE (a(n1))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf1
! ------------------------------------------------
!            d1
! ------------------------------------------------
SUBROUTINE mallocd1(a, n1)

IMPLICIT NONE

INTEGER :: n1

REAL, POINTER :: a(:)

ALLOCATE (a(n1))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd1
! ------------------------------------------------
!            i1
! ------------------------------------------------
SUBROUTINE malloci1(a, n1)

IMPLICIT NONE

INTEGER :: n1
INTEGER, POINTER :: a(:)

ALLOCATE (a(n1))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci1
! ------------------------------------------------
!            l1
! ------------------------------------------------
SUBROUTINE mallocl1(a, n1)

IMPLICIT NONE

INTEGER :: n1
LOGICAL, POINTER :: a(:)

ALLOCATE (a(n1))
a = FALSE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocl1
! ------------------------------------------------
!            f2
! ------------------------------------------------
SUBROUTINE mallocf2(a, n1, n2)

IMPLICIT NONE

INTEGER :: n1, n2
REAL(4), POINTER :: a(:,:)

ALLOCATE (a(n1, n2))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf2
! ------------------------------------------------
!            d2
! ------------------------------------------------
SUBROUTINE mallocd2(a, n1, n2)

IMPLICIT NONE

INTEGER :: n1, n2
REAL(8), POINTER :: a(:,:)

ALLOCATE (a(n1, n2))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd2
! ------------------------------------------------
!            i2
! ------------------------------------------------
SUBROUTINE malloci2(a, n1, n2)

IMPLICIT NONE

INTEGER :: n1, n2
INTEGER, POINTER :: a(:,:)

ALLOCATE (a(n1, n2))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci2
! ------------------------------------------------
!            l2
! ------------------------------------------------
SUBROUTINE mallocl2(a, n1, n2)

IMPLICIT NONE

INTEGER :: n1, n2
LOGICAL, POINTER :: a(:,:)

ALLOCATE (a(n1, n2))
a = FALSE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocl2
! ------------------------------------------------
!            f3
! ------------------------------------------------
SUBROUTINE mallocf3(a, n1, n2, n3)

IMPLICIT NONE

INTEGER :: n1, n2, n3
REAL(4), POINTER :: a(:,:,:)

ALLOCATE (a(n1, n2, n3))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf3
! ------------------------------------------------
!            d3
! ------------------------------------------------
SUBROUTINE mallocd3(a, n1, n2, n3)

IMPLICIT NONE

INTEGER :: n1, n2, n3
REAL(8), POINTER :: a(:,:,:)

ALLOCATE (a(n1, n2, n3))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd3
! ------------------------------------------------
!            i3
! ------------------------------------------------
SUBROUTINE malloci3(a, n1, n2, n3)

IMPLICIT NONE

INTEGER :: n1, n2, n3
INTEGER, POINTER :: a(:,:,:)

ALLOCATE (a(n1, n2, n3))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci3
! ------------------------------------------------
!            f4
! ------------------------------------------------
SUBROUTINE mallocf4(a, n1, n2, n3, n4)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4
REAL(4), POINTER :: a(:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf4
! ------------------------------------------------
!            d4
! ------------------------------------------------
SUBROUTINE mallocd4(a, n1, n2, n3, n4)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4
REAL(8), POINTER :: a(:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd4
! ------------------------------------------------
!            i4
! ------------------------------------------------
SUBROUTINE malloci4(a, n1, n2, n3, n4)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4
INTEGER, POINTER :: a(:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci4
! ------------------------------------------------
!            f5
! ------------------------------------------------
SUBROUTINE mallocf5(a, n1, n2, n3, n4, n5)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5
REAL(4), POINTER :: a(:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf5
! ------------------------------------------------
!            d5
! ------------------------------------------------
SUBROUTINE mallocd5(a, n1, n2, n3, n4, n5)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5
REAL(8), POINTER :: a(:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd5
! ------------------------------------------------
!            i5
! ------------------------------------------------
SUBROUTINE malloci5(a, n1, n2, n3, n4, n5)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5
INTEGER, POINTER :: a(:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci5
! ------------------------------------------------
!            f6
! ------------------------------------------------
SUBROUTINE mallocf6(a, n1, n2, n3, n4, n5, n6)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5, n6
REAL(4), POINTER :: a(:,:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5, n6))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf6
! ------------------------------------------------
!            d6
! ------------------------------------------------
SUBROUTINE mallocd6(a, n1, n2, n3, n4, n5, n6)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5, n6
REAL(8), POINTER :: a(:,:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5, n6))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd6
! ------------------------------------------------
!            i6
! ------------------------------------------------
SUBROUTINE malloci6(a, n1, n2, n3, n4, n5, n6)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5, n6
INTEGER, POINTER :: a(:,:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5, n6))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci6
! ------------------------------------------------
!            f7
! ------------------------------------------------
SUBROUTINE mallocf7(a, n1, n2, n3, n4, n5, n6, n7)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5, n6, n7
REAL(4), POINTER :: a(:,:,:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5, n6, n7))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf7
! ------------------------------------------------
!            d7
! ------------------------------------------------
SUBROUTINE mallocd7(a, n1, n2, n3, n4, n5, n6, n7)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5, n6, n7
REAL(8), POINTER :: a(:,:,:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5, n6, n7))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd7
! ------------------------------------------------
!            i7
! ------------------------------------------------
SUBROUTINE malloci7(a, n1, n2, n3, n4, n5, n6, n7)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5, n6, n7
INTEGER, POINTER :: a(:,:,:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5, n6, n7))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci7
! --------------------------------------------------------------------------------------------------
!                                   02. dmalloc 0
! --------------------------------------------------------------------------------------------------
! ------------------------------------------------
!            f1
! ------------------------------------------------
SUBROUTINE mallocf01(a, nb1, ne1)

IMPLICIT NONE

INTEGER :: nb1, ne1
REAL(4), POINTER :: a(:)

ALLOCATE (a(nb1:ne1))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf01
! ------------------------------------------------
!            d1
! ------------------------------------------------
SUBROUTINE mallocd01(a, nb1, ne1)

IMPLICIT NONE

INTEGER :: nb1, ne1
REAL(8), POINTER :: a(:)

ALLOCATE (a(nb1:ne1))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd01
! ------------------------------------------------
!            i1
! ------------------------------------------------
SUBROUTINE malloci01(a, nb1, ne1)

IMPLICIT NONE

INTEGER :: nb1, ne1
INTEGER, POINTER :: a(:)

ALLOCATE (a(nb1:ne1))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci01
! ------------------------------------------------
!            f2
! ------------------------------------------------
SUBROUTINE mallocf02(a, nb1, ne1, nb2, ne2)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2
REAL(4), POINTER :: a(:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf02
! ------------------------------------------------
!            d2
! ------------------------------------------------
SUBROUTINE mallocd02(a, nb1, ne1, nb2, ne2)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2
REAL(8), POINTER :: a(:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd02
! ------------------------------------------------
!            i2
! ------------------------------------------------
SUBROUTINE malloci02(a, nb1, ne1, nb2, ne2)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2
INTEGER, POINTER :: a(:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci02
! ------------------------------------------------
!            f3
! ------------------------------------------------
SUBROUTINE mallocf03(a, nb1, ne1, nb2, ne2, nb3, ne3)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3
REAL(4), POINTER :: a(:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf03
! ------------------------------------------------
!            d3
! ------------------------------------------------
SUBROUTINE mallocd03(a, nb1, ne1, nb2, ne2, nb3, ne3)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3
REAL(8), POINTER :: a(:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd03
! ------------------------------------------------
!            i3
! ------------------------------------------------
SUBROUTINE malloci03(a, nb1, ne1, nb2, ne2, nb3, ne3)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3
INTEGER, POINTER :: a(:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci03
! ------------------------------------------------
!            f4
! ------------------------------------------------
SUBROUTINE mallocf04(a, nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4
REAL(4), POINTER :: a(:,:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3, nb4:ne4))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf04
! ------------------------------------------------
!            d4
! ------------------------------------------------
SUBROUTINE mallocd04(a, nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4
REAL(8), POINTER :: a(:,:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3, nb4:ne4))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd04
! ------------------------------------------------
!            i4
! ------------------------------------------------
SUBROUTINE malloci04(a, nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4
INTEGER, POINTER :: a(:,:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3, nb4:ne4))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci04
! ------------------------------------------------
!            f5
! ------------------------------------------------
SUBROUTINE mallocf05(a, nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5
REAL(4), POINTER :: a(:,:,:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3, nb4:ne4, nb5:ne5))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf05
! ------------------------------------------------
!            d5
! ------------------------------------------------
SUBROUTINE mallocd05(a, nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5
REAL(8), POINTER :: a(:,:,:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3, nb4:ne4, nb5:ne5))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd05
! ------------------------------------------------
!            i5
! ------------------------------------------------
SUBROUTINE malloci05(a, nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5
INTEGER, POINTER :: a(:,:,:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3, nb4:ne4, nb5:ne5))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci05
! ------------------------------------------------
!            f6
! ------------------------------------------------
SUBROUTINE mallocf06(a, nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5, nb6, ne6)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5, nb6, ne6
REAL(4), POINTER :: a(:,:,:,:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3, nb4:ne4, nb5:ne5, nb6:ne6))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf06
! ------------------------------------------------
!            d6
! ------------------------------------------------
SUBROUTINE mallocd06(a, nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5, nb6, ne6)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5, nb6, ne6
REAL(8), POINTER :: a(:,:,:,:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3, nb4:ne4, nb5:ne5, nb6:ne6))
a = ZERO
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd06
! ------------------------------------------------
!            i6
! ------------------------------------------------
SUBROUTINE malloci06(a, nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5, nb6, ne6)

IMPLICIT NONE

INTEGER :: nb1, ne1, nb2, ne2, nb3, ne3, nb4, ne4, nb5, ne5, nb6, ne6
INTEGER, POINTER :: a(:,:,:,:,:,:)

ALLOCATE (a(nb1:ne1, nb2:ne2, nb3:ne3, nb4:ne4, nb5:ne5, nb6:ne6))
a = 0
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci06
! --------------------------------------------------------------------------------------------------
!                                   03. dmalloc 1
! --------------------------------------------------------------------------------------------------
! ------------------------------------------------
!            f1
! ------------------------------------------------
SUBROUTINE mallocf11(a, n1)

IMPLICIT NONE

INTEGER :: n1
REAL(4), POINTER :: a(:)

ALLOCATE (a(n1))
a = ONE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf11
! ------------------------------------------------
!            d1
! ------------------------------------------------
SUBROUTINE mallocd11(a, n1)

IMPLICIT NONE

INTEGER :: n1
REAL(8), POINTER :: a(:)

ALLOCATE (a(n1))
a = ONE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd11
! ------------------------------------------------
!            i1
! ------------------------------------------------
SUBROUTINE malloci11(a, n1)

IMPLICIT NONE

INTEGER :: n1
INTEGER, POINTER :: a(:)

ALLOCATE (a(n1))
a = 1
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci11
! ------------------------------------------------
!            l1
! ------------------------------------------------
SUBROUTINE mallocl11(a, n1)

IMPLICIT NONE

INTEGER :: n1
LOGICAL, POINTER :: a(:)

ALLOCATE (a(n1))
a = TRUE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocl11
! ------------------------------------------------
!            f2
! ------------------------------------------------
SUBROUTINE mallocf12(a, n1, n2)

IMPLICIT NONE

INTEGER :: n1, n2
REAL(4), POINTER :: a(:,:)

ALLOCATE (a(n1, n2))
a = ONE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf12
! ------------------------------------------------
!            d2
! ------------------------------------------------
SUBROUTINE mallocd12(a, n1, n2)

IMPLICIT NONE
INTEGER :: n1, n2
REAL(8), POINTER :: a(:,:)

ALLOCATE (a(n1, n2))
a = ONE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd12
! ------------------------------------------------
!            i2
! ------------------------------------------------
SUBROUTINE malloci12(a, n1, n2)

IMPLICIT NONE

INTEGER :: n1, n2
INTEGER, POINTER :: a(:,:)

ALLOCATE (a(n1, n2))
a = 1
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci12
! ------------------------------------------------
!            l2
! ------------------------------------------------
SUBROUTINE mallocl12(a, n1, n2)

IMPLICIT NONE

INTEGER :: n1, n2
LOGICAL, POINTER :: a(:,:)

ALLOCATE (a(n1, n2))
a = TRUE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocl12
! ------------------------------------------------
!            f3
! ------------------------------------------------
SUBROUTINE mallocf13(a, n1, n2, n3)

IMPLICIT NONE

INTEGER :: n1, n2, n3
REAL(4), POINTER :: a(:,:,:)

ALLOCATE (a(n1, n2, n3))
a = ONE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf13
! ------------------------------------------------
!            d3
! ------------------------------------------------
SUBROUTINE mallocd13(a, n1, n2, n3)

IMPLICIT NONE

INTEGER :: n1, n2, n3
REAL(8), POINTER :: a(:,:,:)

ALLOCATE (a(n1, n2, n3))
a = ONE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd13
! ------------------------------------------------
!            i3
! ------------------------------------------------
SUBROUTINE malloci13(a, n1, n2, n3)

IMPLICIT NONE

INTEGER :: n1, n2, n3
INTEGER, POINTER :: a(:,:,:)

ALLOCATE (a(n1, n2, n3))
a = 1
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci13
! ------------------------------------------------
!            f4
! ------------------------------------------------
SUBROUTINE mallocf14(a, n1, n2, n3, n4)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4
REAL(4), POINTER :: a(:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4))
a = ONE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf14
! ------------------------------------------------
!            d4
! ------------------------------------------------
SUBROUTINE mallocd14(a, n1, n2, n3, n4)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4
REAL(8), POINTER :: a(:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4))
a = ONE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd14
! ------------------------------------------------
!            i4
! ------------------------------------------------
SUBROUTINE malloci14(a, n1, n2, n3, n4)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4
INTEGER, POINTER :: a(:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4))
a = 1
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci14
! ------------------------------------------------
!            f5
! ------------------------------------------------
SUBROUTINE mallocf15(a, n1, n2, n3, n4, n5)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5
REAL(4), POINTER :: a(:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5))
a = ONE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocf15
! ------------------------------------------------
!            d5
! ------------------------------------------------
SUBROUTINE mallocd15(a, n1, n2, n3, n4, n5)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5
REAL(8), POINTER :: a(:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5))
a = ONE
nbytesf = nbytesf + size(a)

END SUBROUTINE mallocd15
! ------------------------------------------------
!            i5
! ------------------------------------------------
SUBROUTINE malloci15(a, n1, n2, n3, n4, n5)

IMPLICIT NONE

INTEGER :: n1, n2, n3, n4, n5
INTEGER, POINTER :: a(:,:,:,:,:)

ALLOCATE (a(n1, n2, n3, n4, n5))
a = 1
nbytesf = nbytesf + size(a)

END SUBROUTINE malloci15
! --------------------------------------------------------------------------------------------------

END MODULE allocs