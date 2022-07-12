! --------------------------------------------------------------------------------------------------
SUBROUTINE setidxmap(nside, nasy, map1d, map2d)

IMPLICIT NONE

INTEGER :: nside, nasy, npin, ix, ist, ied, iy, iPin
INTEGER, DIMENSION(nside, nside) :: map2d
INTEGER, DIMENSION(2, nasy)  :: map1d
! ------------------------------------------------

nPin  = (nSide + 1) / 2
map2D = 0
map1d = 0
ied   = nPin
iPin  = 0

DO iy = 1, nPin
  DO ix = 1, ied
    iPin = iPin + 1
    
    map2d(ix, iy)  = ipIn
    map1d(1, ipin) = ix
    map1d(2, ipin) = iy
  END DO
  
  ied = ied + 1
END DO

ist = 2

DO iy = nPin+1, nSide
  DO ix = ist, nSide
    iPin = iPin + 1
    
    map2d(ix, iy) = iPIn
    map1d(1, ipin) = ix
    map1d(2, ipin) = iy
  END DO
  
  ist = ist + 1
END DO
! ------------------------------------------------

END SUBROUTINE setidxmap
! --------------------------------------------------------------------------------------------------
FUNCTION chksamepts(x1, y1, x2, y2)

USE param, ONLY : TRUE, FALSE, EPS

IMPLICIT NONE

REAL :: x1, y1, x2, y2, del
LOGICAL :: chksamepts
! ------------------------------------------------

chksamepts = FALSE

del = (x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)
del = sqrt(del)

IF (del .GT. EPS) RETURN

chksamepts = TRUE
! ------------------------------------------------

END FUNCTION chksamepts
! --------------------------------------------------------------------------------------------------
SUBROUTINE rotpt(theta, x1, y1, x2, y2)

USE param, ONLY : EPS, ZERO

IMPLICIT NONE

REAL :: x1, y1, x2, y2, theta
! ------------------------------------------------

x2 = x1 * cos(theta) - y1 * sin(theta)
y2 = x1 * sin(theta) + y1 * cos(theta)

IF (abs(x2) .LT. EPS) x2 = ZERO
IF (abs(y2) .LT. EPS) y2 = ZERO
! ------------------------------------------------

END SUBROUTINE rotpt
! --------------------------------------------------------------------------------------------------
SUBROUTINE refpt(theta, x1, y1, x2, y2)

USE param, ONLY : PI, EPS, ZERO

IMPLICIT NONE

REAL :: x1, y1, x2, y2, theta, tx, ty, tt
! ------------------------------------------------

tx = cos(theta)
ty = sin(theta)
tt = tx * x1 + ty * y1

x2 = 2. * tx * tt - x1
y2 = 2. * ty * tt - y1

IF (abs(x2) .LT. EPS) x2 = ZERO
IF (abs(y2) .LT. EPS) y2 = ZERO
! ------------------------------------------------

END SUBROUTINE refpt
!! --------------------------------------------------------------------------------------------------
SUBROUTINE fndcorebndy()

USE mdat, ONLY : l02, dat01, dat02, ndat, corebndy, powdata_type

IMPLICIT NONE

INTEGER :: ibndy, idat, iobj
REAL :: tmpmin, tmpmax, cnt

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat
! ------------------------------------------------

DO iobj = 1, 2
  IF (iobj.EQ.2 .AND. .NOT. l02) CYCLE
  
  SELECT CASE (iobj)
  CASE (1); locdat => dat01
  CASE (2); locdat => dat02 
  END SELECT
  
  ! x
  tmpmin = locdat(1)%x
  tmpmax = locdat(1)%x
  
  DO idat = 2, ndat(iobj)
    cnt = locdat(idat)%x
    
    IF (cnt .LT. tmpmin) tmpmin = cnt
    IF (cnt .gT. tmpmax) tmpmax = cnt
  END DO
  
  corebndy(1, 1, iobj) = tmpmin
  corebndy(2, 1, iobj) = tmpmax
  
  ! y
  tmpmin = locdat(1)%y
  tmpmax = locdat(1)%y
  
  DO idat = 2, ndat(iobj)
    cnt = locdat(idat)%y
    
    IF (cnt .LT. tmpmin) tmpmin = cnt
    IF (cnt .gT. tmpmax) tmpmax = cnt
  END DO
  
  corebndy(1, 2, iobj) = tmpmin
  corebndy(2, 2, iobj) = tmpmax
END DO

NULLIFY (locdat)
! ------------------------------------------------

END SUBROUTINE fndcorebndy
! --------------------------------------------------------------------------------------------------