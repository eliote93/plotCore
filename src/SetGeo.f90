SUBROUTINE setgeo()
! SET : iasy, ipin, x, y, ptmod

USE allocs
USE param, ONLY : HALF, SQ3, EPS
USE mdat,  ONLY : dat01, dat02, ndat, maRng, boF2F, mPin, qF2F, numthr, lHS, ptmod, powdata_type, objcn, plotobj

IMPLICIT NONE

INTEGER :: iasy, nside, nasy, ix, iy, ipin, npin, idat, iobj

INTEGER, POINTER, DIMENSION(:,:) :: map1D ! (ix/iy, idat)
INTEGER, POINTER, DIMENSION(:,:) :: map2D ! (ix, iy)

REAL :: dx, dy, xx, yy, tmp

REAL, POINTER, DIMENSION(:,:) :: acnt ! (x/y, iasy)
REAL, POINTER, DIMENSION(:,:) :: pcnt ! (x/y, ipin)

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat
! ------------------------------------------------

! ------------------------------------------------
!            01. Asy. Geo.
! ------------------------------------------------
nside = 2 * maRng - 1
nasy  = 3 * maRng * (maRng - 1) + 1

! Asy. Idx. Map.
CALL dmalloc(map1D,     2,  nasy)
CALL dmalloc(map2D, nside, nside)

CALL setidxmap(nside, nasy, map1d, map2d)

! Asy. Cnt.
CALL dmalloc(acnt, 2, nasy)

dx = boF2F * HALF
dy = boF2F * 1.5_8 / SQ3

DO iasy = 1, nasy
  ix = map1d(1, iasy)
  iy = map1d(2, iasy)
  
  acnt(1, iasy) = dx * (ix * 2 - iy - maRng)
  acnt(2, iasy) = dy * (maRng - iy)
END DO

! CnP
DO iobj = 1, 2
  SELECT CASE (iobj)
  CASE (1); locdat => dat01
  CASE (2); locdat => dat02 
  END SELECT
  
  !$OMP PARALLEL PRIVATE(idat, ix, iy) NUM_THREADS(numthr)
  !$OMP DO SCHEDULE(GUIDED)
  DO idat = 1, ndat(iobj)
    ix = locdat(idat)%iax
    iy = locdat(idat)%iay
    
    locdat(idat)%iasy = map2d(ix, iy)
  END DO
  !$OMP END DO
  !$OMP END PARALLEL
END DO
! ------------------------------------------------
!            02. Pin Geo.
! ------------------------------------------------
nside = 2 * mPin - 1
npin  = 3 * mPin * (mPin - 1) + 1

! Pin Idx.
DEALLOCATE (map1D)
DEALLOCATE (map2D)

CALL dmalloc(map1D,     2,  npin)
CALL dmalloc(map2D, nside, nside)

CALL setidxmap(nside, npin, map1D, map2D)

! Pin Cnt.
CALL dmalloc(pcnt, 2, npin)

dx = qF2F * 1.5_8 / SQ3
dy = qF2F * HALF

DO ipin = 1, npin
  ix = map1d(1, ipin)
  iy = map1d(2, ipin)
  
  pcnt(1, ipin) = dx * (ix - iy)
  pcnt(2, ipin) = dy * (2 * mPin - ix - iy)
END DO

! CnP
DO iobj = 1, 2
  SELECT CASE (iobj)
  CASE (1); locdat => dat01
  CASE (2); locdat => dat02
  END SELECT
  
  !$OMP PARALLEL PRIVATE(idat, ix, iy, iasy, ipin) NUM_THREADS(numthr)
  !$OMP DO SCHEDULE(GUIDED)
  DO idat = 1, ndat(iobj)
    ix = locdat(idat)%ipx
    iy = locdat(idat)%ipy
    
    locdat(idat)%ipin = map2d(ix, iy)
    
    iasy = locdat(idat)%iasy
    ipin = locdat(idat)%ipin
    
    locdat(idat)%x = acnt(1, iasy) + pcnt(1, ipin)
    locdat(idat)%y = acnt(2, iasy) + pcnt(2, ipin)
  END DO
  !$OMP END DO
  !$OMP END PARALLEL
END DO

! DEBUG
CALL fndcorebndy
! ------------------------------------------------
!            03. Point Mode
! ------------------------------------------------
IF (lHS(plotobj)) THEN
  CALL dmalloc(ptmod, ndat(plotobj))
  
  SELECT CASE (plotobj)
  CASE (1); locdat => dat01
  CASE (2); locdat => dat02
  END SELECT
  
  !$OMP PARALLEL PRIVATE(idat, xx, yy, tmp) NUM_THREADS(numthr)
  !$OMP DO SCHEDULE(GUIDED)
  DO idat = 1, ndat(plotobj)
    xx  = locdat(idat)%x
    yy  = locdat(idat)%y
    tmp = abs(xx * SQ3 + yy)
    
    IF (-yy .LT. EPS) ptmod(idat) = 1 ! NN
    IF (tmp .LT. EPS) ptmod(idat) = 2 ! SW
  END DO
  !$OMP END DO
  !$OMP END PARALLEL
END IF
! ------------------------------------------------
!            04. FREE
! ------------------------------------------------
NULLIFY (locdat)

DEALLOCATE (map1d)
DEALLOCATE (map2d)
DEALLOCATE (acnt)
DEALLOCATE (pcnt)
! ------------------------------------------------

END SUBROUTINE setgeo