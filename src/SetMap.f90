! --------------------------------------------------------------------------------------------------
SUBROUTINE setmap()
! Points are used in MatLab Plotting

USE mdat, ONLY : lHS, lerr, plotobj
  
IMPLICIT NONE
! ------------------------------------------------

! SET : pt map
IF (lHS(plotobj)) THEN
  CALL setptmap_HS(plotobj)
ELSE
  CALL setptmap_FS(plotobj)
END IF

IF (.NOT. lerr) RETURN

! SET : xy map
IF (lHS(plotobj)) THEN
  CALL setxymap_HS(plotobj)
ELSE
  CALL setxymap_FS(plotobj)
END IF
! ------------------------------------------------

END SUBROUTINE setmap
! --------------------------------------------------------------------------------------------------
SUBROUTINE setptmap_FS(iobj)

USE allocs
USE param, ONLY : HALF, ONE, ZERO, SQ3
USE mdat,  ONLY : powdata_type, nxy, dat01, dat02, qF2F, ptmap

IMPLICIT NONE

INTEGER :: iobj
! ------------------------------------------------
INTEGER :: ixy

REAL :: pPch
REAL :: x0(6), y0(6), x(6), y(6)

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat
! ------------------------------------------------

x0 = [-HALF, HALF,  ONE,  HALF, -HALF, -ONE]
y0 = [ HALF, HALF, ZERO, -HALF, -HALF, ZERO]

pPch = qF2F / SQ3
x0   = x0 * pPch
y0   = y0 * qF2F

CALL dmalloc(ptmap, 2, 6, nxy(iobj))

SELECT CASE (iobj)
CASE (1); locdat => dat01
CASE (2); locdat => dat02
END SELECT
! ------------------------------------------------
DO ixy = 1, nxy(iobj)
  x = x0 + locdat(ixy)%x
  y = y0 + locdat(ixy)%y
  
  ptmap(1, 1:6, ixy) = x(1:6)
  ptmap(2, 1:6, ixy) = y(1:6)
END DO

NULLIFY (locdat)
! ------------------------------------------------

END SUBROUTINE setptmap_FS
! --------------------------------------------------------------------------------------------------
SUBROUTINE setptmap_HS(iobj)

USE allocs
USE param, ONLY : HALF, ONE, ZERO, SQ3
USE mdat,  ONLY : powdata_type, nxy, dat01, dat02, qF2F, ptmod, ptmap
  
IMPLICIT NONE

INTEGER :: iobj
! ------------------------------------------------
INTEGER :: ixy

REAL :: pPch
REAL :: x0(6), y0(6), xNN(6), yNN(6), xSW(6), ySW(6), x(6), y(6)

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat
! ------------------------------------------------

x0  = [-HALF,  HALF,   ONE,  HALF, -HALF, -ONE]
y0  = [ HALF,  HALF,  ZERO, -HALF, -HALF, ZERO]
xNN = [ -ONE, -HALF,  HALF,   ONE,   ONE,  ONE]
yNN = [ ZERO, -HALF, -HALF,  ZERO,  ZERO, ZERO]
xSW = [-HALF,  HALF,   ONE,  HALF,  HALF, HALF]
ySW = [ HALF, -HALF,  ZERO,  HALF,  HALF, HALF]

pPch = qF2F / SQ3
x0   = x0  * pPch
xNN  = xNN * pPch
xSW  = xSW * pPch
y0   = y0  * qF2F
yNN  = yNN * qF2F
ySW  = ySW * qF2F

CALL dmalloc(ptmap, 2, 6, nxy(iobj))

SELECT CASE (iobj)
CASE (1); locdat => dat01
CASE (2); locdat => dat02
END SELECT
! ------------------------------------------------
DO ixy = 1, nxy(iobj)
  SELECT CASE (ptmod(ixy))
  CASE (0)
    x = x0 + locdat(ixy)%x
    y = y0 + locdat(ixy)%y
  CASE (1)
    x = xNN + locdat(ixy)%x
    y = yNN + locdat(ixy)%y
  CASE (2)
    x = xSW + locdat(ixy)%x
    y = ySW + locdat(ixy)%y
  END SELECT
  
  ptmap(1, 1:6, ixy) = x(1:6)
  ptmap(2, 1:6, ixy) = y(1:6)
END DO

NULLIFY (locdat)
! ------------------------------------------------

END SUBROUTINE setptmap_HS
!! --------------------------------------------------------------------------------------------------
SUBROUTINE setxymap_FS(iobj)

USE allocs
USE param, ONLY : MP
USE mdat,  ONLY : powdata_type, xymap, nxy, dat01, dat02, numthr

IMPLICIT NONE

INTEGER :: iobj
! ------------------------------------------------
INTEGER :: ixy, jxy, iasy, jasy, ipin, jpin, jobj

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat, mocdat
! ------------------------------------------------

CALL dmalloc0(xymap, 0, 1, 1, nxy(iobj))

xymap(0, :) = 1

SELECT CASE (iobj)
CASE (1); locdat => dat01; mocdat => dat02
CASE (2); locdat => dat02; mocdat => dat01
END SELECT

jobj = MP(iobj)

!$OMP PARALLEL PRIVATE(ixy, iasy, ipin, jxy, jasy, jpin) NUM_THREADS(numthr)
!$OMP DO SCHEDULE(GUIDED)
DO ixy = 1, nxy(iobj)
  iasy = locdat(ixy)%iasy
  ipin = locdat(ixy)%ipin
  
  DO jxy = 1, nxy(jobj)
    jasy = mocdat(jxy)%iasy
    jpin = mocdat(jxy)%ipin
    
    IF (iasy .NE. jasy) CYCLE
    IF (ipin .NE. jpin) CYCLE
    
    xymap(1, ixy) = jxy
    
    EXIT
  END DO
  
  IF (xymap(1, ixy) .EQ. 0) CALL terminate("SET RADIAL MAP")
END DO
!$OMP END DO
!$OMP END PARALLEL

NULLIFY (locdat)
NULLIFY (mocdat)
! ------------------------------------------------

END SUBROUTINE setxymap_FS
! --------------------------------------------------------------------------------------------------
SUBROUTINE setxymap_HS(iobj)

USE allocs
USE param, ONLY : TRUE, FALSE, PI, MP
USE mdat,  ONLY : powdata_type, xymap, nxy, numthr, dat01, dat02, lrot, ptmod, lHS

IMPLICIT NONE

INTEGER :: iobj
! ------------------------------------------------
LOGICAL :: lchk, chksamepts
INTEGER :: ixy, jxy, idir, ndir, mxy, jobj
REAL :: x0, y0, theta, x1(6), y1(6), x2, y2

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat, mocdat
! ------------------------------------------------

CALL dmalloc0(xymap, 0, 6, 1, nxy(iobj))

SELECT CASE (iobj)
CASE (1); locdat => dat01; mocdat => dat02
CASE (2); locdat => dat02; mocdat => dat01
END SELECT

jobj = MP(iobj)

IF (lHS(Iobj) .AND. .NOT.lHS(jobj)) THEN
  ndir = 6
ELSE
  ndir = 1
END IF

!$OMP PARALLEL PRIVATE(ixy, x0, y0, idir, theta, x1, y1, mxy, jxy, x2, y2, lchk) NUM_THREADS(numthr)
!$OMP DO SCHEDULE(GUIDED)
DO ixy = 1, nxy(iobj)
  x0 = locdat(ixy)%x
  y0 = locdat(ixy)%y
  
  ! SET : Pts
  x1(1) = x0
  y1(1) = y0
    
  IF (lrot(iobj) .OR. ptmod(ixy).GT.0) THEN
    DO idir = 2, ndir
      theta = (idir - 1) * PI / 3.
      
      CALL rotpt(theta, x0, y0, x1(idir), y1(idir))
    END DO
  ELSE
    DO idir = 2, ndir
      theta = (idir - 2) * PI / 3.
      
      CALL refpt(theta, x1(idir-1), y1(idir-1), x1(idir), y1(idir))
    END DO
  END IF
  
  ! SWEEP
  mxy = 0
  
  DO jxy = 1, nxy(jobj)
    x2   = mocdat(jxy)%x
    y2   = mocdat(jxy)%y
    lchk = FALSE
    
    DO idir = 1, 6
      lchk = chksamepts(x1(idir), y1(idir), x2, y2)
      
      IF (lchk) EXIT
    END DO
    
    IF (.NOT. lchk) CYCLE
    
    mxy = mxy + 1
    
    xymap(mxy, ixy) = jxy
    
    IF (mxy .EQ. 6) CYCLE
  END DO
  
  IF (mxy .EQ. 0) CALL terminate("SET RADIAL MAP")
  
  xymap(0, ixy) = mxy
END DO
!$OMP END DO
!$OMP END PARALLEL

NULLIFY (locdat)
NULLIFY (mocdat)
! ------------------------------------------------

END SUBROUTINE setxymap_HS
! --------------------------------------------------------------------------------------------------