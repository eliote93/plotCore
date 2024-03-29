SUBROUTINE calpowerr_3D()

USE allocs
USE param, ONLY : ZERO, MP, ERRABS, ERRREL
USE mdat,  ONLY : powdata_type, lerr, l3d, powerr, dat01, dat02, ndat, xyzmax, xyzrms, xyztotmax, xyztotrms, xymax, xyrms, nz, nxy, xymap, numthr, avghgt, hgt, plotobj

IMPLICIT NONE

INTEGER :: idat, jdat, iz, ixy, jxy, kxy, jobj, mxy, ierr
REAL :: psum, tot01, tot02, rnrm

REAL, POINTER, DIMENSION(:) :: ref

CHARACTER*4 :: ctmp
TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat, mocdat
! ------------------------------------------------

CALL dmalloc0(xyzmax, 0, nz, 1, 2)
CALL dmalloc0(xyzrms, 0, nz, 1, 2)

IF (.NOT. lerr) RETURN

SELECT CASE (plotobj)
CASE (1); locdat => dat01; mocdat => dat02
CASE (2); locdat => dat02; mocdat => dat01
END SELECT
! ------------------------------------------------
!            01. SET : Ref.
! ------------------------------------------------
mxy  = nxy(plotobj)
jobj = MP(plotobj)
CALL dmalloc(ref, ndat(plotobj))

!$OMP PARALLEL PRIVATE(iz, ixy, idat, psum, jxy, kxy, jdat) NUM_THREADS(numthr)
!$OMP DO SCHEDULE(GUIDED)
DO iz = 1, nz
  DO ixy = 1, mxy
    idat = ixy + mxy*(iz-1)
    psum = ZERO
    
    DO jxy = 1, xymap(0, ixy)
      kxy  = xymap(jxy, ixy)
      jdat = kxy + nxy(jobj)*(iz-1)
      
      psum = psum + mocdat(jdat)%pow
    END DO
    
    ref(idat) = psum / real(xymap(0, ixy))
  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

! Norm.
IF (.NOT. l3d) THEN
  psum = sum(ref(1:ndat(plotobj)))
ELSE
  psum = ZERO
  
  DO idat = 1, ndat(plotobj)
    iz = locdat(idat)%iz
    
    psum = psum + ref(idat)*hgt(iz) / avghgt
  END DO
END IF

rnrm = real(ndat(plotobj)) / psum
ref = ref*rnrm
! ------------------------------------------------
!            02. DEBUG
! ------------------------------------------------
! Total Sum
!tot01 = ZERO
!tot02 = ZERO
!
!DO idat = 1, ndat(plotobj)
!  tot01 = tot01 + locdat(idat)%pow
!  tot02 = tot02 + ref(idat)
!END DO

! Print 3-D Power
!OPEN (41, FILE = 'tst.out')
!
!DO iz = 1, nz
!  DO ixy = 1, nxy(2)
!    idat = ixy + nxy(2)*(iz-1)
!    
!    WRITE (41, '(ES13.5)') dat02(idat)%pow
!  END DO
!END DO
!
!CLOSE (41)
!STOP
! ------------------------------------------------
!            03. CAL : 3-D Err.
! ------------------------------------------------
CALL dmalloc0(powerr, 0, mxy, 0, nz, 1, 2)
!$OMP PARALLEL PRIVATE(iz, ixy, idat) NUM_THREADS(numthr)
!$OMP DO SCHEDULE(GUIDED)
DO iz = 1, nz
  DO ixy = 1, mxy
    idat = ixy + mxy*(iz-1)
    
    powerr(ixy, iz, ERRABS) = 100.*(locdat(idat)%pow - ref(idat))
    powerr(ixy, iz, ERRREL) = 100.*(locdat(idat)%pow - ref(idat)) / ref(idat)
  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL
! ------------------------------------------------
!            04. SUMM.
! ------------------------------------------------
! Pln.-wise
DO iz = 1, nz
  DO ierr = 1, 2
    xyzmax(iz, ierr) = max(maxval(powerr(:, iz, ierr)), abs(minval(powerr(:, iz, ierr))))
    
    DO ixy = 1, mxy
      xyzrms(iz, ierr) = xyzrms(iz, ierr) + powerr(ixy, iz, ierr)*powerr(ixy, iz, ierr)
    END DO
    
    xyzrms(iz, ierr) = sqrt(xyzrms(iz, ierr) / real(mxy))
  END DO
END DO

! Total
xyztotrms = ZERO

DO ierr = 1, 2
  xyztotmax(ierr) = maxval(xyzmax(:, ierr))
  
  DO iz = 1, nz
    DO ixy = 1, mxy
      xyztotrms(ierr) = xyztotrms(ierr) + powerr(ixy, iz, ierr)*powerr(ixy, iz, ierr)
    END DO
  END DO
  
  xyztotrms(ierr) = sqrt(xyztotrms(ierr) / real(ndat(plotobj)))
END DO

IF (.NOT. l3d) THEN
  xymax = xyztotmax
  xyrms = xyztotrms
END IF

NULLIFY (locdat)
NULLIFY (mocdat)
DEALLOCATE (ref)
! ------------------------------------------------

END SUBROUTINE calpowerr_3D