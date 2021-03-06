SUBROUTINE calpowerr_int()

USE allocs
USE param, ONLY : ZERO, MP
USE mdat,  ONLY : powdata_type, lerr, l3d, lrel, xyzmax, powerr, nz, nxy, ndat, xymax, xyrms, axmax, axrms, dat01, dat02, numthr, xymap, nz, hgt, avghgt, plotobj, xyzmax, xyzrms

IMPLICIT NONE

INTEGER :: iz, ixy, jxy, kxy, idat, mxy, lxy, jobj
REAL :: totmax, totrms, rnrm, psum, totpow

REAL, POINTER, DIMENSION(:) :: xyobj, xysub, zobj, zsub, ref

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat, mocdat
! ------------------------------------------------

IF (.NOT. lerr) RETURN
IF (.NOT. l3d)  RETURN

jobj = MP (plotobj)
mxy  = nxy(plotobj)
lxy  = nxy(jobj)

SELECT CASE (plotobj)
CASE (1); locdat => dat01; mocdat => dat02
CASE (2); locdat => dat02; mocdat => dat01
END SELECT
! ------------------------------------------------
!            01. 2-D Rad. Err.
! ------------------------------------------------
CALL dmalloc(xyobj, mxy)
CALL dmalloc(xysub, lxy)
CALL dmalloc(ref,   mxy)

! SET : Obj. Pow.
DO ixy = 1, mxy
  DO iz = 1, nz
    idat = ixy + mxy * (iz-1)
    
    xyobj(ixy) = xyobj(ixy) + locdat(idat)%pow
  END DO
END DO

xyobj = xyobj / (sum(xyobj) / real(mxy)) ! Norm.

! SET : Sub. Pow.
DO ixy = 1, lxy
  DO iz = 1, nz
    idat = ixy + lxy * (iz-1)
    
    xysub(ixy) = xysub(ixy) + mocdat(idat)%pow
  END DO
END DO

! SET : Ref.
!$OMP PARALLEL PRIVATE(ixy, psum, jxy, kxy) NUM_THREADS(numthr)
!$OMP DO SCHEDULE(GUIDED)
DO ixy = 1, mxy
  psum = ZERO
  
  DO jxy = 1, xymap(0, ixy)
    kxy = xymap(jxy, ixy)
    
    psum = psum + xysub(kxy)
  END DO
  
  ref(ixy) = psum / real(xymap(0, ixy))
END DO
!$OMP END DO
!$OMP END PARALLEL

rnrm = real(mxy) / sum(ref)
ref  = ref * rnrm ! Norm.

! CAL : Err.
IF (lrel) THEN
  !$OMP PARALLEL PRIVATE(ixy) NUM_THREADS(numthr)
  !$OMP DO SCHEDULE(GUIDED)
  DO ixy = 1, mxy
    powerr(ixy, 0) = 100. * (xyobj(ixy) - ref(ixy)) / ref(ixy)
  END DO
  !$OMP END DO
  !$OMP END PARALLEL
ELSE
  !$OMP PARALLEL PRIVATE(ixy) NUM_THREADS(numthr)
  !$OMP DO SCHEDULE(GUIDED)
  DO ixy = 1, mxy
    powerr(ixy, 0) = 100. * (xyobj(ixy) - ref(ixy))
  END DO
  !$OMP END DO
  !$OMP END PARALLEL
END IF

! SUMM.
xymax = max(maxval(powerr(:, 0)), abs(minval(powerr(:, 0))))
xyrms = ZERO

DO ixy = 1, mxy
  xyrms = xyrms + powerr(ixy, 0) * powerr(ixy, 0)
END DO

xyrms = sqrt(xyrms / real(mxy))

xyzmax(0) = xymax
xyzrms(0) = xyrms
! ------------------------------------------------
!            03. 1-D Ax. Err.
! ------------------------------------------------
CALL dmalloc(zobj, nz)
CALL dmalloc(zsub, nz)

! SET : Obj. Pow.
totpow = ZERO

DO iz = 1, nz
  DO ixy = 1, mxy
    idat = ixy + mxy * (iz-1)
    
    zobj(iz) = zobj(iz) + locdat(idat)%pow
    
    totpow = totpow + locdat(idat)%pow * hgt(iz) / avghgt
  END DO
END DO

zobj = zobj / (totpow / real(nz))

! SET : Sub. Pow.
totpow = ZERO

DO iz = 1, nz
  DO ixy = 1, lxy
    idat = ixy + lxy * (iz-1)
    
    zsub(iz) = zsub(iz) + mocdat(idat)%pow
    
    totpow = totpow + mocdat(idat)%pow * hgt(iz) / avghgt
  END DO
END DO

zsub = zsub / (totpow / real(nz))

! CAL : Err.
IF (lrel) THEN
  DO iz = 1, nz
    powerr(0, iz) = 100 * (zobj(iz) - zsub(iz)) / zsub(iz)
  END DO
ELSE
  DO iz = 1, nz
    powerr(0, iz) = 100 * (zobj(iz) - zsub(iz))
  END DO
END IF

! SUMM.
axmax = max(maxval(powerr(0, :)), abs(minval(powerr(0, :))))
axrms = ZERO

DO iz = 1, nz
  axrms = axrms + powerr(0, iz) * powerr(0, iz)
END DO

axrms = sqrt(axrms / real(nz))
! ------------------------------------------------
!            04. FIN
! ------------------------------------------------
! PRINT
WRITE (*, '(A31, F5.2, X, A3)') '2-D Power Error Max. : ', xymax, '(%)'
WRITE (*, '(A31, F5.2, X, A3)') '2-D Power Error RMS  : ', xyrms, '(%)'
WRITE (*, '(A31, F5.2, X, A3)') '1-D Power Error Max. : ', axmax, '(%)'
WRITE (*, '(A31, F5.2, X, A3)') '1-D Power Error RMS  : ', axrms, '(%)'

! FREE
NULLIFY (locdat)
NULLIFY (mocdat)

DEALLOCATE (xyobj)
DEALLOCATE (xysub)
DEALLOCATE (zobj)
DEALLOCATE (zsub)
DEALLOCATE (ref)
! ------------------------------------------------

END SUBROUTINE calpowerr_int