SUBROUTINE calpowerr_int()

USE allocs
USE param, ONLY : ZERO, MP, ERRABS, ERRREL
USE mdat,  ONLY : powdata_type, lerr, l3d, xyzmax, powerr, nz, nxy, ndat, xymax, xyrms, axmax, axrms, dat01, dat02, numthr, xymap, nz, hgt, avghgt, plotobj, xyzmax, xyzrms

IMPLICIT NONE

INTEGER :: iz, ixy, jxy, kxy, idat, mxy, lxy, jobj, ierr
REAL :: totmax, totrms, rnrm, psum, totpow

REAL, POINTER, DIMENSION(:) :: xyobj, xysub, zobj, zsub, ref

CHARACTER*4 :: ctmp
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
    idat = ixy + mxy*(iz-1)
    
    xyobj(ixy) = xyobj(ixy) + locdat(idat)%pow
  END DO
END DO

xyobj = xyobj / (sum(xyobj) / real(mxy)) ! Norm.

! SET : Sub. Pow.
DO ixy = 1, lxy
  DO iz = 1, nz
    idat = ixy + lxy*(iz-1)
    
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
ref  = ref*rnrm ! Norm.

! CAL : Err.
!$OMP PARALLEL PRIVATE(ixy) NUM_THREADS(numthr)
!$OMP DO SCHEDULE(GUIDED)
DO ixy = 1, mxy
  powerr(ixy, 0, ERRABS) = 100.*(xyobj(ixy) - ref(ixy))
  powerr(ixy, 0, ERRREL) = 100.*(xyobj(ixy) - ref(ixy)) / ref(ixy)
END DO
!$OMP END DO
!$OMP END PARALLEL

! SUMM.
xyrms = ZERO

DO ierr = 1, 2
  xymax(ierr) = max(maxval(powerr(:, 0, ierr)), abs(minval(powerr(:, 0, ierr))))
  
  DO ixy = 1, mxy
    xyrms(ierr) = xyrms(ierr) + powerr(ixy, 0, ierr)*powerr(ixy, 0, ierr)
  END DO
  
  xyrms(ierr) = sqrt(xyrms(ierr) / real(mxy))
  
  xyzmax(0, ierr) = xymax(ierr)
  xyzrms(0, ierr) = xyrms(ierr)
END DO
! ------------------------------------------------
!            03. 1-D Ax. Err.
! ------------------------------------------------
CALL dmalloc(zobj, nz)
CALL dmalloc(zsub, nz)

! SET : Obj. Pow.
totpow = ZERO

DO iz = 1, nz
  DO ixy = 1, mxy
    idat = ixy + mxy*(iz-1)
    
    zobj(iz) = zobj(iz) + locdat(idat)%pow
    
    totpow = totpow + locdat(idat)%pow*hgt(iz) / avghgt
  END DO
END DO

zobj = zobj / (totpow / real(nz))

! SET : Sub. Pow.
totpow = ZERO

DO iz = 1, nz
  DO ixy = 1, lxy
    idat = ixy + lxy*(iz-1)
    
    zsub(iz) = zsub(iz) + mocdat(idat)%pow
    
    totpow = totpow + mocdat(idat)%pow*hgt(iz) / avghgt
  END DO
END DO

zsub = zsub / (totpow / real(nz))

! CAL : Err.
DO iz = 1, nz
  powerr(0, iz, ERRABS) = 100*(zobj(iz) - zsub(iz))
  powerr(0, iz, ERRREL) = 100*(zobj(iz) - zsub(iz)) / zsub(iz)
END DO

! SUMM.
axrms = ZERO

DO ierr = 1, 2
  axmax(ierr) = max(maxval(powerr(0, :, ierr)), abs(minval(powerr(0, :, ierr))))
  
  DO iz = 1, nz
    axrms(ierr) = axrms(ierr) + powerr(0, iz, ierr)*powerr(0, iz, ierr)
  END DO
  
  axrms(ierr) = sqrt(axrms(ierr) / real(nz))
END DO
! ------------------------------------------------
!            04. Free
! ------------------------------------------------
NULLIFY (locdat)
NULLIFY (mocdat)

DEALLOCATE (xyobj)
DEALLOCATE (xysub)
DEALLOCATE (zobj)
DEALLOCATE (zsub)
DEALLOCATE (ref)
! ------------------------------------------------

END SUBROUTINE calpowerr_int