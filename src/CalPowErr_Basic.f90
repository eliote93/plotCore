SUBROUTINE calpowerr_basic()

USE allocs
USE param, ONLY : ZERO, MP
USE mdat,  ONLY : powdata_type, lerr, lrel, l3d, powerr, dat01, dat02, ndat, errmax, errrms, nz, nxy, xymap, numthr, avghgt, hgt, plotobj

IMPLICIT NONE

INTEGER :: idat, jdat, iz, ixy, jxy, kxy, jobj, mxy
REAL :: psum, tot01, tot02, rnrm

REAL, POINTER, DIMENSION(:) :: ref

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat, mocdat
! ------------------------------------------------

CALL dmalloc(errmax, nz)
CALL dmalloc(errrms, nz)

IF (.NOT. lerr) RETURN

mxy = nxy(plotobj)

CALL dmalloc0(powerr, 0, mxy, 0, nz)

CALL dmalloc(ref, ndat(plotobj))

jobj = MP(plotobj)

SELECT CASE (plotobj)
CASE (1); locdat => dat01; mocdat => dat02
CASE (2); locdat => dat02; mocdat => dat01
END SELECT
! ------------------------------------------------
!            01. SET : Ref.
! ------------------------------------------------
!$OMP PARALLEL PRIVATE(iz, ixy, idat, psum, jxy, kxy, jdat) NUM_THREADS(numthr)
!$OMP DO SCHEDULE(GUIDED)
DO iz = 1, nz
  DO ixy = 1, mxy
    idat = ixy + mxy * (iz-1)
    psum = ZERO
    
    DO jxy = 1, xymap(0, ixy)
      kxy  = xymap(jxy, ixy)
      jdat = kxy + nxy(jobj) * (iz-1)
      
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
    
    psum = psum + ref(idat) * hgt(iz) / avghgt
  END DO
END IF

rnrm = real(ndat(plotobj)) / psum
ref = ref * rnrm
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
!    idat = ixy + nxy(2) * (iz-1)
!    
!    WRITE (41, '(ES13.5)') dat02(idat)%pow
!  END DO
!END DO
!
!CLOSE (41)
!STOP
! ------------------------------------------------
!            03. CAL : Err.
! ------------------------------------------------
IF (lrel) THEN
  !$OMP PARALLEL PRIVATE(iz, ixy, idat) NUM_THREADS(numthr)
  !$OMP DO SCHEDULE(GUIDED)
  DO iz = 1, nz
    DO ixy = 1, mxy
      idat = ixy + mxy * (iz-1)
      
      powerr(ixy, iz) = 100. * (locdat(idat)%pow - ref(idat)) / ref(idat)
    END DO
  END DO
  !$OMP END DO
  !$OMP END PARALLEL
ELSE
  !$OMP PARALLEL PRIVATE(iz, ixy, idat) NUM_THREADS(numthr)
  !$OMP DO SCHEDULE(GUIDED)
  DO iz = 1, nz
    DO ixy = 1, mxy
      idat = ixy + mxy * (iz-1)
      
      powerr(ixy, iz) = 100. * (locdat(idat)%pow - ref(idat))
    END DO
  END DO
  !$OMP END DO
  !$OMP END PARALLEL
END IF
! ------------------------------------------------
!            04. SUMMARIZE
! ------------------------------------------------
DO iz = 1, nz
  errmax(iz) = max(maxval(powerr(:, iz)), abs(minval(powerr(:, iz))))
  
  DO ixy = 1, mxy
    errrms(iz) = errrms(iz) + powerr(ixy, iz) * powerr(ixy, iz)
  END DO
  
  errrms(iz) = sqrt(errrms(iz) / real(mxy))
END DO

IF (.NOT. l3d) THEN
  WRITE (*, '(A31, F5.2, X, A3)') '2-D Power Error Max. : ', errmax(1), '(%)'
  WRITE (*, '(A31, F5.2, X, A3)') '2-D Power Error RMS  : ', errrms(1), '(%)'
END IF

NULLIFY (locdat)
NULLIFY (mocdat)
DEALLOCATE (ref)
! ------------------------------------------------

END SUBROUTINE calpowerr_basic