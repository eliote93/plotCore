SUBROUTINE plotpow()
! PLOT : inputted Power instead Power Error

USE allocs
USE param, ONLY : ZERO
USE mdat,  ONLY : lerr, lrel, plotobj, ndat, dat01, dat02, powerr, numthr, powpf, nxy, nz, xypf, axpf, avghgt, hgt, axpow, powdata_type

IMPLICIT NONE

INTEGER :: idat, ixy, iz, iobj
REAL :: totpf

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat
! ------------------------------------------------

CALL dmalloc(powpf, nz)

IF (lerr) RETURN

SELECT CASE (plotobj)
CASE (1); locdat => dat01
CASE (2); locdat => dat02
END SELECT

CALL dmalloc0(powerr, 0, nxy(plotobj), 0, nz)
! ------------------------------------------------
!$OMP PARALLEL PRIVATE(iz, ixy, idat) NUM_THREADS(numthr)
!$OMP DO SCHEDULE(GUIDED)
DO iz = 1, nz
  DO ixy = 1, nxy(plotobj)
    idat = ixy + nxy(plotobj) * (iz-1)
    
    powerr(ixy, iz) = locdat(idat)%pow
  END DO
  
  powpf(iz) = maxval(powerr(:, iz))
END DO
!$OMP END DO
!$OMP END PARALLEL

totpf = maxval(powerr)

WRITE (*, '(A27, F7.2)') '3-D Power Peaking Factor : ', totpf
! ------------------------------------------------
DO ixy = 1, nxy(plotobj)
  DO iz = 1, nz
    idat = ixy + nxy(plotobj) * (iz-1)
    
    powerr(ixy, 0) = powerr(ixy, 0) + powerr(ixy, iz)
    powerr(0,  iz) = powerr(0,  iz) + powerr(ixy, iz) * hgt(iz) / avghgt
  END DO
END DO

powerr(:, 0) = powerr(:, 0) / (sum(powerr(:, 0)) / real(nxy(plotobj)))
powerr(0, :) = powerr(0, :) / (sum(powerr(0, :)) / real(nz))

xypf = maxval(powerr(:, 0))
axpf = maxval(powerr(0, :))
! ------------------------------------------------
NULLIFY (locdat)

IF (.NOT. lrel) RETURN

CALL dmalloc(axpow, nz, 2)

DO iobj = 1, 2
  SELECT CASE (iobj)
  CASE (1); locdat => dat01
  CASE (2); locdat => dat02
  END SELECT

  DO idat = 1, ndat(iobj)
    iz = locdat(idat)%iz
    
    axpow(iz, iobj) = axpow(iz, iobj) + locdat(idat)%pow * hgt(iz) / avghgt
  END DO
  
  axpow(:, iobj) = axpow(:, iobj) / (sum(axpow(1:nz, iobj)) / real(nz))
END DO

NULLIFY (locdat)
! ------------------------------------------------

END SUBROUTINE plotpow