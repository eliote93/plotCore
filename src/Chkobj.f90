SUBROUTINE chkobj()

USE param, ONLY : EPS, MP
USE mdat,  ONLY : lerr, l3d, lHS, naRng, maRng, aoF2F, boF2F, MCpF2F, NTpF2F, RNpF2F, qF2F, MCnPin, NTnPin, RNnPin, mPin, ndat, nxy, nz, dat01, dat02, zlim, objcn, powdata_type, plotobj

IMPLICIT NONE

REAL :: err, pF2F(2)
INTEGER :: nPin(2), iobj, ixy, jxy, iz, iax, jax, iay, jay, ipx, jpx, ipy, jpy

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat
! ------------------------------------------------

IF (lerr .AND. nxy(1).NE.nxy(2)) THEN
  IF (     lHS(1) .AND.      lHS(2)) CALL terminate("# of RADIAL PINS")
  IF (.NOT.lHS(1) .AND. .NOT.lHS(2)) CALL terminate("# of RADIAL PINS")
END IF

maRng = naRng(plotobj)
boF2F = aoF2F(plotobj)

SELECT CASE (objcn(plotobj))
CASE ('NT')
  mPin = NTnPin (1, plotobj)
  qF2F = NTpF2F (1, plotobj)
CASE ('MC')
  mPin = MCnPin (plotobj)
  qF2F = MCpF2F (plotobj)
CASE ('RN')
  mPin = RNnPin (plotobj)
  qF2F = RNpF2F (plotobj)
END SELECT

IF (.NOT. lerr) RETURN

IF (.NOT. lHS(plotobj) .AND. lHS(MP(plotobj))) CALL terminate("OBJ = FS vs. REF = HS")

SELECT CASE (objcn(1))
CASE ('NT'); nPin(1) = NTnPin(1, 1); pF2F(1) = NTpF2F(1, 1)
CASE ('MC'); nPin(1) = MCnPin(1);    pF2F(1) = MCpF2F(1)
CASE ('RN'); nPin(1) = RNnPin(1);    pF2F(1) = RNpF2F(1)
END SELECT

SELECT CASE (objcn(2))
CASE ('NT'); nPin(2) = NTnPin(1, 2); pF2F(2) = NTpF2F(1, 2)
CASE ('MC'); nPin(2) = MCnPin(2);    pF2F(2) = MCpF2F(2)
CASE ('RN'); nPin(2) = RNnPin(2);    pF2F(2) = RNpF2F(2)
END SELECT

IF (naRng(1) .NE. naRng(2)) CALL terminate("ASY RING")
IF (nPin(1)  .NE. nPin (2)) CALL terminate("PIN RING")

err = abs(aoF2F(1) - aoF2F(2))

IF (err .GT. EPS) CALL terminate("ASY F2F")

err = abs(pF2F(1) - pF2F(2))

IF (err .GT. EPS) CALL terminate("PIN F2F")
! ------------------------------------------------
IF (.NOT. l3d)  RETURN

IF (abs(zlim) .LT. EPS) CALL terminate("AXIAL ERROR LEGEND")

DO iobj = 1, 2
  SELECT CASE (iobj)
  CASE (1); locdat => dat01
  CASE (2); locdat => dat02
  END SELECT
  
  DO ixy = 1, nxy(iobj)
    iax = locdat(ixy)%iax
    iay = locdat(ixy)%iay
    ipx = locdat(ixy)%ipx
    ipy = locdat(ixy)%ipy
    
    DO iz = 2, nz
      jxy = ixy + nxy(iobj) * (iz-1)
      
      jax = locdat(jxy)%iax
      jay = locdat(jxy)%iay
      jpx = locdat(jxy)%ipx
      jpy = locdat(jxy)%ipy
      
      IF (iax.NE.jax .OR. iay.NE.jay .OR. ipx.NE.jpx .OR. ipy.NE.jpy) CALL terminate("3-D MAP")
    END DO
  END DO
END DO

NULLIFY (locdat)
! ------------------------------------------------

END SUBROUTINE chkobj