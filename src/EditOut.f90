! --------------------------------------------------------------------------------------------------
SUBROUTINE editinfo()

USE param, ONLY : FALSE, DOT, oneline, probe
USE mdat,  ONLY : indev, objfn, plotobj, l3d, nxy, nz, lerr, lrel, l3d, xstr2d, ystr2d, nsize2d, gcf2D, gca2D, xylim, xstr1d, ystr1d, nsize1d, gcf1D, gca1D, zlim

IMPLICIT NONE

CHARACTER*100 :: locfn

INTEGER :: echodev, nn
! ------------------------------------------------

! Echo
WRITE (locfn, '(A, A5)') trim(objfn(plotobj)), '.info'
CALL openfile(indev, FALSE, locfn)

WRITE (indev, '(A6/)') "$ Echo"

echodev = 29

OPEN (echodev, FILE = 'plotCore.inp')

DO
  READ (echodev, '(A512)') oneline
  WRITE (indev, '(A)') trim(oneline)
  
  IF (probe .NE. DOT) CYCLE
  
  BACKSPACE (indev) ! RMV : DOT
  EXIT
END DO

WRITE (indev, *)
CLOSE (echodev)

! Info. : Rad.
WRITE (indev, '(A6/)') "$ Rad."

IF (l3d) THEN
  nn = nz + 1
ELSE
  nn = 1
END IF

WRITE (indev, '(A9, X, 2I6, A25)') "# of Dat.", nxy(plotobj), nn,        " ! Pin., Img."
WRITE (indev, '(A9, X, 3L6, A21)') "LOGICAL",   lerr, lrel, l3d,         " ! err, rel, 3d"
WRITE (indev, '(A9, X, 3I6, A19)') "String",    xstr2d, ystr2d, nsize2d, " ! x, y, size"
WRITE (indev, '(A9, X, 4I6)')      "GCF",       gcf2D(1:4)
WRITE (indev, '(A9, X, 4F6.3)')    "GCA",       gca2D(1:4)
WRITE (indev, '(A9, X, 2F6.1)')    "yMax",      xylim

IF (lerr) THEN
  WRITE (indev, '(A9, X, A30)')    "Label",     "Normalized Pin Power Error (%)"
ELSE
  WRITE (indev, '(A9, X, A20)')    "Label",     "Normalized Pin Power"
END IF

! Info. : Ax.
IF (l3d) THEN
  WRITE (indev, '(/A5/)') "$ Ax."
  
  nn = 1 ! Manually Inputted
  
  WRITE (indev, '(A9, X, 2I6, A25)')          "# of Dat.", nz, nn,                  " ! Pln., Img."
  WRITE (indev, '(A9, X, I6, F6.3, I6, A19)') "String",    xstr1d, ystr1d, nsize1d, " ! x, y, size"
  WRITE (indev, '(A9, X, 4I6)')               "GCF",       gcf1D(1:4)
  WRITE (indev, '(A9, X, 4F6.3)')             "GCA",       gca1D(1:4)
  WRITE (indev, '(A9, X, F6.3)')              "yMax",      zlim
  
  IF (lerr) THEN
    WRITE (indev, '(A9, X, A30)')    "Label",     "Normalized Pln. Power Eror (%)"
  ELSE
    WRITE (indev, '(A9, X, A21)')    "Label",     "Normalized Pln. Power"
  END IF
END IF

WRITE (indev, '(A1)') DOT
CLOSE (indev) ! 2
! ------------------------------------------------

END SUBROUTINE editinfo
! --------------------------------------------------------------------------------------------------
SUBROUTINE editgrid()

USE param, ONLY : FALSE, DOT
USE mdat,  ONLY : indev, objfn, plotobj, nxy, ptmap, l3d, nz, hgt

IMPLICIT NONE

INTEGER :: ixy, iz
CHARACTER*100 :: locfn
! ------------------------------------------------

WRITE (locfn, '(A, A5)') trim(objfn(plotobj)), '.grid'
CALL openfile(indev, FALSE, locfn)

! Rad.
WRITE (indev, '(A6)') "$ Rad."
WRITE (indev, '(5X, 2A78)') "x", "y"
WRITE (indev, '(5X, 12A13)') (("NW", "NE", "EE", "SE", "SW", "WW"), ixy = 1, 2)

DO ixy = 1, nxy(plotobj)
  WRITE (indev, '(I4, X, 12ES13.5)') ixy, ptmap(1, 1:6, ixy), ptmap(2, 1:6, ixy)
END DO

! Ax.
IF (l3d) THEN
  WRITE (indev, '(/A5)') "$ Ax."
  WRITE (indev, '(5X, A13)') "Thk."
  
  DO iz = 1, nz
    WRITE (indev, '(I4, X, ES13.5)') iz, hgt(iz)
  END DO
END IF

WRITE (indev, '(A1)') DOT
CLOSE (indev)
! ------------------------------------------------

END SUBROUTINE editgrid
! --------------------------------------------------------------------------------------------------
SUBROUTINE editout()

USE param, ONLY : FALSE, MP, DOT, BLANK
USE mdat,  ONLY : indev, objfn, plotobj, l3d, lerr, xyzmax, xyzrms, xyzpf, nxy, nz, powerr, lrel, objcn, axmax, axrms, axpf, axpow

IMPLICIT NONE

INTEGER :: iquo, ixy, iz, istz, istxy, iedxy, refobj
INTEGER, PARAMETER :: NLGH = 100
CHARACTER*100 :: locfn
! ------------------------------------------------

WRITE (locfn, '(A, A4)') trim(objfn(plotobj)), '.out'
CALL openfile(indev, FALSE, locfn)

! Rad.
WRITE (indev, '(A6/)') "$ Rad."

IF (l3d) THEN
  istz = 0
ELSE
  istz = 1
END IF

IF (lerr) THEN
  WRITE (indev, '(A10, 2A7, 1000I13)') "Legend", "Max.", "RMS", (ixy, ixy = 1, NLGH)
ELSE
  WRITE (indev, '(A10, A14, 1000I13)') "Legend", "P.F.",        (ixy, ixy = 1, NLGH)
END IF

DO iz = istz, nz
  IF (lerr) THEN
    WRITE (indev, '(I10, 2F7.2)') iz, xyzmax(iz), xyzrms(iz)
  ELSE
    WRITE (indev, '(I10,  F7.2)') iz, xyzpf(iz)
  END IF
  
  iquo = 0
  
  DO
    iquo  = iquo + 1
    istxy = NLGH*(iquo-1) + 1
    iedxy = min(NLGH*iquo, nxy(plotobj))
    
    IF (istxy .GT. nxy(plotobj)) EXIT
    
    WRITE (indev, '(24X, 1000ES13.5)') (powerr(ixy, iz), ixy = istxy, iedxy)
  END DO
END DO

! Ax.
IF (l3d) THEN
  WRITE (indev, '(/A5/)') "$ Ax."
  
  IF (lerr) THEN
    WRITE (indev, '(A10, 2A13, 1000I13)') "Legend", "Max.", "RMS", (iz, iz = 1, nz)
    WRITE (indev, '(A10, 1000ES13.5)') objcn(plotobj), axmax, axrms, (powerr(0, iz), iz = 1, nz)
  ELSE
    WRITE (indev, '(A10,  A13, 1000I13)') "Legend", "P.F.",        (iz, iz = 1, nz)
    WRITE (indev, '(A10, 1000ES13.5)') objcn(plotobj), axpf,         (axpow(iz, plotobj), iz = 1, nz)
    
    IF (lrel) THEN
      refobj = MP(plotobj)
      
      WRITE (indev, '(A10, A13, 1000ES13.5)') objcn(refobj), BLANK,  (axpow(iz, refobj),  iz = 1, nz)
    END IF
  END IF
END IF

WRITE (indev, '(A1)') DOT
CLOSE (indev) ! 2
! ------------------------------------------------

END SUBROUTINE editout
! --------------------------------------------------------------------------------------------------
SUBROUTINE editout_old()

USE param, ONLY : FALSE, DOT
USE mdat,  ONLY : indev, objfn, objcn, xstr2d, ystr2d, nsize2d, xstr1d, ystr1d, nsize1d, nxy, xyzmax, xyzrms, lerr, lrel, xyzpf, &
                  gcf2D, gca2D, gcf1d, gca1d, nz, l3d, xypf, axpf, xymax, axmax, xyrms, axrms, hgt, powerr, axpow, zlim, plotobj

IMPLICIT NONE

CHARACTER*100 :: fniz

INTEGER :: iz
LOGICAL :: lplnout = FALSE ! Default
! ------------------------------------------------

! 2-D
IF (.NOT. l3d) THEN
  WRITE (fniz, '(A, A4)') trim(objfn(plotobj)), '.inp'
  
  CALL openfile(indev, FALSE, fniz)
  CALL echoinp(indev)
  
  WRITE (indev, '(I5, 3L2)') nxy(plotobj), lerr, lrel, l3d
  WRITE (indev, '(3ES13.5)') xyzpf(1), xyzmax(1), xyzrms(1)
  WRITE (indev, '(3I5)')     xstr2d, ystr2d, nsize2d
  WRITE (indev, '(4I5)')     gcf2D(1:4)
  WRITE (indev, '(4F6.3)')   gca2D(1:4)
  
  CALL editrad(1)
  
  WRITE (indev, '(A1)') DOT
  
  CLOSE (indev)
END IF
! ------------------------------------------------
IF (.NOT. l3d) RETURN

! 3-D
DO iz = 1, nz
  IF (.NOT. lplnout) CYCLE
  
  IF (iz .LT. 10) WRITE (fniz, '(A, A6, I1, A4)') trim(objfn(plotobj)), ' PLN 0', iz, '.inp'
  IF (iz .GE. 10) WRITE (fniz, '(A, A5, I2, A4)') trim(objfn(plotobj)), ' PLN ',  iz, '.inp'
    
  CALL openfile(indev, FALSE, fniz)
  CALL echoinp(indev)
  
  WRITE (indev, '(I5, 3L2)') nxy(plotobj), lerr, lrel, l3d
  WRITE (indev, '(3ES13.5)') xyzpf(iz), xyzmax(iz), xyzrms(iz)
  WRITE (indev, '(3I5)')     xstr2d, ystr2d, nsize2d
  WRITE (indev, '(4I5)')     gcf2D(1:4)
  WRITE (indev, '(4F6.3)')   gca2D(1:4)
  
  CALL editrad(iz)
  
  WRITE (indev, '(A1)') DOT
  
  CLOSE (indev)
END DO

! Integrated : 2-D
WRITE (fniz, '(A, A11)') trim(objfn(plotobj)), ' PLN 00.inp'

CALL openfile(indev, FALSE, fniz)
CALL echoinp(indev)

WRITE (indev, '(I5, 3L2)') nxy(plotobj), lerr, lrel, l3d
WRITE (indev, '(3ES13.5)') xypf, xymax, xyrms
WRITE (indev, '(3I5)')     xstr2d, ystr2d, nsize2d
WRITE (indev, '(4I5)')     gcf2D(1:4)
WRITE (indev, '(4F6.3)')   gca2D(1:4)

CALL editrad(0)

! Integrated : 1-D
WRITE (indev, '(I5, 3L2, 2(X, A2))') nz, lerr, lrel, l3d, objcn(1), objcn(2)
WRITE (indev, '(4ES13.5)')           axpf, axmax, axrms, zlim
WRITE (indev, '(I5, F7.2, I5)')      xstr1d, ystr1d, nsize1d
WRITE (indev, '(4I5)')               gcf1D(1:4)
WRITE (indev, '(4F6.3)')             gca1D(1:4)

IF (.NOT.lerr .AND. lrel) THEN
  DO iz = 1, nz
    WRITE (indev, '(100ES13.5)') hgt(iz), axpow(iz, 1), axpow(iz, 2)
  END DO
ELSE
  DO iz = 1, nz
    WRITE (indev, '(100ES13.5)') hgt(iz), powerr(0, iz)
  END DO
END IF

WRITE (indev, '(A1)') DOT
CLOSE (indev)
! ------------------------------------------------

END SUBROUTINE editout_old
! --------------------------------------------------------------------------------------------------
SUBROUTINE editrad(iz)

USE mdat, ONLY : indev, nxy, powerr, ptmap, plotobj

IMPLICIT NONE

INTEGER :: ixy, iz
! ------------------------------------------------

!OPEN (43, FILE = 'V09', STATUS = 'OLD')
!
!DO ixy = 1, nxy(plotobj)
!  READ (43, *) powerr(ixy, iz)
!END DO
!
!CLOSE (43)

DO ixy = 1, nxy(plotobj)
  WRITE (indev, '(13ES13.5)') powerr(ixy, iz), ptmap(1, 1:6, ixy), ptmap(2, 1:6, ixy)
END DO
! ------------------------------------------------

END SUBROUTINE editrad
! --------------------------------------------------------------------------------------------------