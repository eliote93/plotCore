! --------------------------------------------------------------------------------------------------
SUBROUTINE printout(ierr)

USE param, ONLY : ERRABS, ERRREL
USE mdat, ONLY : l3d, lerr, xyztotmax, xyztotrms, xymax, xyrms, axmax, axrms, xyztotpf, xypf, axpf

IMPLICIT NONE

INTEGER :: ierr
CHARACTER*4 :: ctmp
! ------------------------------------------------

SELECT CASE (ierr)
CASE (ERRABS); ctmp = 'Abs.'
CASE (ERRREL); ctmp = 'Rel.'
END SELECT

IF (lerr) THEN
  IF (l3d) THEN
    WRITE (*, '(A36, F7.2, X, A3)') '3-D Power ' // ctmp // ' Error Max. : ', xyztotmax(ierr), '(%)'
    WRITE (*, '(A36, F7.2, X, A3)') '3-D Power ' // ctmp // ' Error RMS  : ', xyztotrms(ierr), '(%)'
  END IF
  
  WRITE (*, '(A36, F7.2, X, A3)') '2-D Power ' // ctmp // ' Error Max. : ', xymax(ierr), '(%)'
  WRITE (*, '(A36, F7.2, X, A3)') '2-D Power ' // ctmp // ' Error RMS  : ', xyrms(ierr), '(%)'
  
  IF (l3d) THEN
    WRITE (*, '(A36, F7.2, X, A3)') '1-D Power ' // ctmp // ' Error Max. : ', axmax(ierr), '(%)'
    WRITE (*, '(A36, F7.2, X, A3)') '1-D Power ' // ctmp // ' Error RMS  : ', axrms(ierr), '(%)'
  END IF
ELSE
  IF (l3d) WRITE (*, '(A27, F7.2)') '3-D Power Peaking Factor : ', xyztotpf
           WRITE (*, '(A27, F7.2)') '2-D Power Peaking Factor : ', xypf
  IF (l3d) WRITE (*, '(A27, F7.2)') '1-D Power Peaking Factor : ', axpf
END IF

WRITE (*,*)
! ------------------------------------------------

END SUBROUTINE printout
! --------------------------------------------------------------------------------------------------
SUBROUTINE editinfo()

USE param, ONLY : FALSE, DOT, oneline, probe
USE mdat,  ONLY : indev, objfn, plotobj, l3d, nxy, nz, lerr, l3d, xstr2d, ystr2d, nsize2d, gcf2D, gca2D, xylim, xstr1d, ystr1d, nsize1d, gcf1D, gca1D, zlim

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
WRITE (indev, '(A9, X, 2L6, A22)') "LOGICAL",   lerr, l3d,               " ! err, 3d"
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
SUBROUTINE editout(ierr)

USE param, ONLY : FALSE, MP, DOT, BLANK, ERRABS, ERRREL
USE mdat,  ONLY : indev, objfn, plotobj, l3d, lerr, xyzmax, xyzrms, xyzpf, nxy, nz, powerr, objcn, axmax, axrms, axpf, axpow

IMPLICIT NONE

INTEGER :: ierr, iquo, ixy, iz, istz, istxy, iedxy, refobj
INTEGER, PARAMETER :: NLGH = 100
CHARACTER*100 :: locfn
! ------------------------------------------------

! Rad.
IF (lerr) THEN
  SELECT CASE (ierr)
  CASE (ERRABS); WRITE (locfn, '(A, A11)') trim(objfn(plotobj)), '_abs_xy.out'
  CASE (ERRREL); WRITE (locfn, '(A, A11)') trim(objfn(plotobj)), '_rel_xy.out'
  END SELECT
ELSE
  WRITE (locfn, '(A, A7)') trim(objfn(plotobj)), '_xy.out'
END IF

CALL openfile(indev, FALSE, locfn)

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
    WRITE (indev, '(I10, 2F7.2)') iz, xyzmax(iz, ierr), xyzrms(iz, ierr)
  ELSE
    WRITE (indev, '(I10,  F7.2)') iz, xyzpf(iz)
  END IF
  
  iquo = 0
  
  DO
    iquo  = iquo + 1
    istxy = NLGH*(iquo-1) + 1
    iedxy = min(NLGH*iquo, nxy(plotobj))
    
    IF (istxy .GT. nxy(plotobj)) EXIT
    
    WRITE (indev, '(24X, 1000ES13.5)') (powerr(ixy, iz, ierr), ixy = istxy, iedxy)
  END DO
END DO

WRITE (indev, '(A1)') DOT
CLOSE (indev) ! 2

! Ax.
IF (l3d) THEN
  IF (lerr) THEN
    SELECT CASE (ierr)
    CASE (ERRABS); WRITE (locfn, '(A, A10)') trim(objfn(plotobj)), '_abs_z.out'
    CASE (ERRREL); WRITE (locfn, '(A, A10)') trim(objfn(plotobj)), '_rel_z.out'
    END SELECT
  ELSE
    WRITE (locfn, '(A, A6)') trim(objfn(plotobj)), '_z.out'
  END IF
  
  CALL openfile(indev, FALSE, locfn)
  
  IF (lerr) THEN
    WRITE (indev, '(A10, 2A13, 1000I13)') "Legend", "Max.", "RMS", (iz, iz = 1, nz)
    WRITE (indev, '(A10, 1000ES13.5)') objcn(plotobj), axmax(ierr), axrms(ierr), (powerr(0, iz, ierr), iz = 1, nz)
  ELSE
    WRITE (indev, '(A10,  A13, 1000I13)') "Legend", "P.F.",        (iz, iz = 1, nz)
    WRITE (indev, '(A10, 1000ES13.5)') objcn(plotobj), axpf,                     (axpow(iz, plotobj),  iz = 1, nz)
    
    !IF (lrel) THEN
    !  refobj = MP(plotobj)
    !  
    !  WRITE (indev, '(A10, A13, 1000ES13.5)') objcn(refobj), BLANK,              (axpow(iz, refobj),   iz = 1, nz)
    !END IF
  END IF
  
  WRITE (indev, '(A1)') DOT
  CLOSE (indev) ! 2
END IF
! ------------------------------------------------

END SUBROUTINE editout
! --------------------------------------------------------------------------------------------------