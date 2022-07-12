! --------------------------------------------------------------------------------------------------
SUBROUTINE editout()

USE param, ONLY : FALSE, DOT
USE mdat,  ONLY : indev, objfn, objcn, xstr2d, ystr2d, nsize2d, xstr1d, ystr1d, nsize1d, nxy, errmax, errrms, lerr, lrel, powpf, &
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
  WRITE (indev, '(3ES13.5)') powpf(1), errmax(1), errrms(1)
  WRITE (indev, '(3I5)')     xstr2d, ystr2d, nsize2d
  WRITE (indev, '(4I5)')     gcf2D(1:4)
  WRITE (indev, '(4F6.3)')   gca2D(1:4)
  
  CALL editrad(1)
  
  WRITE (indev, '(A1)') DOT
  
  CLOSE (indev)
  
  IF (lerr) THEN
    WRITE (*, '(A31, F5.2, X, A3)') '2-D Power Error Max. : ', errmax(1), '(%)'
    WRITE (*, '(A31, F5.2, X, A3)') '2-D Power Error RMS  : ', errrms(1), '(%)'
  END IF
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
  WRITE (indev, '(3ES13.5)') powpf(iz), errmax(iz), errrms(iz)
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

! PRINT
IF (lerr) THEN
  WRITE (*, '(A31, F5.2, X, A3)') '2-D Power Error Max. : ', xymax, '(%)'
  WRITE (*, '(A31, F5.2, X, A3)') '2-D Power Error RMS  : ', xyrms, '(%)'
  WRITE (*, '(A31, F5.2, X, A3)') '1-D Power Error Max. : ', axmax, '(%)'
  WRITE (*, '(A31, F5.2, X, A3)') '1-D Power Error RMS  : ', axrms, '(%)'
END IF

WRITE (indev, '(A1)') DOT

CLOSE (indev)
! ------------------------------------------------

END SUBROUTINE editout
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