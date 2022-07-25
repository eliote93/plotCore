PROGRAM plotCore
! ASSUME : Only One Fuel Pin Type

USE mdat, ONLY : fdir, numthr, nerr

IMPLICIT NONE

INTEGER :: ierr

fdir   = 'C:\Users\user\Documents\MATLAB\'
numthr = 6

CALL readinp
CALL readobj(1)
CALL readobj(2)
CALL chkobj

CALL setgeo
CALL setmap

CALL plotpow

CALL calpowerr_3D
CALL calpowerr_int

CALL editinfo
CALL editgrid

DO ierr = 1, 2
  CALL editout(ierr)
END DO

END PROGRAM plotCore