PROGRAM plotCore
! ASSUME : Only One Fuel Pin Type

USE mdat, ONLY : fdir, numthr

IMPLICIT NONE

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

CALL editout
!CALL editinfo
!CALL editgrid

END PROGRAM plotCore