SUBROUTINE default

USE param, ONLY : FALSE, ONE, ZERO
USE mdat,  ONLY : lbndy, l02, nz, ninp, xylim, zlim, aoF2F

IMPLICIT NONE

nz    = 1
l02   = FALSE
xylim = ONE
zlim  = ONE
aoF2F = ZERO
ninp  = 0
lbndy = FALSE ! Print Only 4 Points

END SUBROUTINE default