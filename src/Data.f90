MODULE mdat

USE param, ONLY : FALSE

IMPLICIT NONE

CHARACTER*2   :: objcn(2)
CHARACTER*100 :: fdir, objfn(2)

LOGICAL :: l02, lerr, lrel, l3d
LOGICAL :: lbndy = FALSE ! Print Only 4 Points

LOGICAL, DIMENSION(2) :: lHS, lrot
! ------------------------------------------------
INTEGER :: xstr2d, ystr2d, nsize2d, xstr1d, nsize1d, gcf2d(4), gcf1d(4)
INTEGER :: plotobj, numthr, indev, nz, maRng, mPin

INTEGER, DIMENSION(2) :: ninp, nxy, ndat, naRng, MCnPin, RNnPin

INTEGER, DIMENSION(100, 2) :: NTnPin

INTEGER, POINTER, DIMENSION(:) :: ptmod

INTEGER, POINTER, DIMENSION(:,:) :: xymap ! (nMC:iMC, iNT)
! ------------------------------------------------
REAL :: xymax, xyrms, axmax, axrms, xypf, axpf, avghgt, zlim, boF2F, qF2F
REAL :: ystr1d, gca2d(4), gca1d(4)
REAL :: corebndy(2, 2, 2) ! (min/max, x/y, obj)

REAL, DIMENSION(2) :: aoF2F, MCpF2F, rMC, RNpF2F

REAL, DIMENSION(100, 2) :: NTpF2F

REAL, POINTER, DIMENSION(:) :: MCstd01 ! (idat)
REAL, POINTER, DIMENSION(:) :: MCstd02 ! (idat)
REAL, POINTER, DIMENSION(:) :: errmax  ! (iz)
REAL, POINTER, DIMENSION(:) :: errrms  ! (iz)
REAL, POINTER, DIMENSION(:) :: powpf   ! (iz)
REAL, POINTER, DIMENSION(:) :: errxy   ! (ixy)
REAL, POINTER, DIMENSION(:) :: errax   ! (iz)
REAL, POINTER, DIMENSION(:) :: hgt     ! (iz)

REAL, POINTER, DIMENSION(:,:) :: axpow  ! (iz, iobj)
REAL, POINTER, DIMENSION(:,:) :: powerr ! (ixy, iz)

REAL, POINTER, DIMENSION(:,:,:) :: ptmap ! (x/y, idir, ixy)
! ------------------------------------------------
TYPE powdata_type
  REAL :: pow, x, y
  
  INTEGER :: iz, iax, iay, iasy, ipx, ipy, ipin
END TYPE powdata_type

TYPE (powdata_type), POINTER, DIMENSION(:) :: dat01, dat02
! ------------------------------------------------

END MODULE mdat