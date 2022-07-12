! --------------------------------------------------------------------------------------------------
SUBROUTINE readMC(iobj, ninp, fn)

USE allocs
USE param, ONLY : ONE

IMPLICIT NONE

INTEGER :: iobj, ninp
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
INTEGER :: iinp
REAL :: rMC
! ------------------------------------------------

rMC = ONE / real(ninp)

! 1st
CALL readMC_1st(iobj, rMC, fn)

! 2nd
DO iinp = 2, ninp
  CALL readMC_2nd(iobj, iinp, rMC, fn)
END DO

! FIN
CALL calMCstd(iobj)
! ------------------------------------------------

END SUBROUTINE readMC
! --------------------------------------------------------------------------------------------------
SUBROUTINE readMC_1st(iobj, rMC, fn)

USE allocs
USE param, ONLY : SQ3, DOT, BANG, BLANK, ASTR, ZERO, TRUE, oneline, probe
USE mdat,  ONLY : l3d, ndat, dat01, dat02, naRng, numthr, MCpF2F, aoF2F, MCnPin, indev, nz, hgt, MCstd01, MCstd02, avghgt, powdata_type, nxy

IMPLICIT NONE

INTEGER :: iobj
REAL :: rMC
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
CHARACTER*10  :: cn
CHARACTER*100 :: gn, cdum

INTEGER :: iz, iax, iay, ipx, ipy, idat
INTEGER :: nchr, lgh, nminax, nmaxax, nminay, nmaxay, ndel, fndndata, MCndat, MCnz
INTEGER :: ipos(6), jpos(3)

REAL :: locpow, tmp, totpow, rnrm
REAL, POINTER, DIMENSION(:) :: locstd

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat
! ------------------------------------------------

gn = 'MC\' // trim(fn) // ' 01'

CALL openfile(indev, TRUE, gn)
! ------------------------------------------------
!            01. # of data
! ------------------------------------------------
ndat(iobj) = 0

DO
  READ (indev, '(A512)', END = 1000) oneline
  
  IF (probe .EQ. DOT)   EXIT
  IF (probe .EQ. BANG)  CYCLE
  IF (probe .EQ. BLANK) CYCLE
  
  READ (oneline, *) cn
  CALL toupper(cn)
  
  nchr = fndndata(oneline)
  
  IF (cn.EQ.ASTR .AND. nchr.EQ.4) THEN
    READ (oneline, *) cn, cn, cn
    
    SELECT CASE (cn)
      CASE ('PIN_F2F'); READ (oneline, *) cn, cn, cn, MCpF2F(iobj)
      CASE ('PIN_PCH'); READ (oneline, *) cn, cn, cn, tmp; MCpF2F(iobj) = tmp * SQ3
      CASE ('ASY_F2F'); READ (oneline, *) cn, cn, cn, aoF2F(iobj)
      CASE ('ASY_PCH'); READ (oneline, *) cn, cn, cn, tmp; aoF2F (iobj) = tmp * SQ3
    END SELECT
  END IF
  
  IF (cn .NE. '*CELL') CYCLE
  
  READ (oneline, *) cdum, cdum, cdum
  
  CALL fndchr(cdum, ipos, nchr, '_')
  
  IF (nchr .LT. 4) CYCLE
  
  ndat(iobj) = ndat(iobj) + 1
END DO

1000 CONTINUE

REWIND (indev)

MCndat = ndat(iobj)

SELECT CASE (iobj)
CASE (1); ALLOCATE (dat01 (MCndat)); locdat => dat01; CALL dmalloc(MCstd01, MCndat); locstd => MCstd01
CASE (2); ALLOCATE (dat02 (MCndat)); locdat => dat02; CALL dmalloc(MCstd02, MCndat); locstd => MCstd02
END SELECT
! ------------------------------------------------
!            02. 3-D Pin Power
! ------------------------------------------------
MCndat = 0
nmaxax = 0
nmaxay = 0
nminax = 100
nminay = 100
MCnz   = 0

DO
  READ (indev, '(A512)', END = 2000) oneline
  
  IF (probe .EQ. DOT)   EXIT
  IF (probe .EQ. BANG)  CYCLE
  IF (probe .EQ. BLANK) CYCLE
  
  READ (oneline, *) cn
  CALL toupper(cn)
  
  IF (cn .NE. '*CELL') CYCLE
  
  READ (oneline, *) cdum, cdum, cdum
  
  CALL fndchr(cdum, ipos, nchr, '_')
  
  IF (nchr .LT. 4) CYCLE
  
  ! READ
  MCndat = MCndat + 1
  
  ! iz
  IF (nchr .EQ. 4) THEN
    iz = 1
  ELSE
    lgh = len_trim(cdum)
    
    READ (cdum(ipos(5)+1:lgh), '(I)') iz
  END IF
  
  ! Pos.
  CALL fndchr(cdum, jpos, nchr, '>')
  
  READ (cdum(ipos(1)+1:ipos(2)-1), '(I)') iax
  READ (cdum(ipos(2)+1:jpos(1)-1), '(I)') iay
  READ (cdum(ipos(3)+1:ipos(4)-1), '(I)') ipx
  READ (cdum(ipos(4)+1:jpos(2)-1), '(I)') ipy
  
  ! Pow. + Std. Dev.
  CALL fndchr(oneline, ipos, nchr, '=')
  
  oneline = oneline(ipos(1)+1:)
  
  READ (oneline, *) locpow, cdum, cdum, cdum, tmp
  
  ! CnP
  locdat(MCndat)%pow = locpow
  locdat(MCndat)%iz  = iz
  locdat(MCndat)%iax = iax
  locdat(MCndat)%iay = iay
  locdat(MCndat)%ipx = ipx
  locdat(MCndat)%ipy = ipy
  
  locstd(MCndat) = tmp * rMC * 100._8
  
  ! Process
  nminax = min(nminax, iax)
  nminay = min(nminay, iay)
  nmaxax = max(nmaxax, iax)
  nmaxay = max(nmaxay, iay)
  MCnz   = max(MCnz,   iz)
END DO

2000 CONTINUE

CLOSE (indev)
! ------------------------------------------------
!            03. Post-process
! ------------------------------------------------
IF (nz .NE. MCnz) CALL terminate("MC HGT.")

ndel = min(nminax, nminay) - 1

naRng (iobj) = (max(nmaxax, nmaxay) - min(nminax, nminay) + 2) / 2
nxy   (iobj) = MCndat / nz
MCnPin(iobj) = (locdat(MCndat)%ipx + 1) / 2

!$OMP PARALLEL PRIVATE(idat) NUM_THREADS(numthr)
!$OMP DO SCHEDULE(GUIDED)
DO idat = 1, MCndat
  locdat(idat)%iax = locdat(idat)%iax - ndel
  locdat(idat)%iay = locdat(idat)%iay - ndel
END DO
!$OMP END DO
!$OMP END PARALLEL
! ------------------------------------------------
!            04. Norm.
! ------------------------------------------------
totpow = ZERO

IF (.NOT. l3d) THEN
  DO idat = 1, MCndat
    totpow = totpow + locdat(idat)%pow
  END DO
ELSE
  DO idat = 1, MCndat
    iz = locdat(idat)%iz
    
    totpow = totpow + locdat(idat)%pow * hgt(iz) / avghgt
  END DO
END IF

rnrm = rMC * real(MCndat) / totpow

!$OMP PARALLEL PRIVATE(idat) NUM_THREADS(numthr)
!$OMP DO SCHEDULE(GUIDED)
DO idat = 1, MCndat
  locdat(idat)%pow = locdat(idat)%pow * rnrm ! McCARD Power is Point-wise
END DO
!$OMP END DO
!$OMP END PARALLEL

NULLIFY (locdat)
NULLIFY (locstd)
! ------------------------------------------------

END SUBROUTINE readMC_1st
! --------------------------------------------------------------------------------------------------
SUBROUTINE readMC_2nd(iobj, iinp, rMC, fn)

USE allocs
USE param, ONLY : BLANK, ZERO, DOT, BANG, TRUE, oneline, probe
USE mdat,  ONLY : l3d, indev, hgt, numthr, dat01, dat02, MCstd01, MCstd02, avghgt, ndat, powdata_type

IMPLICIT NONE

INTEGER :: iobj, iinp
REAL :: rMC
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
CHARACTER*10  :: cn
CHARACTER*100 :: gn, cdum

INTEGER :: nchr, MCndat, idat, iz
INTEGER :: ipos(6)
REAL :: totpow, rnrm, tmp

REAL, POINTER, DIMENSION(:) :: tmppow, locstd
TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat
! ------------------------------------------------

IF (iinp .LT. 10) THEN
  WRITE (cn, '(I1)') iinp
  
  gn = 'MC\' // trim(fn) // " 0"  // trim(cn)
ELSE
  WRITE (cn, '(I2)') iinp
  
  gn = 'MC\' // trim(fn) // BLANK // trim(cn)
END IF

CALL openfile(indev, TRUE, gn)
! ------------------------------------------------
MCndat = 0

CALL dmalloc(tmppow, ndat(iobj))

SELECT CASE (iobj)
CASE (1); locdat => dat01; locstd => MCstd01
CASE (2); locdat => dat02; locstd => MCstd02
END SELECT

DO
  READ (indev, '(A512)', END = 3000) oneline
  
  IF (probe .EQ. DOT)   EXIT
  IF (probe .EQ. BANG)  CYCLE
  IF (probe .EQ. BLANK) CYCLE
  
  READ (oneline, *) cn
  CALL toupper(cn)
  
  IF (cn .NE. '*CELL') CYCLE
  
  READ (oneline, *) cdum, cdum, cdum
  
  CALL fndchr(cdum, ipos, nchr, '_')
  
  IF (nchr .LT. 4) CYCLE
  
  ! READ
  MCndat = MCndat + 1
  
  CALL fndchr(oneline, ipos, nchr, '=')
  
  oneline = oneline(ipos(1)+1:)
  
  READ (oneline, *) tmppow(MCndat), cdum, cdum, cdum, tmp
  
  locstd(MCndat) = locstd(MCndat) + tmp * rMC * 100._8
END DO

3000 CONTINUE

CLOSE (indev)

IF (ndat(iobj) .NE. MCndat) CALL terminate("MC 2nd")
! ------------------------------------------------
totpow = ZERO

IF (.NOT. l3d) THEN
  DO idat = 1, MCndat
    totpow = totpow + tmppow(idat)
  END DO
ELSE
  DO idat = 1, MCndat
    iz = locdat(idat)%iz
    
    totpow = totpow + tmppow(idat) * hgt(iz) / avghgt
  END DO
END IF

rnrm = rMC * real(MCndat) / totpow

!$OMP PARALLEL PRIVATE(idat) NUM_THREADS(numthr)
!$OMP DO SCHEDULE(GUIDED)
DO idat = 1, MCndat
  locdat(idat)%pow = locdat(idat)%pow + tmppow(idat) * rnrm
END DO
!$OMP END DO
!$OMP END PARALLEL

DEALLOCATE (tmppow)
NULLIFY (locstd)
NULLIFY (locdat)
! ------------------------------------------------

END SUBROUTINE readMC_2nd
! --------------------------------------------------------------------------------------------------
SUBROUTINE calMCstd(iobj)

USE param, ONLY : ZERO
USE mdat,  ONLY : powdata_type, MCstd01, MCstd02, dat01, dat02, ndat, hgt, avghgt

IMPLICIT NONE

INTEGER :: iobj
! ------------------------------------------------
INTEGER :: iz, idat
REAL :: maxstdrel, rmsstdrel, maxstdabs, rmsstdabs, tmpabs

REAL, POINTER, DIMENSION(:) :: locstd

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat
! ------------------------------------------------

SELECT CASE (iobj)
CASE (1); locdat => dat01; locstd => MCstd01
CASE (2); locdat => dat02; locstd => MCstd02
END SELECT

maxstdrel = ZERO
rmsstdrel = ZERO
maxstdabs = ZERO
rmsstdabs = ZERO

DO idat = 1, ndat(iobj)
  iz = locdat(idat)%iz
  
  maxstdrel = max(maxstdrel, locstd(idat))
  rmsstdrel = rmsstdrel + locstd(idat) * locstd(idat) * hgt(iz) / avghgt
  
  tmpabs = locstd(idat) * locdat(idat)%pow
  
  maxstdabs = max(maxstdabs, tmpabs)
  rmsstdabs = rmsstdabs + tmpabs * tmpabs * hgt(iz) / avghgt
END DO

rmsstdrel = rmsstdrel / real(ndat(iobj))
rmsstdabs = rmsstdabs / real(ndat(iobj))

WRITE (*, '(A31, F5.2, X, A3)') 'MC Power Std. Dev. Max. Rel. : ', maxstdrel, '(%)'
WRITE (*, '(A31, F5.2, X, A3)') 'MC Power Std. Dev. RMS  Rel. : ', rmsstdrel, '(%)'
WRITE (*, '(A31, F5.2, X, A3)') 'MC Power Std. Dev. Max. Abs. : ', maxstdabs, '(%)'
WRITE (*, '(A31, F5.2, X, A3)') 'MC Power Std. Dev. RMS  Abs. : ', rmsstdabs, '(%)'

NULLIFY (locdat)
NULLIFY (locstd)
! ------------------------------------------------

END SUBROUTINE calMCstd
! --------------------------------------------------------------------------------------------------