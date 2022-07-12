SUBROUTINE readnTF(iobj, fn)

USE allocs
USE param, ONLY : DOT, BANG, BLANK, MAXNPIN, SLASH, ZERO, EPS, TRUE, oneline, probe
USE mdat,  ONLY : aoF2F, NTnpin, NTpF2F, lHS, nz, ndat, dat01, dat02, naRng, numthr, indev, l3d, lrot, hgt, avghgt, powdata_type, nxy

IMPLICIT NONE

INTEGER :: iobj
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
CHARACTER*1   :: cdum
CHARACTER*10  :: cn
CHARACTER*100 :: gn

LOGICAL :: chknum

INTEGER :: icol, iz, iasy, iax, iay, ipx, jpx, ipy, idat, idum
INTEGER :: natyp, nchr, sym, ncol, fndndata, nmaxax, nmaxay, nminax, nminay, ndel, npin, nprv, NTnz, NTndat
INTEGER :: ipos(5)

REAL :: totpow, rnrm, tmpdat(2*MAXNPIN-1)

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat
! ------------------------------------------------

gn    = 'nTF\' // trim(fn) // '.out'
natyp = 0

CALL openfile(indev, TRUE, gn)
! ------------------------------------------------
!            01. Basic
! ------------------------------------------------
DO
  READ (indev, '(A512)', END = 1000) oneline
  
  IF (probe .EQ. DOT)   EXIT
  IF (probe .EQ. BANG)  CYCLE
  IF (probe .NE. BLANK) CYCLE ! READ : Only Card
  
  READ (oneline, '(A10)') cn
  CALL toupper(cn)
  
  SELECT CASE (cn)
  CASE ('PITCH')
    READ (oneline, *) cn, aoF2F(iobj)
    
  CASE ('ASSEMBLY')
    natyp = natyp + 1
    
    CALL fndchr(oneline, ipos, nchr, SLASH)
    
    IF (nchr .NE. 4) CALL terminate("ASSEMBLY")
    
    READ (oneline(ipos(2)+1:ipos(3)-1), '(I)') NTnPin(natyp, iobj)
    READ (oneline(ipos(3)+1:ipos(4)-1), '(F)') NTpF2F(natyp, iobj)
    
  CASE ('RAD_CONF')
    READ (oneline, *) cn, sym, cn
    
    CALL toupper(cn)
    
    lHS (iobj) = sym .NE. 360
    lrot(iobj) = cn  .EQ. "ROT"
  END SELECT
END DO

1000 CONTINUE

REWIND (indev)
! ------------------------------------------------
!            02. # of data
! ------------------------------------------------
NTnz       = 0
ndat(iobj) = 0

CALL moveline(indev, "- Local Pin Power -")
CALL skipline(indev, 4)

DO
  READ (indev, '(A512)', END = 2000) oneline
  
  IF (probe .EQ. DOT)  EXIT
  IF (probe .EQ. BANG) CYCLE
  
  READ (oneline, '(A9)') cn
  CALL toupper(cn)
  
  IF (cn .NE. "PLANE") EXIT
  
  NTnz = NTnz + 1
  
  ! Asy.
  DO
    READ (indev, '(A512)', END = 2000) oneline
    
    IF (probe .EQ. DOT)   EXIT
    IF (probe .EQ. BANG)  CYCLE
    
    READ (oneline, '(A10)') cn
    CALL toupper(cn)
    
    IF (cn .NE. "ASSEMBLY") THEN
      BACKSPACE (indev)
      
      EXIT
    END IF
    
    ! Pin
    DO
      READ (indev, '(A512)', END = 2000) oneline
      
      IF (.NOT. chknum(oneline)) THEN
        BACKSPACE (indev)
        
        EXIT
      END IF
      
      ncol   = fndndata(oneline)
      tmpdat = ZERO
      
      READ (oneline, *) tmpdat(1:ncol)
      
      DO icol = 1, ncol
        IF (abs(tmpdat(icol)) .LT. 1E-3) CYCLE
        
        ndat(iobj) = ndat(iobj) + 1
      END DO
    END DO
  END DO
END DO

2000 CONTINUE

IF (nz .NE. NTnz) CALL terminate("NT nz")

REWIND (indev)

SELECT CASE (iobj)
CASE (1); ALLOCATE (dat01 (ndat(iobj))); locdat => dat01
CASE (2); ALLOCATE (dat02 (ndat(iobj))); locdat => dat02
END SELECT
! ------------------------------------------------
!            03. 3-D Pin Power
! ------------------------------------------------
CALL moveline(indev, "- Local Pin Power -")
CALL skipline(indev, 4)

nmaxax = 0
nmaxay = 0
nminax = 100
nminay = 100
NTndat = 0

DO iz = 1, nz
  READ (indev, '(A512)', END = 3000) oneline
  
  IF (probe .EQ. DOT)   EXIT
  IF (probe .EQ. BANG)  CYCLE
  
  READ (oneline, '(A9)') cn
  CALL toupper(cn)
  
  IF (cn .NE. "PLANE") EXIT
  
  ! Asy
  DO
    READ (indev, '(A512)', END = 3000) oneline
    
    IF (probe .EQ. DOT)   EXIT
    IF (probe .EQ. BANG)  CYCLE
    
    READ (oneline, '(A10)') cn
    CALL toupper(cn)
    
    IF (cn .NE. "ASSEMBLY") THEN
      BACKSPACE (indev)
      
      EXIT
    END IF
    
    READ (oneline, *) cn, idum, cdum, cdum, iax, iay
    
    ! Pin
    nprv = NTndat
    ipy  = 0
    
    DO
      READ (indev, '(A512)', END = 3000) oneline
      
      IF (.NOT. chknum(oneline)) THEN
        BACKSPACE (indev)
        
        EXIT
      END IF
      
      ipy    = ipy + 1
      ncol   = fndndata(oneline)
      tmpdat = ZERO
      
      READ (oneline, *) tmpdat(1:ncol)
      
      IF (ipy .EQ. 1) npin = ncol
      
      DO ipx = 1, ncol
        IF (abs(tmpdat(ipx)) .LT. 1E-3) CYCLE
        
        NTndat = NTndat + 1
        jpx    = ipx + max(0, ipy - npin)
        
        locdat(NTndat)%pow  = tmpdat(ipx)
        locdat(NTndat)%iz   = iz
        locdat(NTndat)%iax  = iax
        locdat(NTndat)%iay  = iay
        locdat(NTndat)%ipx  = jpx
        locdat(NTndat)%ipy  = ipy
      END DO
    END DO
    
    IF (NTndat .EQ. nprv) CYCLE
    
    nmaxax = max(nmaxax, iax)
    nmaxay = max(nmaxay, iay)
    nminax = min(nminax, iax)
    nminay = min(nminay, iay)
  END DO
END DO

3000 CONTINUE

CLOSE (indev)

nxy(iobj) = NTndat / nz
! ------------------------------------------------
!            04. Post-process
! ------------------------------------------------
IF (.NOT. lHS(iobj)) THEN
  naRng(iobj) = (max(nmaxax, nmaxay) - min(nminax, nminay) + 2) / 2
  ndel        = min(nminax, nminay) - 1
  
  !$OMP PARALLEL PRIVATE(idat) NUM_THREADS(numthr)
  !$OMP DO SCHEDULE(GUIDED)
  DO idat = 1, NTndat
    locdat(idat)%iax = locdat(idat)%iax - ndel
    locdat(idat)%iay = locdat(idat)%iay - ndel
  END DO
  !$OMP END DO
  !$OMP END PARALLEL
ELSE
  naRng(iobj) = max(nmaxax, nmaxay)
  ndel        = naRng(iobj) - 1
  
  !$OMP PARALLEL PRIVATE(idat) NUM_THREADS(numthr)
  !$OMP DO SCHEDULE(GUIDED)
  DO idat = 1, NTndat
    locdat(idat)%iax = locdat(idat)%iax + ndel
    locdat(idat)%iay = locdat(idat)%iay + ndel
  END DO
  !$OMP END DO
  !$OMP END PARALLEL
END IF
! ------------------------------------------------
!            05. Norm.
! ------------------------------------------------
totpow = ZERO

DO idat = 1, NTndat
  totpow = totpow + locdat(idat)%pow
END DO

rnrm = real(NTndat) / totpow

IF (.NOT. l3d) THEN
  !$OMP PARALLEL PRIVATE(idat) NUM_THREADS(numthr)
  !$OMP DO SCHEDULE(GUIDED)
  DO idat = 1, NTndat
    locdat(idat)%pow = locdat(idat)%pow * rnrm
  END DO
  !$OMP END DO
  !$OMP END PARALLEL
ELSE
  !$OMP PARALLEL PRIVATE(idat, iz) NUM_THREADS(numthr)
  !$OMP DO SCHEDULE(GUIDED)
  DO idat = 1, NTndat
    iz = locdat(idat)%iz
    
    locdat(idat)%pow = locdat(idat)%pow * rnrm * avghgt / hgt(iz) ! nTF Power is Volume-wise
  END DO
  !$OMP END DO
  !$OMP END PARALLEL
END IF

NULLIFY (locdat)
! ------------------------------------------------

END SUBROUTINE readnTF