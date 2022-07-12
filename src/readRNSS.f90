SUBROUTINE readRNSS(iobj, fn)

USE allocs
USE param, ONLY : BANG, BLANK, MAXNPIN, SLASH, ZERO, EPS, TRUE, SQ3, oneline, probe
USE mdat,  ONLY : aoF2F, RNnpin, RNpF2F, lHS, nz, ndat, dat01, dat02, naRng, indev, lrot, powdata_type, nxy

IMPLICIT NONE

INTEGER :: iobj
CHARACTER(*), INTENT(IN) :: fn
! ------------------------------------------------
CHARACTER*1   :: cdum
CHARACTER*10  :: cn
CHARACTER*100 :: gn

LOGICAL :: chknum

INTEGER :: icol, iz, iasy, iax, iay, ipx, jpx, ipy, idat, idum, ix0
INTEGER :: ncol, fndndata, nmaxax, nmaxay, nminax, nminay, npin, nprv, RNnz, RNndat

REAL :: tmpdat(2*MAXNPIN-1)

TYPE (powdata_type), POINTER, DIMENSION(:) :: locdat
! ------------------------------------------------

gn = 'RNSS\' // trim(fn) // '.out'
CALL openfile(indev, TRUE, gn)

! Default
lHS (iobj) = FALSE
lrot(iobj) = FALSE
! ------------------------------------------------
!            01. Basic
! ------------------------------------------------
DO
  READ (indev, '(A512)', END = 1000) oneline
  
  IF (probe .EQ. BANG) CYCLE
  
  CALL toupper(oneline) ! Delte Front Blank
  
  IF (oneline(1:8) .EQ. 'GRID_HEX') THEN
    READ (oneline, *) cn, aoF2F(iobj)
  ELSE IF (oneline(1:27) .EQ. '$  3 PIN POWER DISTRIBUTION') THEN
    DO idat = 1, 4
      READ (indev, *)
    END DO
    
    RNnPin(iobj) = fndndata(oneline)
    RNpF2F(iobj) = 2. * SQ3 * aoF2F(iobj) / 3. / (2*RNnPin(iobj))
  END IF
END DO

1000 CONTINUE

REWIND (indev)
! ------------------------------------------------
!            02. # of data
! ------------------------------------------------
RNnz       = 0
ndat(iobj) = 0

CALL moveline(indev, "$  3 Pin Power Distribution")
READ (indev, *)

DO
  READ (indev, '(A512)', END = 2000) oneline
  
  IF (probe .EQ. BANG) CYCLE
  
  READ (oneline, '(A5)') cn
  CALL toupper(cn)
  
  IF (cn .NE. "PLANE") EXIT
  
  RNnz = RNnz + 1
  
  ! Asy.
  DO
    READ (indev, '(A512)', END = 2000) oneline
    
    IF (probe .EQ. BANG) CYCLE
    
    READ (oneline, '(A8)') cn
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
        IF (abs(tmpdat(icol)) .LT. 1E-7) CYCLE
        
        ndat(iobj) = ndat(iobj) + 1
      END DO
    END DO
    
    ndat(iobj) = ndat(iobj)
  END DO
END DO

2000 CONTINUE

IF (nz .NE. RNnz) CALL terminate("RN nz")

REWIND (indev)

SELECT CASE (iobj)
CASE (1); ALLOCATE (dat01 (ndat(iobj))); locdat => dat01
CASE (2); ALLOCATE (dat02 (ndat(iobj))); locdat => dat02
END SELECT
! ------------------------------------------------
!            03. 3-D Pin Power
! ------------------------------------------------
CALL moveline(indev, "$  3 Pin Power Distribution")
READ (indev, *)

nmaxax = 0
nmaxay = 0
nminax = 100
nminay = 100
RNndat = 0

DO iz = 1, nz
  READ (indev, '(A512)', END = 3000) oneline
  
  IF (probe .EQ. BANG) CYCLE
  
  READ (oneline, '(A5)') cn
  CALL toupper(cn)
  
  IF (cn .NE. "PLANE") EXIT
  
  ! Asy
  DO
    READ (indev, '(A512)', END = 3000) oneline
    
    IF (probe .EQ. BANG) CYCLE
    
    READ (oneline, '(A8)') cn
    CALL toupper(cn)
    
    IF (cn .NE. "ASSEMBLY") THEN
      BACKSPACE (indev)
      
      EXIT
    END IF
    
    READ (oneline, *) cn, idum, cdum, cdum, iax, iay
    
    ! Pin
    nprv = RNndat
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
        
        RNndat = RNndat + 1
        jpx    = ipx + max(0, ipy - npin)
        
        locdat(RNndat)%pow  = tmpdat(ipx)
        locdat(RNndat)%iz   = iz
        locdat(RNndat)%iax  = iax
        locdat(RNndat)%iay  = iay
        locdat(RNndat)%ipx  = jpx
        locdat(RNndat)%ipy  = ipy
      END DO
    END DO
    
    IF (RNndat .EQ. nprv) CYCLE
    
    nmaxax = max(nmaxax, iax)
    nmaxay = max(nmaxay, iay)
    nminax = min(nminax, iax)
    nminay = min(nminay, iay)
  END DO
END DO

3000 CONTINUE

CLOSE (indev)

IF ((nmaxax + nminax) .NE. 0) CALL terminate("ASYMMETRIC")
IF ((nmaxay + nminay) .NE. 0) CALL terminate("ASYMMETRIC")

nxy(iobj) = RNndat / nz
! ------------------------------------------------
!            04. Post-process
! ------------------------------------------------
naRng(iobj) = nmaxay / 2 + 1

DO idat = 1, RNndat
  iax = locdat(idat)%iax / 2
  iay = locdat(idat)%iay / 2
  
  IF (mod(iay, 2) .EQ. 0) THEN
    ix0 = naRng(iobj) + iay / 2
  ELSE
    IF (iay .LT. 0) THEN
      ix0 = naRng(iobj) + (iay - 1) / 2 + 1 ! Right Asy.
    ELSE
      ix0 = naRng(iobj) + (iay + 1) / 2     ! Right Asy.
    END IF
  END IF
  
  IF (mod(iax, 2) .EQ. 0) THEN
    iax = ix0 + iax / 2
  ELSE
    IF (iax .LT. 0) THEN
      iax = ix0 + (iax - 1) / 2
    ELSE
      iax = ix0 + (iax + 1) / 2 - 1
    END IF
  END IF
  
  locdat(idat)%iax = iax
  locdat(idat)%iay = iay + naRng(iobj)
END DO
! ------------------------------------------------
!            05. Norm.
! ------------------------------------------------
! Pin Power is mutiplied with Volume

NULLIFY (locdat)
! ------------------------------------------------

END SUBROUTINE readRNSS