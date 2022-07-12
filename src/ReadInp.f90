SUBROUTINE readinp

USE allocs
USE param, ONLY : DOT, BANG, BLANK, SLASH, TRUE, FALSE, ZERO, MP, oneline, probe
USE mdat,  ONLY : l3d, l02, objcn, objfn, ninp, lerr, plotobj, lrel, xstr2d, ystr2d, nsize2d, xstr1d, ystr1d, nsize1d, indev, gcf2d, gca2d, gcf1d, gca1d, nz, hgt, avghgt, zlim

IMPLICIT NONE

CHARACTER*12 :: fn, cn, tmp2

INTEGER :: lgh, fndndata, ndat, idat, nchr
INTEGER :: ipos(2)
LOGICAL :: lext

CHARACTER*30, DIMENSION(100) :: tmp1
! ------------------------------------------------

fn    = 'plotCore.inp'
indev = 42
nz    = 1
l02   = FALSE
zlim  = ZERO

INQUIRE (FILE = fn, EXIST = lext)

IF (.NOT.lext) CALL terminate("FILE DOES NOT EXIST - " // fn)

OPEN (indev, FILE = fn)

DO
  READ (indev, '(A512)', END = 1000) oneline
  
  IF (probe .EQ. DOT)   EXIT
  IF (probe .EQ. BANG)  CYCLE
  IF (probe .EQ. BLANK) CYCLE
  
  READ (oneline, '(A12)') cn
  CALL toupper(cn)
  
  lgh = len(oneline)
  
  SELECT CASE (cn)
  CASE ('ID_01')
    READ  (oneline, *) cn, objcn(1)
    
    CALL fndchr(oneline, ipos, nchr, SLASH)
    
    objfn(1) = oneline(ipos(1)+1:lgh)
    
    CALL rmvremainder(objfn(1))
    
  CASE ('ID_02')
    l02 = TRUE
    
    READ  (oneline, *) cn, objcn(2)
    
    CALL fndchr(oneline, ipos, nchr, SLASH)
    
    objfn(2) = oneline(ipos(1)+1:lgh)
    
    CALL rmvremainder(objfn(2))
    
  CASE ('NUM_01')
    READ (oneline, *) cn, ninp(1)
  
  CASE ('NUM_02')
    READ (oneline, *) cn, ninp(2)
    
  CASE ('PLOT_ERR')
    READ (oneline, *) cn, lerr, plotobj
        
  CASE ('CAL_REL')
    READ (oneline, *) cn, lrel
    
  CASE ('TPOS_1D')
    READ (oneline, *) cn, xstr1d, ystr1d
    
  CASE ('TPOS_2D')
    READ (oneline, *) cn, xstr2d, ystr2d
  
  CASE ('TSIZE_1D')
    READ (oneline, *) cn, nsize1D
    
  CASE ('TSIZE_2D')
    READ (oneline, *) cn, nsize2D
    
  CASE ('GCF_1D')
    READ (oneline, *) cn, gcf1d(1:4)
    
  CASE ('GCF_2D')
    READ (oneline, *) cn, gcf2d(1:4)
    
  CASE ('GCA_1D')
    READ (oneline, *) cn, gca1d(1:4)
    
  CASE ('GCA_2D')
    READ (oneline, *) cn, gca2d(1:4)
    
  CASE ('HGT')
    ndat = fndndata(oneline)-1
    tmp1 = BLANK
    
    READ (oneline, *, END = 500) cn, tmp1
    
    500 CONTINUE
    
    DO idat = 1, 100
      READ (tmp1(idat), '(A)') tmp2
      
      IF (tmp2(1:1).EQ.BLANK .OR. tmp2(1:1).EQ.BANG) EXIT
    END DO
    
    nz = idat-1
    
    CALL dmalloc(hgt, nz)
    
    READ (oneline, *) cn, hgt(1:nz)
    
  CASE ('ZLIM')
    READ (oneline, *) cn, zlim
    
  CASE DEFAULT
    CALL terminate("READ INP")
  END SELECT
END DO

1000 CONTINUE
! ------------------------------------------------
IF (probe .NE. DOT) CALL terminate("INPUT MUST END WITH DOT")

CLOSE (indev)

! CHK : plot mod
IF (.NOT.l02 .AND. plotobj.EQ.2) CALL terminate("WRONG PLOTTING OBJECT")

IF (lerr .AND. objcn(plotobj).EQ.'MC' .AND. objcn(MP(plotobj)).EQ.'NT') CALL terminate("WRONG PLOTTING OBJECT")

! Basic
l3d = nz .GT. 1

IF (.NOT. associated(hgt)) CALL dmalloc1(hgt, 1)

avghgt = sum(hgt(1:nz)) / nz
! ------------------------------------------------

END SUBROUTINE readinp