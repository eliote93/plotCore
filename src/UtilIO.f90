! --------------------------------------------------------------------------------------------------
!                                   01. Terminate
! --------------------------------------------------------------------------------------------------
SUBROUTINE terminate(errmesg)

IMPLICIT NONE

CHARACTER   :: errmesg*(*)
CHARACTER*6 :: stars
! ------------------------------------------------

DATA stars/'#####'/

PRINT '(A, A)', stars, errmesg
PRINT '(A, A)', stars, "Abnormal Termination"

PAUSE
STOP
! ------------------------------------------------

END SUBROUTINE terminate
! --------------------------------------------------------------------------------------------------
!                                   02. RMV : remainder
! --------------------------------------------------------------------------------------------------
SUBROUTINE rmvremainder(aa)
! REMOVE : Blanks at head & Tale

USE param, ONLY : BLANK

IMPLICIT NONE
  
INTEGER, PARAMETER :: INDXA = 97
INTEGER, PARAMETER :: IDNXZ = 122

CHARACTER :: aa*(*)

INTEGER :: lgh1, lgh2, icol, ist, ied
! ------------------------------------------------

lgh1 = len_trim(aa)

DO icol = 1, lgh1
  IF (aa(icol:icol) .EQ. BLANK) CYCLE
  
  EXIT
END DO

ist = icol

DO icol = lgh1, 1, -1
  IF (aa(icol:icol) .EQ. BLANK) CYCLE
  
  EXIT
END DO

ied  = icol
lgh2 = ied - ist + 1

aa(1:lgh2) = aa(ist:ied)
aa(lgh2+1:) = ''
aa = trim(aa)
! ------------------------------------------------

END SUBROUTINE rmvremainder
! --------------------------------------------------------------------------------------------------
!                                   03. to upper
! --------------------------------------------------------------------------------------------------
SUBROUTINE toupper(aa)
! CONVERT : lower case string to upper case

USE param, ONLY : BLANK

IMPLICIT NONE
  
INTEGER, PARAMETER :: INDXA = 97
INTEGER, PARAMETER :: IDNXZ = 122

CHARACTER :: aa*(*)

INTEGER :: icol, ia
! ------------------------------------------------

DO icol = 1, len_trim(aa)
  IF (aa(icol:icol) .EQ. BLANK) CYCLE
  
  ia = ichar(aa(icol:icol))
  
  IF (ia .GE. INDXA) aa(icol:icol) = char(ia - 32)
END DO

CALL rmvremainder(aa)
! ------------------------------------------------

END SUBROUTINE toupper
! --------------------------------------------------------------------------------------------------
!                                   04. FIND : Character
! --------------------------------------------------------------------------------------------------
SUBROUTINE fndchr(aa, ipos, nchar, onec)

USE param, ONLY : BANG

IMPLICIT NONE

CHARACTER(*), INTENT(IN) :: aa
CHARACTER(1) :: onec
INTEGER, DIMENSION(*) :: ipos

INTEGER :: nchar, nstr, istr
! ------------------------------------------------

nchar = 0
nstr  = len_trim(aa)

DO istr = 1,nstr
  IF (aa(istr:istr) .EQ. BANG) EXIT
  
  IF (aa(istr:istr) .EQ. onec) THEN
    nchar = nchar + 1
    
    ipos(nchar) = istr
  END IF
END DO

ipos(nchar+1) = nstr + 1
! ------------------------------------------------

END SUBROUTINE fndchr
! --------------------------------------------------------------------------------------------------
!                                   05. SKIP
! --------------------------------------------------------------------------------------------------
SUBROUTINE skipline(indev, nline)
! SKIP 'lin' lines from reading

IMPLICIT NONE

INTEGER :: indev, nline, iline
CHARACTER :: tmp*80
! ------------------------------------------------

DO iline = 1, nline
    READ (indev, '(A)') tmp
END DO
! ------------------------------------------------

END SUBROUTINE skipline
! --------------------------------------------------------------------------------------------------
!                                   06. mvline
! --------------------------------------------------------------------------------------------------
SUBROUTINE moveline(indev, aa)

USE param, ONLY : TRUE, FALSE, oneline

IMPLICIT NONE

CHARACTER(*), INTENT(IN) :: aa

INTEGER :: indev
LOGICAL :: led
! ------------------------------------------------

led = TRUE

DO
  READ (indev, '(A512)', END = 1000) oneline
  
  oneline = trim(oneline)
  
  IF (oneline .NE. aa) CYCLE
  
  led = FALSE
  
  EXIT
END DO

1000 CONTINUE

IF (led) CALL terminate("MOVE LINE : " // aa)
! ------------------------------------------------

END SUBROUTINE moveline
! --------------------------------------------------------------------------------------------------
!                                   07. CHK : numeric
! --------------------------------------------------------------------------------------------------
FUNCTION chknum(aa)

USE param, ONLY : BLANK, TRUE, FALSE

IMPLICIT NONE

CHARACTER :: aa*(*)
LOGICAL   :: chknum

INTEGER :: icol, iascii
! ------------------------------------------------

chknum = FALSE

DO icol = 1, len_trim(aa)
  IF (aa(icol:icol) .EQ. BLANK) CYCLE
  
  
  iascii = ichar(aa(icol:icol))
  
  IF ((iascii .NE. 9) .AND. ((iascii-48)*(iascii-57) .LE. 0)) chknum = TRUE
  
  RETURN
END DO
! ------------------------------------------------

END FUNCTION chknum
! --------------------------------------------------------------------------------------------------
!                                   08. FIND : # of data
! --------------------------------------------------------------------------------------------------
FUNCTION fndndata(aa)

USE param, ONLY : BLANK

IMPLICIT NONE

INTEGER :: fndndata

CHARACTER :: aa*(*)

INTEGER :: nblk, icol, jcol
LOGICAL :: lchk1, lchk2
! ------------------------------------------------

nblk = 0

DO icol = len_trim(aa)-1, 1, -1
  jcol = icol + 1
  
  lchk1 = aa(jcol:jcol) .NE. BLANK
  lchk2 = aa(icol:icol) .EQ. BLANK
  
  IF (lchk1 .AND. lchk2) nblk = nblk + 1
END DO

IF (aa(1:1) .EQ. BLANK) THEN
  fndndata = nblk
ELSE
  fndndata = nblk + 1
END IF
! ------------------------------------------------

END FUNCTION fndndata
! --------------------------------------------------------------------------------------------------
!                                   09. OPEN : file
! --------------------------------------------------------------------------------------------------
SUBROUTINE openfile(indev, lread, fn)

USE mdat, ONLY : fdir

IMPLICIT NONE

CHARACTER(*), INTENT(IN) :: fn

CHARACTER*10   :: fstt
CHARACTER*1000 :: gn

INTEGER :: indev
LOGICAL :: lread, lext
! ------------------------------------------------

IF (lread) THEN
  fstt = 'old'
ELSE
  fstt = 'unknown'
END IF

gn = trim(fdir) // trim(fn)

INQUIRE (FILE = gn, EXIST = lext)

IF (lread .AND. .NOT.lext) CALL terminate("FILE DOES NOT EXIST - " // gn)

OPEN (indev, FILE = gn, STATUS = fstt)
! ------------------------------------------------

END SUBROUTINE openfile
! --------------------------------------------------------------------------------------------------
!                                   10. ECHO : inp
! --------------------------------------------------------------------------------------------------
SUBROUTINE echoinp(indev)

USE param, ONLY : DOT, oneline, probe

IMPLICIT NONE

INTEGER :: indev, echodev
! ------------------------------------------------

echodev = 29

OPEN (echodev, FILE = 'plotCore.inp')

DO
  READ (echodev, '(A512)') oneline
  
  WRITE (indev, '(A512)') oneline
  
  IF (probe .EQ. DOT)   EXIT
END DO

CLOSE (echodev)
! ------------------------------------------------

END SUBROUTINE echoinp
! --------------------------------------------------------------------------------------------------
!                                   11. READ : obj
! --------------------------------------------------------------------------------------------------
SUBROUTINE readobj(iobj)

USE mdat, ONLY : l02, objcn, ninp, objfn

IMPLICIT NONE

INTEGER :: iobj

IF (iobj.EQ.2 .AND. .NOT.l02) RETURN

SELECT CASE (objcn(iobj))
CASE ('NT');  CALL readnTF(iobj, objfn(iobj))
CASE ('MC');  CALL readMC(iobj, ninp(iobj), objfn(iobj))
CASE ('RN');  CALL readRNSS(iobj, objfn(iobj))
CASE DEFAULT; CALL terminate("WRONG CODE NAME")
END SELECT

END SUBROUTINE readobj
! --------------------------------------------------------------------------------------------------