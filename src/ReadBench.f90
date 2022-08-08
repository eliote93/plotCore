SUBROUTINE readbench(oneline)

USE allocs
USE mdat,  ONLY : l3d, xstr2d, ystr2d, nsize2d, gcf2d, gca2d, xstr1d, ystr1d, nsize1d, gcf1d, gca1d, nz, hgt

IMPLICIT NONE

CHARACTER*512 :: oneline
CHARACTER*20 :: cn, tmp
! ------------------------------------------------

READ (oneline, *) cn, tmp
CALL toupper(tmp)

! Rad.
IF (tmp(1:3) .EQ. 'RAD') THEN
  IF (tmp(5:17) .EQ. 'C5G7_H_HS_ERR') THEN
    xstr2d  =  35
    ystr2d  = -40
    nsize2d =  30
    
    gcf2d = [500, 100, 1050, 810]
    gca2d = [0.12, 0.04, 0.75, 1.05]
  ELSE IF (tmp(5:17) .EQ. 'C5G7_H_FS_ERR') THEN
    xstr2d  =  33
    ystr2d  = -40
    nsize2d =  20
    
    gcf2d = [500, 100, 1050, 820]
    gca2d = [0.115, 0.035, 0.755, 1.05]
  ELSE IF (tmp(5:16) .EQ. 'C5G7_H_FS_MC') THEN
    xstr2d  =  35
    ystr2d  = -40
    nsize2d =  20
    
    gcf2d = [500, 100, 1080, 840]
    gca2d = [0.11, 0.035, 0.75, 1.05]
  ELSE IF (tmp(5:16) .EQ. 'V-4.4_HS_ERR') THEN
    xstr2d  =  100
    ystr2d  = -105
    nsize2d =   20
    
    gcf2d = [500, 100, 1085, 860]
    gca2d = [0.13, 0.11, 0.72, 0.9]
  ELSE IF (tmp(5:16) .EQ. 'V-4.4_FS_ERR') THEN
    xstr2d  =  100
    ystr2d  = -120
    nsize2d =   17
    
    gcf2d = [500, 100, 1050, 830]
    gca2d = [0.135, 0.11, 0.735, 0.9]
  ELSE IF (tmp(5:15) .EQ. 'V-10_HS_ERR') THEN
    xstr2d  =  115
    ystr2d  = -120
    nsize2d =   30
    
    gcf2d = [500, 100, 1050, 830]
    gca2d = [0.135, 0.11, 0.735, 0.9]
  ELSE IF (tmp(5:15) .EQ. 'V-10_FS_ERR') THEN
    xstr2d  =  120
    ystr2d  = -130
    nsize2d =   15
    
    gcf2d = [500, 100, 1050, 830]
    gca2d = [0.135, 0.11, 0.735, 0.9]
  ELSE IF (tmp(5:14) .EQ. 'V-10_FS_MC') THEN
    xstr2d  =  105
    ystr2d  = -120
    nsize2d =   25
    
    gcf2d = [500, 100, 1100, 830]
    gca2d = [0.13, 0.13, 0.74, 0.85]
  ELSE IF (tmp(5:7) .EQ. 'BCN') THEN
    xstr2d  =  40
    ystr2d  = -55
    nsize2d =  30
    
    gcf2d = [500, 100, 1100, 820]
    gca2d = [0.13, 0.13, 0.74, 0.85]
  END IF
! ------------------------------------------------
! Ax.
ELSE IF (tmp(1:2) .EQ. 'AX') THEN
  nsize1d = 30
  
  IF (tmp(4:9) .EQ. 'C5G7_H') THEN
    nz = 24
    CALL dmalloc(hgt, nz)
    hgt(1:nz) = 1.785
    
    gcf1d = [500, 100, 1100, 730]
    
    IF (tmp(11:13) .EQ. 'ERR') THEN
      xstr1d = 30
      ystr1d =  0.
      
      gca1d = [0.13, 0.17, 0.85, 0.8]
    ELSE IF (tmp(11:12) .EQ. 'MC') THEN
      xstr1d = 5
      ystr1d = 0.5
      
      gca1d = [0.11, 0.16, 0.88, 0.84]
    END IF
  ELSE IF (tmp(4:11) .EQ. 'V-10_ORG') THEN
    IF (tmp(9:11) .EQ. 'ORG') THEN
      nz = 28
      CALL dmalloc(hgt, nz)
      hgt(1:nz) = [20.8, 3., 22.5, 3., 22.5, 3., 22.5, 3., 22.5, 3., 22.5, 3., 22.5, 3., 22.5, 3., 22.5, 3., 22.5, 3., 22.5, 3., 22.5, 3., 22.5, 3., 11.6, 11.6]
    ELSE IF (tmp(9:11) .EQ. 'SMP') THEN
      nz = 18
      CALL dmalloc(hgt, nz)
      hgt(1:nz) = 19.61111111111111
    END IF
    
    gcf1d = [500, 100, 1100, 730]
    
    IF (tmp(13:15) .EQ. 'ERR') THEN
      xstr1d = 30
      ystr1d =  0.
      
      gca1d = [0.13, 0.17, 0.85, 0.8]
    ELSE IF (tmp(13:14) .EQ. 'MC') THEN
      xstr1d = 10
      ystr1d =  2.5
      
      gca1d = [0.105, 0.16, 0.87, 0.83]
    END IF
  ELSE IF (tmp(4:6) .EQ. 'BCN') THEN
    nz = 20
    CALL dmalloc(hgt, nz)
    hgt(1:nz) = 4.
    
    gcf1d = [500, 100, 1050, 730]
    
    IF (tmp(8:10) .EQ. 'ERR') THEN
      xstr1d = 10
      ystr1d =  0.
      
      gca1d = [0.125, 0.17, 0.85, 0.8]
    ELSE IF (tmp(8:10) .EQ. 'POW') THEN
      xstr1d = 10
      ystr1d =  0.2
      
      gca1d = [0.11, 0.16, 0.86, 0.82]
    END IF
  END IF
END IF
! ------------------------------------------------

END SUBROUTINE readbench