PROGRAM simple_read
IMPLICIT NONE
REAL(KIND=8):: x,y
INTEGER:: i,ios

OPEN(UNIT=1,FILE='data.dat',STATUS='OLD',IOSTAT=ios)
IF(ios.ne.0)THEN
  STOP 'Arquivo não foi aberto com sucesso.'
END IF
DO i=1,5
  READ(1,*) x,y
  WRITE(*,*) 'f(',x,')=',y
END DO

CLOSE(UNIT=1)

END PROGRAM simple_read
