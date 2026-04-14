PROGRAM simple_write
IMPLICIT NONE
REAL(KIND=8):: x,y
INTEGER:: i

OPEN(UNIT=2,FILE='data-w.dat',STATUS='NEW')

DO i=1,5
  x=2.0D0*i
  y=x*x
  WRITE(2,*) x,y
END DO

CLOSE(UNIT=2)

END PROGRAM simple_write
