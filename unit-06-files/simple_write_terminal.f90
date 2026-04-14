PROGRAM simple_write
IMPLICIT NONE
REAL(KIND=8):: x,y
INTEGER:: i

DO i=1,5
  x=2.0D0*i
  y=x*x
  WRITE(6,*) x,y
END DO

END PROGRAM simple_write
