PROGRAM simple_read
IMPLICIT NONE
REAL(KIND=8):: x,y
INTEGER:: i,ios

DO i=1,5
  READ(5,*) x,y
  WRITE(*,*) 'f(',x,')=',y
END DO

END PROGRAM simple_read
