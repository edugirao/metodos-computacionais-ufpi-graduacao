PROGRAM halley
IMPLICIT NONE
INTEGER:: year,delta,i

year=1986
delta=76

DO i=1,10
  year=year+delta
  WRITE(*,*) 'Pŕoxima aparição:',year
END DO

END PROGRAM halley
