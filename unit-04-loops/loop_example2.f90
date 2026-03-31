PROGRAM simple_loop
IMPLICIT NONE
INTEGER:: i,j,produto

DO i=1,10
  DO j=1,10
    produto=i*j
    WRITE(*,*) 'Produto de ',i,' e ',j,' é ',produto,'.'
  END DO
END DO

END PROGRAM simple_loop
