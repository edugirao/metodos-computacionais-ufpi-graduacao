PROGRAM simple_loop
IMPLICIT NONE
INTEGER:: i,quadrado

DO i=1,12
  quadrado=i**2
  WRITE(*,*) 'O quadrado de ',i,' é igual a',quadrado,'.'
END DO


END PROGRAM simple_loop
