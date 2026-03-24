PROGRAM simple_if
IMPLICIT NONE
INTEGER:: a,b,c

c=0
WRITE(*,*) 'Entre com dois valores inteiros, a e b:'
READ(*,*) a,b

IF(a.gt.b)THEN
  c=a+b
ELSE IF(a.eq.b)THEN
  c=a*b
ELSE
  c=b-a
END IF
WRITE(*,*) 'c=',c

END PROGRAM simple_if
