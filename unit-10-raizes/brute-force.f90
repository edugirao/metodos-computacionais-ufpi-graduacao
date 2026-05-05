PROGRAM brute_force
IMPLICIT NONE
INTEGER:: n,i,contador
REAL(KIND=8):: a,b,h,x,f,tol
LOGICAL:: solution
! O problema
  ! f(x)=x-DCOS(x)=0 -> x=DCOS(x)
  ! f(x) tem uma raiz entre 0 e pi
! Grid
a=0.0D0
b=DACOS(-1.0D0)
n=1000000
h=(b-a)/DFLOAT(n-1)
! Tolerância
tol=1.0D-5
! Testando na força bruta
contador=0
solution=.false.
DO i=1,n
  contador=contador+1
  x=a+DFLOAT(i-1)*h
  f=x-DCOS(x)
  IF(ABS(f).lt.tol)THEN
    solution=.true.
    EXIT
  END IF
END DO
IF(solution)THEN
  WRITE(*,'(A,1X,I0,1X,A)') 'Solução encontrada em',contador,'passos.'
  WRITE(*,'(A,F0.7)') 'x=',x
END IF

END PROGRAM brute_force
