PROGRAM fixed_point_iteration
IMPLICIT NONE
INTEGER:: contador
REAL(KIND=8):: x,tol,f,h,df
! O problema
  ! f(x)=x-DCOS(x)=0 -> x=DCOS(x)
  ! f(x) tem uma raiz entre 0 e pi
! Valor inicial de busca
x=0.0D0
! Loop de busca
contador=0
tol=1.0D-10
DO 
  contador=contador+1
  f=x-DCOS(x)
  IF(ABS(f).lt.tol)THEN
    EXIT
  ELSE
    x=DCOS(x)
  END IF
END DO
WRITE(*,'(A,1X,I0,1X,A)') 'Solução encontrada em',contador,'passos.'
WRITE(*,'(A,F0.12)') 'x==',x
WRITE(*,'(A,F0.12)') 'fx=',f
  
END PROGRAM fixed_point_iteration
