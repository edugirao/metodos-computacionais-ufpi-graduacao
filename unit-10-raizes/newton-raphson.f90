PROGRAM newton_raphson
IMPLICIT NONE
INTEGER:: contador
REAL(KIND=8):: x,tol,f,h,df
! O problema
  ! f(x)=x-DCOS(x)=0 -> x=DCOS(x)
  ! f(x) tem uma raiz entre 0 e pi
! Valor inicial de busca
x=0.0D0
! Busca por Newton-Raphson
tol=1.0D-10
contador=0
h=1.0D-5
DO
  ! Atualizando o contador
  contador=contador+1
  ! Calculando a função tentativa
  f=x-DCOS(x)
  ! Teste
  IF(ABS(f).lt.tol)THEN
    ! Saída caso solução encontrada
    EXIT
  ELSE
    ! Atualizando a tentativa
    df=((x+h-DCOS(x+h))-(x-h-DCOS(x-h)))/(h+h)
    x=x-f/df
  END IF
END DO
WRITE(*,'(A,1X,I0,1X,A)') 'Solução encontrada em',contador,'passos.'
WRITE(*,'(A,F0.12)') 'x==',x
WRITE(*,'(A,F0.12)') 'fx=',f
  
END PROGRAM 
