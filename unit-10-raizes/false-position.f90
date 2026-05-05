PROGRAM false_position
IMPLICIT NONE
INTEGER:: contador
REAL(KIND=8):: x1,x2,xm,tol,f1,f2,fm
! O problema
  ! f(x)=x-DCOS(x)=0 -> x=DCOS(x)
  ! f(x) tem uma raiz entre 0 e pi
! Intervalo inicial de busca
x1=0.0D0
x2=DACOS(-1.0D0)
f1=x1-DCOS(x1)
f2=x2-DCOS(x2)
! Busca por falsa posição
contador=0
tol=1.0D-10
DO 
  ! Atualizar o contador
  contador=contador+1
  ! x médio
  xm=(x1*f2-x2*f1)/(f2-f1)
  ! f(xm)
  fm=xm-DCOS(xm)
  ! Teste de tolerância
  IF(ABS(fm).lt.tol)THEN
    ! Solução encontrada
    EXIT
  ELSE
    ! Preparar o próximo passo
    IF(f1*fm.gt.0.0D0)THEN
      x1=xm
      f1=fm
    ELSE
      x2=xm
      f2=fm
    END IF
  END IF
END DO
WRITE(*,'(A,1X,I0,1X,A)') 'Solução encontrada em',contador,'passos.'
WRITE(*,'(A,F0.12)') 'x==',xm
WRITE(*,'(A,F0.12)') 'fx=',fm
  
END PROGRAM false_position
