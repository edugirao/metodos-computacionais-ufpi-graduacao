PROGRAM cartao
IMPLICIT NONE
INTEGER:: i
REAL(KIND=8):: g,p,t,delta,f,j,jt

g=1280.0D0 ! Gasto mensal
p=1500.0D0 ! Capacidade de pagamento mensal
t=0.15D0   ! taxa de juros
delta=3000.0D0-1500.0D0 ! Delta do 0-ésimo mês
jt=0.0D0
DO i=1,120
  j=(delta)*(1.0D0+t)
  jt=jt+j
  f=g+j
  delta=f-p! Delta do mês atual
  WRITE(*,*) 'F',f,'J',j,'D',delta
  IF(delta.lt.0.01)THEN
    WRITE(*,*) 'Dívida paga em ',i,'meses.'
    EXIT
  END IF
END DO
WRITE(*,*) 'Juro total ',jt
END PROGRAM cartao
