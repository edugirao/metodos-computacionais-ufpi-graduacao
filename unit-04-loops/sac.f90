PROGRAM sac
IMPLICIT NONE
INTEGER:: n,i
REAL(KIND=8):: a,p,v,e,t,d,m,j,jt

! Dados iniciais
WRITE(*,*) 'Entre com o valor total do imóvel.'
READ(*,*) v
WRITE(*,*) 'Entre com o valor de entrada.'
READ(*,*) e
WRITE(*,*) 'Entre com a taxa de juros mensal.'
READ(*,*) t
WRITE(*,*) 'Entre com o prazo em meses.'
READ(*,*) n
! Valor financiado
d=v-e
WRITE(*,*) 'Valor financiado:',d
! Valor da amortização
a=d/n

! Calculando as parcelas
jt=0.0D0
DO i=1,n
  m=d-(i-1)*a
  j=m*t
  jt=jt+j
  p=a+j
  WRITE(*,*) 'Mês ',i,'P',p,'A',a,'J',j
END DO
WRITE(*,*) 'Juro total:',jt




END PROGRAM sac
