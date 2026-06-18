PROGRAM circuit1
IMPLICIT NONE
INTEGER:: IPIV(3),INFO
REAL(KIND=8):: r1,r2,r3,e1,e2
REAL(KIND=8):: A(3,3),B(3,1)

! Resistências
WRITE(*,*) 'Forneça as resistências R1, R2 e R3:'
WRITE(*,*) 'R1='
READ(*,*) r1
WRITE(*,*) 'R2='
READ(*,*) r2
WRITE(*,*) 'R3='
READ(*,*) r3
! Forças eletromotrizes
WRITE(*,*) 'Forneça as F.E.M.s E1 e E2:'
WRITE(*,*) 'E1='
READ(*,*) e1
WRITE(*,*) 'E2='
READ(*,*) e2

! Matriz A
A(1,1)=r1
A(1,2)=0.0D0
A(1,3)=r3
A(2,1)=0.0D0
A(2,2)=r2
A(2,3)=r3
A(3,1)=1.0D0
A(3,2)=1.0D0
A(3,3)=-1.0D0

! Matriz (vetor) B
B(1,1)=e1
B(2,1)=e2
B(3,1)=0.0D0

! Resolvendo o sistema 
CALL DGESV(3,1,A,3,IPIV,B,3,INFO)
IF(INFO.ne.0)THEN
  STOP 'DGESV não resolveu o problema.'
END IF

! Imprimir os resultados
WRITE(*,*) 'I1=',B(1,1)
WRITE(*,*) 'I2=',B(2,1)
WRITE(*,*) 'I3=',B(3,1)



END PROGRAM circuit1


