PROGRAM circuit1
IMPLICIT NONE
INTEGER:: IPIV(3),INFO
REAL(KIND=8):: A(3,3),B(3,1)

! Matriz A
A(1,1)=9.0D0
A(1,2)=6.0D0
A(1,3)=0.0D0
A(2,1)=0.0D0
A(2,2)=-6.0D0
A(2,3)=8.0D0
A(3,1)=1.0D0
A(3,2)=-1.0D0
A(3,3)=-1.0D0

! Matriz (vetor) B
B(1,1)=12.0D0
B(2,1)=9.0D0
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


