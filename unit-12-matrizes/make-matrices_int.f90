PROGRAM make_mat
IMPLICIT NONE
INTEGER:: i,j,N,M,P
INTEGER,ALLOCATABLE:: A(:,:),B(:,:)
REAL(KIND=8),ALLOCATABLE:: Aa(:,:),Bb(:,:)
WRITE(*,*) 'Entre com o número de linhas de A'
READ(*,*) N
WRITE(*,*) 'Entre com o número de colunas de A'
READ(*,*) M
WRITE(*,*) 'Entre com o número de colunas de B'
READ(*,*) P
ALLOCATE(A(N,M),B(M,P))
ALLOCATE(Aa(N,M),Bb(M,P))

CALL RANDOM_NUMBER(Aa)
CALL RANDOM_NUMBER(Bb)
Aa=10.0*Aa
Bb=10.0*Bb
A=FLOOR(Aa)
B=FLOOR(Bb)

OPEN(UNIT=1,FILE='mat_i-a.dat')
WRITE(1,*) N,M
DO i=1,N
  WRITE(1,*) (A(i,j),j=1,M)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

OPEN(UNIT=1,FILE='mat_i-b.dat')
WRITE(1,*) M,P
DO i=1,M
  WRITE(1,*) (B(i,j),j=1,P)
END DO
WRITE(1,*)
CLOSE(UNIT=1)



END PROGRAM make_mat


