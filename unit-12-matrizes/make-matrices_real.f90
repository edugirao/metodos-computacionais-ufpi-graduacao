PROGRAM make_mat
IMPLICIT NONE
INTEGER:: i,j,N,M,P
REAL(KIND=8),ALLOCATABLE:: A(:,:),B(:,:)
WRITE(*,*) 'Entre com o número de linhas de A'
READ(*,*) N
WRITE(*,*) 'Entre com o número de colunas de A'
READ(*,*) M
WRITE(*,*) 'Entre com o número de colunas de B'
READ(*,*) P
ALLOCATE(A(N,M),B(M,P))

CALL RANDOM_NUMBER(A)
CALL RANDOM_NUMBER(B)

! Writing unformatted files
WRITE(*,*) 'Writing unformatted files'
OPEN(UNIT=1,FILE='mat_r-a.b.dat',FORM='UNFORMATTED')
WRITE(1) N,M
WRITE(1) A
CLOSE(UNIT=1)

OPEN(UNIT=1,FILE='mat_r-b.b.dat',FORM='UNFORMATTED')
WRITE(1) M,P
WRITE(1) B
CLOSE(UNIT=1)

WRITE(*,*) 'Writing formatted files'
! Writing formatted files
OPEN(UNIT=1,FILE='mat_r-a.dat')
WRITE(1,*) N,M
DO i=1,N
  WRITE(1,*) (A(i,j),j=1,M)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

OPEN(UNIT=1,FILE='mat_r-b.dat')
WRITE(1,*) M,P
DO i=1,M
  WRITE(1,*) (B(i,j),j=1,P)
END DO
WRITE(1,*)
CLOSE(UNIT=1)


END PROGRAM make_mat


