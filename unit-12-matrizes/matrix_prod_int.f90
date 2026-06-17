PROGRAM matrix_prod
IMPLICIT NONE
INTEGER:: n,m,p,i,j,l
INTEGER,ALLOCATABLE:: A(:,:),B(:,:),C(:,:)

! Lendo a matriz A
OPEN(UNIT=1,FILE='mat_i-a.dat')
READ(1,*) n,m
ALLOCATE(A(n,m))
DO i=1,n
  READ(1,*) (A(i,j),j=1,m)
END DO
CLOSE(UNIT=1)
! Lendo a matriz B
OPEN(UNIT=1,FILE='mat_i-b.dat')
READ(1,*) m,p
ALLOCATE(B(m,p))
DO i=1,m
  READ(1,*) (B(i,j),j=1,p)
END DO
CLOSE(UNIT=1)

! Produto de matrizes
ALLOCATE(C(n,p))
DO i=1,n
  DO j=1,p
    C(i,j)=0
    DO l=1,m
      C(i,j)=C(i,j)+A(i,l)*B(l,j)
    END DO
  END DO
END DO

! Escrevendo a matriz C
OPEN(UNIT=1,FILE='mat_i-c.dat')
WRITE(1,*) n,p
DO i=1,n
  WRITE(1,*) (C(i,j),j=1,p)
END DO
WRITE(1,*)
CLOSE(UNIT=1)


END PROGRAM matrix_prod


