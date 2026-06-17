PROGRAM matrix_prod
IMPLICIT NONE
INTEGER:: n,m,p,i,j,l
REAL(KIND=8):: tmp
REAL(KIND=8),ALLOCATABLE:: A(:,:),B(:,:),C(:,:)

! Lendo a matriz A
OPEN(UNIT=1,FILE='mat_r-a.b.dat',FORM='UNFORMATTED')
READ(1) n,m
ALLOCATE(A(n,m))
READ(1) A
CLOSE(UNIT=1)
! Lendo a matriz B
OPEN(UNIT=1,FILE='mat_r-b.b.dat',FORM='UNFORMATTED')
READ(1) m,p
ALLOCATE(B(m,p))
READ(1) B
CLOSE(UNIT=1)

! Produto de matrizes
ALLOCATE(C(n,p))
C=0.0D0
DO j=1,p
  DO l=1,m
    tmp=B(l,j)
    DO i=1,n
      C(i,j)=C(i,j)+A(i,l)*tmp
    END DO
  END DO
END DO

! Escrevendo a matriz C

OPEN(UNIT=1,FILE='mat_r-c.b.dat',FORM='UNFORMATTED')
WRITE(1) n,p
WRITE(1) C
CLOSE(UNIT=1)

! OPEN(UNIT=1,FILE='mat_r-c.dat')
! WRITE(1,*) n,p
! DO i=1,n
!   WRITE(1,*) (C(i,j),j=1,p)
! END DO
! WRITE(1,*)
! CLOSE(UNIT=1)


END PROGRAM matrix_prod


