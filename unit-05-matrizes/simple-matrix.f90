PROGRAM simple_matrix
IMPLICIT NONE
REAL(KIND=8):: a(3,3),maior,menor,soma
INTEGER:: i,j,ijloc(2)

a(1,1)=0.0D0
a(1,2)=3.0D0
a(1,3)=2.0D0
a(2,1)=4.0D0
a(2,2)=5.0D0
a(2,3)=a(2,1)+a(2,2)
a(3,1)=-1.0D0
a(3,2)=-2.0D0
a(3,3)=-3.0D0

! WRITE(*,*) (a(1,i),i=1,3)
! WRITE(*,*) (a(2,i),i=1,3)
! WRITE(*,*) (a(3,i),i=1,3)
DO i=1,3
  WRITE(*,*) (a(i,j),j=1,3)
END DO


maior=MAXVAL(a)
WRITE(*,*) 'Maior valor:',maior

menor=MINVAL(a)
WRITE(*,*) 'Menor valor:',menor

ijloc=MINLOC(a)
WRITE(*,*) 'Índices do menor valor:'
WRITE(*,*) ijloc(1),ijloc(2)

ijloc=MAXLOC(a)
WRITE(*,*) 'Índices do maior valor:'
WRITE(*,*) ijloc(1),ijloc(2)

soma=SUM(a)
WRITE(*,*) 'Soma dos elementos:',soma


a=TRANSPOSE(a)
DO i=1,3
  WRITE(*,*) (a(i,j),j=1,3)
END DO

END PROGRAM simple_matrix

